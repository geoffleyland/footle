use std::cell::OnceCell;
use std::collections::HashMap;
use std::fmt;
use std::mem;

use typed_arena::Arena;
use enumset::EnumSet;
use bit_set::BitSet;

use crate::core::BinaryOperator;
use crate::vir;
use super::machine;

//-------------------------------------------------------------------------------------------------

#[derive(Debug, Clone, Copy)]
enum Operand<'arena> {
    Constant(usize),
    Value(&'arena Value<'arena>),
}

impl<'arena> Operand<'arena> {
    fn value(&self) -> Option<&'arena Value<'arena>> {
        if let Self::Value(v) = self { Some(v) } else { None }
    }
    fn needs_scheduling(&self) -> bool {
        matches!(self, Self::Value(v) if matches!(v.def, ValueDef::Instr(..)))
    }
}

impl fmt::Display for Operand<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Operand::*;
        match self {
            Value(v)                        => write!(f, "I{}", v.address),
            Constant(i)                     => write!(f, "K{i}"),
        }
    }
}


//-------------------------------------------------------------------------------------------------

#[derive(Debug, Clone)]
enum ValueDef {
    Instr(&'static machine::Code),
    Argument
}

impl ValueDef {
    fn instr(
        code:                               &'static machine::Code) -> Self {
        Self::Instr(code)
    }
}

impl fmt::Display for ValueDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ValueDef::*;
        match self {
            Instr(code)                     => write!(f, "{}", code.name),
            Argument                        => write!(f, "ARGUMENT"),
        }
    }
}


//-------------------------------------------------------------------------------------------------

#[derive(Debug)]
struct Value<'arena> {
    address:                                usize,
    def:                                    ValueDef,
    operands:                               Vec<Operand<'arena>>,
    operand_registers:                      Option<Vec<u8>>,
    entry_register:                         OnceCell<u8>,
    exit_register:                          OnceCell<u8>,
    register:                               OnceCell<u8>,

    // span:                                   Span,
}

impl<'arena> Value<'arena> {
    fn new(
        address:                            usize,
        def:                                ValueDef,
        operands:                           Vec<Operand<'arena>>,
        operand_registers:                  Option<Vec<u8>>) -> Self {
        Self { address, def, operands, operand_registers,
            entry_register:                 OnceCell::new(),
            exit_register:                  OnceCell::new(),
            register:                       OnceCell::new(),
        }
    }


    fn code(&self) -> Option<&'static machine::Code> {
        if let ValueDef::Instr(code) = &self.def { Some(code) } else { None }
    }

    fn set_entry_register(&self, reg: u8)   { self.entry_register.set(reg).expect("Internal Compiler Error: entry register set twice"); }
    fn set_exit_register(&self, reg: u8)    { self.exit_register.set(reg).expect("Internal Compiler Error: exit register set twice"); }

    fn latency(&self) -> u8                 { self.code().map_or(0, |c| c.latency) }
    fn has_output(&self) -> bool            { self.code().is_some_and(|c| c.has_output) }
}

impl fmt::Display for Value<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "I{}: {} {}", self.address, self.def,
            self.operands
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(" "))

    }
}


//-------------------------------------------------------------------------------------------------

struct Block<'arena> {
    values:                                 Vec<&'arena Value<'arena>>,
    constants:                              Vec<f64>,
    operand_map:                            HashMap<usize, Operand<'arena>>,
}

impl Block<'_> {
    fn new() -> Self {
        Self { values: vec![], constants: vec![], operand_map: HashMap::new() }
    }
}


//-------------------------------------------------------------------------------------------------

pub fn run(block: &vir::Block) {
    let arena = Arena::<Value>::new();
    let Block { values, constants, .. } = emit_instrs(&arena, block);
    let code = schedule_instrs(&values);
    allocate_registers(&values, &code);

    for (i, c) in constants.iter().enumerate() { println!("K{i}: {c}"); }
    for v in &code {
        println!("{v} ({}{})",
            v.register.get().map_or_else(String::new, |r| format!("r{r}<-")),
            v.operands.iter().map(|o| match o {
                Operand::Value(v) => format!("r{}", v.register.get().unwrap()),
                Operand::Constant(i) => format!("K{i}"),
            }).collect::<Vec<_>>().join(" "));
    }

    emit_machine_code(&code, &constants);
}


//-------------------------------------------------------------------------------------------------
// Generate instructions

fn emit_instrs<'arena>(arena: &'arena Arena<Value<'arena>>, input: &vir::Block) -> Block<'arena> {
    let mut block = Block::new();
    for (i, arg) in input.arguments().iter().enumerate() {
        let op = emit_expr(arena, &mut block, arg);
        if let Some(v) = op.value() { v.set_entry_register(u8::try_from(i).unwrap()); }
    }
    for stmt in input.stmts() {
        match &stmt.kind {
            vir::StmtKind::Return(exprs) => {
                let operands =
                    exprs.iter().enumerate().map(|(i, expr)| {
                        let op = emit_expr(arena, &mut block, expr);
                        if let Some(v) = op.value() { v.set_exit_register(u8::try_from(i).unwrap()); }
                        op
                    }).collect::<Vec<_>>();

                // RETURN doesn't produce a value you can use, so add it to the instructions, but
                // not to the operand_map.
                let operand_registers = (0..u8::try_from(operands.len()).unwrap()).collect::<Vec<u8>>();
                let ret = arena.alloc(Value::new(
                    block.values.len(),
                    ValueDef::instr(&machine::RET),
                    operands,
                    Some(operand_registers)));
                block.values.push(ret);
            }
        }
    }
    block
}


fn emit_expr<'arena>(
    arena:                                  &'arena Arena<Value<'arena>>,
    block:                                  &mut Block<'arena>,
    expr:                                   &vir::Expr) -> Operand<'arena> {
    if let Some(&operand) = block.operand_map.get(&expr.pool_index()) {
        operand
    } else {
        match expr.kind() {
            vir::ExprKind::Argument(..) => {
                insert_value(arena, block, expr.pool_index(), vec![], ValueDef::Argument)
            }
            vir::ExprKind::Number(v) => {
                block.constants.push(*v);
                insert_value(arena, block, expr.pool_index(), vec![Operand::Constant(block.constants.len() - 1)],
                    ValueDef::instr(&machine::LDR))
            }
            vir::ExprKind::Binary(op, lhs, rhs) => {
                let machine_instr = match op {
                    BinaryOperator::Add             => &machine::FADD,
                    BinaryOperator::Subtract        => &machine::FSUB,
                    BinaryOperator::Multiply        => &machine::FMUL,
                    BinaryOperator::Divide          => &machine::FDIV,

//                    BinaryOperator::Power           => &machine::FADD,

                    _                               => todo!("More machine ops")

                    // BinaryOperator::Equal           => &machine::FADD,
                    // BinaryOperator::NotEqual        => &machine::FADD,
                    // BinaryOperator::LessThan        => &machine::FADD,
                    // BinaryOperator::LessEqual       => &machine::FADD,
                    // BinaryOperator::GreaterThan     => &machine::FADD,
                    // BinaryOperator::GreaterEqual    => &machine::FADD,
                };
                let operands = vec![emit_expr(arena, block, lhs), emit_expr(arena, block, rhs)];
                insert_value(arena, block, expr.pool_index(), operands, ValueDef::instr(machine_instr))
            }
        }
    }
}


fn insert_value<'arena>(
    arena:                                  &'arena Arena<Value<'arena>>,
    block:                                  &mut Block<'arena>,
    pool_index:                             usize,
    operands:                               Vec<Operand<'arena>>,
    def:                                    ValueDef) -> Operand<'arena> {
    let value = arena.alloc(Value::new(block.values.len(), def, operands, None));
    block.values.push(value);
    let operand = Operand::Value(value);
    block.operand_map.insert(pool_index, operand);
    operand
}


//-------------------------------------------------------------------------------------------------
// Instruction Scheduling

fn compute_depth<'arena>(values: &[&'arena Value<'arena>], users: &[Vec<usize>], depths: &mut [usize], address: usize) -> usize {
    if depths[address] != usize::MAX { return depths[address] }
    let depth = values[address].latency() as usize +
        users[address].iter()
            .map(|&user_address| compute_depth(values, users, depths, user_address))
            .max().unwrap_or(0);
    depths[address] = depth;
    depth
}


fn schedule_instrs<'arena>(values: &'arena [&Value<'arena>]) -> Vec<&'arena Value<'arena>> {
    // Count how many operands (that need scheduling, arguments are always available) each
    // instruction has so we can figure out when they're ready to go.
    let mut unresolved_operands =
        values.iter().map(|v| v.operands.iter().filter(|o| o.needs_scheduling()).count()).collect::<Vec<_>>();

    // Find all the users of each value
    let mut users: Vec<Vec<usize>> = vec![vec![]; values.len()];
    for value in values {
        for op in &value.operands {
            let Operand::Value(operand) = op else { continue };
            users[operand.address].push(value.address);
        }
    }

    // And count them
    let mut remaining_uses = values.iter().map(|v| users[v.address].len() ).collect::<Vec<_>>();

    // Find the critical path depths of each Value
    let mut depths = vec![usize::MAX; values.len()];
    for value in values { compute_depth(values, &users, &mut depths, value.address); }
    let mut current_critical_path_depth = depths.iter().copied().max().unwrap_or(0);

    let expected_length = values.iter().filter(|v| v.code().is_some()).count();

    let mut cycle = 0usize;
    let mut ready_results: Vec<Vec<&Value>>  = vec![vec![]; current_critical_path_depth];

    let mut code = vec![];

    let mut ready_instrs: Vec<&Value> = values.iter()
        .filter(|v| v.code().is_some() && unresolved_operands[v.address] == 0)
        .copied()
        .collect();

    while code.len() < expected_length {
        let mut free_units: EnumSet<machine::Unit> = EnumSet::all();

        while let Some(&best_instr) = ready_instrs.iter()
            .filter(|i| i.code().unwrap().try_pick_unit(free_units).is_some())
            .max_by_key(|i| {
                let critical_path_depth = depths[i.address];
                let on_critical_path = critical_path_depth >= current_critical_path_depth;
                let retiring_count = i.operands.iter().filter(|o| o.value().is_some_and(|v| remaining_uses[v.address] == 1)).count();
                (on_critical_path, retiring_count, critical_path_depth)
            }) {

            // Add this instr to the code and remove it from the list of ready instructions
            code.push(best_instr);
            ready_instrs.retain(|&i| std::ptr::from_ref(i) != std::ptr::from_ref(best_instr));

            // Pick a unit we think is going to run this instruction and reserve it.
            free_units -= best_instr.code().unwrap().try_pick_unit(free_units).unwrap();

            // Mark that the results of this instruction will be ready in the appropriate cycle.
            let ready_cycle: usize = cycle + best_instr.latency() as usize;
            if ready_results.len() <= ready_cycle {
                ready_results.resize_with(ready_cycle+1, Vec::new);
            }
            ready_results[ready_cycle].push(best_instr);

            // Update the remaining uses of our operands so we can keep track of which instructions
            // will retire (the most) registers.
            for op in &best_instr.operands   { if let Some(v) = op.value() { remaining_uses[v.address] -= 1; } }

            // Update the critical path depth if this instruction is worse than what we thought.
            current_critical_path_depth = std::cmp::max(
                current_critical_path_depth, depths[best_instr.address]);
        }
        // We can't dispatch any more instructions to units in the cycle above.  Move ahead a
        // cycle.
        cycle += 1;
        current_critical_path_depth = current_critical_path_depth.saturating_sub(1);
        // For all the results that are ready in this (new) cycle, mark all the instructions using
        // them as having that operand ready
        for completed in &ready_results[cycle] {
            for &user_address in &users[completed.address] {
                unresolved_operands[user_address] -= 1;
                if unresolved_operands[user_address] == 0 {
                    ready_instrs.push(values[user_address]);
                }
            }
        }
    }

    code
}


//-------------------------------------------------------------------------------------------------
// Register Allocation

fn set_register(value: &Value<'_>, r: u8, interfering_values: &[BitSet], available_registers: &mut [u32]) {
    let mri = machine::REGISTER_INDEX[r as usize];
    let mri_bits = 1 << mri;
    available_registers[value.address] = mri_bits;
    value.register.set(r).expect("internal compiler error: trying to set a register twice");
    for address in &interfering_values[value.address] {
        available_registers[address] &= !mri_bits;
    }
}


fn allocate_registers<'arena>(values: &[&Value<'arena>], code: &[&Value<'arena>]) {
    // Find which values interfere with which.
    let mut live_values = BitSet::new();
    let mut interfering_values = vec![BitSet::new(); values.len()];
    for value in code.iter().rev() {
        live_values.remove(value.address);
        for op in &value. operands {
            let Operand::Value(v) = op else { continue };
            live_values.insert(v.address);
        }
        for address in &live_values {
            interfering_values[address].union_with(&live_values);
        }
    }

    let mut available_registers = vec![0xFFFF_FFFFu32; values.len()];
    // Put all the arguments in their entry registers
    for value in values {
        if let Some(&r) = value.entry_register.get() {
            let mri = machine::REGISTER_INDEX[r as usize];
            assert!((available_registers[value.address] >> mri) & 1 == 1, "internal compiler error: entry register not available");
            set_register(value, r, &interfering_values, &mut available_registers);
        }
    }

    for value in values {
        if value.register.get().is_some() { continue; }
        let Some(&r) = value.exit_register.get() else { continue; };
        let mri = machine::REGISTER_INDEX[r as usize];
        let r2 = if (available_registers[value.address] >> mri) & 1 == 1 { r }
        else {
            let mri2 = available_registers[value.address].trailing_zeros();
            machine::REGISTER_ORDER[mri2 as usize]
        };
        set_register(value, r2, &interfering_values, &mut available_registers);
    }

    for value in values {
        if value.register.get().is_some() || !value.has_output() { continue; }
        let mri = available_registers[value.address].trailing_zeros();
        let r = machine::REGISTER_ORDER[mri as usize];
        set_register(value, r, &interfering_values, &mut available_registers);
    }
}


//-------------------------------------------------------------------------------------------------
// Actual machine code!

fn emit_machine_code(code: &[&Value<'_>], constants: &[f64]) {
    let raw_code_len = measure_machine_code(code);
    // Pad out to 8-byte alignment
    let constant_start = raw_code_len + usize::from(raw_code_len.is_multiple_of(2));
    let total_code_size_bytes = constant_start * 4 + 8 * constants.len();

    let code_ptr = unsafe { alloc_jit(total_code_size_bytes) };

    println!("RAW CODE LEN {raw_code_len} {code_ptr:?}");
    write_machine_code(code, constants, code_ptr, total_code_size_bytes, constant_start);

    let add_one: fn(f64) -> f64 = unsafe { mem::transmute(code_ptr) };
    let result = add_one(42.0);
    println!("f(42) = {result}");
}


fn measure_machine_code(code: &[&Value<'_>]) -> usize {
    let mut len = 0usize;
    for value in code {
        let ValueDef::Instr(..) = &value.def else { continue };
        len += 1;

        // Count FMOVs for any operands that need to be in specific registers
        if let Some(required_regs) = &value.operand_registers {
            for (op, &required) in value.operands.iter().zip(required_regs) {
                let Operand::Value(v) = op else { continue };
                let actual = *v.register.get().unwrap();
                if actual != required { len += 1; }
            }
        }
    }
    len
}


fn write_machine_code(code: &[&Value<'_>], constants: &[f64], code_ptr: *mut u32, total_code_size_bytes: usize, constant_start_words: usize) {
    let output = unsafe {
        pthread_jit_write_protect_np(0);  // enable writing

        std::slice::from_raw_parts_mut(
            code_ptr,
            total_code_size_bytes / 4
        )};

    let mut index = 0;
    for value in code {
        let ValueDef::Instr(machine_code) = &value.def else { continue };

        // Count FMOVs for any operands that need to be in specific registers
        if let Some(required_regs) = &value.operand_registers {
            for (op, &required) in value.operands.iter().zip(required_regs) {
                let Operand::Value(v) = op else { continue };
                let actual = *v.register.get().unwrap();
                if actual != required {
                    output[index] = (machine::FMOV.encode)(&[u32::from(required), u32::from(actual)]);
                    index += 1;
                }
            }
        }

        let dest = u32::from(value.register.get().copied().unwrap_or(0));
        let regs: Vec<u32> = std::iter::once(dest)
            .chain(value.operands.iter().map(|op| match op {
                Operand::Value(v)    => u32::from(*v.register.get().unwrap()),
                Operand::Constant(i) => u32::try_from((constant_start_words - index) * 4 + (*i * 8)).unwrap(),
            }))
        .collect();
        output[index] = (machine_code.encode)(&regs);
        index += 1;
    }

    index = constant_start_words;
    for c in constants {
        let bits = c.to_bits();
        output[index] = (bits & 0xFFFF_FFFF) as u32;
        index += 1;
        output[index] = (bits >> 32) as u32;
        index += 1;
    }

    unsafe {
        pthread_jit_write_protect_np(1);
        sys_icache_invalidate(code_ptr.cast(), total_code_size_bytes);
    }
}


//-------------------------------------------------------------------------------------------------
// Apple Silicon Memory Management

use libc::{mmap, pthread_jit_write_protect_np,
        MAP_ANON, MAP_JIT, MAP_PRIVATE, PROT_EXEC, PROT_READ, PROT_WRITE, MAP_FAILED};

unsafe extern "C" {
    fn sys_icache_invalidate(start: *mut std::ffi::c_void, size: usize);
}

unsafe fn alloc_jit(size: usize) -> *mut u32 {
    let ptr = unsafe { mmap(
        std::ptr::null_mut(),
        size,
        PROT_READ | PROT_WRITE | PROT_EXEC,
        MAP_PRIVATE | MAP_ANON | MAP_JIT,
        -1, 0,
    ) };
    assert!(ptr != MAP_FAILED, "mmap failed");
    ptr.cast()
}
