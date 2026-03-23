use std::cell::OnceCell;
use std::collections::HashMap;
use std::fmt;
use std::mem;

use typed_arena::Arena;
use enumset::EnumSet;
use bit_set::BitSet;

use crate::core::{BinaryOperator, Span, Styleable, LineStyle};
use crate::vir;
use super::isa;
use super::sys;

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
    Instr(&'static isa::Code),
    Argument(usize, String),
}

impl fmt::Display for ValueDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ValueDef::*;
        match self {
            Instr(code)                     => write!(f, "{}", code.name),
            Argument(i, name)               => write!(f, "ARGUMENT r{i} ({name})"),
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
    span:                                   Span,
}

impl<'arena> Value<'arena> {
    fn new(
        address:                            usize,
        def:                                ValueDef,
        operands:                           Vec<Operand<'arena>>,
        operand_registers:                  Option<Vec<u8>>,
        span:                               Span) -> Self {
        Self { address, def, operands, operand_registers, span }
    }


    fn code(&self) -> Option<&'static isa::Code> {
        if let ValueDef::Instr(code) = &self.def { Some(code) } else { None }
    }

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

#[derive(Debug, Copy, Clone)]
struct Constant {
    value:                                  f64,
    span:                                   Span
}

struct Block<'arena> {
    arguments:                              Vec<&'arena Value<'arena>>,
    values:                                 Vec<&'arena Value<'arena>>,
    constants:                              Vec<Constant>,
    operand_map:                            HashMap<usize, Operand<'arena>>,
}

impl Block<'_> {
    fn new() -> Self {
        Self { arguments: vec![], values: vec![], constants: vec![], operand_map: HashMap::new() }
    }
}


//-------------------------------------------------------------------------------------------------

pub fn run(block: &vir::Block) -> fn(f64) -> f64 {
    let arena = Arena::<Value>::new();
    let Block { arguments, values, constants, .. } = emit_instrs(&arena, block);
    let code = schedule_instrs(&values);
    let registers = allocate_registers(&arguments, &values, &code);
    let assembler = emit_assembler(&code, &constants, &registers);
    emit_binary(&assembler)
}


//-------------------------------------------------------------------------------------------------
// Generate instructions

fn emit_instrs<'arena>(arena: &'arena Arena<Value<'arena>>, input: &vir::Block) -> Block<'arena> {
    let mut block = Block::new();
    for arg in input.arguments() {
        emit_expr(arena, &mut block, arg);
    }
    for stmt in input.stmts() {
        match &stmt.kind {
            vir::StmtKind::Return(exprs) => {
                let operands =
                    exprs.iter().map(|expr| { emit_expr(arena, &mut block, expr) }).collect::<Vec<_>>();

                // RETURN doesn't produce a value you can use, so add it to the instructions, but
                // not to the operand_map.
                let operand_registers = (0..u8::try_from(operands.len()).unwrap()).collect::<Vec<u8>>();
                let ret = arena.alloc(Value::new(
                    block.values.len(),
                    ValueDef::Instr(&isa::RET),
                    operands,
                    Some(operand_registers),
                    stmt.span));
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
            vir::ExprKind::Argument(index, name) => {
                let (value, operand) = insert_value(arena, block, expr.pool_index(), vec![], ValueDef::Argument(*index, name.clone()), *expr.span());
                block.arguments.push(value);
                operand
            }
            vir::ExprKind::Number(v) => {
                block.constants.push(Constant{ value: *v, span: *expr.span() });
                insert_value(arena, block, expr.pool_index(), vec![Operand::Constant(block.constants.len() - 1)],
                    ValueDef::Instr(&isa::LDR_PC_F64), *expr.span()).1
            }
            vir::ExprKind::Binary(op, lhs, rhs) => {
                let machine_instr = match op {
                    BinaryOperator::Add             => &isa::FADD,
                    BinaryOperator::Subtract        => &isa::FSUB,
                    BinaryOperator::Multiply        => &isa::FMUL,
                    BinaryOperator::Divide          => &isa::FDIV,

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
                insert_value(arena, block, expr.pool_index(), operands, ValueDef::Instr(machine_instr), *expr.span()).1
            }
        }
    }
}


fn insert_value<'arena>(
    arena:                                  &'arena Arena<Value<'arena>>,
    block:                                  &mut Block<'arena>,
    pool_index:                             usize,
    operands:                               Vec<Operand<'arena>>,
    def:                                    ValueDef,
    span:                                   Span) -> (&'arena Value<'arena>, Operand<'arena>)  {
    let value = arena.alloc(Value::new(block.values.len(), def, operands, None, span));
    block.values.push(value);
    let operand = Operand::Value(value);
    block.operand_map.insert(pool_index, operand);
    (value, operand)
}


//-------------------------------------------------------------------------------------------------
// Instruction Scheduling

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
        let mut free_units: EnumSet<isa::Unit> = EnumSet::all();

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


fn compute_depth<'arena>(values: &[&'arena Value<'arena>], users: &[Vec<usize>], depths: &mut [usize], address: usize) -> usize {
    if depths[address] != usize::MAX { return depths[address] }
    let depth = values[address].latency() as usize +
        users[address].iter()
            .map(|&user_address| compute_depth(values, users, depths, user_address))
            .max().unwrap_or(0);
    depths[address] = depth;
    depth
}


//-------------------------------------------------------------------------------------------------
// Register Allocation

fn allocate_registers<'arena>(
    arguments: &[&Value<'arena>],
    values: &[&Value<'arena>],
    code: &[&Value<'arena>]) -> Vec<u8> {
    let mut registers: Vec<OnceCell<u8>> = vec![OnceCell::new(); values.len()];

    // Find which values interfere with which.
    let mut live_values = BitSet::new();
    let mut interfering_values = vec![BitSet::new(); values.len()];
    for value in code.iter().rev() {
        live_values.remove(value.address);
        for op in &value.operands {
            let Operand::Value(v) = op else { continue };
            live_values.insert(v.address);
        }
        for address in &live_values {
            interfering_values[address].union_with(&live_values);
        }
    }

    let mut available_registers = vec![0xFFFF_FFFFu32; values.len()];
    for (r, value) in arguments.iter().enumerate() {
        set_register(value, u8::try_from(r).unwrap(), &registers, &interfering_values, &mut available_registers);
    }

    // Scan instructions for any register constraints
    for value in code {
        if let Some(operand_registers) = &value.operand_registers {
            for (op, &r) in value.operands.iter().zip(operand_registers) {
                if let Operand::Value(v) = op {
                    if registers[v.address].get().is_some() { continue; }
                    let mri = isa::REGISTER_INDEX[r as usize];
                    let r2 = if (available_registers[v.address] >> mri) & 1 == 1 { r }
                        else {
                            let mri2 = available_registers[v.address].trailing_zeros();
                            isa::REGISTER_ORDER[mri2 as usize]
                        };
                    set_register(v, r2, &registers, &interfering_values, &mut available_registers);
                } else {
                    panic!("internal compiler error: register constraint on constant");
                }
            }
        }
    }

    for value in code {
        if registers[value.address].get().is_some() || !value.has_output() { continue; }
        let mri = available_registers[value.address].trailing_zeros();
        let r = isa::REGISTER_ORDER[mri as usize];
        set_register(value, r, &registers, &interfering_values, &mut available_registers);
    }

    registers.iter_mut().map(|c| c.take().unwrap_or(0xFFu8)).collect::<Vec<_>>()
}


fn set_register(value: &Value<'_>, r: u8, registers: &[OnceCell<u8>], interfering_values: &[BitSet], available_registers: &mut [u32]) {
    let mri = isa::REGISTER_INDEX[r as usize];
    let mri_bits = 1 << mri;
    available_registers[value.address] = mri_bits;
    registers[value.address].set(r).expect("internal compiler error: trying to set a register twice");
    for address in &interfering_values[value.address] {
        available_registers[address] &= !mri_bits;
    }
}


//-------------------------------------------------------------------------------------------------
// Assembler!

macro_rules! asm_op {
    (Register($r:expr))     => { AssemblyOperand::Register($r) };
    (Constant($i:expr))     => { AssemblyOperand::Constant($i) };
}

macro_rules! assemble {
    ($vec:expr, $span:expr, $op:ident $(, $($operand:tt $operand_arg:expr),*)?) => {
        $vec.push(AssemblyInstr {
            code: &isa::$op,
            span: $span,
            operands: vec![$($(asm_op!($operand($operand_arg))),*)?],
        })
    }
}


enum AssemblyOperand {
    Register(u8),
    Constant(usize),
}

struct AssemblyInstr {
    code:                   &'static isa::Code,
    operands:               Vec<AssemblyOperand>,
    span:                   Option<Span>,
}


pub struct AssemblyBlock {
    assembler:              Vec<AssemblyInstr>,
    constants:              Vec<Constant>,
}


fn emit_assembler(schedule: &[&Value<'_>], constants: &[Constant], registers: &[u8]) -> AssemblyBlock{
    let mut assembler = Vec::new();
    for value in schedule {
        let ValueDef::Instr(code) = &value.def else { continue };

        // Count FMOVs for any operands that need to be in specific registers
        if let Some(required_regs) = &value.operand_registers {
            for (op, &required) in value.operands.iter().zip(required_regs) {
                let Operand::Value(v) = op else { continue };
                let actual = registers[v.address];
                if actual != required {
                    assemble!(assembler, None, FMOV, Register(required), Register(actual));
                }
            }
        }

        let operands = code.has_output
            .then(|| AssemblyOperand::Register(registers[value.address]))
            .into_iter()
            .chain(value.operands.iter().map(|op| match op {
                Operand::Value(v)    => AssemblyOperand::Register(registers[v.address]),
                Operand::Constant(i) => AssemblyOperand::Constant(*i),
            }))
        .collect();
        assembler.push(AssemblyInstr{ code, operands, span: Some(value.span) });
    }

    AssemblyBlock{ assembler, constants: constants.into() }
}


// fn emit_glue(arguments: usize, return_values: usize) {
//         assembler.push(AssemblyInstr{
//             code: &isa::MOV(r0 to r16)
//         });

//     set mut assembler = Vec::new();
//     for i in 0..arguments {
//         assembler.push(AssemblyInstr{
//             code: &isa::LDR_RELATIVE_TO_R16
//             operands: vec![
//                 AssemblyOperand::Register(r16),
//                 AssemblyOperand::Register(i),
//                 AssemblyOperand::Offset(i*4),
//             ],
//             Span: no span
//         });
//     }

//     assembler.push(AssemblyInstr{
//         code: &isa::STP,
//         operands[],
//         Span: no span
//         }
//     })

//     assembler.push(AssemblyInstr{
//         BL
//     })
// }

//-------------------------------------------------------------------------------------------------
// Binary!

fn emit_binary(block: &AssemblyBlock) -> fn(f64) -> f64 {
    let instr_words = block.assembler.len();
    let constant_start_words = instr_words + usize::from(instr_words.is_multiple_of(2));
    let total_code_size_bytes = constant_start_words * 4 + 8 * block.constants.len();

    let code_ptr = sys::alloc_jit(total_code_size_bytes);
    let words = jit_as_words_mut(code_ptr, total_code_size_bytes);

    sys::start_jit_compile();

    encode_instrs(&block.assembler, words, constant_start_words);
    encode_constants(&block.constants, words, constant_start_words);

    sys::finish_jit_compile(code_ptr, total_code_size_bytes);
    unsafe { mem::transmute(code_ptr) }
}


fn encode_instrs(assembler: &[AssemblyInstr], words: &mut [u32], constant_start_words: usize) {
    for (address, instr) in assembler.iter().enumerate() {
        let operands = instr.operands.iter().map(|op| {
            match op {
                AssemblyOperand::Register(i)            => u32::from(*i),
                AssemblyOperand::Constant(i)            => u32::try_from((constant_start_words - address) * 4 + (*i * 8)).unwrap(),
            }}).collect::<Vec<_>>();
        words[address] = (instr.code.encode)(&operands);
    }
}


fn encode_constants(constants: &[Constant], words: &mut [u32], constant_start_words: usize) {
    let mut index = constant_start_words;
    for c in constants {
        let bits = c.value.to_bits();
        words[index] = (bits & 0xFFFF_FFFF) as u32;
        index += 1;
        words[index] = (bits >> 32) as u32;
        index += 1;
    }
}


fn jit_as_words_mut<'a>(ptr: *mut u32, size_bytes: usize) -> &'a mut [u32] {
    unsafe { std::slice::from_raw_parts_mut(ptr, size_bytes / 4) }
}


//-------------------------------------------------------------------------------------------------
// Text output for the scheduler

enum InstrOperand {
    Constant(usize),
    Instr(usize)
}
struct Instr {
    address:                                usize,
    opcode:                                 String,
    operands:                               Vec<InstrOperand>,
    span:                                   Span,
}

pub struct Schedule {
    arguments:                              Vec<(usize, Span)>,
    instrs:                                 Vec<Instr>,
    constants:                              Vec<Constant>,
}

impl Styleable for Schedule {
    fn write<W: LineStyle>(&self, f: &mut fmt::Formatter, indent: u16, writer: &W) -> fmt::Result {
        for (i, span) in &self.arguments {
            writer.writeln(f, indent, Some(*span), &format!("I{i}: argument"))?;
        }
        for i in &self.instrs {
            let operands = i.operands.iter().map(|o|
                match o {
                    InstrOperand::Constant(i)   => format!("K{i}"),
                    InstrOperand::Instr(i)      => format!("I{i}")
                }).collect::<Vec<_>>();
            writer.writeln(f, indent, Some(i.span), &format!("I{}: {} {}", i.address, i.opcode,
                operands.join(" ")))?;
        }
        for (i, c) in self.constants.iter().enumerate() {
            writer.writeln (f, indent, Some(c.span), &format!("K{i}: {:?}", c.value))?;
        }
        Ok(())
    }
}

pub fn schedule(block: &vir::Block) -> Schedule {
    let arena = Arena::<Value>::new();
    let Block { arguments, values, constants, .. } = emit_instrs(&arena, block);
    let code = schedule_instrs(&values);

    let arguments = arguments.iter().map(|a| (a.address, a.span)).collect::<Vec<_>>();
    let instrs = code.iter().map(|c|
        Instr{
            address:        c.address,
            opcode:         c.code().unwrap().name.to_string(),
            operands:       c.operands.iter().map(|o|
                match o {
                    Operand::Constant(i)        => InstrOperand::Constant(*i),
                    Operand::Value(v)           => InstrOperand::Instr(v.address)
                }).collect(),
            span:           c.span})
        .collect();

    Schedule{ arguments, instrs, constants }
}


//-------------------------------------------------------------------------------------------------
// Text output for assembler

pub fn assemble(block: &vir::Block) -> AssemblyBlock {
    let arena = Arena::<Value>::new();
    let Block { arguments, values, constants, .. } = emit_instrs(&arena, block);
    let code = schedule_instrs(&values);
    let registers = allocate_registers(&arguments, &values, &code);
    emit_assembler(&code, &constants, &registers)
}


impl Styleable for AssemblyBlock {
    fn write<W: LineStyle>(&self, f: &mut fmt::Formatter, indent: u16, writer: &W) -> fmt::Result {
        for i in &self.assembler {
            let operands = i.operands.iter().map(|o|
                match o {
                    AssemblyOperand::Constant(i)   => format!("K{i}"),
                    AssemblyOperand::Register(i)   => format!("d{i}")
                }).collect::<Vec<_>>();
            writer.writeln(f, indent, i.span, &format!("{} {}", i.code.name,
                operands.join(" ")))?;
        }
        for (i, c) in self.constants.iter().enumerate() {
            writer.writeln (f, indent, Some(c.span), &format!("K{i}: {:?}", c.value))?;
        }
        Ok(())
    }
}
