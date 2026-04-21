use std::collections::HashMap;
use std::fmt;

use typed_arena::Arena;
use enumset::EnumSet;

use crate::core::{BinaryOperator, Span};
use crate::vir;
use super::isa;


//-------------------------------------------------------------------------------------------------

#[derive(Debug, Clone, Copy)]
pub(super) enum Operand<'arena> {
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
            Value(v)                        => write!(f, "I{}", v.slot),
            Constant(i)                     => write!(f, "K{i}"),
        }
    }
}


//-------------------------------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub(super) enum ValueDef {
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
pub(super) struct Value<'arena> {
    pub(super) slot:                        usize,
    pub(super) def:                         ValueDef,
    pub(super) operands:                    Vec<Operand<'arena>>,
    pub(super) operand_regs:                Option<Vec<u8>>,
    pub(super) result_reg:                  Option<u8>,
    pub(super) span:                        Span,
}

impl<'arena> Value<'arena> {
    fn new(
        slot:                               usize,
        def:                                ValueDef,
        operands:                           Vec<Operand<'arena>>,
        operand_regs:                       Option<Vec<u8>>,
        result_reg:                         Option<u8>,
        span:                               Span) -> Self {
        Self { slot, def, operands, operand_regs, result_reg, span }
    }


    pub(super) fn code(&self) -> Option<&'static isa::Code> {
        if let ValueDef::Instr(code) = &self.def { Some(code) } else { None }
    }

    fn latency(&self) -> u8                 { self.code().map_or(0, |c| c.latency) }
}

impl fmt::Display for Value<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "I{}: {} {}", self.slot, self.def,
            self.operands
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(" "))
    }
}


//-------------------------------------------------------------------------------------------------

#[derive(Debug, Copy, Clone)]
pub(super) struct Constant {
    pub(super) value:                       f64,
    pub(super) span:                        Span
}


pub(super) struct Block<'arena> {
    pub(super) arguments:                   Vec<&'arena Value<'arena>>,
    pub(super) values:                      Vec<&'arena Value<'arena>>,
    pub(super) constants:                   Vec<Constant>,
    pub(super) return_count:                u8,
    operand_map:                            HashMap<usize, Operand<'arena>>,
}

impl Block<'_> {
    fn new() -> Self {
        Self { arguments: vec![], values: vec![], constants: vec![], return_count: 0,
            operand_map: HashMap::new() }
    }
}


//-------------------------------------------------------------------------------------------------
// Generate instructions

pub(super) fn lower_vir<'arena>(arena: &'arena Arena<Value<'arena>>, input: &vir::Block) -> Block<'arena> {
    let mut block = Block::new();
    for arg in input.arguments() {
        lower_expr(arena, &mut block, arg);
    }
    for stmt in input.stmts() {
        match &stmt.kind {
            vir::StmtKind::Return(exprs) => {
                let operands =
                    exprs.iter().map(|expr| { lower_expr(arena, &mut block, expr) }).collect::<Vec<_>>();

                // RETURN doesn't produce a value you can use, so add it to the instructions, but
                // not to the operand_map.
                let operand_count = u8::try_from(operands.len())
                    .expect("internal compiler error: too many return values");
                let operand_regs = (0..operand_count).collect::<Vec<_>>();
                let ret = arena.alloc(Value::new(
                    arena.len(),
                    ValueDef::Instr(&isa::RET),
                    operands,
                    Some(operand_regs),
                    None,
                    stmt.span));
                block.values.push(ret);
                block.return_count = operand_count;
            }
        }
    }
    block
}


fn lower_expr<'arena>(
    arena:                                  &'arena Arena<Value<'arena>>,
    block:                                  &mut Block<'arena>,
    expr:                                   &vir::Expr) -> Operand<'arena> {
    if let Some(&operand) = block.operand_map.get(&expr.pool_index()) {
        operand
    } else {
        match expr.kind() {
            vir::ExprKind::Argument(index, name) => {
                let (value, operand) = insert_value(arena, block, expr,
                    vec![], None, None, ValueDef::Argument(*index, name.clone()));
                block.arguments.push(value);
                operand
            }
            vir::ExprKind::Number(v) => {
                block.constants.push(Constant{ value: *v, span: *expr.span() });
                insert_value(arena, block, expr,
                    vec![Operand::Constant(block.constants.len() - 1)], None, None,
                    ValueDef::Instr(&isa::LDR_PC_F64)).1
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
                let operands = vec![lower_expr(arena, block, lhs), lower_expr(arena, block, rhs)];
                insert_value(arena, block, expr, operands, operand_regs, result_reg,
                    ValueDef::Instr(machine_instr)).1
            }
        }
    }
}


fn insert_value<'arena>(
    arena:                                  &'arena Arena<Value<'arena>>,
    block:                                  &mut Block<'arena>,
    expr:                                   &vir::Expr,
    operands:                               Vec<Operand<'arena>>,
    operand_regs:                           Option<Vec<u8>>,
    result_reg:                             Option<u8>,
    def:                                    ValueDef) -> (&'arena Value<'arena>, Operand<'arena>)  {
    let value = arena.alloc(Value::new(arena.len(), def, operands, operand_regs, result_reg, *expr.span()));
    block.values.push(value);
    let operand = Operand::Value(value);
    block.operand_map.insert(expr.pool_index(), operand);
    (value, operand)
}


//-------------------------------------------------------------------------------------------------
// Instruction Scheduling

pub(super) fn schedule<'arena>(values: &'arena [&Value<'arena>]) -> Vec<&'arena Value<'arena>> {
    // Count how many operands (that need scheduling, arguments are always available) each
    // instruction has so we can figure out when they're ready to go.
    let mut unresolved_operands =
        values.iter().map(|v| v.operands.iter().filter(|o| o.needs_scheduling()).count()).collect::<Vec<_>>();

    // Find all the users of each value
    let mut users: Vec<Vec<usize>> = vec![vec![]; values.len()];
    for value in values {
        for op in &value.operands {
            let Operand::Value(operand) = op else { continue };
            users[operand.slot].push(value.slot);
        }
    }

    // And count the uses of each value
    let mut remaining_uses = values.iter().map(|v| users[v.slot].len() ).collect::<Vec<_>>();

    // Find the critical path depths of each Value
    let mut depths = vec![usize::MAX; values.len()];
    for value in values { compute_depth(values, &users, &mut depths, value.slot); }
    let mut current_critical_path_depth = depths.iter().copied().max().unwrap_or(0);

    let expected_length = values.iter().filter(|v| v.code().is_some()).count();

    let mut cycle = 0usize;
    let mut results_by_cycle: Vec<Vec<&Value>>  = vec![vec![]; current_critical_path_depth];

    let mut scheduled = vec![];

    let mut ready_instrs: Vec<&Value> = values.iter()
        .filter(|v| v.code().is_some() && unresolved_operands[v.slot] == 0)
        .copied()
        .collect();

    while scheduled.len() < expected_length {
        let mut free_units: EnumSet<isa::Unit> = EnumSet::all();

        while let Some(&best_instr) = ready_instrs.iter()
            .filter(|i| i.code().expect("internal compiler error: instruction without opcode").try_pick_unit(free_units).is_some())
            .max_by_key(|i| {
                let critical_path_depth = depths[i.slot];
                let on_critical_path = critical_path_depth >= current_critical_path_depth;
                let retiring_count = i.operands.iter().filter(|o| o.value().is_some_and(|v| remaining_uses[v.slot] == 1)).count();
                (on_critical_path, retiring_count, critical_path_depth)
            }) {

            // Add this instr to the code and remove it from the list of ready instructions
            scheduled.push(best_instr);
            ready_instrs.retain(|&i| std::ptr::from_ref(i) != std::ptr::from_ref(best_instr));

            // Pick a unit we think is going to run this instruction and reserve it.
            free_units -= best_instr.code().expect("internal compiler error: instruction without opcode")
                .try_pick_unit(free_units).expect("internal compiler error: not enough registers");

            // Mark that the results of this instruction will be ready in the appropriate cycle.
            let ready_cycle = cycle + usize::from(best_instr.latency());
            if results_by_cycle.len() <= ready_cycle {
                results_by_cycle.resize_with(ready_cycle+1, Vec::new);
            }
            results_by_cycle[ready_cycle].push(best_instr);

            // Update the remaining uses of our operands so we can keep track of which instructions
            // will retire (the most) registers.
            for op in &best_instr.operands   { if let Some(v) = op.value() { remaining_uses[v.slot] -= 1; } }

            // Update the critical path depth if this instruction is worse than what we thought.
            current_critical_path_depth = std::cmp::max(
                current_critical_path_depth, depths[best_instr.slot]);
        }
        // We can't dispatch any more instructions to units in the cycle above.  Move ahead a
        // cycle.
        cycle += 1;
        current_critical_path_depth = current_critical_path_depth.saturating_sub(1);
        // For all the results that are ready in this (new) cycle, mark all the instructions using
        // them as having that operand ready
        for completed in &results_by_cycle[cycle] {
            for &user_slot in &users[completed.slot] {
                unresolved_operands[user_slot] -= 1;
                if unresolved_operands[user_slot] == 0 {
                    ready_instrs.push(values[user_slot]);
                }
            }
        }
    }

    scheduled
}


fn compute_depth<'arena>(values: &[&'arena Value<'arena>], users: &[Vec<usize>], depths: &mut [usize], slot: usize) -> usize {
    if depths[slot] != usize::MAX { return depths[slot] }
    let depth = usize::from(values[slot].latency()) +
        users[slot].iter()
            .map(|&user_slot| compute_depth(values, users, depths, user_slot))
            .max().unwrap_or(0);
    depths[slot] = depth;
    depth
}


//-------------------------------------------------------------------------------------------------
