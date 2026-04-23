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
    Function(&'static str),
    Value(&'arena Value<'arena>),
}

impl<'arena> Operand<'arena> {
    fn value(&self) -> Option<&'arena Value<'arena>> {
        if let Self::Value(v) = self { Some(v) } else { None }
    }
}

impl fmt::Display for Operand<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Operand::*;
        match self {
            Value(v)                        => write!(f, "I{}", v.slot),
            Constant(i)                     => write!(f, "K{i}"),
            Function(s)                     => write!(f, "{s}"),
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
    pub(super) fixed_inputs:                Vec<(&'arena Self, u8)>,
    pub(super) fixed_output:                Option<u8>,
    pub(super) span:                        Span,
}

impl<'arena> Value<'arena> {
    fn new(
        slot:                               usize,
        def:                                ValueDef,
        operands:                           Vec<Operand<'arena>>,
        fixed_inputs:                       Vec<(&'arena Self, u8)>,
        fixed_output:                       Option<u8>,
        span:                               Span) -> Self {
        Self { slot, def, operands, fixed_inputs, fixed_output, span }
    }


    pub(super) fn code(&self) -> Option<&'static isa::Code> {
        if let ValueDef::Instr(code) = &self.def { Some(code) } else { None }
    }

    fn latency(&self) -> u8                 { self.code().map_or(0, |c| c.latency) }

    pub(super) fn predecessors(&self) -> impl Iterator<Item = &'arena Value<'arena>> {
        let operands = self.operands.iter().filter_map(Operand::value);
        let fixed_inputs = self.fixed_inputs.iter().map(|(v, _)| *v);
        operands.chain(fixed_inputs)
    }

    fn needs_scheduling(&self) -> bool      { matches!(self.def, ValueDef::Instr(..)) }
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


struct Builder<'arena> {
    arguments:                              Vec<&'arena Value<'arena>>,
    values:                                 Vec<&'arena Value<'arena>>,
    constants:                              Vec<Constant>,
    return_count:                           u8,
    operand_map:                            HashMap<usize, Operand<'arena>>,
    function_map:                           HashMap<&'static str, &'arena Value<'arena>>,
}

impl Builder<'_> {
    fn new() -> Self {
        Self { arguments: vec![], values: vec![], constants: vec![], return_count: 0,
            operand_map: HashMap::new(), function_map: HashMap::new() }
    }
}


pub(super) struct Block<'arena> {
    pub(super) value_count:                 usize,
    pub(super) argument_count:              u8,
    pub(super) return_count:                u8,
    pub(super) instrs:                      Vec<&'arena Value<'arena>>,
    pub(super) constants:                   Vec<Constant>,
    pub(super) functions:                   Vec<&'static str>,
}


//-------------------------------------------------------------------------------------------------

pub(super) fn run<'arena>(arena: &'arena Arena<Value<'arena>>, input: &vir::Block) -> Block<'arena> {
    let builder = lower_vir(arena, input);
    let argument_count = u8::try_from(builder.arguments.len())
        .expect("internal compiler error: too many arguments");
    let instrs = schedule(&builder.values);

    Block { argument_count, instrs,
        value_count: builder.values.len(), return_count: builder.return_count,
        constants: builder.constants,
        functions: builder.function_map.keys().copied().collect()
    }
}


//-------------------------------------------------------------------------------------------------
// Generate instructions

fn lower_vir<'arena>(arena: &'arena Arena<Value<'arena>>, input: &vir::Block) -> Builder<'arena> {
    let mut builder = Builder::new();
    for arg in input.arguments() {
        lower_expr(arena, &mut builder, arg);
    }
    for stmt in input.stmts() {
        match &stmt.kind {
            vir::StmtKind::Return(exprs) => {
                // RETURN doesn't produce a value you can use, so add it to the instructions, but
                // not to the operand_map.
                let fixed_inputs = exprs.iter().enumerate()
                    .map(|(reg, expr)| (
                        if let Operand::Value(v) = lower_expr(arena, &mut builder, expr) { v }
                        else { panic!("internal compiler error: constant as argument to return")},
                        u8::try_from(reg).expect("internal compiler error: too many return values")
                    ))
                    .collect::<Vec<_>>();

                let ret = arena.alloc(Value::new(
                    arena.len(),
                    ValueDef::Instr(&isa::RET),
                    vec![],
                    fixed_inputs,
                    None,
                    stmt.span));
                builder.values.push(ret);
                builder.return_count = u8::try_from(exprs.len())
                    .expect("internal compiler error: too many return values");
            }
        }
    }
    builder
}


fn lower_expr<'arena>(
    arena:                                  &'arena Arena<Value<'arena>>,
    builder:                                &mut Builder<'arena>,
    expr:                                   &vir::Expr) -> Operand<'arena> {
    if let Some(&operand) = builder.operand_map.get(&expr.pool_index()) {
        operand
    } else {
        match expr.kind() {
            vir::ExprKind::Argument(index, name) => {
                let (value, operand) = insert_value(arena, builder, expr,
                    vec![], vec![], None, ValueDef::Argument(*index, name.clone()));
                builder.arguments.push(value);
                operand
            }
            vir::ExprKind::Number(v) => {
                builder.constants.push(Constant{ value: *v, span: *expr.span() });
                insert_value(arena, builder, expr,
                    vec![Operand::Constant(builder.constants.len() - 1)], vec![], None,
                    ValueDef::Instr(&isa::LDR_PC_F64)).1
            }
            vir::ExprKind::Binary(op, lhs, rhs) => {
                if *op == BinaryOperator::Power {
                    let exprs = [lhs, rhs];
                    let fixed_inputs = exprs.iter().enumerate()
                        .map(|(reg, expr)| (
                            if let Operand::Value(v) = lower_expr(arena, builder, expr) {
                                v
                            } else { panic!("internal compiler error: constant as argument to pow")},
                            u8::try_from(reg).expect("internal compiler error: too many return values")
                        ))
                        .collect::<Vec<_>>();
                        let function_value = intern_function(arena, builder, "pow", *expr.span());
                        insert_value(arena, builder, expr, vec![function_value], fixed_inputs, Some(0u8),
                            ValueDef::Instr(&isa::BLR)).1
                } else {
                    let machine_instr = match op {
                        BinaryOperator::Add             => &isa::FADD,
                        BinaryOperator::Subtract        => &isa::FSUB,
                        BinaryOperator::Multiply        => &isa::FMUL,
                        BinaryOperator::Divide          => &isa::FDIV,

                        _                               => todo!("More machine ops")

                        // BinaryOperator::Equal           => &machine::FADD,
                        // BinaryOperator::NotEqual        => &machine::FADD,
                        // BinaryOperator::LessThan        => &machine::FADD,
                        // BinaryOperator::LessEqual       => &machine::FADD,
                        // BinaryOperator::GreaterThan     => &machine::FADD,
                        // BinaryOperator::GreaterEqual    => &machine::FADD,
                    };

                    let operands = vec![lower_expr(arena, builder, lhs), lower_expr(arena, builder, rhs)];
                    insert_value(arena, builder, expr, operands, vec![], None,
                        ValueDef::Instr(machine_instr)).1
                }
            }
        }
    }
}


fn intern_function<'arena>(
    arena:                                  &'arena Arena<Value<'arena>>,
    builder:                                &mut Builder<'arena>,
    name:                                   &'static str,
    span:                                   Span) -> Operand<'arena> {
    let v = if let Some(&v) = builder.function_map.get(name) {
        v
    } else {
        let v = create_value(arena, builder, vec![Operand::Function(name)], vec![], None,
            ValueDef::Instr(&isa::LDR_PC_I64), span);
        builder.function_map.insert(name, v);
        v
    };
    Operand::Value(v)
}


fn create_value<'arena>(
    arena:                                  &'arena Arena<Value<'arena>>,
    builder:                                &mut Builder<'arena>,
    operands:                               Vec<Operand<'arena>>,
    fixed_inputs:                           Vec<(&'arena Value<'arena>, u8)>,
    fixed_output:                           Option<u8>,
    def:                                    ValueDef,
    span:                                   Span) -> &'arena Value<'arena>  {
    let value = arena.alloc(Value::new(arena.len(), def, operands, fixed_inputs, fixed_output, span));
    builder.values.push(value);
    value
}


fn insert_value<'arena>(
    arena:                                  &'arena Arena<Value<'arena>>,
    builder:                                &mut Builder<'arena>,
    expr:                                   &vir::Expr,
    operands:                               Vec<Operand<'arena>>,
    fixed_inputs:                           Vec<(&'arena Value<'arena>, u8)>,
    fixed_output:                           Option<u8>,
    def:                                    ValueDef) -> (&'arena Value<'arena>, Operand<'arena>)  {
    let value = create_value(arena, builder, operands, fixed_inputs, fixed_output, def, *expr.span());
    let operand = Operand::Value(value);
    builder.operand_map.insert(expr.pool_index(), operand);
    (value, operand)
}


//-------------------------------------------------------------------------------------------------
// Instruction Scheduling

fn schedule<'arena>(values: &[&'arena Value<'arena>]) -> Vec<&'arena Value<'arena>> {
    // Count how many operands (that need scheduling, arguments are always available) each
    // instruction has so we can figure out when they're ready to go.
    let mut unresolved_operands =
        values.iter().map(|v| v.predecessors().filter(|v| v.needs_scheduling()).count()).collect::<Vec<_>>();

    // Find all the users of each value
    let mut users: Vec<Vec<usize>> = vec![vec![]; values.len()];
    for value in values {
        for predecessor in value.predecessors() {
            users[predecessor.slot].push(value.slot);
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
                let retiring_count = i.predecessors().filter(|p| remaining_uses[p.slot] == 1).count();
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
            for p in best_instr.predecessors() { remaining_uses[p.slot] -= 1; }

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
