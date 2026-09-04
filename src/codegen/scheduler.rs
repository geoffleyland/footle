use std::collections::HashMap;
use std::fmt;

use typed_arena::Arena;
use enumset::EnumSet;

use crate::core::{BinaryOperator, Span};
use crate::vir;
use super::isa;


//-------------------------------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub(super) enum Operand<'arena> {
    Constant(usize),
    Function(String),
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
            Instr(code)                     => write!(f, "{}", code.mnemonic()),
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


pub(super) struct Block<'arena> {
    pub(super) value_count:                 usize,
    pub(super) argument_count:              u8,
    pub(super) return_count:                u8,
    pub(super) instrs:                      Vec<&'arena Value<'arena>>,
    pub(super) constants:                   Vec<Constant>,
    pub(super) functions:                   Vec<String>,
}


//-------------------------------------------------------------------------------------------------

pub(super) fn run<'arena>(arena: &'arena Arena<Value<'arena>>, input: &vir::Block) -> Block<'arena> {
    let mut builder = Builder::new(arena);
    builder.lower_vir(input);
    let argument_count = u8::try_from(builder.arguments.len())
        .expect("internal compiler error: too many arguments");
    let return_count = u8::try_from(input.return_values.len())
        .expect("internal compiler error: too many return values");

    let instrs = schedule(&builder.values);

    Block { argument_count, return_count, instrs,
        value_count: builder.values.len(),
        constants: builder.constants,
        functions: builder.function_map.keys().cloned().collect()
    }
}


//-------------------------------------------------------------------------------------------------
// Generate instructions
trait IntoOperand<'arena> {
    fn into_operand(self, builder: &Builder<'arena>) -> Operand<'arena>;
}

impl<'arena> IntoOperand<'arena> for Operand<'arena> {
    fn into_operand(self, _: &Builder<'arena>) -> Self {
        self
    }
}

impl<'arena> IntoOperand<'arena> for &'arena Value<'arena> {
    fn into_operand(self, _: &Builder<'arena>) -> Operand<'arena> {
        Operand::Value(self)
    }
}

impl<'arena> IntoOperand<'arena> for &vir::Expr {
    fn into_operand(self, builder: &Builder<'arena>) -> Operand<'arena> {
        builder.operand_map[&self.pool_index()].clone()
    }
}

macro_rules! operands {
    ($builder:expr, $( $op:expr ),* $(,)?) => {
        vec![$( ($op).into_operand($builder) ),*]
    };
}


struct Builder<'arena> {
    arena:                                  &'arena Arena<Value<'arena>>,
    arguments:                              Vec<&'arena Value<'arena>>,
    values:                                 Vec<&'arena Value<'arena>>,
    constants:                              Vec<Constant>,
    operand_map:                            HashMap<usize, Operand<'arena>>,
    function_map:                           HashMap<String, &'arena Value<'arena>>,
}

impl<'arena> Builder<'arena> {
    fn new(arena: &'arena Arena<Value<'arena>>) -> Self {
        Self { arena, arguments: vec![], values: vec![], constants: vec![],
            operand_map: HashMap::new(), function_map: HashMap::new() }
    }


    fn lower_vir(&mut self, input: &vir::Block) {
        for expr in &input.instrs {
            let span = *expr.span();
            match expr.kind() {
                vir::ExprKind::Argument(index, name) => {
                    let value = self.lower_value(expr,
                        vec![], vec![], None, ValueDef::Argument(*index, name.clone()));
                    self.arguments.push(value);
                }
                vir::ExprKind::Number(value) => {
                    self.constants.push(Constant{ value: *value, span });
                    let constant_index = self.constants.len() - 1;
                    self.lower_instr(&isa::ldr_d_literal, vec![Operand::Constant(constant_index)], expr);
                }
                vir::ExprKind::Binary(op, lhs, rhs) => {
                    if *op == BinaryOperator::Power {
                        self.lower_call("pow", &[lhs.clone(), rhs.clone()], 0, expr);

                    } else if *op == BinaryOperator::Modulo {
                        // AArch64 has no fmod; compute a - trunc(a / b) * b instead.
                        let quotient = self.make_instr(
                            &isa::fdiv_d, operands!(self, lhs, rhs), span);
                        let truncated = self.make_instr(
                            &isa::frintz_d, operands!(self, quotient), span);
                        self.lower_instr(&isa::fmsub_d, operands!(self, truncated, rhs, lhs), expr);

                    } else {
                        let machine_instr = match op {
                            BinaryOperator::Add             => &isa::fadd_d,
                            BinaryOperator::Subtract        => &isa::fsub_d,
                            BinaryOperator::Multiply        => &isa::fmul_d,
                            BinaryOperator::Divide          => &isa::fdiv_d,

                            _                               => todo!("More machine ops")
                        };
                        self.lower_instr(machine_instr, operands!(self, lhs, rhs), expr);
                    }
                }
                vir::ExprKind::Call(name, exprs) => {
                    self.lower_call(name, exprs, 0, expr);
                }
            }
        }

        let fixed_inputs = self.exprs_to_fixed_inputs(&input.return_values);
        self.make_value(vec![], fixed_inputs, None, ValueDef::Instr(&isa::ret), input.return_span);
    }


    fn lower_instr(
        &mut self,
        code:                                   &'static isa::Code,
        operands:                               Vec<Operand<'arena>>,
        expr:                                   &vir::Expr,
    ) -> &'arena Value<'arena> {
        self.lower_value(expr, operands, vec![], None, ValueDef::Instr(code))
    }

    fn lower_call(
        &mut self,
        name:                                   &str,
        operands:                               &[vir::Expr],
        fixed_output:                           u8,
        expr:                                   &vir::Expr,
    ) -> &'arena Value<'arena> {
        let fixed_inputs = self.exprs_to_fixed_inputs(operands);

        let function_value = if let Some(&v) = self.function_map.get(name) {
            v
        } else {
            let v = self.make_value(vec![Operand::Function(name.into())], vec![], None,
                ValueDef::Instr(&isa::ldr_x_literal), *expr.span());
            self.function_map.insert(name.into(), v);
            v
        };

        self.lower_value(expr, operands!(self, function_value), fixed_inputs, Some(fixed_output),
            ValueDef::Instr(&isa::blr))
    }

    fn lower_value(
        &mut self,
        expr:                                   &vir::Expr,
        operands:                               Vec<Operand<'arena>>,
        fixed_inputs:                           Vec<(&'arena Value<'arena>, u8)>,
        fixed_output:                           Option<u8>,
        def:                                    ValueDef) -> &'arena Value<'arena> {
        let value = self.make_value(operands, fixed_inputs, fixed_output, def, *expr.span());
        let operand = value.into_operand(self);
        self.operand_map.insert(expr.pool_index(), operand.clone());
        value
    }

    fn make_instr(
        &mut self,
        code:                                   &'static isa::Code,
        operands:                               Vec<Operand<'arena>>,
        span:                                   Span) -> &'arena Value<'arena>  {
        self.make_value(operands, vec![], None, ValueDef::Instr(code), span)
    }

    fn make_value(
        &mut self,
        operands:                               Vec<Operand<'arena>>,
        fixed_inputs:                           Vec<(&'arena Value<'arena>, u8)>,
        fixed_output:                           Option<u8>,
        def:                                    ValueDef,
        span:                                   Span) -> &'arena Value<'arena>  {
        let value = self.arena.alloc(Value::new(self.arena.len(), def, operands, fixed_inputs, fixed_output, span));
        self.values.push(value);
        value
    }


    fn exprs_to_fixed_inputs(
        &self,
        exprs:                                  &[vir::Expr]) -> Vec<(&'arena Value<'arena>, u8)> {
        exprs.iter().enumerate()
            .map(|(reg, expr)|
                (
                    if let Operand::Value(v) = self.operand_map[&expr.pool_index()] {
                        v
                    } else {
                        panic!("internal compiler error: constant as a fixed input")
                    },
                    u8::try_from(reg).expect("internal compiler error: too many return values")
                )
            )
            .collect::<Vec<_>>()
    }
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

    // Find the critical path depths of each Value.
    // Because the Values are already topologically ordered, we can do this backwards and always
    // find that the depths of our users that we need are already calculated.
    let depths = values.iter().rev().fold(vec![0usize; values.len()], |mut d, value| {
        d[value.slot] = usize::from(value.latency()) +
            users[value.slot].iter().map(|&s| d[s]).max().unwrap_or(0);
        d
    });
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

        // Pick an instruction from those that:
        //  * have all their operands ready
        //  * can be executed on a free unit
        //  * are on the critical path (if any)
        //  * that retire the most registers
        //  * that are deepest on the critical path (as in, if none are ON the critical path, and
        //  *   more than one retires the most registers, pick the one deepest on the critical
        //      path)
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


//-------------------------------------------------------------------------------------------------
