use std::collections::HashMap;

use typed_arena::Arena;

use crate::vir;
use super::{Code, Instr, Argument, Value, ValueSlot, value::Operand, Type, Constant, MachineReg, REGS};

#[cfg(feature = "dogfood")]
use crate::core::Span;


//-------------------------------------------------------------------------------------------------

pub struct Block<'arena> {
    pub(super) arguments:                   Vec<&'arena Argument>,
    pub(super) instrs:                      Vec<&'arena Instr<'arena>>,
    pub(super) slot_count:                  usize,
    pub(super) return_types :               Vec<Type>,
    pub(super) constants:                   Vec<Constant>,
    pub(super) functions:                   Vec<String>,
}


pub(super) fn run<'arena>(
    argument_arena:                         &'arena Arena<Argument>,
    instr_arena:                            &'arena Arena<Instr<'arena>>,
    input:                                  &vir::Block,
    types:                                  &[vir::TypeInfo],
) -> Block<'arena> {
    let mut builder = Builder::new(argument_arena, instr_arena);
    builder.lower_arguments(input, types);
    // Call arch-specific instruction selection.
    builder.select_instrs(input, types);

    Block {
        arguments:                          builder.arguments,
        instrs:                             builder.instrs,
        slot_count:                         builder.slot_count,
        return_types:                       builder.return_types,
        constants:                          builder.constants,
        functions:                          builder.functions.into_iter().map(|(n, _)| n).collect()
    }
}


//-------------------------------------------------------------------------------------------------

pub(super) trait IntoOperand<'arena> {
    fn into_operand(self, builder: &Builder<'arena>) -> Operand<'arena>;
}

impl<'arena> IntoOperand<'arena> for Operand<'arena> {
    fn into_operand(self, _: &Builder<'arena>) -> Self {
        self
    }
}

impl<'arena> IntoOperand<'arena> for &'arena Instr<'arena> {
    fn into_operand(self, _: &Builder<'arena>) -> Operand<'arena> {
        Operand::Reg(Value::Instr(self))
    }
}

impl<'arena> IntoOperand<'arena> for &vir::Expr {
    fn into_operand(self, builder: &Builder<'arena>) -> Operand<'arena> {
        builder.operand_map[&self.pool_index()].clone()
    }
}

macro_rules! operands {
    ($builder:expr, $( $op:expr ),* $(,)?) => {
        vec![$( $crate::codegen::lower::IntoOperand::into_operand($op, $builder) ),*]
    };
}
pub(super) use operands;


//-------------------------------------------------------------------------------------------------

pub struct Builder<'arena> {
    argument_arena:                         &'arena Arena<Argument>,
    instr_arena:                            &'arena Arena<Instr<'arena>>,
    arguments:                              Vec<&'arena Argument>,
    instrs:                                 Vec<&'arena Instr<'arena>>,
    slot_count:                             usize,
    pub(super) return_types:                Vec<Type>,
    pub(super) constants:                   Vec<Constant>,
    operand_map:                            HashMap<usize, Operand<'arena>>,
    pub(super) functions:                   Vec<(String, &'arena Instr<'arena>)>,
}

impl<'arena> Builder<'arena> {
    fn new(
        argument_arena:                     &'arena Arena<Argument>,
        instr_arena:                        &'arena Arena<Instr<'arena>>
    ) -> Self {
        Self { argument_arena, instr_arena,
            arguments: vec![], instrs: vec![], slot_count: 0, return_types: vec![],
            constants: vec![], operand_map: HashMap::new(), functions: vec![] }
    }

    fn new_slot(&mut self) -> ValueSlot {
        let slot = ValueSlot::from(self.slot_count);
        self.slot_count += 1;
        slot
    }


    fn lower_arguments(&mut self, input: &vir::Block, types: &[vir::TypeInfo]) {
        for vir_argument in &input.arguments {
            let ty = Type::from(types[vir_argument.pool_index]);
            let argument = self.argument_arena.alloc(Argument{ slot: self.new_slot(), ty,
                #[cfg(feature = "dogfood")]
                span: vir_argument.definition_span,
            });
            self.arguments.push(argument);
            let operand = Operand::Reg(Value::Argument(argument));
            self.operand_map.insert(vir_argument.pool_index, operand);
        }
    }

    pub(super) fn lower_instr(
        &mut self,
        ty:                                     Type,
        code:                                   &'static Code,
        operands:                               Vec<Operand<'arena>>,
        instr:                                  &vir::Instr,
    ) -> &'arena Instr<'arena> {
        self.lower_fixed_instr(ty, code, operands, vec![], None, instr)
    }

    pub(super) fn lower_fixed_instr(
        &mut self,
        ty:                                     Type,
        code:                                   &'static Code,
        operands:                               Vec<Operand<'arena>>,
        fixed_inputs:                           Vec<(Value<'arena>, MachineReg)>,
        fixed_output:                           Option<MachineReg>,
        vir_instr:                              &vir::Instr,
    ) -> &'arena Instr<'arena> {
        let instr = self.make_fixed_instr(ty, code, operands, fixed_inputs, fixed_output,
            #[cfg(feature = "dogfood")]
            vir_instr.definition_span
        );
        let operand = instr.into_operand(self);
        self.operand_map.insert(vir_instr.pool_index, operand);
        instr
    }

    pub(super) fn make_instr(
        &mut self,
        ty:                                     Type,
        code:                                   &'static Code,
        operands:                               Vec<Operand<'arena>>,
        #[cfg(feature = "dogfood")]
        span:                                   Span,
    ) -> &'arena Instr<'arena>  {
        self.make_fixed_instr(ty, code, operands, vec![], None,
            #[cfg(feature = "dogfood")]
            span
        )
    }

    pub(super) fn make_fixed_instr(
        &mut self,
        ty:                                     Type,
        code:                                   &'static Code,
        operands:                               Vec<Operand<'arena>>,
        fixed_inputs:                           Vec<(Value<'arena>, MachineReg)>,
        fixed_output:                           Option<MachineReg>,
        #[cfg(feature = "dogfood")]
        span:                                   Span,
    ) -> &'arena Instr<'arena>  {
        let instr = self.instr_arena.alloc(Instr::new(self.new_slot(), ty, code,
            operands, fixed_inputs, fixed_output,
            #[cfg(feature = "dogfood")]
            span
        ));
        self.instrs.push(instr);
        instr
    }


    pub(super) fn exprs_to_fixed_inputs(
        &self,
        exprs:                                  &[vir::Expr],
        types:                                  &[vir::TypeInfo],
    ) -> Vec<(Value<'arena>, MachineReg)> {
        exprs.iter()
            .zip(REGS.abi_regs(exprs.iter().map(|expr| Type::from(types[expr.pool_index()]))))
            .map(|(expr, reg)| (
                if let Operand::Reg(v) = self.operand_map[&expr.pool_index()] { v } else {
                    panic!("internal compiler error: constant as a fixed input")
                },
                reg)
            ).collect()
    }
}


//-------------------------------------------------------------------------------------------------
