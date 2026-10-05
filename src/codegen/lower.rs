use std::collections::HashMap;

use typed_arena::Arena;
use typed_index_collections::TiVec;

use crate::vir;
use super::{isa, Value, ValueDef, ValueSlot, value::Operand, Type, Constant, MachineReg, REGS};

#[cfg(feature = "dogfood")]
use crate::core::Span;


//-------------------------------------------------------------------------------------------------

pub struct Block<'arena> {
    pub(super) arguments:                   Vec<&'arena Value<'arena>>,
    pub(super) values:                      TiVec<ValueSlot, &'arena Value<'arena>>,
    pub(super) return_types :               Vec<Type>,
    pub(super) constants:                   Vec<Constant>,
    pub(super) functions:                   Vec<String>,
}


pub(super) fn run<'arena>(
    arena:                                  &'arena Arena<Value<'arena>>,
    input:                                  &vir::Block,
    types:                                  &[vir::TypeInfo],
) -> Block<'arena> {
    let mut builder = Builder::new(arena);
    // Call arch-specific instruction selection.
    builder.select_instrs(input, types);

    Block {
        arguments:                          builder.arguments,
        values:                             builder.values,
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

impl<'arena> IntoOperand<'arena> for &'arena Value<'arena> {
    fn into_operand(self, _: &Builder<'arena>) -> Operand<'arena> {
        Operand::Reg(self)
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
pub(super) use operands;


pub(super) trait IntoValueDef {
    fn into_value_def(self) -> ValueDef;
}

impl IntoValueDef for ValueDef {
    fn into_value_def(self) -> ValueDef     { self }
}

impl IntoValueDef for &'static isa::Code {
    fn into_value_def(self) -> ValueDef     { ValueDef::Instr(self) }
}


//-------------------------------------------------------------------------------------------------

pub struct Builder<'arena> {
    arena:                                  &'arena Arena<Value<'arena>>,
    pub(super) arguments:                   Vec<&'arena Value<'arena>>,
    values:                                 TiVec<ValueSlot, &'arena Value<'arena>>,
    pub(super) return_types:                Vec<Type>,
    pub(super) constants:                   Vec<Constant>,
    operand_map:                            HashMap<usize, Operand<'arena>>,
    pub(super) functions:                   Vec<(String, &'arena Value<'arena>)>,
}

impl<'arena> Builder<'arena> {
    fn new(arena: &'arena Arena<Value<'arena>>) -> Self {
        Self { arena, arguments: vec![], values: TiVec::new(), return_types: vec![],
            constants: vec![], operand_map: HashMap::new(), functions: vec![] }
    }


    pub(super) fn lower_instr(
        &mut self,
        ty:                                     Type,
        code:                                   &'static isa::Code,
        operands:                               Vec<Operand<'arena>>,
        expr:                                   &vir::Expr,
    ) -> &'arena Value<'arena> {
        self.lower_value(ty, code, operands, vec![], None, expr)
    }


    pub(super) fn lower_value<VD: IntoValueDef>(
        &mut self,
        ty:                                     Type,
        def:                                    VD,
        operands:                               Vec<Operand<'arena>>,
        fixed_inputs:                           Vec<(&'arena Value<'arena>, MachineReg)>,
        fixed_output:                           Option<MachineReg>,
        expr:                                   &vir::Expr,
    ) -> &'arena Value<'arena> {
        let value = self.make_value(ty, def, operands, fixed_inputs, fixed_output,
            #[cfg(feature = "dogfood")]
            *expr.definition_span()
        );
        let operand = value.into_operand(self);
        self.operand_map.insert(expr.pool_index(), operand.clone());
        value
    }

    pub(super) fn make_instr(
        &mut self,
        ty:                                     Type,
        code:                                   &'static isa::Code,
        operands:                               Vec<Operand<'arena>>,
        #[cfg(feature = "dogfood")]
        span:                                   Span,
    ) -> &'arena Value<'arena>  {
        self.make_value(ty, code, operands, vec![], None,
            #[cfg(feature = "dogfood")]
            span
        )
    }

    pub(super) fn make_value<VD: IntoValueDef>(
        &mut self,
        ty:                                     Type,
        def:                                    VD,
        operands:                               Vec<Operand<'arena>>,
        fixed_inputs:                           Vec<(&'arena Value<'arena>, MachineReg)>,
        fixed_output:                           Option<MachineReg>,
        #[cfg(feature = "dogfood")]
        span:                                   Span,
    ) -> &'arena Value<'arena>  {
        let def = def.into_value_def();
        let value = self.arena.alloc(Value::new(self.values.next_key(), ty, def,
            operands, fixed_inputs, fixed_output,
            #[cfg(feature = "dogfood")]
            span
        ));
        self.values.push(value);
        value
    }


    pub(super) fn exprs_to_fixed_inputs(
        &self,
        exprs:                                  &[vir::Expr],
        types:                                  &[vir::TypeInfo],
    ) -> Vec<(&'arena Value<'arena>, MachineReg)> {
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
