use std::collections::HashMap;

use typed_arena::Arena;
use typed_index_collections::TiVec;

use crate::{vir, core::BinaryOperator};
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
    builder.lower_vir(input, types);

    Block {
        arguments:                          builder.arguments,
        values:                             builder.values,
        return_types:                       builder.return_types,
        constants:                          builder.constants,
        functions:                          builder.functions.into_iter().map(|(n, _)| n).collect()
    }
}


//-------------------------------------------------------------------------------------------------

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


trait IntoValueDef {
    fn into_value_def(self) -> ValueDef;
}

impl IntoValueDef for ValueDef {
    fn into_value_def(self) -> ValueDef     { self }
}

impl IntoValueDef for &'static isa::Code {
    fn into_value_def(self) -> ValueDef     { ValueDef::Instr(self) }
}


//-------------------------------------------------------------------------------------------------

struct Builder<'arena> {
    arena:                                  &'arena Arena<Value<'arena>>,
    arguments:                              Vec<&'arena Value<'arena>>,
    values:                                 TiVec<ValueSlot, &'arena Value<'arena>>,
    return_types:                           Vec<Type>,
    constants:                              Vec<Constant>,
    operand_map:                            HashMap<usize, Operand<'arena>>,
    functions:                              Vec<(String, &'arena Value<'arena>)>,
}

impl<'arena> Builder<'arena> {
    fn new(arena: &'arena Arena<Value<'arena>>) -> Self {
        Self { arena, arguments: vec![], values: TiVec::new(), return_types: vec![],
            constants: vec![], operand_map: HashMap::new(), functions: vec![] }
    }


    fn lower_vir(&mut self, input: &vir::Block, types: &[vir::TypeInfo]) {
        for expr in &input.instrs {
            let ty = Type::from(types[expr.pool_index()]);
            match expr.kind() {
                #[cfg(any(feature = "dogfood", test))]
                vir::ExprKind::Argument(index, name) => {
                    let value = self.lower_value(ty, ValueDef::Argument(*index, name.clone()),
                        vec![], vec![], None, expr);
                    self.arguments.push(value);
                }
                #[cfg(not(any(feature = "dogfood", test)))]
                vir::ExprKind::Argument(..) => {
                    let value = self.lower_value(ty, ValueDef::Argument,
                        vec![], vec![], None, expr);
                    self.arguments.push(value);
                }
                vir::ExprKind::Number(value) => {
                    self.constants.push(Constant{ value: *value,
                    #[cfg(feature = "dogfood")]
                         span: *expr.definition_span()
                    });

                    let constant_index = self.constants.len() - 1;
                    self.lower_instr(ty, &isa::ldr_d_literal, vec![Operand::PooledF64(constant_index)], expr);
                }
                vir::ExprKind::Bool(value) => {
                    self.lower_instr(ty,
                        &isa::mov_x_imm, vec![Operand::ImmU16((*value).into())], expr);
                },
                vir::ExprKind::Binary(op, lhs, rhs) => {
                    if *op == BinaryOperator::Power {
                        self.lower_call("pow", ty, &[lhs.clone(), rhs.clone()], types,
                            REGS.abi_regs([ty]).next().unwrap(), expr);

                    } else if *op == BinaryOperator::Modulo {
                        // AArch64 has no fmod; compute a - trunc(a / b) * b instead.
                        let quotient = self.make_instr(ty,
                            &isa::fdiv_d, operands!(self, lhs, rhs),
                            #[cfg(feature = "dogfood")]
                            *expr.definition_span()
                        );
                        let truncated = self.make_instr(ty,
                            &isa::frintz_d, operands!(self, quotient),
                            #[cfg(feature = "dogfood")]
                            *expr.definition_span()
                        );
                        self.lower_instr(ty,
                            &isa::fmsub_d, operands!(self, truncated, rhs, lhs), expr);

                    } else {
                        let machine_instr = match op {
                            BinaryOperator::Add             => &isa::fadd_d,
                            BinaryOperator::Subtract        => &isa::fsub_d,
                            BinaryOperator::Multiply        => &isa::fmul_d,
                            BinaryOperator::Divide          => &isa::fdiv_d,

                            _                               => todo!("More machine ops")
                        };
                        self.lower_instr(ty, machine_instr, operands!(self, lhs, rhs), expr);
                    }
                }
                vir::ExprKind::Call(name, exprs) => {
                    self.lower_call(name, ty, exprs, types,
                        REGS.abi_regs([ty]).next().unwrap(), expr);
                }
            }
        }

        let return_values = self.exprs_to_fixed_inputs(&input.return_values, types);
        self.return_types = return_values.iter().map(|(v, _)| v.ty).collect();
        self.make_value(Type::NoValue, &isa::ret, vec![], return_values, None,
            #[cfg(feature = "dogfood")]
            input.return_span
        );
    }


    fn lower_instr(
        &mut self,
        ty:                                     Type,
        code:                                   &'static isa::Code,
        operands:                               Vec<Operand<'arena>>,
        expr:                                   &vir::Expr,
    ) -> &'arena Value<'arena> {
        self.lower_value(ty, code, operands, vec![], None, expr)
    }

    fn lower_call(
        &mut self,
        name:                                   &str,
        ty:                                     Type,
        operands:                               &[vir::Expr],
        types:                                  &[vir::TypeInfo],
        fixed_output:                           MachineReg,
        expr:                                   &vir::Expr,
    ) -> &'arena Value<'arena> {
        let fixed_inputs = self.exprs_to_fixed_inputs(operands, types);

        let function_value = if let Some(&(_, v)) = self.functions.iter().find(|(n, _)| n == name) {
            v
        } else {
            let v = self.make_instr(Type::FunctionPointer,
                &isa::ldr_x_literal, vec![Operand::Function(name.into())],
                #[cfg(feature = "dogfood")]
                *expr.definition_span()
            );
            self.functions.push((name.into(), v));
            v
        };

        self.lower_value(ty,
            &isa::blr, operands!(self, function_value), fixed_inputs, Some(fixed_output), expr)
    }

    fn lower_value<VD: IntoValueDef>(
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

    fn make_instr(
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

    fn make_value<VD: IntoValueDef>(
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


    fn exprs_to_fixed_inputs(
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
