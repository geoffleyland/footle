use crate::{vir, core::BinaryOperator};
use crate::codegen::{isa, RelativeTo, Constant, Value, ValueDef, Type, MachineReg, REGS,
    lower::{Builder, operands}, value::Operand};


//-------------------------------------------------------------------------------------------------

// This is the arch-specific part of codegen::lower::Builder.
impl<'arena> Builder<'arena> {
    pub fn select_instrs(&mut self, input: &vir::Block, types: &[vir::TypeInfo]) {
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
                    let base = self.make_instr(Type::Pointer, &isa::auipc,
                        vec![Operand::PooledF64(constant_index, RelativeTo::ThisInstr)],
                            #[cfg(feature = "dogfood")]
                            *expr.definition_span()
                     );
                    self.lower_instr(ty, &isa::fld,
                        operands!(self, base, Operand::PooledF64(constant_index, RelativeTo::InstrOf(base.slot))), expr);
                }
                vir::ExprKind::Bool(value) => {
                    self.lower_instr(ty,
                        &isa::li, vec![Operand::ImmU16((*value).into())], expr);
                },
                vir::ExprKind::Binary(op, lhs, rhs) => {
                    if *op == BinaryOperator::Power {
                        self.select_call("pow", ty, &[lhs.clone(), rhs.clone()], types,
                            REGS.abi_regs([ty]).next().unwrap(), expr);

                    } else if *op == BinaryOperator::Modulo {
                        self.select_call("fmod", ty, &[lhs.clone(), rhs.clone()], types,
                            REGS.abi_regs([ty]).next().unwrap(), expr);

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
                    self.select_call(name, ty, exprs, types,
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


    fn select_call(
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
            let base = self.make_instr(Type::Pointer,
                &isa::auipc, vec![Operand::Function(name.into(), RelativeTo::ThisInstr)],
                #[cfg(feature = "dogfood")]
                *expr.definition_span()
            );
            let v = self.make_instr(Type::Pointer, &isa::ld,
                operands!(self, base, Operand::Function(name.into(), RelativeTo::InstrOf(base.slot))),
                #[cfg(feature = "dogfood")]
                *expr.definition_span()
            );

            self.functions.push((name.into(), v));
            v
        };

        self.lower_value(ty,
            &isa::jalr, operands!(self, function_value), fixed_inputs, Some(fixed_output), expr)
    }
}


//-------------------------------------------------------------------------------------------------
