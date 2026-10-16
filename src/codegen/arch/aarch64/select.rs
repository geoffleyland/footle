use crate::{vir, core::BinaryOperator};
use crate::codegen::{isa, RelativeTo, Constant, Instr, Type, MachineReg, REGS,
    lower::{Builder, operands}, value::Operand};

//-------------------------------------------------------------------------------------------------

// This is the arch-specific part of codegen::lower::Builder.
impl<'arena> Builder<'arena> {
    pub fn select_instrs(&mut self, input: &vir::Block, types: &[vir::TypeInfo]) {
        for instr in &input.instrs {
            let ty = Type::from(types[instr.pool_index]);
            match &instr.kind {
                vir::InstrKind::Number(value) => {
                    self.constants.push(Constant{ value: *value,
                    #[cfg(feature = "dogfood")]
                         span: instr.definition_span
                    });

                    let constant_index = self.constants.len() - 1;
                    self.lower_instr(ty, &isa::ldr_d_literal,
                        vec![Operand::PooledF64(constant_index, RelativeTo::ThisInstr)], instr);
                }
                vir::InstrKind::Bool(value) => {
                    self.lower_instr(ty,
                        &isa::mov_x_imm, vec![Operand::ImmU16((*value).into())], instr);
                },
                vir::InstrKind::Binary(op, lhs, rhs) => {
                    if *op == BinaryOperator::Power {
                        self.select_call("pow", ty, &[lhs.clone(), rhs.clone()], types,
                            REGS.abi_regs([ty]).next().unwrap(), instr);

                    } else if *op == BinaryOperator::Modulo {
                        self.select_call("fmod", ty, &[lhs.clone(), rhs.clone()], types,
                            REGS.abi_regs([ty]).next().unwrap(), instr);

                    } else {
                        let machine_instr = match op {
                            BinaryOperator::Add             => &isa::fadd_d,
                            BinaryOperator::Subtract        => &isa::fsub_d,
                            BinaryOperator::Multiply        => &isa::fmul_d,
                            BinaryOperator::Divide          => &isa::fdiv_d,

                            _                               => todo!("More machine ops")
                        };
                        self.lower_instr(ty, machine_instr, operands!(self, lhs, rhs), instr);
                    }
                }
                vir::InstrKind::Call(name, exprs) => {
                    self.select_call(name, ty, exprs, types,
                        REGS.abi_regs([ty]).next().unwrap(), instr);
                }
            }
        }

        let return_values = self.exprs_to_fixed_inputs(&input.return_values, types);
        self.return_types = return_values.iter().map(|(v, _)| v.ty()).collect();
        self.make_fixed_instr(Type::NoValue, &isa::ret, vec![], return_values, None,
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
        instr:                                  &vir::Instr,
    ) -> &'arena Instr<'arena> {
        let fixed_inputs = self.exprs_to_fixed_inputs(operands, types);

        let function_value = if let Some(&(_, v)) = self.functions.iter().find(|(n, _)| n == name) {
            v
        } else {
            let v = self.make_instr(Type::Pointer,
                &isa::ldr_x_literal, vec![Operand::Function(name.into(), RelativeTo::ThisInstr)],
                #[cfg(feature = "dogfood")]
                instr.definition_span
            );
            self.functions.push((name.into(), v));
            v
        };

        self.lower_fixed_instr(ty,
            &isa::blr, operands!(self, function_value), fixed_inputs, Some(fixed_output), instr)
    }
}


//-------------------------------------------------------------------------------------------------
