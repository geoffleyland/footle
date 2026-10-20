use crate::{vir, core::{BinaryOperator, Span}};
use crate::codegen::{isa, RelativeTo, Constant, Value, Type, MachineReg, REGS,
    lower::{Builder, operands}, value::Operand};


//-------------------------------------------------------------------------------------------------

// This is the arch-specific part of codegen::lower::Builder.
impl Builder {
    pub fn select_instr(&mut self, instr: &vir::Instr, types: &[vir::TypeInfo]) -> Value {
        let ty = Type::from(types[instr.pool_index]);
        let span = instr.definition_span;
        match &instr.kind {
            vir::InstrKind::Number(value) => {
                self.constants.push(Constant{ value: *value, span });
                let constant_index = self.constants.len() - 1;
                let base = self.make_instr(Type::Pointer, &isa::auipc,
                    vec![Operand::PooledF64(constant_index, RelativeTo::ThisInstr)], span);
                self.make_instr(ty, &isa::fld,
                    operands!(self, base, Operand::PooledF64(constant_index, RelativeTo::InstrOf(base.slot()))),
                    span)
            }
            vir::InstrKind::Bool(value) => {
                self.make_instr(ty, &isa::li, vec![Operand::ImmU16((*value).into())], span)
            },
            vir::InstrKind::Binary(op, lhs, rhs) => {
                if *op == BinaryOperator::Power {
                    self.select_call("pow", ty, &[lhs.clone(), rhs.clone()], types,
                        REGS.abi_regs([ty]).next().unwrap(), span)

                } else if *op == BinaryOperator::Modulo {
                    self.select_call("fmod", ty, &[lhs.clone(), rhs.clone()], types,
                        REGS.abi_regs([ty]).next().unwrap(), span)

                } else {
                    let machine_instr = match op {
                        BinaryOperator::Add             => &isa::fadd_d,
                        BinaryOperator::Subtract        => &isa::fsub_d,
                        BinaryOperator::Multiply        => &isa::fmul_d,
                        BinaryOperator::Divide          => &isa::fdiv_d,

                        _                               => todo!("More machine ops")
                    };
                    self.make_instr(ty, machine_instr, operands!(self, lhs, rhs), span)
                }
            }
            vir::InstrKind::Call(name, exprs) => {
                self.select_call(name, ty, exprs, types,
                    REGS.abi_regs([ty]).next().unwrap(), span)
            }
        }
    }


    pub fn select_return(&mut self, input: &vir::Block, types: &[vir::TypeInfo]) {
        let return_values = self.exprs_to_fixed_inputs(&input.return_values, types);
        self.return_types = return_values.iter().map(|(v, _)| v.ty()).collect();
        self.make_fixed_instr(Type::NoValue, &isa::ret, vec![], return_values, None, input.return_span);
    }


    fn select_call(
        &mut self,
        name:                                   &str,
        ty:                                     Type,
        operands:                               &[vir::Expr],
        types:                                  &[vir::TypeInfo],
        fixed_output:                           MachineReg,
        span:                                   Span,
    ) -> Value {
        let fixed_inputs = self.exprs_to_fixed_inputs(operands, types);

        let function_value = if let Some(&(_, v)) = self.functions.iter().find(|(n, _)| n == name) {
            v
        } else {
            let base = self.make_instr(Type::Pointer,
                &isa::auipc, vec![Operand::Function(name.into(), RelativeTo::ThisInstr)], span);
            let v = self.make_instr(Type::Pointer, &isa::ld,
                operands!(self, base, Operand::Function(name.into(), RelativeTo::InstrOf(base.slot()))),
                span);

            self.functions.push((name.into(), v));
            v
        };

        self.make_fixed_instr(ty,
            &isa::jalr, operands!(self, function_value), fixed_inputs, Some(fixed_output), span)
    }
}


//-------------------------------------------------------------------------------------------------
