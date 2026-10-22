use crate::{vir, core::{Span, BinaryOperator}};
use crate::codegen::{isa, Code, RelativeTo, Constant, Value, Type,
    lower::{Builder, operands}, value::Operand};


//-------------------------------------------------------------------------------------------------

// This is the arch-specific part of codegen::lower::Builder.
impl Builder {
    pub(in crate::codegen) const RETURN: &'static Code = &isa::ret;
    pub(in crate::codegen) const CALL_REG: &'static Code = &isa::blr;

    pub(in crate::codegen) fn select_instr(&mut self, instr: &vir::Instr, types: &[vir::TypeInfo]) -> Value {
        let ty = Type::from(types[instr.pool_index]);
        let span = instr.definition_span;
        match &instr.kind {
            vir::InstrKind::Number(value) => {
                let index = self.constants.push_and_get_key(Constant{ value: *value, span });
                self.emit_instr(ty, &isa::ldr_d_literal,
                    vec![Operand::constant(index, RelativeTo::ThisInstr)], span)
            }
            vir::InstrKind::Bool(value) => {
                self.emit_instr(ty,
                    &isa::mov_x_imm, vec![Operand::ImmU16((*value).into())], span)
            },
            vir::InstrKind::Binary(op, lhs, rhs) => {
                if *op == BinaryOperator::Power {
                    self.lower_call("pow", ty, &[lhs.clone(), rhs.clone()], types, span)

                } else if *op == BinaryOperator::Modulo {
                    self.lower_call("fmod", ty, &[lhs.clone(), rhs.clone()], types, span)

                } else {
                    let machine_instr = match op {
                        BinaryOperator::Add             => &isa::fadd_d,
                        BinaryOperator::Subtract        => &isa::fsub_d,
                        BinaryOperator::Multiply        => &isa::fmul_d,
                        BinaryOperator::Divide          => &isa::fdiv_d,

                        _                               => todo!("More machine ops")
                    };
                    self.emit_instr(ty, machine_instr, operands!(self, lhs, rhs), span)
                }
            }
            vir::InstrKind::Call(name, exprs) => self.lower_call(name, ty, exprs, types, span)
        }
    }

    pub(in crate::codegen) fn load_function_address(&mut self, name: &str, span: Span) -> Value {
        self.emit_instr(Type::Pointer,
            &isa::ldr_x_literal, vec![Operand::function(name, RelativeTo::ThisInstr)],
            span)
    }
}


//-------------------------------------------------------------------------------------------------
