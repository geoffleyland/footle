use crate::{vir, core::{Span, BinaryOperator}};
use crate::codegen::{isa, RelativeTo, Constant, Value, Type, MachineReg,
    lower::{Builder, operands}, value::Operand};

//-------------------------------------------------------------------------------------------------

// This is the arch-specific part of codegen::lower::Builder.
impl Builder {
    pub(in crate::codegen) fn select_instr(&mut self, instr: &vir::Instr, types: &[vir::TypeInfo]) -> Value {
        let ty = Type::from(types[instr.pool_index]);
        let span = instr.definition_span;
        match &instr.kind {
            vir::InstrKind::Number(value) => {
                self.constants.push(Constant{ value: *value, span });
                let constant_index = self.constants.len() - 1;
                self.emit_instr(ty, &isa::ldr_d_literal,
                    vec![Operand::constant(constant_index, RelativeTo::ThisInstr)], span)
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


    pub(in crate::codegen) fn select_return(&mut self, input: &vir::Block, types: &[vir::TypeInfo]) {
        let return_values = self.exprs_to_abi_regs(&input.return_values, types);
        self.return_types = return_values.iter().map(|(v, _)| v.ty()).collect();
        self.emit_return(&isa::ret, return_values, input.return_span);
    }


    pub(in crate::codegen) fn load_function_address(&mut self, name: &str, span: Span) -> Value {
        self.emit_instr(Type::Pointer,
            &isa::ldr_x_literal, vec![Operand::function(name, RelativeTo::ThisInstr)],
            span)
    }

    pub(in crate::codegen) fn select_call(
        &mut self,
        ty:                                     Type,
        function:                               Value,
        args:                                   Vec<(Value, MachineReg)>,
        output:                                 MachineReg,
        span:                                   Span,
    ) -> Value {
        self.emit_abi_instr(ty, &isa::blr, operands!(self, function), args, Some(output), span)
    }
}


//-------------------------------------------------------------------------------------------------
