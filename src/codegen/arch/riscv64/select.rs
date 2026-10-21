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
                assemble!(self, span,
                    auipc base: Pointer, %constant(constant_index);
                    fld : ty, %constant(constant_index)(base)
                )
            }
            vir::InstrKind::Bool(value) => {
                assemble!(self, span, li :ty, Operand::ImmU16((*value).into()))
            },
            vir::InstrKind::Binary(op, lhs, rhs) => {
                if *op == BinaryOperator::Power {
                    self.select_call("pow", ty, &[lhs.clone(), rhs.clone()], types,
                        REGS.abi_regs([ty]).next().unwrap(), span)

                } else if *op == BinaryOperator::Modulo {
                    self.select_call("fmod", ty, &[lhs.clone(), rhs.clone()], types,
                        REGS.abi_regs([ty]).next().unwrap(), span)

                } else {
                    match op {
                        BinaryOperator::Add         => assemble!(self, span, fadd.d : ty, lhs, rhs),
                        BinaryOperator::Subtract    => assemble!(self, span, fsub.d : ty, lhs, rhs),
                        BinaryOperator::Multiply    => assemble!(self, span, fmul.d : ty, lhs, rhs),
                        BinaryOperator::Divide      => assemble!(self, span, fdiv.d : ty, lhs, rhs),
                        _                           => todo!("More machine ops")
                    }
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
            let v = assemble!(self, span,
                auipc base: Pointer, %function(name);
                ld : Pointer, %function(name)(base)
            );
            self.functions.push((name.into(), v));
            v
        };
        self.make_fixed_instr(ty,
            &isa::jalr, operands!(self, function_value), fixed_inputs, Some(fixed_output), span)
    }
}


//-------------------------------------------------------------------------------------------------

macro_rules! assemble {
    // The entry point.
    ($builder:expr, $span:expr, $($rest:tt)*)  => {{ assemble!(@suffix [$builder, $span] $($rest)*) }};

    // Deal with instruction suffixes by turning, say, `fmv.d` into `fmv_d`.
    (@suffix [$($header:tt),*] $opcode:ident . $suffix:ident $($rest:tt)*) => {
        ::paste::paste! { assemble!(@dest [$($header,)* [<$opcode _ $suffix>]] $($rest)*) }
    };
    (@suffix [$($header:tt),*] $opcode:ident $($rest:tt)*) => {
        assemble!(@dest [$($header,)* $opcode] $($rest)*)
    };

    (@dest [$($header:tt),*] $($dest:ident)? : $ty:expr, $($rest:tt)*) => {
        assemble!(@operands [$($header,)* ($($dest)?, $ty)] $($rest)*)
    };

    // Offset to a function or constant
    (@operands $header:tt % $kind:ident($arg:expr)($base:expr) $($rest:tt)*) => {
        assemble!(@line $header
            ($base, Operand::$kind($arg, RelativeTo::InstrOf($base.slot())))
            $($rest)*)
    };
    (@operands $header:tt % $kind:ident($arg:expr) $($rest:tt)*) => {
        assemble!(@line $header
            (Operand::$kind($arg, RelativeTo::ThisInstr))
            $($rest)*)
    };
    (@operands $header:tt $($operands:expr),* ; $($rest:tt)*) => {
        assemble!(@line $header ($($operands),*) ; $($rest)*)
    };
    (@operands $header:tt $($operands:expr),*) => {
        assemble!(@line $header ($($operands),*))
    };

    // The last line
    (@line [$builder:expr, $span:expr, $opcode:ident, ($($dest:ident)?,$ty:expr)] $operands:tt $(;)*) => {
        assemble!(@emit [$builder, $span, $opcode, $ty] $operands)
    };

    // Assemble a line and pass on the rest.
    (@line [$builder:expr, $span:expr, $opcode:ident, ($dest:ident,$ty:expr)] $operands:tt ; $($rest:tt)*) => {{
        let $dest = assemble!(@emit [$builder, $span, $opcode, $ty] $operands);
        assemble!(@suffix [$builder, $span] $($rest)*)
    }};

    // Finally, actually do some work.
    (@emit [$builder:expr, $span:expr, $opcode:ident, $ty:expr] ($($operands:expr),*)) => {
        $builder.make_instr(
            {
                #[allow(unused_imports)]
                use Type::*;
                $ty
            },
            &$crate::codegen::isa::$opcode,
            operands!($builder, $($operands),*),
            $span)
    };
}
use assemble;


//-------------------------------------------------------------------------------------------------
