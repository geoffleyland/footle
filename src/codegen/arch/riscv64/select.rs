use crate::{vir, core::{Span, BinaryOperator}};
use crate::codegen::{isa, Code, RelativeTo, Constant, Value, Type,
    lower::{Builder, operands}, value::Operand};


//-------------------------------------------------------------------------------------------------

// This is the arch-specific part of codegen::lower::Builder.
impl Builder {
    pub(in crate::codegen) const RETURN: &'static Code = &isa::ret;
    pub(in crate::codegen) const CALL_REG: &'static Code = &isa::jalr;

    pub(in crate::codegen) fn select_instr(&mut self, instr: &vir::Instr, types: &[vir::TypeInfo]) -> Value {
        let ty = Type::from(types[instr.pool_index]);
        let span = instr.definition_span;
        match &instr.kind {
            vir::InstrKind::Number(value) => {
                let index = self.constants.push_and_get_key(Constant{ value: *value, span });
                assemble!(self, span,
                    auipc base: Pointer, %constant(index);
                    fld : ty, %constant(index)(base)
                )
            }
            vir::InstrKind::Bool(value) => {
                assemble!(self, span, li :ty, Operand::ImmU16((*value).into()))
            },
            vir::InstrKind::Binary(op, lhs, rhs) => {
                match op {
                    BinaryOperator::Add         => assemble!(self, span, fadd.d : ty, lhs, rhs),
                    BinaryOperator::Subtract    => assemble!(self, span, fsub.d : ty, lhs, rhs),
                    BinaryOperator::Multiply    => assemble!(self, span, fmul.d : ty, lhs, rhs),
                    BinaryOperator::Divide      => assemble!(self, span, fdiv.d : ty, lhs, rhs),

                    BinaryOperator::Power =>
                        self.lower_call("pow",  ty, &[lhs.clone(), rhs.clone()], types, span),
                    BinaryOperator::Modulo =>
                        self.lower_call("fmod", ty, &[lhs.clone(), rhs.clone()], types, span),

                    _                           => todo!("More machine ops")
                }
            }
            vir::InstrKind::Call(name, exprs) => self.lower_call(name, ty, exprs, types, span)
        }
    }

    pub(in crate::codegen) fn load_function_address(&mut self, name: &str, span: Span) -> Value {
        assemble!(self, span,
            auipc base: Pointer, %function(name);
            ld : Pointer, %function(name)(base)
        )
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
        $builder.emit_instr(
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
