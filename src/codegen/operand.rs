#[derive(Debug, Clone)]
pub(super) enum Operand<REG> {
    PooledF64(usize),
    Function(String),
    Reg(REG),
}

impl<REG> Operand<REG> {
    pub(super) fn map_reg<R2>(self, f: impl FnOnce(REG) -> R2) -> Operand<R2> {
        match self {
            Self::PooledF64(c)      => Operand::PooledF64(c),
            Self::Function(s)       => Operand::Function(s),
            Self::Reg(r)            => Operand::Reg(f(r)),
        }
    }
}

#[cfg(any(feature = "dogfood", test))]
pub mod display {
    use std::fmt;
    use super::*;

    pub trait OperandDisplay {
        fn fmt_operand(&self, f: &mut fmt::Formatter) -> fmt::Result;
    }

    impl<REG: OperandDisplay> fmt::Display for Operand<REG> {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match self {
                Self::PooledF64(i)              => write!(f, "K{i}"),
                Self::Function(s)               => write!(f, "{s}"),
                Self::Reg(r)                    => r.fmt_operand(f),
            }
        }
    }

}
