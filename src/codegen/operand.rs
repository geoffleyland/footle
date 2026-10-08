use super::ValueSlot;


//-------------------------------------------------------------------------------------------------

/// What are offsets from the PC to the constant pool relative to?
///
/// Nearly always to the instruction that's being executed (`ThisInstr`), but on RISC-V in the
/// auipc, load combo, the offset the load uses has to be relative to the auipc instr.
#[derive(Debug, Copy, Clone)]
pub(super) enum RelativeTo {
    ThisInstr,
    #[cfg_attr(not(target_arch = "riscv64"),
        expect(dead_code, reason = "only RISC-V's auipc + load pairs measure from another instruction"))]
    InstrOf(ValueSlot)
}

#[derive(Debug, Clone)]
pub(super) enum Operand<REG> {
    PooledF64(usize, RelativeTo),
    ImmU16(u16),
    Function(String, RelativeTo),
    Reg(REG),
}

impl<REG> Operand<REG> {
    pub(super) fn map_reg<R2>(self, f: impl FnOnce(REG) -> R2) -> Operand<R2> {
        match self {
            Self::PooledF64(c, r)   => Operand::PooledF64(c, r),
            Self::ImmU16(v)         => Operand::ImmU16(v),
            Self::Function(s, r)    => Operand::Function(s, r),
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
                Self::PooledF64(i, ..)          => write!(f, "K{i}"),
                Self::ImmU16(v)                 => write!(f, "#{v}"),
                Self::Function(s, ..)           => write!(f, "{s}"),
                Self::Reg(r)                    => r.fmt_operand(f),
            }
        }
    }

}
