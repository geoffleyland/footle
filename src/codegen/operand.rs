use derive_more::{From, Into, Display};

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

#[derive(Debug, Copy, Clone, From, Into, Display)]
pub(super) struct ConstantIndex(usize);
#[derive(Debug, Copy, Clone, From, Into, Display)]
pub(super) struct FunctionIndex(usize);

#[derive(Debug, Copy, Clone)]
pub(super) enum Operand<REG> {
    Reg(REG),
    Imm(i32),
    PooledF64(ConstantIndex, RelativeTo),
    Function(FunctionIndex, RelativeTo),
}

impl<REG> Operand<REG> {
    pub(super) fn map_reg<R2>(self, f: impl FnOnce(REG) -> R2) -> Operand<R2> {
        match self {
            Self::Reg(r)            => Operand::Reg(f(r)),
            Self::Imm(v)            => Operand::Imm(v),
            Self::PooledF64(c, r)   => Operand::PooledF64(c, r),
            Self::Function(i, r)    => Operand::Function(i, r),
        }
    }
    pub(super) fn constant(index: ConstantIndex, relative_to: RelativeTo) -> Self {
        Self::PooledF64(index, relative_to)
    }
    pub(super) fn function(index: FunctionIndex, relative_to: RelativeTo) -> Self {
        Self::Function(index, relative_to)
    }
}

#[cfg(feature = "dogfood")]
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
                Self::Imm(v)                    => write!(f, "#{v}"),
                Self::Function(i, ..)           => write!(f, "F{i}"),
                Self::Reg(r)                    => r.fmt_operand(f),
            }
        }
    }

}
