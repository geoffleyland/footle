use std::fmt;

use derive_more::{From, Into, Display};

use super::{MachineReg, Code};
use crate::vir;
#[cfg(feature = "dogfood")]
use crate::core::Span;


//-------------------------------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub(super) enum ValueDef {
    Instr(&'static Code),
    Argument,
}


//-------------------------------------------------------------------------------------------------

#[derive(Debug, Copy, Clone)]
pub(super) enum Type {
    NoValue,
    F64,
    I64,
    Pointer,
}

impl From<vir::TypeInfo> for Type {
    fn from(ty: vir::TypeInfo) -> Self {
        match ty {
            vir::TypeInfo::F64          => Self::F64,
            vir::TypeInfo::Bool         => Self::I64,
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Type::*;
        let str = match self {
            NoValue                     => "NoValue",
            I64                         => "i64",
            F64                         => "f64",
            Pointer                     => "Pointer",
        };
        write!(f, "{str}")
    }
}


//-------------------------------------------------------------------------------------------------

pub(super) type Operand<'arena> = super::operand::Operand<&'arena Instr<'arena>>;
impl<'arena> Operand<'arena> {
    fn value(&self) -> Option<&'arena Instr<'arena>> {
        if let Self::Reg(v) = self { Some(v) } else { None }
    }
}

/// The index of a `Value` in `Builder`'s `values` `TiVec`
///
/// `Values` are stored in the order they come out of `select_instrs`.
/// Scheduling re-orders the list of instructions and we use the indexes into `values` to create
/// side info arrays like list of users of the `Value` or critical path depths.
/// Only use `TiVec`s, not `TiSlice`s (even when passing references to `TiVec`s) - `TiSlices` risk
/// re-setting the zero on a slice and the numbering going out the window.
#[derive(Debug, Copy, Clone, From, Into, Display, PartialEq, Eq, Hash)]
pub(super) struct ValueSlot(usize);

#[derive(Debug)]
pub(super) struct Instr<'arena> {
    pub(super) slot:                        ValueSlot,
    pub(super) ty:                          Type,
    pub(super) def:                         ValueDef,
    pub(super) operands:                    Vec<Operand<'arena>>,
    pub(super) fixed_inputs:                Vec<(&'arena Self, MachineReg)>,
    pub(super) fixed_output:                Option<MachineReg>,
    #[cfg(feature = "dogfood")]
    pub(super) span:                        Span,
}

impl<'arena> Instr<'arena> {
    pub(super) fn new(
        slot:                               ValueSlot,
        ty:                                 Type,
        def:                                ValueDef,
        operands:                           Vec<Operand<'arena>>,
        fixed_inputs:                       Vec<(&'arena Self, MachineReg)>,
        fixed_output:                       Option<MachineReg>,
        #[cfg(feature = "dogfood")]
        span:                               Span
    ) -> Self {
        Self { slot, ty, def, operands, fixed_inputs, fixed_output,
        #[cfg(feature = "dogfood")]
            span
        }
    }


    pub(super) fn code(&self) -> Option<&'static Code> {
        if let ValueDef::Instr(code) = &self.def { Some(code) } else { None }
    }

    pub(super) fn latency(&self) -> u8                 { self.code().map_or(0, Code::latency) }

    pub(super) fn predecessors(&self) -> impl Iterator<Item = &'arena Instr<'arena>> {
        let operands = self.operands.iter().filter_map(Operand::value);
        let fixed_inputs = self.fixed_inputs.iter().map(|(v, _)| *v);
        operands.chain(fixed_inputs)
    }

    pub(super) fn needs_scheduling(&self) -> bool      { matches!(self.def, ValueDef::Instr(..)) }
}


//-------------------------------------------------------------------------------------------------

#[derive(Debug, Copy, Clone)]
pub(super) struct Constant {
    pub(super) value:                       f64,
    #[cfg(feature = "dogfood")]
    pub(super) span:                        Span
}


//-------------------------------------------------------------------------------------------------

#[cfg(feature = "dogfood")]
mod display {
    use super::*;

    impl super::super::operand::display::OperandDisplay for &'_ Instr<'_> {
        fn fmt_operand(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "I{}", self.slot)
        }
    }
}


//-------------------------------------------------------------------------------------------------
