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
    #[cfg(any(feature = "dogfood", test))]
    Argument(usize, String),
    #[cfg(not(any(feature = "dogfood", test)))]
    Argument,
}


//-------------------------------------------------------------------------------------------------

#[derive(Debug, Copy, Clone)]
pub(super) enum Type {
    NoValue,
    F64,
    I64,
    FunctionPointer,
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
            NoValue                         => "NoValue",
            I64                             => "i64",
            F64                             => "f64",
            FunctionPointer                 => "FunctionPointer",
        };
        write!(f, "{str}")
    }
}


//-------------------------------------------------------------------------------------------------

pub(super) type Operand<'arena> = super::operand::Operand<&'arena Value<'arena>>;
impl<'arena> Operand<'arena> {
    fn value(&self) -> Option<&'arena Value<'arena>> {
        if let Self::Reg(v) = self { Some(v) } else { None }
    }
}

/// The index of a `Value` in `Builder`'s `values` `TiVec`
///
/// `Values` are stored in the order they come out of `lower_vir`.
/// Scheduling re-orders the list of instructions and we use the indexes into `values` to create
/// side info arrays like list of users of the `Value` or critical path depths.
/// Only use `TiVec`s, not `TiSlice`s (even when passing references to `TiVec`s) - `TiSlices` risk
/// re-setting the zero on a slice and the numbering going out the window.
#[derive(Debug, Copy, Clone, From, Into, Display, PartialEq, Eq, Hash)]
pub(super) struct ValueSlot(usize);

#[derive(Debug)]
pub(super) struct Value<'arena> {
    pub(super) slot:                        ValueSlot,
    pub(super) ty:                          Type,
    pub(super) def:                         ValueDef,
    pub(super) operands:                    Vec<Operand<'arena>>,
    pub(super) fixed_inputs:                Vec<(&'arena Self, MachineReg)>,
    pub(super) fixed_output:                Option<MachineReg>,
    #[cfg(feature = "dogfood")]
    pub(super) span:                        Span,
}

impl<'arena> Value<'arena> {
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

    pub(super) fn predecessors(&self) -> impl Iterator<Item = &'arena Value<'arena>> {
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

#[cfg(any(feature = "dogfood", test))]
mod display {
    use super::*;

    impl super::super::operand::display::OperandDisplay for &'_ Value<'_> {
        fn fmt_operand(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "I{}", self.slot)
        }
    }


    impl fmt::Display for ValueDef {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            use ValueDef::*;
            match self {
                Instr(code)                     => write!(f, "{}", code.mnemonic()),
                Argument(i, name)               => write!(f, "ARGUMENT r{i} ({name})"),
            }
        }
    }


    impl fmt::Display for Value<'_> {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "I{}: {} {}", self.slot, self.def,
                self.operands
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(" "))
        }
    }
}

//-------------------------------------------------------------------------------------------------
