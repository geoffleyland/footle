use std::fmt;

use derive_more::{From, Into, Display};

use super::{MachineReg, Code};
use crate::vir;
#[cfg(feature = "dogfood")]
use crate::core::Span;


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

#[derive(Debug, Copy, Clone)]
pub(super) enum Value<'arena> {
    Instr(&'arena Instr<'arena>),
    Arg(ValueSlot, Type)
}
impl Value<'_> {
    fn needs_scheduling(&self) -> bool { matches!(self, Value::Instr(..)) }
    pub(super) fn slot(&self) -> ValueSlot {
        match self {
            Value::Instr(v)             => v.slot,
            Value::Arg(slot, _)         => *slot,
        }
    }
    pub(super) fn ty(&self) -> Type {
        match self {
            Value::Instr(v)             => v.ty,
            Value::Arg(_, ty)           => *ty,
        }
    }
}


pub(super) type Operand<'arena> = super::operand::Operand<Value<'arena>>;
impl<'arena> Operand<'arena> {
    fn value(&self) -> Option<Value<'arena>> {
        if let Self::Reg(v) = self { Some(*v) } else { None }
    }
}

/// The number of an `Instr` or `Arg` handed out by `Builder` in lowering order.
///
/// `Arg`s are stored in order and `Instr`s are stored in the order they come out of
/// `select_instrs`.  Scheduling re-orders the list of instructions and we use `ValueSlot`s
/// to create side-info arrays like lists of users of the `Instr`s or critical path depths.
/// The info arrays contain slots for `Arg`s too, since `Instr`'s operands will reference
/// them.
/// Only use `TiVec`s, not `TiSlice`s (even when passing references to `TiVec`s) - `TiSlices` risk
/// re-setting the zero on a slice and the numbering going out the window.
#[derive(Debug, Copy, Clone, From, Into, Display, PartialEq, Eq, Hash)]
pub(super) struct ValueSlot(usize);

#[derive(Debug)]
pub(super) struct Instr<'arena> {
    pub(super) slot:                        ValueSlot,
    pub(super) ty:                          Type,
    pub(super) code:                        &'static Code,
    pub(super) operands:                    Vec<Operand<'arena>>,
    pub(super) fixed_inputs:                Vec<(Value<'arena>, MachineReg)>,
    pub(super) fixed_output:                Option<MachineReg>,
    #[cfg(feature = "dogfood")]
    pub(super) span:                        Span,
}

impl<'arena> Instr<'arena> {
    pub(super) fn new(
        slot:                               ValueSlot,
        ty:                                 Type,
        code:                               &'static Code,
        operands:                           Vec<Operand<'arena>>,
        fixed_inputs:                       Vec<(Value<'arena>, MachineReg)>,
        fixed_output:                       Option<MachineReg>,
        #[cfg(feature = "dogfood")]
        span:                               Span
    ) -> Self {
        Self { slot, ty, code, operands, fixed_inputs, fixed_output,
        #[cfg(feature = "dogfood")]
            span
        }
    }

    pub(super) fn latency(&self) -> u8                 { self.code.latency() }

    fn predecessors(&self) -> impl Iterator<Item = Value<'arena>> {
        self.operands.iter().filter_map(Operand::value)
            .chain(self.fixed_inputs.iter().map(|(v, _)| *v))
    }
    pub(super) fn predecessor_instr_count(&self) -> usize {
        self.predecessors().filter(Value::needs_scheduling).count()
    }
    pub(super) fn predecessor_slots(&self) -> impl Iterator<Item = ValueSlot> {
        self.predecessors().map(|p| p.slot())
    }
}


//-------------------------------------------------------------------------------------------------

#[derive(Debug, Copy, Clone)]
pub(super) struct Arg {
    pub(super) slot:                        ValueSlot,
    pub(super) ty:                          Type,
    #[cfg(feature = "dogfood")]
    pub(super) span:                        Span,
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

    impl super::super::operand::display::OperandDisplay for Value<'_> {
        fn fmt_operand(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "I{}", self.slot())
        }
    }
}


//-------------------------------------------------------------------------------------------------
