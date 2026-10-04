mod operand;
mod pass;
mod value;
mod lower;
pub mod scheduler;
mod allocator;
pub mod assembler;
mod binary;
mod isa;
mod sys;

pub use pass::run;
pub use binary::CompiledFn;

use value::{Value, ValueDef, ValueSlot, Type, Constant};
use isa::{MachineReg, RegBank, RegFile, RegSet, REGS};
use scheduler::SchedulePosition;

#[cfg(feature = "dogfood")]
mod disassembler;
#[cfg(feature = "dogfood")]
pub use disassembler::disassemble;
