mod operand;
mod pass;
mod value;
mod lower;
pub mod scheduler;
mod allocator;
pub mod assembler;
mod binary;
mod code;
mod regs;
mod arch;
mod sys;

pub use pass::run;
pub use binary::CompiledFn;
pub use lower::{Pool, Signature};

use value::{Arg, Instr, Value, ValueSlot, Type, Constant};
use regs::{MachineReg, RegBank, RegFile, RegSet, RankSet};
use arch::{isa, isa::REGS, isa::Unit, abi};
use code::Code;
use scheduler::SchedulePosition;
use assembler::{Layout, Resolved};
use operand::{ConstantIndex, RelativeTo};

#[cfg(feature = "dogfood")]
mod disassembler;
#[cfg(feature = "dogfood")]
pub use disassembler::disassemble;
