mod operand;
mod pass;
pub mod scheduler;
mod allocator;
pub mod assembler;
mod binary;
mod isa;
mod sys;

#[cfg(feature = "dogfood")]
mod disassembler;

pub use pass::run;

#[cfg(feature = "dogfood")]
pub use binary::CompiledFn;
#[cfg(feature = "dogfood")]
pub use pass::{Observer, run_observed};
#[cfg(feature = "dogfood")]
pub use disassembler::disassemble;
