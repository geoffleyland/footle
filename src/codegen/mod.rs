mod pass;
mod scheduler;
mod allocator;
mod assembler;
mod binary;
mod isa;
mod sys;

#[cfg(feature = "dogfood")]
mod disassembler;

pub use pass::run;

#[cfg(feature = "dogfood")]
pub use binary::CompiledFn;
#[cfg(feature = "dogfood")]
pub use pass::{schedule, assemble};
#[cfg(feature = "dogfood")]
pub use disassembler::disassemble;
