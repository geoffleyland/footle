#[cfg(target_arch = "aarch64")]
mod aarch64;
#[cfg(target_arch = "riscv64")]
mod riscv64;

#[cfg(target_arch = "aarch64")]
pub(super) use aarch64::{isa, emit};
#[cfg(target_arch = "riscv64")]
pub(super) use riscv64::{isa, emit};

#[cfg(not(any(target_arch = "aarch64", target_arch = "riscv64")))]
compile_error!("footle only generates code for aarch64 and riscv64");
