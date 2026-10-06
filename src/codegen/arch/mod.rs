#[cfg(target_arch = "aarch64")]
mod aarch64;
#[cfg(target_arch = "riscv64")]
mod riscv64;

#[cfg(target_arch = "aarch64")]
pub(super) use aarch64::{isa, abi};
#[cfg(target_arch = "riscv64")]
pub(super) use riscv64::{isa, abi};
