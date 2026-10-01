#[cfg(unix)]
mod unix;

#[cfg(target_os = "macos")]
mod macos;

#[cfg(target_os = "macos")]
pub(super) use macos::{alloc_jit, free_jit, start_jit_compile, finish_jit_compile, resolve_symbol};

#[cfg(target_os = "linux")]
mod linux;

#[cfg(target_os = "linux")]
pub(super) use linux::{alloc_jit, free_jit, start_jit_compile, finish_jit_compile, resolve_symbol};

#[cfg(not(any(target_os = "macos", target_os = "linux")))]
compile_error!("footle only runs on macOS and Linux");
