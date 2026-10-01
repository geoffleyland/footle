#[cfg(target_os = "macos")]
mod macos;

#[cfg(target_os = "macos")]
pub(super) use macos::{alloc_jit, free_jit, start_jit_compile, finish_jit_compile, resolve_symbol};
