//-------------------------------------------------------------------------------------------------
// Linux memory management

use libc::{mmap, mprotect, MAP_ANON, MAP_PRIVATE, PROT_EXEC, PROT_READ, PROT_WRITE, MAP_FAILED};

pub use super::unix::{free_jit, resolve_symbol};

unsafe extern "C" {
    // From libgcc (or compiler-rt).  Takes start and end, not start and size.
    fn __clear_cache(start: *mut std::ffi::c_char, end: *mut std::ffi::c_char);
}

pub fn alloc_jit(size: usize) -> *mut u32 {
    let ptr = unsafe { mmap(
        std::ptr::null_mut(),
        size,
        PROT_READ | PROT_WRITE,
        MAP_PRIVATE | MAP_ANON,
        -1, 0,
    ) };
    assert!(ptr != MAP_FAILED, "mmap failed");
    ptr.cast()
}


pub fn start_jit_compile() {}

pub fn finish_jit_compile(code_ptr: *mut u32, size: usize) {
    unsafe {
        let result = mprotect(code_ptr.cast(), size, PROT_READ | PROT_EXEC);
        assert!(result == 0, "mprotect failed");
        let start = code_ptr.cast::<std::ffi::c_char>();
        __clear_cache(start, start.add(size));
    }
}
