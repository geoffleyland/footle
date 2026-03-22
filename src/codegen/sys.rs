//-------------------------------------------------------------------------------------------------
// Apple Silicon Memory Management

use libc::{mmap, pthread_jit_write_protect_np,
        MAP_ANON, MAP_JIT, MAP_PRIVATE, PROT_EXEC, PROT_READ, PROT_WRITE, MAP_FAILED};

unsafe extern "C" {
    fn sys_icache_invalidate(start: *mut std::ffi::c_void, size: usize);
}

pub unsafe fn alloc_jit(size: usize) -> *mut u32 {
    let ptr = unsafe { mmap(
        std::ptr::null_mut(),
        size,
        PROT_READ | PROT_WRITE | PROT_EXEC,
        MAP_PRIVATE | MAP_ANON | MAP_JIT,
        -1, 0,
    ) };
    assert!(ptr != MAP_FAILED, "mmap failed");
    ptr.cast()
}

pub unsafe fn start_jit_compile() {
    unsafe { pthread_jit_write_protect_np(0); }
}

pub unsafe fn finish_jit_compile(code_ptr: *mut u32, size: usize) {
    unsafe {
        pthread_jit_write_protect_np(1);
        sys_icache_invalidate(code_ptr.cast(), size);
    }
}
