//-------------------------------------------------------------------------------------------------
// Unix memory management and symbol lookup common to all platforms

pub fn free_jit(ptr: *mut u32, size: usize) {
    unsafe { libc::munmap(ptr.cast(), size); }
}


pub fn resolve_symbol(name: &str) -> u64 {
    let c_name = std::ffi::CString::new(name)
        .expect("internal compiler error: function name has interior null");
    let ptr = unsafe { libc::dlsym(libc::RTLD_DEFAULT, c_name.as_ptr()) };
    assert!(!ptr.is_null(), "internal compiler error: function '{name}' not found");
    ptr as u64
}
