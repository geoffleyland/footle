use std::mem;

use super::{assembler, sys, Pool, Signature, Type, Layout, Resolved};
use crate::runtime::Value;


//-------------------------------------------------------------------------------------------------

pub struct CompiledFn {
    ptr:                            *mut u32,
    pub(super) layout:              Layout,
    signature:                      Signature,
    func:                           unsafe extern "C" fn(*const u64, *mut u64),
}


impl CompiledFn {
    fn new(
        ptr:                        *mut u32,
        layout:                     Layout,
        signature:                  Signature,
    ) -> Self {
        let func = unsafe {
            mem::transmute::<*mut u32, unsafe extern "C" fn(*const u64, *mut u64)>(
                ptr.add(layout.glue_start_words)) };
        Self { ptr, layout, signature, func }
    }

    /// # Safety
    ///
    /// `input` must match `self.signature.args` in length and types: the compiled code reads its
    /// arguments through a raw pointer.  Our only caller (`runtime::Block::call`) has to get it
    /// right because it dispatches off the arguments.
    pub unsafe fn call(&self, input: &[Value]) -> Vec<Value> {
        debug_assert_eq!(self.signature.args.len(), input.len(), "wrong number of arguments");
        debug_assert!(self.signature.args.iter().map(|a| a.ty).zip(input)
            .all(|p| matches!(p, (Type::F64, Value::F64(..)) | (Type::I64, Value::Bool(..)))),
            "wrong type for argument");

        let lowered_input: Vec<_> = input.iter().map(|i| match i {
            Value::F64(v)           => { v.to_bits() },
            Value::Bool(v)          => { (*v).into() },
        }).collect();

        let mut output = vec![0u64; self.signature.return_types.len()];
        unsafe { (self.func)(lowered_input.as_ptr(), output.as_mut_ptr()) };
        output.iter().zip(&self.signature.return_types).map(|(&bits, &ty)| match ty {
            Type::F64           => Value::F64(f64::from_bits(bits)),
            // FIXME: at the moment our only I64 is a bool, but it won't always be that way.
            Type::I64           => Value::Bool(bits != 0),
            // FIXME: a pointer type shouldn't even get to here.
            Type::Pointer       => unreachable!("internal compiler error: not a return type"),
        }).collect()
    }

    #[cfg(feature = "dogfood")]
    pub(super) fn bytes(&self) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.ptr.cast::<u8>(), self.layout.total_bytes) }
    }
}


impl Drop for CompiledFn {
    fn drop(&mut self) { sys::free_jit(self.ptr, self.layout.total_bytes); }
}


//-------------------------------------------------------------------------------------------------

pub fn emit(
    block:              &assembler::Block,
    signature:          Signature,
    pool:               &Pool
) -> CompiledFn {
    let ptr = sys::alloc_jit(block.layout.total_bytes);
    let words = jit_as_words_mut(ptr, block.layout.total_bytes);

    sys::start_jit_compile();

    encode_instrs(&block.instrs, words);
    encode_u64s(pool.constants.iter().map(|c| c.value.to_bits()), words, block.layout.constant_start_words);
    encode_u64s(pool.functions.iter().map(|f| sys::resolve_symbol(f)), words, block.layout.function_start_words);

    sys::finish_jit_compile(ptr, block.layout.total_bytes);

    CompiledFn::new(ptr, block.layout, signature)
}


fn encode_instrs(
    instrs:                         &[assembler::Instr<Resolved>],
    words:                          &mut [u32]) {
    for (word_index, instr) in instrs.iter().enumerate() {
        words[word_index] = instr.code.encode(&instr.operands);
    }
}


fn encode_u64s(values: impl Iterator<Item = u64>, words: &mut [u32], start_words: usize) {
    for (index, value) in values.enumerate() {
        let (lo, hi) = u64_words(value);
        words[start_words + 2 * index] = lo;
        words[start_words + 2 * index + 1] = hi;
    }
}


#[allow(clippy::cast_possible_truncation)]
fn u64_words(value: u64) -> (u32, u32) {
    (value as u32, (value >> 32) as u32)
}


//-------------------------------------------------------------------------------------------------

fn jit_as_words_mut<'a>(ptr: *mut u32, size_bytes: usize) -> &'a mut [u32] {
    unsafe { std::slice::from_raw_parts_mut(ptr, size_bytes / 4) }
}

//-------------------------------------------------------------------------------------------------
