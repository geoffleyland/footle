use std::mem;

use anyhow::{bail, Result};
use typed_index_collections::TiVec;

use super::{assembler, sys, Constant, ConstantIndex, Pool, Signature, Type, Layout, Resolved};
use crate::runtime::Value;


//-------------------------------------------------------------------------------------------------

pub struct CompiledFn {
    ptr:                            *mut u32,
    pub(super) layout:              Layout,
    signature:                      Signature,
    func:                           fn(*const u64, *mut u64),
}


impl CompiledFn {
    fn new(
        ptr:                        *mut u32,
        layout:                     Layout,
        signature:                  Signature,
    ) -> Self {
        let func = unsafe { mem::transmute::<*mut u32, fn(*const u64, *mut u64)>(ptr.add(layout.glue_start_words)) };

        Self { ptr, layout, signature, func }
    }

    pub fn call(&self, input: &[Value]) -> Result<Vec<Value>> {
        if self.signature.args.len() != input.len() {
            bail!("wrong number of arguments: expected {}, got {}",
                self.signature.args.len(), input.len());
        }
        let mut lowered_input = vec![];
        for (index, (e, i)) in self.signature.args.iter().map(|a| a.ty).zip(input).enumerate() {
            let bits = match (e, i) {
                (Type::F64, Value::F64(v))  => { v.to_bits() },
                (Type::I64, Value::Bool(v)) => { (*v).into() },
                _ => {
                    bail!("wrong type for argument #{}.  Expected `{e}`, got `{i}`", index+1);
                }
            };
            lowered_input.push(bits);
        }

        let mut output = vec![0u64; self.signature.return_types.len()];
        (self.func)(lowered_input.as_ptr(), output.as_mut_ptr());
        let result = output.iter().zip(&self.signature.return_types).map(|(&bits, &ty)| match ty {
            Type::F64           => Value::F64(f64::from_bits(bits)),
            Type::I64           => Value::Bool(bits != 0),
            // FIXME: a pointer type shouldn't even get to here.
            Type::Pointer       => unreachable!("internal compiler error: not a return type"),
        }).collect();
        Ok(result)
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
    encode_constants(&pool.constants, words, block.layout.constant_start_words);
    encode_functions(&pool.functions, words, block.layout.function_start_words);

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


fn encode_constants(constants: &TiVec<ConstantIndex, Constant>, words: &mut [u32], constant_start_words: usize) {
    let mut index = constant_start_words;
    for c in constants {
        let bits = c.value.to_bits();
        let (lo, hi) = u64_words(bits);
        words[index] = lo;
        words[index+1] = hi;
        index += 2;
    }
}


fn encode_functions(functions: &[String], words: &mut [u32], function_start_words: usize) {
    let mut index = function_start_words;
    for f in functions {
        let ptr = sys::resolve_symbol(f);
        let (lo, hi) = u64_words(ptr);
        words[index] = lo;
        words[index+1] = hi;
        index += 2;
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
