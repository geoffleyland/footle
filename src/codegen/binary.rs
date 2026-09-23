use std::mem;

use anyhow::{bail, Result};

use super::scheduler::{Constant, Type};
use super::assembler;
use super::sys;
use crate::runtime::Value;


//-------------------------------------------------------------------------------------------------

pub struct CompiledFn {
    ptr:                            *mut u32,
    size:                           usize,
    argument_types:                 Vec<Type>,
    return_types:                   Vec<Type>,
    func:                           fn(*const u64, *mut u64),

    #[cfg(feature = "dogfood")]
    pub(super) instruction_count:   usize,
}


impl CompiledFn {
    fn new(
        ptr:                        *mut u32,
        size:                       usize,
        glue_start_words:           usize,
        argument_types:             &[Type],
        return_types:               &[Type],
        #[cfg(feature = "dogfood")]
        instruction_count:          usize,
    ) -> Self {
        let func = unsafe { mem::transmute::<*mut u32, fn(*const u64, *mut u64)>(ptr.add(glue_start_words)) };

        Self { ptr, size, func,
            argument_types: argument_types.to_vec(),
            return_types: return_types.to_vec(),
        #[cfg(feature = "dogfood")]
            instruction_count
        }
    }

    pub fn call(&self, input: &[Value]) -> Result<Vec<Value>> {
        if self.argument_types.len() != input.len() {
            bail!("wrong number of arguments: expected {}, got {}",
                self.argument_types.len(), input.len());
        }
        let mut lowered_input = vec![];
        for (e, i) in self.argument_types.iter().zip(input) {
            let bits = match (e, i) {
                (Type::F64, Value::F64(v))  => { v.to_bits() },
                (Type::I64, Value::Bool(v)) => { (*v).into() },
                _ => {
                    bail!("wrong type for argument #{}.  Expected `{e}`, got `{i}`",
                        lowered_input.len() + 1);
                }
            };
            lowered_input.push(bits);
        }

        let mut output = vec![0u64; self.return_types.len()];
        (self.func)(lowered_input.as_ptr(), output.as_mut_ptr());
        let result = output.iter().zip(&self.return_types).map(|(&bits, &ty)| match ty {
            Type::F64 => Value::F64(f64::from_bits(bits)),
            Type::I64 => Value::Bool(bits != 0),
            _ => unreachable!("internal compiler error: not a return type"),
        }).collect();
        Ok(result)
    }

    #[cfg(feature = "dogfood")]
    pub(super) fn bytes(&self) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.ptr.cast::<u8>(), self.size) }
    }
}


impl Drop for CompiledFn {
    fn drop(&mut self) { sys::free_jit(self.ptr, self.size); }
}


//-------------------------------------------------------------------------------------------------

pub fn emit(block: &assembler::Block) -> CompiledFn {
    let instr_words = block.instrs.len();
    let constant_start_words = instr_words + usize::from(instr_words.is_multiple_of(2));
    let function_start_words = constant_start_words + block.constants.len() * 2;
    let total_code_size_bytes = function_start_words * 4 + 8 * block.functions.len();

    let ptr = sys::alloc_jit(total_code_size_bytes);
    let words = jit_as_words_mut(ptr, total_code_size_bytes);

    sys::start_jit_compile();

    encode_instrs(&block.instrs, words, constant_start_words, function_start_words);
    encode_constants(&block.constants, words, constant_start_words);
    encode_functions(&block.functions, words, function_start_words);

    sys::finish_jit_compile(ptr, total_code_size_bytes);

    CompiledFn::new(ptr, total_code_size_bytes, block.glue_start_words, &block.argument_types, &block.return_types,
        #[cfg(feature = "dogfood")]
        instr_words
        )
}


fn encode_instrs(
    instrs:                         &[assembler::Instr],
    words:                          &mut [u32],
    constant_start_words:           usize,
    function_start_words:           usize) {
    use assembler::Operand::*;
    for (word_index, instr) in instrs.iter().enumerate() {
        let operands = instr.operands.iter().map(|op| {
            match op {
                Reg(i)                  => u32::from(*i),
                ImmU16(v)               => u32::from(*v),
                PooledF64(i)            => u32::try_from((constant_start_words - word_index) * 4 + (*i * 8))
                                            .expect("internal compiler error: constant offset too large"),
                Function(i)             => u32::try_from((function_start_words - word_index) * 4 + (*i * 8))
                                            .expect("internal compiler error: function offset too large"),
                Offset(o)               => o.cast_unsigned(),
            }}).collect::<Vec<_>>();
        words[word_index] = (instr.code.encode)(&operands);
    }
}


fn encode_constants(constants: &[Constant], words: &mut [u32], constant_start_words: usize) {
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
