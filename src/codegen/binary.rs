use std::mem;

use super::scheduler::Constant;
use super::assembler;
use super::sys;


//-------------------------------------------------------------------------------------------------

pub struct CompiledFn {
    ptr:                *mut u32,
    size:               usize,
    pub func:           fn(f64) -> f64,
}


impl CompiledFn {
    fn new(ptr: *mut u32, size: usize) -> CompiledFn {
        Self { ptr, size, func: unsafe { mem::transmute(ptr) } }
    }

    pub fn func(&self) -> fn(f64) -> f64        { self.func }
}


impl Drop for CompiledFn {
    fn drop(&mut self) {
        sys::free_jit(self.ptr, self.size);
    }
}


//-------------------------------------------------------------------------------------------------

pub fn emit(block: &assembler::Block) -> CompiledFn {
    let instr_words = block.instrs.len();
    let constant_start_words = instr_words + usize::from(instr_words.is_multiple_of(2));
    let total_code_size_bytes = constant_start_words * 4 + 8 * block.constants.len();

    let ptr = sys::alloc_jit(total_code_size_bytes);
    let words = jit_as_words_mut(ptr, total_code_size_bytes);

    sys::start_jit_compile();

    encode_instrs(&block.instrs, words, constant_start_words);
    encode_constants(&block.constants, words, constant_start_words);

    sys::finish_jit_compile(ptr, total_code_size_bytes);

    CompiledFn::new(ptr, total_code_size_bytes)
}


fn encode_instrs(instrs: &[assembler::Instr], words: &mut [u32], constant_start_words: usize) {
    use assembler::Operand::*;
    for (address, instr) in instrs.iter().enumerate() {
        let operands = instr.operands.iter().map(|op| {
            match op {
                Register(i)            => u32::from(*i),
                Constant(i)            => u32::try_from((constant_start_words - address) * 4 + (*i * 8)).unwrap(),
            }}).collect::<Vec<_>>();
        words[address] = (instr.code.encode)(&operands);
    }
}


fn encode_constants(constants: &[Constant], words: &mut [u32], constant_start_words: usize) {
    let mut index = constant_start_words;
    for c in constants {
        let bits = c.value.to_bits();
        words[index] = (bits & 0xFFFF_FFFF) as u32;
        index += 1;
        words[index] = (bits >> 32) as u32;
        index += 1;
    }
}


//-------------------------------------------------------------------------------------------------

fn jit_as_words_mut<'a>(ptr: *mut u32, size_bytes: usize) -> &'a mut [u32] {
    unsafe { std::slice::from_raw_parts_mut(ptr, size_bytes / 4) }
}

//-------------------------------------------------------------------------------------------------
