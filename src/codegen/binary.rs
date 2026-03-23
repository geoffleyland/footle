use std::mem;
use super::pass;
use super::sys;


//-------------------------------------------------------------------------------------------------

pub fn emit(block: &pass::AssemblyBlock) -> fn(f64) -> f64 {
    let instr_words = block.assembler.len();
    let constant_start_words = instr_words + usize::from(instr_words.is_multiple_of(2));
    let total_code_size_bytes = constant_start_words * 4 + 8 * block.constants.len();

    let code_ptr = sys::alloc_jit(total_code_size_bytes);
    let words = jit_as_words_mut(code_ptr, total_code_size_bytes);

    sys::start_jit_compile();

    encode_instrs(&block.assembler, words, constant_start_words);
    encode_constants(&block.constants, words, constant_start_words);

    sys::finish_jit_compile(code_ptr, total_code_size_bytes);
    unsafe { mem::transmute(code_ptr) }
}


fn encode_instrs(assembler: &[pass::AssemblyInstr], words: &mut [u32], constant_start_words: usize) {
    use pass::AssemblyOperand::*;
    for (address, instr) in assembler.iter().enumerate() {
        let operands = instr.operands.iter().map(|op| {
            match op {
                Register(i)            => u32::from(*i),
                Constant(i)            => u32::try_from((constant_start_words - address) * 4 + (*i * 8)).unwrap(),
            }}).collect::<Vec<_>>();
        words[address] = (instr.code.encode)(&operands);
    }
}


fn encode_constants(constants: &[pass::Constant], words: &mut [u32], constant_start_words: usize) {
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
