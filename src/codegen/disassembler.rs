use capstone::prelude::*;

use super::binary::CompiledFn;

pub fn disassemble(func: &CompiledFn) -> Vec<String> {
    Capstone::new()
        .arm64()
        .mode(capstone::arch::arm64::ArchMode::Arm)
        .build()
        .expect("Couldn't build disassembler")
        .disasm_count(func.bytes(), 0x1000, func.instruction_count)
        .expect("Couldn't disassemble code")
        .as_ref()
        .iter()
        .map(std::string::ToString::to_string)
        .collect()
}
