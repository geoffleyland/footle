use capstone::prelude::*;

use super::CompiledFn;

pub fn disassemble(func: &CompiledFn) -> Vec<String> {
    #[cfg(target_arch = "aarch64")]
    let builder = Capstone::new().arm64().mode(capstone::arch::arm64::ArchMode::Arm);

    builder
        .build()
        .expect("Couldn't build disassembler")
        .disasm_count(func.bytes(), 0x1000, func.instr_count)
        .expect("Couldn't disassemble code")
        .as_ref()
        .iter()
        .map(std::string::ToString::to_string)
        .collect()
}
