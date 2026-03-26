use std::cell::OnceCell;

use bit_set::BitSet;

use super::scheduler::{Operand, Value};
use super::isa;


//-------------------------------------------------------------------------------------------------
// Register Allocation

pub(super) fn run<'arena>(
    arguments: &[&Value<'arena>],
    values: &[&Value<'arena>],
    schedule: &[&Value<'arena>]) -> Vec<u8> {
    let mut registers: Vec<OnceCell<u8>> = vec![OnceCell::new(); values.len()];

    // Find which values interfere with which.
    let mut live_values = BitSet::new();
    let mut interfering_values = vec![BitSet::new(); values.len()];
    for value in schedule.iter().rev() {
        live_values.remove(value.slot);
        for op in &value.operands {
            let Operand::Value(v) = op else { continue };
            live_values.insert(v.slot);
        }
        for slot in &live_values {
            interfering_values[slot].union_with(&live_values);
        }
    }

    let mut available_registers = vec![0xFFFF_FFFFu32; values.len()];
    for (r, value) in (0u8..).zip(arguments) {
        set_register(value, r, &registers, &interfering_values, &mut available_registers);
    }

    // Scan instructions for any register constraints
    for value in schedule {
        if let Some(operand_registers) = &value.operand_registers {
            for (op, &r) in value.operands.iter().zip(operand_registers) {
                if let Operand::Value(v) = op {
                    if registers[v.slot].get().is_some() { continue; }
                    let mri = isa::REGISTER_INDEX[usize::from(r)];
                    let r2 = if (available_registers[v.slot] >> mri) & 1 == 1 { r }
                        else {
                            let mri2 = available_registers[v.slot].trailing_zeros();
                            isa::REGISTER_ORDER[mri2 as usize]
                        };
                    set_register(v, r2, &registers, &interfering_values, &mut available_registers);
                } else {
                    panic!("internal compiler error: register constraint on constant");
                }
            }
        }
    }

    for value in schedule {
        if registers[value.slot].get().is_some() || !value.has_output() { continue; }
        let mri = available_registers[value.slot].trailing_zeros();
        let r = isa::REGISTER_ORDER[mri as usize];
        set_register(value, r, &registers, &interfering_values, &mut available_registers);
    }

    registers.iter_mut().map(|c| c.take().unwrap_or(0xFFu8)).collect::<Vec<_>>()
}


fn set_register(value: &Value<'_>, r: u8, registers: &[OnceCell<u8>], interfering_values: &[BitSet], available_registers: &mut [u32]) {
    let mri = isa::REGISTER_INDEX[usize::from(r)];
    let mri_bits = 1 << mri;
    available_registers[value.slot] = mri_bits;
    registers[value.slot].set(r).expect("internal compiler error: trying to set a register twice");
    for slot in &interfering_values[value.slot] {
        available_registers[slot] &= !mri_bits;
    }
}


//-------------------------------------------------------------------------------------------------
