use std::cell::OnceCell;

use bit_set::BitSet;

use super::scheduler::{Operand, Value};
use super::isa;


//-------------------------------------------------------------------------------------------------
// Register Allocation

pub(super) fn run<'arena>(
    arguments:              &[&Value<'arena>],
    values:                 &[&Value<'arena>],
    schedule:               &[&Value<'arena>]) -> Vec<u8> {
    allocate(arguments.len(), values, schedule)
}


//-------------------------------------------------------------------------------------------------
// Register Allocation

fn allocate<'arena>(
    argument_count:         usize,
    values:                 &[&Value<'arena>],
    schedule:               &[&Value<'arena>]) -> Vec<u8> {
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
    for slot in 0..argument_count {
        set_register(slot,
            u8::try_from(slot).expect("internal compiler error: too many arguments"),
            &registers, &interfering_values, &mut available_registers);
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
                    set_register(v.slot, r2, &registers, &interfering_values, &mut available_registers);
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
        set_register(value.slot, r, &registers, &interfering_values, &mut available_registers);
    }

    registers.iter_mut().map(|c| c.take().unwrap_or(0xFFu8)).collect::<Vec<_>>()
}


fn set_register(slot: usize, r: u8, registers: &[OnceCell<u8>], interfering_values: &[BitSet], available_registers: &mut [u32]) {
    let mri = isa::REGISTER_INDEX[usize::from(r)];
    let mri_bits = 1 << mri;
    available_registers[slot] = mri_bits;
    registers[slot].set(r).expect("internal compiler error: trying to set a register twice");
    for interfering_slot in &interfering_values[slot] {
        available_registers[interfering_slot] &= !mri_bits;
    }
}


//-------------------------------------------------------------------------------------------------
