use std::cell::OnceCell;

use bit_set::BitSet;

use crate::core::Span;
use super::scheduler::Value;
use super::scheduler;
use super::isa;


//-------------------------------------------------------------------------------------------------
// Register Allocation

pub(super) fn run(
    argument_count:                     u8,
    slot_count:                         usize,
    scheduled:                           &[&Value<'_>]) -> Vec<Instr> {
    let lowered = lower_to_slots(scheduled);
    let registers = allocate(argument_count, slot_count, &lowered);
    lower_to_registers(lowered, &registers)
}


//-------------------------------------------------------------------------------------------------
// Lower Values to SlotInstrs

#[derive(Debug)]
enum SlotOperand {
    Constant(usize),
    Slot(usize),
}

#[derive(Debug)]
struct SlotInstr {
    slot:                               usize,
    code:                               &'static isa::Code,
    operands:                           Vec<SlotOperand>,
    operand_registers:                  Option<Vec<u8>>,
    span:                               Span,
}


fn lower_to_slots(
    schedule: &[&Value<'_>]) -> Vec<SlotInstr> {

    let mut lowered = vec![];
    for value in schedule {
        let operands = value.operands.iter().map(|op|
            match op {
                scheduler::Operand::Constant(i)             => SlotOperand::Constant(*i),
                scheduler::Operand::Value(v)                => SlotOperand::Slot(v.slot),
            }).collect();
        let code = value.code().expect("internal compiler error: expected an excutable instruction");
        lowered.push(SlotInstr{
            operands, code,
            slot:                       value.slot,
            operand_registers:          value.operand_registers.clone(),
            span:                       value.span,
        });
    }
    lowered
}


//-------------------------------------------------------------------------------------------------
// Register Allocation

fn allocate(
    argument_count:                     u8,
    slot_count:                         usize,
    instrs:                             &[SlotInstr]) -> Vec<u8> {
    let mut registers: Vec<OnceCell<u8>> = vec![OnceCell::new(); slot_count];

    // Find which values interfere with which.
    let mut live_values = BitSet::new();
    let mut interfering_values = vec![BitSet::new(); slot_count];
    for instr in instrs.iter().rev() {
        live_values.remove(instr.slot);
        for op in &instr.operands {
            let SlotOperand::Slot(op_slot) = op else { continue };
            live_values.insert(*op_slot);
        }
        for slot in &live_values {
            interfering_values[slot].union_with(&live_values);
        }
    }

    let mut available_registers = vec![0xFFFF_FFFFu32; slot_count];
    for slot in 0..argument_count {
        set_register(usize::from(slot), slot, &registers, &interfering_values, &mut available_registers);
    }

    // Scan instructions for any register constraints
    for instr in instrs {
        if let Some(operand_registers) = &instr.operand_registers {
            for (op, &r) in instr.operands.iter().zip(operand_registers) {
                if let SlotOperand::Slot(op_slot) = op {
                    if registers[*op_slot].get().is_some() { continue; }
                    let mri = isa::REGISTER_INDEX[usize::from(r)];
                    let r2 = if (available_registers[*op_slot] >> mri) & 1 == 1 { r }
                        else {
                            let mri2 = available_registers[*op_slot].trailing_zeros();
                            isa::REGISTER_ORDER[mri2 as usize]
                        };
                    set_register(*op_slot, r2, &registers, &interfering_values, &mut available_registers);
                } else {
                    panic!("internal compiler error: register constraint on constant");
                }
            }
        }
    }

    for instr in instrs {
        if registers[instr.slot].get().is_some() || !instr.code.has_output { continue; }
        let mri = available_registers[instr.slot].trailing_zeros();
        let r = isa::REGISTER_ORDER[mri as usize];
        set_register(instr.slot, r, &registers, &interfering_values, &mut available_registers);
    }

    registers.iter_mut().map(|c| c.take().unwrap_or(0xFFu8)).collect::<Vec<_>>()
}


fn set_register(
    slot:                               usize,
    r:                                  u8,
    registers:                          &[OnceCell<u8>],
    interfering_values:                 &[BitSet],
    available_registers:                &mut [u32]) {
    let mri = isa::REGISTER_INDEX[usize::from(r)];
    let mri_bits = 1 << mri;
    available_registers[slot] = mri_bits;
    registers[slot].set(r).expect("internal compiler error: trying to set a register twice");
    for interfering_slot in &interfering_values[slot] {
        available_registers[interfering_slot] &= !mri_bits;
    }
}


//-------------------------------------------------------------------------------------------------
// Lower SlotIntrs to allocator::Instrs

#[derive(Debug)]
pub(super) enum Operand {
    Constant(usize),
    Register(u8),
}

#[derive(Debug)]
pub(super) struct Instr {
    pub(super) code:                    &'static isa::Code,
    pub(super) output_register:         u8,
    pub(super) operands:                Vec<Operand>,
    pub(super) operand_registers:       Option<Vec<u8>>,
    pub(super) span:                    Span,
}


fn lower_to_registers(
    instrs:                             Vec<SlotInstr>,
    registers:                          &[u8]) -> Vec<Instr> {

    let mut lowered = vec![];
    for instr in instrs {
        let operands = instr.operands.iter().map(|op|
            match op {
                SlotOperand::Constant(i)                    => Operand::Constant(*i),
                SlotOperand::Slot(s)                        => Operand::Register(registers[*s]),
            }).collect();
        lowered.push(Instr{
            operands,
            code:                       instr.code,
            output_register:            registers[instr.slot],
            operand_registers:          instr.operand_registers,
            span:                       instr.span,
        });
    }
    lowered
}


//-------------------------------------------------------------------------------------------------
