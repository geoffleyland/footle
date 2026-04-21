use std::cell::OnceCell;
use std::collections::HashSet;

use bit_set::BitSet;

use crate::codegen::isa::real_reg_to_ordered_reg_mask;
use crate::core::Span;
use super::scheduler::Value;
use super::scheduler;
use super::isa;


//-------------------------------------------------------------------------------------------------
// Register Allocation

pub(super) fn run(
    argument_count:                     u8,
    slot_count:                         usize,
    scheduled:                           &[&Value<'_>]) -> (Vec<Instr>, Vec<u8>) {
    let (lowered, slot_count) = lower_to_slots_and_split(argument_count, slot_count, scheduled);
    let (registers, temp_registers) = allocate(argument_count, slot_count, &lowered);
    let mut registers_to_save = HashSet::new();
    for r in &registers {
        if *r < 32 && isa::CALLEE_SAVED_REGISTERS & (1 << r) != 0 {
            registers_to_save.insert(*r);
        }
    }
    let mut registers_to_save: Vec<_> = registers_to_save.into_iter().collect();
    registers_to_save.sort_unstable();
    (
        lower_to_registers(&lowered, &registers, &temp_registers),
        registers_to_save
    )
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
    slot_moves:                         Vec<(usize, usize)>,
    span:                               Span,
}


fn lower_to_slots_and_split(
    argument_count:                     u8,
    slot_count:                         usize,
    scheduled:                          &[&Value<'_>]) -> (Vec<SlotInstr>, usize) {

    // Walk backwards through the scheduled instructions finding out when instructions retire
    let mut retirements = vec![0; slot_count];
    let mut used_slots = BitSet::new();
    for (i, value) in scheduled.iter().enumerate().rev() {
        for op in &value.operands {
            let scheduler::Operand::Value(v) = op else { continue };
            if !used_slots.contains(v.slot) {
                retirements[v.slot] = i;
                used_slots.insert(v.slot);
            }
        }
    }

    let mut register_slots = vec![usize::MAX; 32];
    let mut slot_map = (0..slot_count).collect::<Vec<_>>();

    for slot in 0..argument_count {
        register_slots[usize::from(slot)] = usize::from(slot);
    }

    let mut slot_count = usize::from(argument_count);
    let mut new_schedule = vec![];

    for (i, value) in scheduled.iter().enumerate() {
        let operands = value.operands.iter().map(|op|
            match op {
                scheduler::Operand::Constant(i)             => SlotOperand::Constant(*i),
                scheduler::Operand::Value(v)                => SlotOperand::Slot(slot_map[v.slot]),
            }).collect();
        let mut slot_moves: Vec<(usize, usize, usize)> = vec![];
        if let Some(c) = value.code() && c.clobbers != 0 {
            let mut bits = c.clobbers;
            while bits != 0 {
                let reg = bits.trailing_zeros() as usize;
                let slot = register_slots[reg];
                if slot != usize::MAX &&
                    retirements[slot] > i {
                    slot_moves.push((slot, slot_map[slot], slot_count));
                    slot_map[slot] = slot_count;
                    slot_count += 1;
                }
                bits &= bits - 1;
            }
            let mut bits = c.clobbers;
            while bits != 0 {
                let reg = bits.trailing_zeros() as usize;
                register_slots[reg] = usize::MAX;
                bits &= bits - 1;
            }
        }
        let code = value.code().expect("internal compiler error: expected an excutable instruction");
        new_schedule.push(SlotInstr{
            operands, code,
            slot:                           slot_count,
            operand_registers:              value.operand_registers.clone(),
            slot_moves:                     slot_moves.iter().map(|(_, b, c)| (*b, *c)).collect(),
            span:                           value.span,
        });
        slot_map[value.slot] = slot_count;
        slot_count += 1;
    }
    (new_schedule, slot_count)
}


//-------------------------------------------------------------------------------------------------
// Register Allocation

fn allocate(
    argument_count:                     u8,
    slot_count:                         usize,
    instrs:                             &[SlotInstr]) -> (Vec<u8>, Vec<u8>) {
    let mut registers: Vec<OnceCell<u8>> = vec![OnceCell::new(); slot_count];

    // Find which slots interfere with which.
    let mut live_slots = BitSet::new();
    let mut interfering_slots = vec![BitSet::new(); slot_count];
    for instr in instrs.iter().rev() {
        live_slots.remove(instr.slot);
        for (_, dest) in &instr.slot_moves { live_slots.remove(*dest); }
        for op in &instr.operands {
            let SlotOperand::Slot(op_slot) = op else { continue };
            live_slots.insert(*op_slot);
        }
        for (source, _) in &instr.slot_moves { live_slots.insert(*source); }
        for slot in &live_slots {
            interfering_slots[slot].union_with(&live_slots);
        }
    }
    // Slots don't interfere with themselves - and this matters because we use available_registers
    // post-allocation to find a temporary register for every instruction (only used if the
    // instruction needs moves), and available_registers in turn depends on interfering_slots.
    for instr in instrs { interfering_slots[instr.slot].remove(instr.slot); }

    // Compute available registers for each instruction: any values that are live across a call
    // have to be in a callee-saved register.
    let mut available_registers = vec![0xFFFF_FFFFu32; slot_count];
    for instr in instrs {
        if instr.code.clobbers != 0 {
            let mask = !real_reg_to_ordered_reg_mask(instr.code.clobbers);
            available_registers[instr.slot] &= mask;
            for slot in &interfering_slots[instr.slot] {
                available_registers[slot] &= mask;
            }
        }
    }

    // Allocate registers for arguments
    for slot in 0..argument_count {
        set_register(usize::from(slot), slot, &registers, &interfering_slots, &mut available_registers);
    }

    // Allocate registers for any values that need to be in a register as an argument.
    for instr in instrs {
        if let Some(operand_registers) = &instr.operand_registers {
            for (op, &r) in instr.operands.iter().zip(operand_registers) {
                if let SlotOperand::Slot(op_slot) = op {
                    if registers[*op_slot].get().is_some() { continue; }
                    let mri = isa::REGISTER_INDEX[usize::from(r)];
                    // If we can get the register we want, great!  But if we can't just assign the
                    // slot to any old register, and the move machinery will get the value into
                    // the right register at the right moment.
                    // (Getting the right register here is just about avoiding a move if we can,
                    // but in all cases, we'll get the move right)
                    let r2 = if (available_registers[*op_slot] >> mri) & 1 == 1 { r }
                        else {
                            let mri2 = available_registers[*op_slot].trailing_zeros();
                            isa::REGISTER_ORDER[mri2 as usize]
                        };
                    set_register(*op_slot, r2, &registers, &interfering_slots, &mut available_registers);
                } else {
                    panic!("internal compiler error: register constraint on constant");
                }
            }
        }
    }

    // Allocate registers for remaining instructions
    for instr in instrs {
        if registers[instr.slot].get().is_some() || !instr.code.has_output { continue; }
        let mri = available_registers[instr.slot].trailing_zeros();
        let r = isa::REGISTER_ORDER[mri as usize];
        set_register(instr.slot, r, &registers, &interfering_slots, &mut available_registers);
    }

    // Allocate registers for any slots that get moved (which don't show up in instructions)
    for instr in instrs {
        for (_, dest) in &instr.slot_moves {
            if registers[*dest].get().is_some() { continue; }
            let mri = available_registers[*dest].trailing_zeros();
            let r = isa::REGISTER_ORDER[mri as usize];
            set_register(*dest, r, &registers, &interfering_slots, &mut available_registers);
        }
    }

    (
        registers.iter_mut().map(|c| c.take().unwrap_or(0xFFu8)).collect(),
        available_registers.iter().map(|&a| isa::REGISTER_ORDER[a.trailing_zeros() as usize]).collect()
    )
}


fn set_register(
    slot:                               usize,
    r:                                  u8,
    registers:                          &[OnceCell<u8>],
    interfering_slots:                  &[BitSet],
    available_registers:                &mut [u32]) {
    let mri = isa::REGISTER_INDEX[usize::from(r)];
    let mri_bits = 1 << mri;
    registers[slot].set(r).expect("internal compiler error: trying to set a register twice");
    for interfering_slot in &interfering_slots[slot] {
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
    pub(super) moves:                   Vec<(u8, u8)>,
    pub(super) temp_register:           u8,
    pub(super) span:                    Span,
}


fn lower_to_registers(
    instrs:                             &[SlotInstr],
    registers:                          &[u8],
    temp_registers:                     &[u8]) -> Vec<Instr> {

    instrs.iter().map(|instr| {
        let mut operands = vec![];
        let mut moves = vec![];
        for (i, op) in instr.operands.iter().enumerate() {
            match op {
                SlotOperand::Constant(i)    => operands.push(Operand::Constant(*i)),
                SlotOperand::Slot(s) => {
                    // If there's a required register for this operand, us it.  We don't fix up the
                    // register for the slot here (because it might be a last use of the operand)
                    // we just trust the move machinery in the assembler to get things in the
                    // right place (which they do)
                    let maybe_required_register = instr.operand_registers.as_ref().map(|r| r[i]);
                    let slot_register = registers[*s];
                    if let Some(required_register) = maybe_required_register
                        && slot_register != required_register {
                        moves.push((slot_register, required_register));
                        operands.push(Operand::Register(required_register));
                    } else {
                        operands.push(Operand::Register(slot_register));
                    }
                }
            }
        }
        for (source, dest) in &instr.slot_moves {
            if registers[*source] != registers[*dest] {
                moves.push((registers[*source], registers[*dest]));
            }
        }

        Instr{
            operands, moves,
            code:                       instr.code,
            output_register:            registers[instr.slot],
            temp_register:              temp_registers[instr.slot],
            span:                       instr.span,
        }
    })
    .collect()
}


//-------------------------------------------------------------------------------------------------
