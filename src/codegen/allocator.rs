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
    let (regs, temp_regs) = allocate(argument_count, slot_count, &lowered);
    let mut regs_to_save = HashSet::new();
    for r in &regs {
        if *r != u8::MAX && isa::CALLEE_SAVED_REGS & (1 << r) != 0 {
            regs_to_save.insert(*r);
        }
    }
    let mut regs_to_save: Vec<_> = regs_to_save.into_iter().collect();
    regs_to_save.sort_unstable();
    (
        lower_to_regs(&lowered, &regs, &temp_regs),
        regs_to_save
    )
}


//-------------------------------------------------------------------------------------------------
// Lower Values to SlotInstrs

#[derive(Debug)]
enum SlotOperand {
    Constant(usize),
    Function(&'static str),
    Slot(usize),
}

#[derive(Debug)]
struct SlotInstr {
    slot:                               usize,
    code:                               &'static isa::Code,
    operands:                           Vec<SlotOperand>,
    fixed_inputs:                       Vec<(usize, u8)>,
    fixed_output:                       Option<u8>,
    slot_moves:                         Vec<(usize, usize)>,
    span:                               Span,
}


impl SlotInstr {
    pub(super) fn predecessors(&self) -> impl Iterator<Item = usize> {
        let operands = self.operands.iter().filter_map(|op| {
            if let SlotOperand::Slot(s) = op { Some(*s) } else { None }
        });
        let fixed_inputs = self.fixed_inputs.iter().map(|(v, _)| *v);
        operands.chain(fixed_inputs)
    }
}


fn lower_to_slots_and_split(
    argument_count:                     u8,
    slot_count:                         usize,
    scheduled:                          &[&Value<'_>]) -> (Vec<SlotInstr>, usize) {

    // Walk backwards through the scheduled instructions finding out when instructions retire
    let mut retirements = vec![0; slot_count];
    let mut used_slots = BitSet::new();
    for (i, value) in scheduled.iter().enumerate().rev() {
        for predecessor in value.predecessors() {
            if !used_slots.contains(predecessor.slot) {
                retirements[predecessor.slot] = i;
                used_slots.insert(predecessor.slot);
            }
        }
    }

    let mut reg_slots = vec![usize::MAX; 32];
    let mut slot_map = (0..slot_count).collect::<Vec<_>>();

    for slot in 0..argument_count {
        reg_slots[usize::from(slot)] = usize::from(slot);
    }

    let mut slot_count = usize::from(argument_count);
    let mut new_schedule = vec![];

    for (i, value) in scheduled.iter().enumerate() {
        let operands = value.operands.iter().map(|op|
            match op {
                scheduler::Operand::Constant(i)             => SlotOperand::Constant(*i),
                scheduler::Operand::Value(v)                => SlotOperand::Slot(slot_map[v.slot]),
                scheduler::Operand::Function(s)             => SlotOperand::Function(s),
            }).collect();
        let fixed_inputs = value.fixed_inputs.iter().map(|(v, reg)| (slot_map[v.slot], *reg)).collect();
        let mut slot_moves: Vec<(usize, usize)> = vec![];
        if let Some(c) = value.code() && c.clobbers != 0 {
            let mut bits = c.clobbers;
            while bits != 0 {
                let reg = bits.trailing_zeros() as usize;
                let slot = reg_slots[reg];
                if slot != usize::MAX &&
                    retirements[slot] > i {
                    slot_moves.push((slot_map[slot], slot_count));
                    slot_map[slot] = slot_count;
                    slot_count += 1;
                }
                bits &= bits - 1;
            }
            let mut bits = c.clobbers;
            while bits != 0 {
                let reg = bits.trailing_zeros() as usize;
                reg_slots[reg] = usize::MAX;
                bits &= bits - 1;
            }
        }
        if let Some(fixed_output) = value.fixed_output {
            reg_slots[usize::from(fixed_output)] = value.slot;
        }
        let code = value.code().expect("internal compiler error: expected an excutable instruction");
        new_schedule.push(SlotInstr{
            operands, code, slot_moves, fixed_inputs,
            slot:                           slot_count,
            fixed_output:                   value.fixed_output,
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
    let mut regs: Vec<OnceCell<u8>> = vec![OnceCell::new(); slot_count];

    // Find which slots interfere with which, and which are live across calls.
    // If they are live, make sure they're not in a clobbered register.
    let mut live_slots = BitSet::new();
    let mut interfering_slots = vec![BitSet::new(); slot_count];
    let mut available_regs = vec![0xFFFF_FFFFu32; slot_count];

    for instr in instrs.iter().rev() {
        live_slots.remove(instr.slot);
        if instr.code.clobbers != 0 {
            let mask = !real_reg_to_ordered_reg_mask(instr.code.clobbers);
            for slot in &live_slots {
                available_regs[slot] &= mask;
            }
        }
        for (_, dest) in &instr.slot_moves { live_slots.remove(*dest); }
        for slot in instr.predecessors() { live_slots.insert(slot); }
        for (source, _) in &instr.slot_moves { live_slots.insert(*source); }
        for slot in &live_slots {
            interfering_slots[slot].union_with(&live_slots);
        }
    }
    // Slots don't interfere with themselves - and this matters because we use available_regs
    // post-allocation to find a temporary register for every instruction (only used if the
    // instruction needs moves), and available_registers in turn depends on interfering_slots.
    for instr in instrs { interfering_slots[instr.slot].remove(instr.slot); }

    // Allocate registers for arguments
    for slot in 0..argument_count {
        set_reg(usize::from(slot), slot, &regs, &interfering_slots, &mut available_regs);
    }

    // Allocate registers for value with constrained output registers.
    for instr in instrs {
        if let Some(fixed_output) = instr.fixed_output {
            set_reg(instr.slot, fixed_output, &regs, &interfering_slots, &mut available_regs);
        }
    }

    // Allocate registers for values with constrained operand registers.
    for instr in instrs {
        for (input_slot, reg) in &instr.fixed_inputs {
            if regs[*input_slot].get().is_some() { continue; }
            let mri = isa::REG_INDEX[usize::from(*reg)];
            // If we can get the register we want, great!  But if we can't just assign the
            // slot to any old register, and the move machinery will get the value into
            // the right register at the right moment.
            // (Getting the right register here is just about avoiding a move if we can,
            // but in all cases, we'll get the move right)
            let r2 = if (available_regs[*input_slot] >> mri) & 1 == 1 { *reg }
                else {
                    let mri2 = available_regs[*input_slot].trailing_zeros();
                    isa::REG_ORDER[mri2 as usize]
                };
            set_reg(*input_slot, r2, &regs, &interfering_slots, &mut available_regs);
        }
    }

    // Allocate registers for remaining instructions
    for instr in instrs {
        if regs[instr.slot].get().is_some() || !instr.code.has_output() { continue; }
        let mri = available_regs[instr.slot].trailing_zeros();
        let r = isa::REG_ORDER[mri as usize];
        set_reg(instr.slot, r, &regs, &interfering_slots, &mut available_regs);
    }

    // Allocate registers for any slots that get moved (which don't show up in instructions)
    for instr in instrs {
        for (_, dest) in &instr.slot_moves {
            if regs[*dest].get().is_some() { continue; }
            let mri = available_regs[*dest].trailing_zeros();
            let r = isa::REG_ORDER[mri as usize];
            set_reg(*dest, r, &regs, &interfering_slots, &mut available_regs);
        }
    }

    (
        regs.iter_mut().map(|c| c.take().unwrap_or(u8::MAX)).collect(),
        available_regs.iter().map(|&a| isa::REG_ORDER[a.trailing_zeros() as usize]).collect()
    )
}


fn set_reg(
    slot:                               usize,
    r:                                  u8,
    regs:                               &[OnceCell<u8>],
    interfering_slots:                  &[BitSet],
    available_regs:                     &mut [u32]) {
    let mri = isa::REG_INDEX[usize::from(r)];
    let mri_bits = 1 << mri;
    regs[slot].set(r).expect("internal compiler error: trying to set a register twice");
    for interfering_slot in &interfering_slots[slot] {
        available_regs[interfering_slot] &= !mri_bits;
    }
}


//-------------------------------------------------------------------------------------------------
// Lower SlotIntrs to allocator::Instrs

#[derive(Debug)]
pub(super) enum Operand {
    Constant(usize),
    Function(&'static str),
    Reg(u8),
}

#[derive(Debug)]
pub(super) struct Instr {
    pub(super) code:                    &'static isa::Code,
    pub(super) result_reg:              u8,
    pub(super) operands:                Vec<Operand>,
    pub(super) moves:                   Vec<(u8, u8)>,
    pub(super) temp_reg:                u8,
    pub(super) span:                    Span,
}


fn lower_to_regs(
    instrs:                             &[SlotInstr],
    regs:                               &[u8],
    temp_regs:                     &[u8]) -> Vec<Instr> {

    instrs.iter().map(|instr| {
        let mut operands = vec![];
        for op in &instr.operands {
            match op {
                SlotOperand::Constant(i)    => operands.push(Operand::Constant(*i)),
                SlotOperand::Function(s)    => operands.push(Operand::Function(s)),
                SlotOperand::Slot(s)        => operands.push(Operand::Reg(regs[*s])),
            }
        }
        let mut moves = vec![];
        for (slot, required_reg) in &instr.fixed_inputs {
            let slot_reg = regs[*slot];
            if slot_reg != *required_reg { moves.push((slot_reg, *required_reg)); }
        }
        for (source, dest) in &instr.slot_moves {
            if regs[*source] != regs[*dest] { moves.push((regs[*source], regs[*dest])); }
        }

        Instr{
            operands, moves,
            code:                       instr.code,
            result_reg:                 regs[instr.slot],
            temp_reg:                   temp_regs[instr.slot],
            span:                       instr.span,
        }
    })
    .collect()
}


//-------------------------------------------------------------------------------------------------
