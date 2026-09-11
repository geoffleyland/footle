use std::cell::OnceCell;
use std::collections::BTreeSet;

use bit_set::BitSet;

use super::scheduler::Value;
use super::scheduler;
use super::isa;
use super::isa::{REGS, D_BANK, MachineReg};

#[cfg(feature = "dogfood")]
use crate::core::Span;


//-------------------------------------------------------------------------------------------------
// Register Allocation

pub(super) fn run(
    slot_count:                         usize,
    arguments:                          &[&Value<'_>],
    scheduled:                          &[&Value<'_>],
) -> (Vec<Instr>, Vec<MachineReg>) {
    let (lowered, slot_count) = lower_to_slots_and_split(slot_count, arguments, scheduled);
    let (regs, temp_regs) = allocate(arguments.len(), slot_count, &lowered);
    let mut regs_to_save = BTreeSet::new();
    for maybe_reg in &regs {
        if let Some(r) = REGS.is_callee_saved(D_BANK, *maybe_reg) {
            regs_to_save.insert(r);
        }
    }
    let regs_to_save: Vec<_> = regs_to_save.into_iter().collect();
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
    Function(String),
    Slot(usize),
}

#[derive(Debug)]
struct SlotInstr {
    slot:                               usize,
    code:                               &'static isa::Code,
    operands:                           Vec<SlotOperand>,
    fixed_inputs:                       Vec<(usize, MachineReg)>,
    fixed_output:                       Option<MachineReg>,
    slot_moves:                         Vec<(usize, usize)>,

    #[cfg(feature = "dogfood")]
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


/// Lower the Scheduler's Values to Instrs, and split any live ranges that cross calls.
fn lower_to_slots_and_split(
    slot_count:                         usize,
    arguments:                          &[&Value<'_>],
    scheduled:                          &[&Value<'_>],
) -> (Vec<SlotInstr>, usize) {
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

    for (slot, _) in arguments.iter().enumerate() {
        reg_slots[slot] = slot;
    }

    let mut slot_count = arguments.len();
    let mut new_schedule = vec![];

    for (i, value) in scheduled.iter().enumerate() {
        let operands = value.operands.iter().map(|op|
            match op {
                scheduler::Operand::Constant(i)             => SlotOperand::Constant(*i),
                scheduler::Operand::Value(v)                => SlotOperand::Slot(slot_map[v.slot]),
                scheduler::Operand::Function(s)             => SlotOperand::Function(s.clone()),
            }).collect();
        let fixed_inputs = value.fixed_inputs.iter().map(|(v, reg)| (slot_map[v.slot], *reg)).collect();
        let mut slot_moves: Vec<(usize, usize)> = vec![];
        if let Some(c) = value.code() && c.clobber_mask() != 0 {
            let mut bits = c.clobber_mask();
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
            let mut bits = c.clobber_mask();
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

            #[cfg(feature = "dogfood")]
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
    argument_count:                     usize,
    slot_count:                         usize,
    instrs:                             &[SlotInstr]
) -> (Vec<Option<MachineReg>>, Vec<MachineReg>) {
    let mut regs: Vec<OnceCell<MachineReg>> = vec![OnceCell::new(); slot_count];

    // Find which slots interfere with which, and which are live across calls.
    // If they are live, make sure they're not in a clobbered register.
    let mut live_slots = BitSet::new();
    let mut interfering_slots = vec![BitSet::new(); slot_count];
    let mut available_ranks = vec![REGS.available_rank_mask(D_BANK); slot_count];

    for instr in instrs.iter().rev() {
        live_slots.remove(instr.slot);
        if instr.code.ranked_clobber_mask() != 0 {
            let mask = instr.code.ranked_clobber_mask();
            for slot in &live_slots {
                available_ranks[slot] &= !mask;
            }
        }
        for (_, dest) in &instr.slot_moves { live_slots.remove(*dest); }
        for slot in instr.predecessors() { live_slots.insert(slot); }
        for (source, _) in &instr.slot_moves { live_slots.insert(*source); }
        for slot in &live_slots {
            interfering_slots[slot].union_with(&live_slots);
        }
    }
    // Slots don't interfere with themselves - and this matters because we use available_ranks
    // post-allocation to find a temporary register for every instruction (only used if the
    // instruction needs moves), and available_registers in turn depends on interfering_slots.
    for instr in instrs { interfering_slots[instr.slot].remove(instr.slot); }

    // Allocate registers for arguments
    for slot in 0..argument_count {
        set_reg(slot,
            MachineReg::try_from(slot).expect("internal compiler error: too many arguments"),
            &regs, &interfering_slots, &mut available_ranks);
    }

    // Allocate registers for value with constrained output registers.
    for instr in instrs {
        if let Some(fixed_output) = instr.fixed_output {
            set_reg(instr.slot, fixed_output, &regs, &interfering_slots, &mut available_ranks);
        }
    }

    // Allocate registers for values with constrained operand registers.
    for instr in instrs {
        for (input_slot, preferred_reg) in &instr.fixed_inputs {
            if regs[*input_slot].get().is_some() { continue; }
            let reg = REGS.best_reg(D_BANK, available_ranks[*input_slot], Some(*preferred_reg));
            set_reg(*input_slot, reg, &regs, &interfering_slots, &mut available_ranks);
        }
    }

    // Allocate registers for remaining instructions
    for instr in instrs {
        if regs[instr.slot].get().is_some() || !instr.code.has_output() { continue; }
        let reg = REGS.best_reg(D_BANK, available_ranks[instr.slot], None);
        set_reg(instr.slot, reg, &regs, &interfering_slots, &mut available_ranks);
    }

    // Allocate registers for any slots that get moved (which don't show up in instructions)
    for instr in instrs {
        for (_, dest) in &instr.slot_moves {
            if regs[*dest].get().is_some() { continue; }
            let reg = REGS.best_reg(D_BANK, available_ranks[instr.slot], None);
            set_reg(*dest, reg, &regs, &interfering_slots, &mut available_ranks);
        }
    }

    // If an instruction needs a temporary register (for swaps *before* the instruction), the
    // registers available for a temp are the registers available for the instruction MINUS
    // the arguments to the instruction (which, if this is the last use of the argument are
    // available for the function's return value, but NOT during swaps before the instruction).
    let mut temp_reg_pool = available_ranks.clone();
    for instr in instrs {
        // This just says (in rank space) available regs minus the predecessors' regs.
        temp_reg_pool[instr.slot] &=
            !instr.predecessors().fold(0,
                |mask, p| mask | REGS.get_rank_bits(D_BANK, *regs[p].get().unwrap()));
    }
    let temp_regs = temp_reg_pool.iter()
        .map(|&a| REGS.best_reg(D_BANK, a, None))
        .collect::<Vec<_>>();

    (regs.iter_mut().map(OnceCell::take).collect(), temp_regs)
}


fn set_reg(
    slot:                               usize,
    reg:                                MachineReg,
    regs:                               &[OnceCell<MachineReg>],
    interfering_slots:                  &[BitSet],
    available_ranks:                    &mut [u32]) {
    regs[slot].set(reg)
        .expect("internal compiler error: trying to set a register twice");
    let rank_bits = REGS.get_rank_bits(D_BANK, reg);
    for interfering_slot in &interfering_slots[slot] {
        available_ranks[interfering_slot] &= !rank_bits;
    }
}


//-------------------------------------------------------------------------------------------------
// Lower SlotIntrs to allocator::Instrs

#[derive(Debug)]
pub(super) enum Operand {
    Constant(usize),
    Function(String),
    Reg(MachineReg),
}

#[derive(Debug)]
pub(super) struct Instr {
    pub(super) code:                    &'static isa::Code,
    pub(super) result_reg:              Option<MachineReg>,
    pub(super) operands:                Vec<Operand>,
    pub(super) moves:                   Vec<(MachineReg, MachineReg)>,
    pub(super) temp_reg:                MachineReg,

    #[cfg(feature = "dogfood")]
    pub(super) span:                    Span,
}


fn lower_to_regs(
    instrs:                             &[SlotInstr],
    regs:                               &[Option<MachineReg>],
    temp_regs:                          &[MachineReg]
) -> Vec<Instr> {
    instrs.iter().map(|instr| {
        let mut operands = vec![];
        for op in &instr.operands {
            match op {
                SlotOperand::Constant(i)    => operands.push(Operand::Constant(*i)),
                SlotOperand::Function(name) => operands.push(Operand::Function(name.clone())),
                SlotOperand::Slot(s)        => {
                    operands.push(Operand::Reg(regs[*s]
                        .expect("internal compiler error: no register assigned for slot")));
                }
            }
        }
        let mut moves = vec![];
        for (slot, required_reg) in &instr.fixed_inputs {
            let slot_reg = regs[*slot]
                .expect("internal compiler error: no register assigned for slot");
            if slot_reg != *required_reg { moves.push((slot_reg, *required_reg)); }
        }
        for (source, dest) in &instr.slot_moves {
            let source_reg = regs[*source].expect("internal compiler error: no register assigned for slot");
            let dest_reg = regs[*dest].expect("internal compiler error: no register assigned for slot");
            if source_reg != dest_reg { moves.push((source_reg, dest_reg)); }
        }

        Instr{
            operands, moves,
            code:                       instr.code,
            result_reg:                 regs[instr.slot],
            temp_reg:                   temp_regs[instr.slot],

            #[cfg(feature = "dogfood")]
            span:                       instr.span,
        }
    })
    .collect()
}


//-------------------------------------------------------------------------------------------------
