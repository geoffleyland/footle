use std::cell::OnceCell;
use std::collections::BTreeSet;

use bit_set::BitSet;

use super::scheduler::Value;
use super::isa;
use super::isa::{REGS, Bank, MachineReg};

#[cfg(feature = "dogfood")]
use crate::core::Span;


//-------------------------------------------------------------------------------------------------
// Register Allocation

pub(super) fn run(
    slot_count:                         usize,
    arguments:                          &[&Value<'_>],
    scheduled:                          &[&Value<'_>],
) -> (Vec<Instr>, [Vec<MachineReg>; REGS.num_banks]) {
    let (lowered, slot_banks) = lower_to_slots_and_split(slot_count, arguments, scheduled);
    let (regs, available_ranks) = allocate(arguments.len(), &lowered, &slot_banks);
    lower_to_regs(&lowered, &slot_banks, &regs, &available_ranks)
}


//-------------------------------------------------------------------------------------------------
// Lower Values to SlotInstrs

type SlotOperand = super::operand::Operand<usize>;

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
            if let SlotOperand::Reg(s) = op { Some(*s) } else { None }
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
) -> (Vec<SlotInstr>, Vec<Option<Bank>>) {
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

    // We're just keeping track of the slots (like arguments) that are given to us in a fixed
    // register - our arguments and fixed function outputs
    let mut fixed_reg_slots: [[Option<usize>; 32]; REGS.num_banks] = [[None; 32]; REGS.num_banks];
    // We're creating new slots, so we need to keep track fo the renumbering from "old slots"
    // to new slots.
    let mut slot_map = (0..slot_count).collect::<Vec<_>>();
    let mut slot_banks = vec![];

    let mut bank_argument_counts = [0; REGS.num_banks];
    for value in arguments {
        let bank = isa::bank_for(value.ty)
            .expect("internal compiler error: no bank for argument");
        let reg = bank_argument_counts[bank.0];
        bank_argument_counts[bank.0] += 1;
        fixed_reg_slots[bank.0][reg] = Some(value.slot);
        slot_banks.push(Some(bank));
    }

    let mut new_schedule = vec![];

    for (i, value) in scheduled.iter().enumerate() {
        let operands = value.operands.iter().cloned().map(|o| o.map_reg::<usize>(|v| slot_map[v.slot])).collect();

        // Renumber any fixed inputs
        let fixed_inputs = value.fixed_inputs.iter()
            .map(|(v, reg)| (slot_map[v.slot], *reg))
            .collect();

        // If this instruction clobbers anything, check it against what we've got sitting in
        // fixed_reg_slots (our arguments or results from functions).  If they're live after this
        // we need to move them.
        let mut slot_moves: Vec<(usize, usize)> = vec![];
        if let Some(c) = value.code() && c.clobbers() {
            for fixed in &mut fixed_reg_slots {
                for (reg, maybe_slot) in fixed.iter_mut().enumerate() {
                    if let Some(slot) = *maybe_slot &&
                        (c.clobber_mask(slot_banks[slot]) >> reg) & 1 != 0 {
                        if  retirements[slot] > i {
                            slot_moves.push((slot_map[slot], slot_banks.len()));
                            slot_map[slot] = slot_banks.len();
                            slot_banks.push(slot_banks[slot]);
                        }
                        *maybe_slot = None;
                    }
                }
            }
        }

        let maybe_bank = isa::bank_for(value.ty);
        if let Some(fixed_output) = value.fixed_output {
            let bank = maybe_bank
                .expect("internal compiler error: no bank for slot");
            fixed_reg_slots[bank.0][usize::from(fixed_output)] = Some(value.slot);
        }

        let code = value.code().expect("internal compiler error: expected an excutable instruction");
        new_schedule.push(SlotInstr{
            operands, code, slot_moves, fixed_inputs,
            slot:                           slot_banks.len(),
            fixed_output:                   value.fixed_output,

            #[cfg(feature = "dogfood")]
            span:                           value.span,
        });
        slot_map[value.slot] = slot_banks.len();
        slot_banks.push(maybe_bank);
    }
    (new_schedule, slot_banks)
}


//-------------------------------------------------------------------------------------------------
// Register Allocation

fn allocate(
    argument_count:                     usize,
    instrs:                             &[SlotInstr],
    slot_banks:                         &[Option<Bank>],
) -> (Vec<Option<MachineReg>>, Vec<u32>) {

    let mut regs: Vec<OnceCell<MachineReg>> = vec![OnceCell::new(); slot_banks.len()];

    // Find which slots interfere with which, and which are live across calls.
    // If they are live, make sure they're not in a clobbered register.
    let mut live_slots = BitSet::new();
    let mut interfering_slots = vec![BitSet::new(); slot_banks.len()];
    let mut available_ranks = slot_banks.iter()
        .map(|&b| REGS.available_rank_mask(b))
        .collect::<Vec<_>>();

    for instr in instrs.iter().rev() {
        live_slots.remove(instr.slot);
        if instr.code.clobbers() {
            for slot in &live_slots {
                available_ranks[slot] &= !instr.code.ranked_clobber_mask(slot_banks[slot]);
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
    let mut bank_argument_counts = [0u8; REGS.num_banks];
    for slot in 0..argument_count {
        let bank = slot_banks[slot]
            .expect("internal compiler error: no bank for argument");
        let reg = bank_argument_counts[bank.0];
        bank_argument_counts[bank.0] += 1;
        set_reg(slot,
            MachineReg::try_from(reg).expect("internal compiler error: too many arguments"),
            &regs, &interfering_slots, slot_banks, &mut available_ranks);
    }

    // Allocate registers for value with constrained output registers.
    for instr in instrs {
        if let Some(fixed_output) = instr.fixed_output {
            set_reg(instr.slot, fixed_output, &regs, &interfering_slots, slot_banks, &mut available_ranks);
        }
    }

    // Allocate registers for values with constrained operand registers.
    for instr in instrs {
        for (input_slot, preferred_reg) in &instr.fixed_inputs {
            if regs[*input_slot].get().is_some() { continue; }
            let reg = REGS.best_reg(slot_banks[*input_slot], available_ranks[*input_slot], Some(*preferred_reg));
            set_reg(*input_slot, reg, &regs, &interfering_slots, slot_banks, &mut available_ranks);
        }
    }

    // Allocate registers for remaining instructions
    for instr in instrs {
        if regs[instr.slot].get().is_some() || !instr.code.has_output() { continue; }
        let reg = REGS.best_reg(slot_banks[instr.slot], available_ranks[instr.slot], None);
        set_reg(instr.slot, reg, &regs, &interfering_slots, slot_banks, &mut available_ranks);
    }

    // Allocate registers for any slots that get moved (which don't show up in instructions)
    for instr in instrs {
        for (_, dest) in &instr.slot_moves {
            if regs[*dest].get().is_some() { continue; }
            let reg = REGS.best_reg(slot_banks[*dest], available_ranks[*dest], None);
            set_reg(*dest, reg, &regs, &interfering_slots, slot_banks, &mut available_ranks);
        }
    }

    // Collect all the registers from the OnceCells into a Vec<Option<MachineReg>>
    let regs: Vec<_> = regs.iter_mut().map(OnceCell::take).collect();
    (regs, available_ranks)
}


fn set_reg(
    slot:                               usize,
    reg:                                MachineReg,
    regs:                               &[OnceCell<MachineReg>],
    interfering_slots:                  &[BitSet],
    slot_banks:                         &[Option<Bank>],
    available_ranks:                    &mut [u32]) {
    regs[slot].set(reg)
        .expect("internal compiler error: trying to set a register twice");
    let bank = slot_banks[slot]
        .expect("internal compiler error: trying to set a register for an instruction without a register bank");
    let rank_bits = REGS.get_rank_bits(bank, reg);
    for interfering_slot in &interfering_slots[slot] {
        if slot_banks[interfering_slot].is_some_and(|b| b.0 == bank.0) {
            available_ranks[interfering_slot] &= !rank_bits;
        }
    }
}


//-------------------------------------------------------------------------------------------------
// Lowering to Instrs with registers and register swaps.

type Operand = super::operand::Operand<MachineReg>;

#[derive(Debug)]
pub(super) struct Instr {
    pub(super) code:                    &'static isa::Code,
    pub(super) result_reg:              Option<MachineReg>,
    pub(super) operands:                Vec<Operand>,
    pub(super) moves:                   [Vec<(MachineReg, MachineReg)>; REGS.num_banks],

    #[cfg(feature = "dogfood")]
    pub(super) span:                    Span,
}


fn lower_to_regs(
    instrs:                             &[SlotInstr],
    slot_banks:                         &[Option<Bank>],
    regs:                               &[Option<MachineReg>],
    available_ranks:                    &[u32],
) -> (Vec<Instr>, [Vec<MachineReg>; REGS.num_banks]) {

    let mut reg_instrs = vec![];
    let mut regs_to_save = [const { BTreeSet::new() }; REGS.num_banks];
    let mut note_if_callee_saved = |bank: Bank, reg: MachineReg| {
        if REGS.is_callee_saved(bank, reg) { regs_to_save[bank.0].insert(reg); }
    };

    for instr in instrs {
        let operands: Vec<Operand> = instr.operands.iter().cloned().map(|o| o.map_reg(|s|
            regs[s].expect("internal compiler error: no register assigned for slot"))).collect();

        // Unify fixed_input moves and slot_moves and transform them into an ordered list of moves
        // between registers.
        let mut moves = [const { Vec::new() }; REGS.num_banks];

        #[allow(clippy::needless_range_loop)]
        for bank_index in 0..REGS.num_banks {
            // Unify the fixed_input moves and slot_moves for this bank, lower them to registers,
            // and filter out any that turn out to be between the same register.
            let unordered_moves: Vec<_> = instr.fixed_inputs.iter()
                .filter(|(slot, _)| slot_banks[*slot].is_some_and(|b| b.0 == bank_index))
                .map(|(slot, reg)| (*slot, regs[*slot].unwrap(), *reg))
                .chain(instr.slot_moves.iter()
                    .filter(|(src, _)| slot_banks[*src].is_some_and(|b| b.0 == bank_index))
                    .map(|(src, dst)| (*src, regs[*src].unwrap(), regs[*dst].unwrap())))
                .filter(|(_, source, dest)| source != dest)
                .collect();

            let bank = Bank(bank_index);
            // Turn the moves into an ordered set of register moves that don't overwrite before
            // they read.
            let bank_moves = if let Some(&(representative_slot_for_bank, _, _)) = unordered_moves.first() {
                // We might need a temporary register.  That can only be picked from registers
                // available to any of the slots getting moved around minus all the registers we're
                // using for the moves.
                let temp_reg_pool = available_ranks[representative_slot_for_bank] &
                    !unordered_moves.iter().fold(0, |mask, (_, source, dest)|
                        mask | REGS.get_rank_bits(bank, *source) | REGS.get_rank_bits(bank, *dest));

                move_regs(bank, &unordered_moves, temp_reg_pool)
            } else { vec![] };

            // Check if anything we used needs to be saved by us before we use it (and restored
            // before we leave)
            for (s, d) in &bank_moves {
                note_if_callee_saved(bank, *s);
                note_if_callee_saved(bank, *d);
            }
            moves[bank_index] = bank_moves;
        }

        match (regs[instr.slot], slot_banks[instr.slot]) {
            (Some(reg), Some(bank)) => note_if_callee_saved(bank, reg),
            (None, None)            => {}
            _                       => panic!("internal compiler error: expected a register and a bank or neither")
        }

        reg_instrs.push(Instr{
            operands, moves,
            code:                       instr.code,
            result_reg:                 regs[instr.slot],

            #[cfg(feature = "dogfood")]
            span:                       instr.span,
        });
    }

    (reg_instrs, regs_to_save.map(|r| r.into_iter().collect()))
}


/// Given a list of register moves (source, dest), move values between registers.
///
/// To do this correctly, you have to be careful not to overwrite values before you've read them.
///  * If all the moves are disjoint, it's easy, just do the moves.
///  * If there are any chains, you have to move them from the destination end to the source end
///    (otherwise you'll write a source to a destination, and then copy that source again, rather
///    than the over-written value, to the next destination)
///  * If there are any cycles, you can start anywhere, and work your way backwards around the
///    cycle, using a temp register to hold the value of the first register you write to, and then
///    moving the temp register into the last register you read from.  If you've already moved one
///    of the values in the cycle as part of a chain, you can save yourself the temp register.
fn move_regs(
    bank:                               Bank,
    moves:                              &[(usize, MachineReg, MachineReg)],
    temp_reg_pool:                      u32,
) -> Vec<(MachineReg, MachineReg)> {
    let mut sources = [None; 32];
    let mut destination_counts = [0u8; 32];
    for (_, source, destination) in moves {
        sources[usize::from(*destination)] = Some(*source);
        destination_counts[usize::from(*source)] += 1;
    }

    let mut new_moves = vec![];
    // Keep track of any copies we make of a value as we move them - they could be useful later
    // if we have to resolve a cycle including the value, where we could avoid using a temporary
    // register.
    let mut copies = [None; 32];
    // Handle all the chains by starting from their ends
    for (.., destination) in moves {
        if let Some(source) = sources[usize::from(*destination)] &&
            destination_counts[usize::from(*destination)] == 0 {
            move_regs_backwards(*destination, &mut sources, &mut destination_counts, &mut new_moves);
            copies[usize::from(source)] = Some(*destination);
        }
    }
    // All the remaining moves are cycles.  Do the ones where we've already got a copy and don't
    // need a temp
    for (.., destination) in moves {
    if let Some(source) = sources[usize::from(*destination)] &&
        let Some(copy) = copies[usize::from(source)] {
            sources[usize::from(*destination)] = None;
            move_regs_backwards(source, &mut sources, &mut destination_counts, &mut new_moves);
            new_moves.push((copy, *destination));
        }
    }

    // Now do the ones where there's no other copy and we need a temp.
    if moves.iter().any(|(.., destination)| sources[usize::from(*destination)].is_some()) {
        let temp_reg = REGS.best_reg(Some(bank), temp_reg_pool, None);
        for (.., destination) in moves {
            let Some(source) = sources[usize::from(*destination)] else { continue };
            new_moves.push((source, temp_reg));
            sources[usize::from(*destination)] = None;
            move_regs_backwards(source, &mut sources, &mut destination_counts, &mut new_moves);
            new_moves.push((temp_reg, *destination));
        }
    }

    new_moves
}


fn move_regs_backwards(
    mut destination:                    MachineReg,
    sources:                            &mut[Option<MachineReg>],
    destination_counts:                 &mut[u8],
    new_moves:                          &mut Vec<(MachineReg, MachineReg)>) {
    loop {
        let Some(source) = sources[usize::from(destination)] else { return };
        new_moves.push((source, destination));
        sources[usize::from(destination)] = None;
        destination_counts[usize::from(source)] -= 1;
        if destination_counts[usize::from(source)] > 0 { return }
        destination = source;
    }
}


//-------------------------------------------------------------------------------------------------
