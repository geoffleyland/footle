use std::cell::OnceCell;
use std::collections::BTreeSet;

use bit_set::BitSet;
use typed_index_collections::TiVec;
use derive_more::{From, Into};

use super::scheduler::{Value, ValueSlot, SchedulePosition, Type};
use super::isa;
use super::isa::{REGS, MachineReg, RegFile};

#[cfg(feature = "dogfood")]
use crate::core::Span;


//-------------------------------------------------------------------------------------------------
// Register Allocation

pub(super) fn run(
    slot_count:                         usize,
    arguments:                          &[&Value<'_>],
    scheduled:                          &TiVec<SchedulePosition, &Value<'_>>,
) -> (Vec<Instr>, Vec<MachineReg>) {
    let slot_block = lower_to_slots_and_split(slot_count, arguments, scheduled);
    let (regs, available_ranks) = allocate(&slot_block);
    lower_to_regs(&slot_block, &regs, &available_ranks)
}


//-------------------------------------------------------------------------------------------------
// Lower Values to SlotInstrs

/// Index of a slot in the allocator's numbering.
///
/// `lower_to_slots_and_split` has to split live ranges, and as a result, slots have to be
/// re-numbered.
#[derive(Debug, Copy, Clone, From, Into)]
struct AllocatorSlot(usize);

type SlotOperand = super::operand::Operand<AllocatorSlot>;

#[derive(Debug)]
struct SlotInstr {
    slot:                               AllocatorSlot,
    code:                               &'static isa::Code,
    operands:                           Vec<SlotOperand>,
    fixed_inputs:                       Vec<(AllocatorSlot, MachineReg)>,
    fixed_output:                       Option<MachineReg>,
    slot_moves:                         Vec<(AllocatorSlot, AllocatorSlot)>,

    #[cfg(feature = "dogfood")]
    span:                               Span,
}


impl SlotInstr {
    pub(super) fn predecessors(&self) -> impl Iterator<Item = AllocatorSlot> {
        let operands = self.operands.iter().filter_map(|op| {
            if let SlotOperand::Reg(s) = op { Some(*s) } else { None }
        });
        let fixed_inputs = self.fixed_inputs.iter().map(|(v, _)| *v);
        operands.chain(fixed_inputs)
    }
}


struct SlotBlock {
    arguments:                          Vec<(AllocatorSlot, MachineReg)>,
    instrs:                             Vec<SlotInstr>,
    slot_types:                         TiVec<AllocatorSlot, Type>,
}


/// Lower the Scheduler's Values to Instrs, and split any live ranges that cross calls.
fn lower_to_slots_and_split(
    slot_count:                         usize,
    arguments:                          &[&Value<'_>],
    scheduled:                          &TiVec<SchedulePosition, &Value<'_>>,
) -> SlotBlock {
    // Walk through the scheduled instructions finding out when values retire.
    let mut retirements: TiVec<ValueSlot, _> = vec![None; slot_count].into();
    for (i, value) in scheduled.iter_enumerated() {
        for predecessor in value.predecessors() { retirements[predecessor.slot] = Some(i); }
    }

    // We're just keeping track of the slots (like arguments) that are given to us in a fixed
    // register - our arguments and fixed function outputs
    let mut fixed_reg_slots: [Option<ValueSlot>; RegFile::REG_COUNT] = [None; RegFile::REG_COUNT];
    // We're creating new AllocatorSlots, so we need to keep track of the renumbering from
    // ValueSlots to AllocatorSlots.  Start with `None` - we work through the slots in dependency
    // order so we should always write to `slot_map` before we read from it.
    let mut slot_map: TiVec<ValueSlot, _> = vec![None; slot_count].into();
    let mut slot_types: TiVec<AllocatorSlot, Type> = TiVec::new();

    let mut argument_regs = vec![];

    for (value, reg) in arguments.iter()
        .zip(RegFile::abi_regs(arguments.iter().map(|v| v.ty))) {
        fixed_reg_slots[usize::from(reg)] = Some(value.slot);
        let new_slot = slot_types.push_and_get_key(value.ty);
        argument_regs.push((new_slot, reg));
        slot_map[value.slot] = Some(new_slot);
    }

    let mut new_schedule = vec![];

    for (i, value) in scheduled.iter_enumerated() {
        let operands = value.operands.iter().cloned()
            .map(|o| o.map_reg::<AllocatorSlot>(|v| slot_map[v.slot].unwrap())).collect();

        // Renumber any fixed inputs
        let fixed_inputs = value.fixed_inputs.iter()
            .map(|(v, reg)| (slot_map[v.slot].unwrap(), *reg))
            .collect();

        // If this instruction clobbers anything, check it against what we've got sitting in
        // fixed_reg_slots (our arguments or results from functions).  If they're live after this
        // we need to move them.
        let mut slot_moves: Vec<(AllocatorSlot, AllocatorSlot)> = vec![];
        if let Some(c) = value.code() && c.clobbers_anything() {
            for (reg, maybe_slot) in fixed_reg_slots.iter_mut().enumerate() {
                if let Some(slot) = *maybe_slot &&
                    c.clobbers(MachineReg::try_from(reg).unwrap()) {
                    if retirements[slot].is_some_and(|r| r > i) {
                        let old_slot = slot_map[slot].unwrap();
                        let new_slot = slot_types.push_and_get_key(slot_types[old_slot]);
                        slot_moves.push((old_slot, new_slot));
                        slot_map[slot] = Some(new_slot);
                    }
                    *maybe_slot = None;
                }
            }
        }

        if let Some(fixed_output) = value.fixed_output {
            fixed_reg_slots[usize::from(fixed_output)] = Some(value.slot);
        }

        let code = value.code().expect("internal compiler error: expected an excutable instruction");
        let new_slot = slot_types.push_and_get_key(value.ty);
        new_schedule.push(SlotInstr{
            operands, code, slot_moves, fixed_inputs,
            slot:                           new_slot,
            fixed_output:                   value.fixed_output,

            #[cfg(feature = "dogfood")]
            span:                           value.span,
        });
        slot_map[value.slot] = Some(new_slot);
    }
    SlotBlock{ arguments: argument_regs, instrs: new_schedule, slot_types }
}


//-------------------------------------------------------------------------------------------------
// Register Allocation

/// A `BitSet` of `AllocatorSlot` indexes.
///
/// We need to keep track of which slots are are live at each instruction, and which slots
/// interfere with each other.  We do that with a `BitSet` and try to keep things clean by using
/// the right type for indexing the right kind of slots.
#[derive(Clone, Default)]
struct SlotSet(BitSet);

impl SlotSet {
    fn insert(&mut self, slot: AllocatorSlot) { self.0.insert(slot.into()); }
    fn remove(&mut self, slot: AllocatorSlot) { self.0.remove(slot.into()); }
    fn union_with(&mut self, other: &Self)    { self.0.union_with(&other.0); }
    fn iter(&self) -> impl Iterator<Item = AllocatorSlot> + '_ {
        self.0.iter().map(AllocatorSlot::from)
    }
}


fn allocate(
    block:                              &SlotBlock,
) -> (TiVec<AllocatorSlot, Option<MachineReg>>, TiVec<AllocatorSlot, isa::RankSet>) {
    let mut regs: TiVec<AllocatorSlot, _> =
        vec![OnceCell::new(); block.slot_types.len()].into();

    // Find which slots interfere with which, and which are live across calls.
    // If they are live, make sure they're not in a clobbered register.
    let mut live_slots = SlotSet::default();
    let mut interfering_slots: TiVec<AllocatorSlot, _> =
        vec![SlotSet::default(); block.slot_types.len()].into();
    let mut available_ranks: TiVec<AllocatorSlot, _> = block.slot_types.iter()
        .map(|&ty| REGS.available_ranks(ty))
        .collect();

    for instr in block.instrs.iter().rev() {
        live_slots.remove(instr.slot);
        if instr.code.clobbers_anything() {
            for slot in live_slots.iter() {
                available_ranks[slot].remove(instr.code.clobbered_ranks());
            }
        }
        for (_, dest) in &instr.slot_moves { live_slots.remove(*dest); }
        for slot in instr.predecessors() { live_slots.insert(slot); }
        for (source, _) in &instr.slot_moves { live_slots.insert(*source); }
        for slot in live_slots.iter() {
            interfering_slots[slot].union_with(&live_slots);
        }
    }
    // Slots don't interfere with themselves - and this matters because we use available_ranks
    // post-allocation to find a temporary register for every instruction (only used if the
    // instruction needs moves), and available_registers in turn depends on interfering_slots.
    for instr in &block.instrs { interfering_slots[instr.slot].remove(instr.slot); }

    // Allocate registers for arguments
    for &(slot, reg) in &block.arguments {
        set_reg(slot, reg, &regs, &interfering_slots, &mut available_ranks);
    }

    // Allocate registers for value with constrained output registers.
    for instr in &block.instrs {
        if let Some(fixed_output) = instr.fixed_output {
            set_reg(instr.slot, fixed_output, &regs, &interfering_slots, &mut available_ranks);
        }
    }

    // Allocate registers for values with constrained operand registers.
    for instr in &block.instrs {
        for (input_slot, preferred_reg) in &instr.fixed_inputs {
            if regs[*input_slot].get().is_some() { continue; }
            let reg = REGS.best_reg(available_ranks[*input_slot], Some(*preferred_reg));
            set_reg(*input_slot, reg, &regs, &interfering_slots, &mut available_ranks);
        }
    }

    // Allocate registers for remaining instructions
    for instr in &block.instrs {
        if regs[instr.slot].get().is_some() || !instr.code.has_output() { continue; }
        let reg = REGS.best_reg(available_ranks[instr.slot], None);
        set_reg(instr.slot, reg, &regs, &interfering_slots, &mut available_ranks);
    }

    // Allocate registers for any slots that get moved (which don't show up in instructions)
    for instr in &block.instrs {
        for (_, dest) in &instr.slot_moves {
            if regs[*dest].get().is_some() { continue; }
            let reg = REGS.best_reg(available_ranks[*dest], None);
            set_reg(*dest, reg, &regs, &interfering_slots, &mut available_ranks);
        }
    }

    // Collect all the registers from the OnceCells into Options.
    let regs: TiVec<_, _> = regs.iter_mut().map(OnceCell::take).collect();
    (regs, available_ranks)
}


fn set_reg(
    slot:                               AllocatorSlot,
    reg:                                MachineReg,
    regs:                               &TiVec<AllocatorSlot, OnceCell<MachineReg>>,
    interfering_slots:                  &TiVec<AllocatorSlot, SlotSet>,
    available_ranks:                    &mut TiVec<AllocatorSlot, isa::RankSet>
) {
    regs[slot].set(reg).expect("internal compiler error: trying to set a register twice");
    for interfering_slot in interfering_slots[slot].iter() {
        available_ranks[interfering_slot].remove_reg(reg);
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
    pub(super) moves:                   Vec<(MachineReg, MachineReg)>,

    #[cfg(feature = "dogfood")]
    pub(super) span:                    Span,
}


fn lower_to_regs(
    block:                              &SlotBlock,
    regs:                               &TiVec<AllocatorSlot, Option<MachineReg>>,
    available_ranks:                    &TiVec<AllocatorSlot, isa::RankSet>,
) -> (Vec<Instr>, Vec<MachineReg>) {

    let mut reg_instrs = vec![];
    let mut regs_to_save = BTreeSet::new();
    let mut note_if_callee_saved = |reg: MachineReg| {
        if REGS.is_callee_saved(reg) { regs_to_save.insert(reg); }
    };

    for instr in &block.instrs {
        let operands: Vec<Operand> = instr.operands.iter().cloned().map(|o| o.map_reg(|s|
            regs[s].expect("internal compiler error: no register assigned for slot"))).collect();

        // Unify fixed_input moves and slot_moves and transform them into an ordered list of moves
        // between registers.
        // While we're unifying, filter out any that turn out to be between the same register.
        // We store the source slot for each move so that, if the moves need a temp register,
        // we pick from any of the registers available to the moved slots (minus the registers
        // they're using)
        let unordered_moves: Vec<_> = instr.fixed_inputs.iter()
            .map(|(slot, reg)| (*slot, regs[*slot].unwrap(), *reg))
            .chain(instr.slot_moves.iter()
                .map(|(src, dst)| (*src, regs[*src].unwrap(), regs[*dst].unwrap())))
            .filter(|(_, source, dest)| source != dest)
            .collect();

        let moves = if unordered_moves.is_empty() { vec![] } else {
            // We might need a temporary register.  That can only be picked from registers
            // available to any of the slots getting moved around minus all the registers we're
            // using for the moves.
            let mut temp_reg_pool = unordered_moves.iter()
                .fold(isa::RankSet::EMPTY, |pool, (slot, ..)| pool.union(available_ranks[*slot]));
            for (_, source, dest) in &unordered_moves {
                temp_reg_pool.remove_reg(*source);
                temp_reg_pool.remove_reg(*dest);
            }

            move_regs(&unordered_moves, temp_reg_pool)
        };

        // Check if anything we used needs to be saved by us before we use it (and restored
        // before we leave)
        for (s, d) in &moves {
            note_if_callee_saved(*s);
            note_if_callee_saved(*d);
        }

        if let Some(reg) = regs[instr.slot] { note_if_callee_saved(reg); }

        reg_instrs.push(Instr{
            operands, moves,
            code:                       instr.code,
            result_reg:                 regs[instr.slot],

            #[cfg(feature = "dogfood")]
            span:                       instr.span,
        });
    }

    (reg_instrs, regs_to_save.into_iter().collect())
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
///
/// The `AllocatorSlot` in `moves` is not used here - its purpose is explained above in
/// `lower_to_regs`.
fn move_regs(
    moves:                              &[(AllocatorSlot, MachineReg, MachineReg)],
    temp_reg_pool:                      isa::RankSet,
) -> Vec<(MachineReg, MachineReg)> {
    let mut sources = [None; RegFile::REG_COUNT];
    let mut destination_counts = [0u8; RegFile::REG_COUNT];
    for (_, source, destination) in moves {
        sources[usize::from(*destination)] = Some(*source);
        destination_counts[usize::from(*source)] += 1;
    }

    let mut new_moves = vec![];
    // Keep track of any copies we make of a value as we move them - they could be useful later
    // if we have to resolve a cycle including the value, where we could avoid using a temporary
    // register.
    let mut copies = [None; RegFile::REG_COUNT];
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
    for (.., destination) in moves {
        let Some(source) = sources[usize::from(*destination)] else { continue };
        let temp_reg = REGS.best_reg(temp_reg_pool.intersection(REGS.class_ranks(source)), None);
        new_moves.push((source, temp_reg));
        sources[usize::from(*destination)] = None;
        move_regs_backwards(source, &mut sources, &mut destination_counts, &mut new_moves);
        new_moves.push((temp_reg, *destination));
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
