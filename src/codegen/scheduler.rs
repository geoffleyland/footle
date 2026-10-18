use enumset::EnumSet;
use derive_more::{From, Into, Display};
use typed_index_collections::TiVec;

use super::{isa, lower, Arg, Instr, ValueSlot, Type, Constant};


//-------------------------------------------------------------------------------------------------

/// Index of an instruction in `Block`'s `instrs` `TiVec` of scheduled instructions.
///
/// It's (currently) only used to see when values retire in `allocator`s
/// `lower_to_slots_and_split`.
#[derive(Debug, Copy, Clone, From, Into, Display, PartialOrd, Ord, PartialEq, Eq)]
pub(super) struct SchedulePosition(usize);

pub struct Block<'arena> {
    pub(super) slot_count:                  usize,
    pub(super) args:                        Vec<&'arena Arg>,
    pub(super) instrs:                      TiVec<SchedulePosition, &'arena Instr<'arena>>,
    pub(super) return_types :               Vec<Type>,
    pub(super) constants:                   Vec<Constant>,
    pub(super) functions:                   Vec<String>,
}


//-------------------------------------------------------------------------------------------------

pub(super) fn run(
    block:                  lower::Block<'_>,
) -> Block<'_> {
    let instrs = schedule(&block.instrs, block.slot_count);

    Block { instrs,
        args:           block.args,
        return_types:   block.return_types,
        slot_count:     block.slot_count,
        constants:      block.constants,
        functions:      block.functions,
    }
}


//-------------------------------------------------------------------------------------------------
// Instruction Scheduling

fn schedule<'arena>(
    instrs:             &[&'arena Instr<'arena>],
    slot_count:         usize,
) -> TiVec<SchedulePosition, &'arena Instr<'arena>> {
    // Count how many operands (that need scheduling, arguments are always available) each
    // instruction has so we can figure out when they're ready to go.
    let mut unresolved_operand_count: TiVec<ValueSlot, usize> = vec![0; slot_count].into();
    for instr in instrs { unresolved_operand_count[instr.slot] = instr.predecessor_instr_count(); }

    // Find all the users of each value
    let mut users: TiVec<ValueSlot, _> = vec![vec![]; slot_count].into();
    for instr in instrs {
        for slot in instr.predecessor_slots() {
            users[slot].push(instr);
        }
    }

    // And count the uses of each value
    let mut remaining_use_count: TiVec<ValueSlot, _> = users.iter().map(Vec::len).collect();

    // Find the critical path depths of each Instr.
    // Because the Instrs are already topologically ordered, we can do this backwards and always
    // find that the depths of our users that we need are already calculated.
    let depths: TiVec<ValueSlot, _> = instrs.iter().rev()
        .fold(vec![0usize; slot_count].into(), |mut d, instr| {
            d[instr.slot] = usize::from(instr.latency()) +
                users[instr.slot].iter().map(|user| d[user.slot]).max().unwrap_or(0);
            d
        });
    let mut current_critical_path_depth = depths.iter().copied().max().unwrap_or(0);

    let expected_length = instrs.len();

    let mut cycle = 0usize;
    let mut results_by_cycle: Vec<Vec<&Instr>>  = vec![vec![]; current_critical_path_depth];

    let mut scheduled: TiVec<SchedulePosition, _> = TiVec::new();

    let mut ready_instrs: Vec<&Instr> = instrs.iter()
        .filter(|instr| unresolved_operand_count[instr.slot] == 0)
        .copied().collect();

    while scheduled.len() < expected_length {
        let mut free_units: EnumSet<isa::Unit> = EnumSet::all();

        // Pick an instruction from those that:
        //  * have all their operands ready
        //  * can be executed on a free unit
        //  * are on the critical path (if any)
        //  * that retire the most registers
        //  * that are deepest on the critical path (as in, if none are ON the critical path, and
        //  *   more than one retires the most registers, pick the one deepest on the critical
        //      path)
        while let Some(&best_instr) = ready_instrs.iter()
            .filter(|instr| instr.code.try_pick_unit(free_units).is_some())
            .max_by_key(|instr| {
                let critical_path_depth = depths[instr.slot];
                let on_critical_path = critical_path_depth >= current_critical_path_depth;
                let retiring_count = instr.predecessor_slots().filter(|slot| remaining_use_count[*slot] == 1).count();
                (on_critical_path, retiring_count, critical_path_depth)
            }) {

            // Add this instr to the code and remove it from the list of ready instructions
            scheduled.push(best_instr);
            ready_instrs.retain(|&instr| std::ptr::from_ref(instr) != std::ptr::from_ref(best_instr));

            // Pick a unit we think is going to run this instruction and reserve it.
            free_units -= best_instr.code.try_pick_unit(free_units)
                .expect("internal compiler error: no free unit");

            // Mark that the results of this instruction will be ready in the appropriate cycle.
            let ready_cycle = cycle + usize::from(best_instr.latency());
            if results_by_cycle.len() <= ready_cycle {
                results_by_cycle.resize_with(ready_cycle+1, Vec::new);
            }
            results_by_cycle[ready_cycle].push(best_instr);

            // Update the remaining uses of our operands so we can keep track of which instructions
            // will retire (the most) registers.
            for slot in best_instr.predecessor_slots() { remaining_use_count[slot] -= 1; }

            // Update the critical path depth if this instruction is worse than what we thought.
            current_critical_path_depth = std::cmp::max(
                current_critical_path_depth, depths[best_instr.slot]);
        }
        // We can't dispatch any more instructions to units in the cycle above.  Move ahead a
        // cycle.
        cycle += 1;
        current_critical_path_depth = current_critical_path_depth.saturating_sub(1);
        // For all the results that are ready in this (new) cycle, mark all the instructions using
        // them as having that operand ready
        for completed in &results_by_cycle[cycle] {
            for &user in &users[completed.slot] {
                unresolved_operand_count[user.slot] -= 1;
                if unresolved_operand_count[user.slot] == 0 {
                    ready_instrs.push(user);
                }
            }
        }
    }

    scheduled
}


//-------------------------------------------------------------------------------------------------

#[cfg(feature = "dogfood")]
mod display {
    use std::fmt;

    use super::*;
    use crate::core::{Styleable, LineStyle};

    impl Styleable for Block<'_> {
        fn write<W: LineStyle>(&self, f: &mut fmt::Formatter, indent: u16, writer: &W) -> fmt::Result {
            for a in &self.args {
                writer.writeln(f, indent, Some(a.span), &format!("I{}: argument", a.slot))?;
            }
            for instr in &self.instrs {
                let operands = instr.operands.iter().map(|o| format!("{o}")).collect::<Vec<_>>();
                writer.writeln(f, indent, Some(instr.span), &format!("I{}: {}{}{}{}{}",
                    instr.slot,
                    instr.code.mnemonic(),
                    if operands.is_empty() { "" } else { " " }, operands.join(" "),
                    if instr.fixed_inputs.is_empty() { "" } else { " " },
                    instr.fixed_inputs.iter().map(|(v, _)| format!("I{}", v.slot())).collect::<Vec<_>>().join(" ")))?;
            }
            for (i, c) in self.constants.iter().enumerate() {
                writer.writeln(f, indent, Some(c.span), &format!("K{i}: {:?}", c.value))?;
            }
            Ok(())
        }
    }

}


//-------------------------------------------------------------------------------------------------
