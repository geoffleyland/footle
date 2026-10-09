use crate::codegen::{Type, RegBank, MachineReg, RegSet, REGS,
    assembler::{assemble, split_regs_by_bank, Instr, Offset, Operand}};


//-------------------------------------------------------------------------------------------------

#[allow(non_upper_case_globals)]
const a0: MachineReg = MachineReg::new(10);      // glue: the arguments pointer arrives here
#[allow(non_upper_case_globals)]
const a1: MachineReg = MachineReg::new(11);      // glue: the results pointer arrives here

//-------------------------------------------------------------------------------------------------

pub fn emit_moves(instrs: &mut Vec<Instr<Operand>>, moves: &[(MachineReg, MachineReg)]) {
    for (source, destination) in moves {
        debug_assert_eq!(RegBank::of_reg(*source), RegBank::of_reg(*destination));
        match RegBank::of_reg(*source) {
            RegBank::Int    => assemble!(instrs, mv, *destination, *source),
            RegBank::Float  => assemble!(instrs, fmv_d, *destination, *source)
        }
    }
}


//-------------------------------------------------------------------------------------------------

fn frame_size(regs: RegSet) -> i32 {
    i32::try_from((regs.len() * 8).next_multiple_of(16))
        .expect("internal compiler error: frame too big")
}

pub fn emit_save_regs(instrs: &mut Vec<Instr<Operand>>, regs: RegSet) {
    if regs.is_empty() { return }
    let (int_regs, float_regs) = split_regs_by_bank(regs);
    assemble!(instrs, addi, REGS.stack_reg, REGS.stack_reg, Offset(-frame_size(regs)));
    let mut offset = 0;
    for reg in int_regs         { assemble!(instrs, sd, reg, REGS.stack_reg, Offset(offset)); offset += 8; }
    for reg in float_regs       { assemble!(instrs, fsd, reg, REGS.stack_reg, Offset(offset)); offset += 8; }
}


pub fn emit_restore_regs(instrs: &mut Vec<Instr<Operand>>, regs: RegSet) {
    if regs.is_empty() { return }
    let (int_regs, float_regs) = split_regs_by_bank(regs);
    let mut offset = 0;
    for reg in int_regs         { assemble!(instrs, ld, reg, REGS.stack_reg, Offset(offset)); offset += 8; }
    for reg in float_regs       { assemble!(instrs, fld, reg, REGS.stack_reg, Offset(offset)); offset += 8; }
    assemble!(instrs, addi, REGS.stack_reg, REGS.stack_reg, Offset(frame_size(regs)));
}


//-------------------------------------------------------------------------------------------------

pub fn emit_glue(argument_types: &[Type], return_types: &[Type], instrs: &mut Vec<Instr<Operand>>) {
    // Move the input buffer pointer to the scratch reg  so it doesn't get clobbered by arguments
    // to our function.
    assemble!(instrs, mv, REGS.scratch_reg, a0);

    // Move the output buffer and the return address to the stack, since they're about to get
    // overwritten and we need them later.
    assemble!(instrs, addi, REGS.stack_reg, REGS.stack_reg, Offset(-16));
    assemble!(instrs, sd, a1, REGS.stack_reg, Offset(0));
    assemble!(instrs, sd, REGS.link_reg, REGS.stack_reg, Offset(8));

    // Move the arguments from the input buffer into the argument registers.
    for (offset, reg) in (0i32..).step_by(8)
        .zip(REGS.abi_regs(argument_types.iter().copied())) {
        match RegBank::of_reg(reg) {
            RegBank::Int        => assemble!(instrs, ld, reg, REGS.scratch_reg, Offset(offset)),
            RegBank::Float      => assemble!(instrs, fld, reg, REGS.scratch_reg, Offset(offset)),
        }
    }

    // Call our function
    assemble!(instrs, jal,
        Offset(-4 * i32::try_from(instrs.len())
            .expect("internal compiler error: function too long for jump")));

    // Load the output buffer in to the scratch reg and the return address to the appropriate spot
    assemble!(instrs, ld, REGS.scratch_reg, REGS.stack_reg, Offset(0));
    assemble!(instrs, ld, REGS.link_reg, REGS.stack_reg, Offset(8));
    assemble!(instrs, addi, REGS.stack_reg, REGS.stack_reg, Offset(16));

    for (offset, reg) in (0i32..).step_by(8)
        .zip(REGS.abi_regs(return_types.iter().copied())) {
        match RegBank::of_reg(reg) {
            RegBank::Int        => assemble!(instrs, sd, reg, REGS.scratch_reg, Offset(offset)),
            RegBank::Float      => assemble!(instrs, fsd, reg, REGS.scratch_reg, Offset(offset)),
        }
    }
    assemble!(instrs, ret);
}


//-------------------------------------------------------------------------------------------------
