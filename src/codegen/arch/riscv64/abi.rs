use crate::codegen::{Signature, RegBank, MachineReg, RegSet, REGS,
    assembler::{assemble_instr, split_regs_by_bank, Instr, Offset, Operand}};


//-------------------------------------------------------------------------------------------------

#[allow(non_upper_case_globals)]
const sp: MachineReg = REGS.stack_reg;
#[allow(non_upper_case_globals)]
const ra: MachineReg = REGS.link_reg;
#[allow(non_upper_case_globals)]
const t6: MachineReg = REGS.scratch_reg;

#[allow(non_upper_case_globals)]
const a0: MachineReg = MachineReg::new(10);      // glue: the arguments pointer arrives here
#[allow(non_upper_case_globals)]
const a1: MachineReg = MachineReg::new(11);      // glue: the results pointer arrives here

//-------------------------------------------------------------------------------------------------

pub fn emit_moves(instrs: &mut Vec<Instr<Operand>>, moves: &[(MachineReg, MachineReg)]) {
    for &(source, destination) in moves {
        debug_assert_eq!(RegBank::of_reg(source), RegBank::of_reg(destination));
        match RegBank::of_reg(source) {
            RegBank::Int    => assemble_instr!(instrs, mv, destination, source),
            RegBank::Float  => assemble_instr!(instrs, fmv_d, destination, source)
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
    assemble_instr!(instrs, addi, sp, sp, Offset(-frame_size(regs)));
    let mut offset = 0;
    for reg in int_regs         { assemble_instr!(instrs, sd, reg, sp, Offset(offset)); offset += 8; }
    for reg in float_regs       { assemble_instr!(instrs, fsd, reg, sp, Offset(offset)); offset += 8; }
}


pub fn emit_restore_regs(instrs: &mut Vec<Instr<Operand>>, regs: RegSet) {
    if regs.is_empty() { return }
    let (int_regs, float_regs) = split_regs_by_bank(regs);
    let mut offset = 0;
    for reg in int_regs         { assemble_instr!(instrs, ld, reg, sp, Offset(offset)); offset += 8; }
    for reg in float_regs       { assemble_instr!(instrs, fld, reg, sp, Offset(offset)); offset += 8; }
    assemble_instr!(instrs, addi, sp, sp, Offset(frame_size(regs)));
}


//-------------------------------------------------------------------------------------------------

pub fn emit_glue(signature: &Signature, instrs: &mut Vec<Instr<Operand>>) {
    // Move the input buffer pointer to the scratch reg  so it doesn't get clobbered by arguments
    // to our function.
    assemble_instr!(instrs, mv, t6, a0);

    // Move the output buffer and the return address to the stack, since they're about to get
    // overwritten and we need them later.
    assemble_instr!(instrs, addi, sp, sp, Offset(-16));
    assemble_instr!(instrs, sd, a1, sp, Offset(0));
    assemble_instr!(instrs, sd, ra, sp, Offset(8));

    // Move the arguments from the input buffer into the argument registers.
    for (offset, reg) in (0i32..).step_by(8)
        .zip(REGS.abi_regs(signature.args.iter().map(|a| a.ty))) {
        match RegBank::of_reg(reg) {
            RegBank::Int        => assemble_instr!(instrs, ld, reg, t6, Offset(offset)),
            RegBank::Float      => assemble_instr!(instrs, fld, reg, t6, Offset(offset)),
        }
    }

    // Call our function
    assemble_instr!(instrs, jal,
        Offset(-4 * i32::try_from(instrs.len())
            .expect("internal compiler error: function too long for jump")));

    // Load the output buffer in to the scratch reg and the return address to the appropriate spot
    assemble_instr!(instrs, ld, t6, sp, Offset(0));
    assemble_instr!(instrs, ld, ra, sp, Offset(8));
    assemble_instr!(instrs, addi, sp, sp, Offset(16));

    for (offset, reg) in (0i32..).step_by(8)
        .zip(REGS.abi_regs(signature.return_types.iter().copied())) {
        match RegBank::of_reg(reg) {
            RegBank::Int        => assemble_instr!(instrs, sd, reg, t6, Offset(offset)),
            RegBank::Float      => assemble_instr!(instrs, fsd, reg, t6, Offset(offset)),
        }
    }
    assemble_instr!(instrs, ret);
}


//-------------------------------------------------------------------------------------------------
