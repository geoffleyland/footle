use crate::codegen::{isa, Code, Signature, RegBank, MachineReg, RegSet, REGS,
    assembler::{assemble_instr, assemble_expr, split_regs_by_bank, Instr, Operand}};


//-------------------------------------------------------------------------------------------------

#[allow(non_upper_case_globals)]
const x0: MachineReg = MachineReg::new(0);      // glue: the arguments pointer arrives here
#[allow(non_upper_case_globals)]
const x1: MachineReg = MachineReg::new(1);      // glue: the results pointer arrives here

//-------------------------------------------------------------------------------------------------

pub fn emit_moves(instrs: &mut Vec<Instr<Operand>>, moves: &[(MachineReg, MachineReg)]) {
    for &(source, destination) in moves {
        debug_assert_eq!(RegBank::of_reg(source), RegBank::of_reg(destination));
        match RegBank::of_reg(source) {
            RegBank::Int    => assemble_instr!(instrs, mov_x, destination, source),
            RegBank::Float  => assemble_instr!(instrs, fmov_d, destination, source)
        }
    }
}


//-------------------------------------------------------------------------------------------------

pub fn emit_save_regs(instrs: &mut Vec<Instr<Operand>>, regs: RegSet) {
    let (int_regs, float_regs) =  split_regs_by_bank(regs);
    for pair in int_regs.chunks(2)          { save_restore(instrs, pair, &isa::stp_x_pre, &isa::str_x_pre, REGS.stack_reg, -16) }
    for pair in float_regs.chunks(2)        { save_restore(instrs, pair, &isa::stp_d_pre, &isa::str_d_pre, REGS.stack_reg, -16) }
}


pub fn emit_restore_regs(instrs: &mut Vec<Instr<Operand>>, regs: RegSet) {
    let (int_regs, float_regs) = split_regs_by_bank(regs);
    for pair in float_regs.chunks(2).rev()  { save_restore(instrs, pair, &isa::ldp_d_post, &isa::ldr_d_post, REGS.stack_reg, 16) }
    for pair in int_regs.chunks(2).rev()    { save_restore(instrs, pair, &isa::ldp_x_post, &isa::ldr_x_post, REGS.stack_reg, 16) }
}


fn save_restore(
    instrs:                         &mut Vec<Instr<Operand>>,
    pair:                           &[MachineReg],
    pair_op:                        &'static Code,
    single_op:                      &'static Code,
    offset_reg:                     MachineReg,
    offset:                         i32) {
    match *pair {
        [a, b]  => assemble_expr!(instrs, pair_op, a, b, offset_reg, offset),
        [a]     => assemble_expr!(instrs, single_op, a, offset_reg, offset),
        _       => unreachable!()
    }
}


//-------------------------------------------------------------------------------------------------

pub fn emit_glue(signature: &Signature, instrs: &mut Vec<Instr<Operand>>) {
    // Move the input buffer pointer to x16 so it doesn't get clobbered by arguments to our function.
    assemble_instr!(instrs, mov_x, REGS.scratch_reg, x0);

    // Move the output buffer and the return address to the stack, since they're about to get
    // overwritten and we need them later.
    assemble_instr!(instrs, stp_x_pre, x1, REGS.link_reg, REGS.stack_reg, -16);

    // Move the arguments from the input buffer into the argument registers.
    for (offset, reg) in (0i32..).step_by(8)
        .zip(REGS.abi_regs(signature.args.iter().map(|a| a.ty))) {
        match RegBank::of_reg(reg) {
            RegBank::Int        => assemble_instr!(instrs, ldr_x_offset, reg, REGS.scratch_reg, offset),
            RegBank::Float      => assemble_instr!(instrs, ldr_d_offset, reg, REGS.scratch_reg, offset),
        }
    }

    // Call our function
    let offset_to_func = -4 * i32::try_from(instrs.len())
        .expect("internal compiler error: function too long for jump");
    assemble_instr!(instrs, bl, offset_to_func);

    // Load the output buffer in to x16 and the return address to the appropriate spot
    assemble_instr!(instrs, ldp_x_post, REGS.scratch_reg, REGS.link_reg, REGS.stack_reg, 16);

    for (offset, reg) in (0i32..).step_by(8)
        .zip(REGS.abi_regs(signature.return_types.iter().copied())) {
        match RegBank::of_reg(reg) {
            RegBank::Int        => assemble_instr!(instrs, str_x_offset, reg, REGS.scratch_reg, offset),
            RegBank::Float      => assemble_instr!(instrs, str_d_offset, reg, REGS.scratch_reg, offset),
        }
    }
    assemble_instr!(instrs, ret);
}


//-------------------------------------------------------------------------------------------------
