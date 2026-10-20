use crate::codegen::{Signature, RegBank, MachineReg, RegSet, REGS,
    assembler::{assemble_instr, split_regs_by_bank, Instr, Operand}};


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
            RegBank::Int    => assemble!(instrs, mv    destination, source),
            RegBank::Float  => assemble!(instrs, fmv.d destination, source),
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
    assemble!(instrs, addi sp, sp, -frame_size(regs));
    let (int_regs, float_regs) = split_regs_by_bank(regs);
    let mut offset = 0;
    for reg in int_regs         { assemble!(instrs, sd  reg, offset(sp)); offset += 8; }
    for reg in float_regs       { assemble!(instrs, fsd reg, offset(sp)); offset += 8; }
}


pub fn emit_restore_regs(instrs: &mut Vec<Instr<Operand>>, regs: RegSet) {
    if regs.is_empty() { return }
    let (int_regs, float_regs) = split_regs_by_bank(regs);
    let mut offset = 0;
    for reg in int_regs         { assemble!(instrs, ld  reg, offset(sp)); offset += 8; }
    for reg in float_regs       { assemble!(instrs, fld reg, offset(sp)); offset += 8; }
    assemble!(instrs, addi sp, sp, frame_size(regs));
}


//-------------------------------------------------------------------------------------------------

pub fn emit_glue(signature: &Signature, instrs: &mut Vec<Instr<Operand>>) {
    assemble!(instrs,
        mv t6, a0;                  // Move the input buffer pointer to t6 so it doesn't get
                                    // clobbered as we write arguments to the function.
        addi sp, sp, -16;           // Adjust sp for the output buffer and the return address.
        sd a1, 0(sp);               // Move the output buffer pointer to the stack.
        sd ra, 8(sp);               // Move the return address onto the stack.
    );

    // Move the arguments from the input buffer (t6) into the argument registers.
    for (offset, reg) in (0i32..).step_by(8)
        .zip(REGS.abi_regs(signature.args.iter().map(|a| a.ty))) {
        match RegBank::of_reg(reg) {
            RegBank::Int        => assemble!(instrs, ld  reg, offset(t6)),
            RegBank::Float      => assemble!(instrs, fld reg, offset(t6)),
        }
    }

    let offset_to_func = -4 * i32::try_from(instrs.len())
        .expect("internal compiler error: function too long for jump");
    assemble!(instrs,
        jal offset_to_func;         // Call the function.
        ld t6, 0(sp);               // Load the output buffer point into t6.
        ld ra, 8(sp);               // Put the return address back into ra.
        addi sp, sp, 16;            // Adjust sp back.
    );

    // Move the function outputs from their registers into the output buffer (t6).
    for (offset, reg) in (0i32..).step_by(8)
        .zip(REGS.abi_regs(signature.return_types.iter().copied())) {
        match RegBank::of_reg(reg) {
            RegBank::Int        => assemble!(instrs, sd  reg, offset(t6)),
            RegBank::Float      => assemble!(instrs, fsd reg, offset(t6)),
        }
    }
    assemble!(instrs, ret);
}


//-------------------------------------------------------------------------------------------------

/// Nifty little assembler macro that lets us write RISC-V assembly like RISC-V assembly!
///
/// Examples are above, but roughly: assemble!(instrs, opcode arg1, arg2; ..).
/// Instructions are separated by semicolons (the last one is optional) and you can use RISC-V
/// dot-syntax (fmv.d) on instructions.
/// Exceptions from "real" RISC-V assembler are:
/// * if an offset is more than one token (including just a negative number) it needs to be in
///   curly braces: `ld reg, {-8}(t6)`;
/// * if the second operand of an instruction is a function call, the whole call needs to be in
///   braces, otherwise it will be confused for an offset: `li reg, {foo(value)}`.
macro_rules! assemble {
    // The entry point.
    ($instrs:expr, $($rest:tt)*)     => { assemble!(@line $instrs; $($rest)* ;) };
    // The exit point.
    (@line $instrs:expr; )           => { () };
    // An empty line: the added `;` after a listing that already ended with one.
    (@line $instrs:expr; ; $($rest:tt)*) => { assemble!(@line $instrs; $($rest)*) };

    // Offset form: opcode reg, offset(base).  This has to come first: otherwise the general form
    // will match it.
    (@line $instrs:expr; $opcode:ident $(. $suffix:ident)? $reg:expr, $offset:tt($base:expr); $($rest:tt)*) => {{
        ::paste::paste! { assemble_instr!($instrs, [<$opcode $(_ $suffix)?>], $reg, $base, $offset) };
        assemble!(@line $instrs; $($rest)*)
    }};
    // G eneral form: opcode operand1, operand2..
    (@line $instrs:expr; $opcode:ident $(. $suffix:ident)? $($operands:expr),*; $($rest:tt)*) => {{
        ::paste::paste! { assemble_instr!($instrs, [<$opcode $(_ $suffix)?>] $(,$operands)*) };
        assemble!(@line $instrs; $($rest)*)
    }};
}
use assemble;


//-------------------------------------------------------------------------------------------------
