use seq_macro::seq;

use super::scheduler::{Constant, Type};
use super::allocator;
use super::isa;
use super::isa::{REGS, X_BANK, D_BANK, MachineReg, bank_for};

#[cfg(feature = "dogfood")]
use crate::core::Span;


//-------------------------------------------------------------------------------------------------

pub(super) enum Operand {
    Reg(u8),
    PooledF64(usize),
    ImmU16(u16),
    Offset(i32),
    Function(usize),
}

struct Reg(u8);
impl From<Reg> for Operand {
    fn from(r: Reg) -> Self { Self::Reg(r.0) }
}
seq!(N in 0..32 {
    #[allow(non_upper_case_globals, dead_code)]
    const x~N: Reg = Reg(N);
    #[allow(non_upper_case_globals, dead_code)]
    const d~N: Reg = Reg(N);
});

struct Offset(i32);
impl From<Offset> for Operand {
    fn from(o: Offset) -> Self { Self::Offset(o.0) }
}

struct PooledF64(usize);
impl From<PooledF64> for Operand {
    fn from(p: PooledF64) -> Self { Self::PooledF64(p.0) }
}

impl From<MachineReg> for Operand {
    fn from(r: MachineReg) -> Self { Self::Reg(r.0) }
}


macro_rules! assemble_expr {
    ($instrs:expr, $op:expr $(, $($operand:expr),*)?) => {
        $instrs.push(Instr {
            code: $op,
            operands: vec![$($($operand.into()),*)?],

            #[cfg(feature = "dogfood")]
            span: None,
        })
    }
}


macro_rules! assemble {
    ($instrs:expr, $op:ident $(, $($operand:expr),*)?) => {
        assemble_expr!($instrs, &isa::$op $(, $($operand),*)?)
    }
}


//-------------------------------------------------------------------------------------------------

pub(super) struct Instr {
    pub(super) code:                &'static isa::Code,
    pub(super) operands:            Vec<Operand>,

    #[cfg(feature = "dogfood")]
    span:                           Option<Span>,
}


pub struct Block {
    pub(super) instrs:              Vec<Instr>,
    pub(super) glue_start_words:    usize,
    pub(super) constants:           Vec<Constant>,
    pub(super) functions:           Vec<String>,
    pub(super) argument_types:      Vec<Type>,
    pub(super) return_types:        Vec<Type>,
}


//-------------------------------------------------------------------------------------------------

pub(super) fn run(
    allocated:                      Vec<allocator::Instr>,
    constants:                      &[Constant],
    functions:                      &[String],
    argument_types:                 Vec<Type>,
    return_types:                   Vec<Type>,
    regs_to_save:                   &[Vec<MachineReg>; REGS.num_banks]) -> Block{
    let mut instrs = Vec::new();
    emit_function(allocated, &mut instrs, functions, regs_to_save);
    let glue_start_words = instrs.len();
    emit_glue(&argument_types, &return_types, &mut instrs);

    Block{ instrs, glue_start_words, constants: constants.into(), functions: functions.to_vec(),
        argument_types, return_types }
}


fn emit_function(
    allocated:                      Vec<allocator::Instr>,
    instrs:                         &mut Vec<Instr>,
    functions:                      &[String],
    regs_to_save:                   &[Vec<MachineReg>; REGS.num_banks]) {
    let stack = REGS.stack_reg;
    // Save any callee saved registers
    for pair in regs_to_save[0].chunks(2) { save_restore(instrs, pair, &isa::stp_x_pre, &isa::str_x_pre, stack, -16) }
    for pair in regs_to_save[1].chunks(2) { save_restore(instrs, pair, &isa::stp_d_pre, &isa::str_d_pre, stack, -16) }

    for ai in allocated {
        for (move_op, moves) in [&isa::mov_x, &isa::fmov_d].iter().zip(&ai.moves) {
            for (source, destination) in moves {
                assemble_expr!(instrs, *move_op, *destination, *source);
            }
        }

        let operands = ai.code.has_output()
            .then(|| Operand::Reg(ai.result_reg
                .expect("internal compiler error: no register allocated for instruction result").into()))
            .into_iter()
            .chain(ai.operands.iter().map(|op| match op {
                super::operand::Operand::Reg(r)             => (*r).into(),
                super::operand::Operand::PooledF64(i)       => PooledF64(*i).into(),
                super::operand::Operand::ImmU16(v)          => Operand::ImmU16(*v),
                super::operand::Operand::Function(name) => {
                    let index = functions.iter().position(|s| s == name)
                        .expect("internal compiler error: unknown function name");
                    Operand::Function(index)
                }
            }))
        .collect();

        // Restore callee saved registers before a `ret`.
        if ai.code.restore_regs() {
            for pair in regs_to_save[1].chunks(2).rev() { save_restore(instrs, pair, &isa::ldp_d_post, &isa::ldr_d_post, stack, 16) }
            for pair in regs_to_save[0].chunks(2).rev() { save_restore(instrs, pair, &isa::ldp_x_post, &isa::ldr_x_post, stack, 16) }
        }

        if ai.code.save_link_reg() {
            assemble!(instrs, str_x_pre, REGS.link_reg, REGS.stack_reg, Offset(-16));
        }

        instrs.push(Instr{ code: ai.code, operands,
        #[cfg(feature = "dogfood")]
            span: Some(ai.span)
        });

        if ai.code.save_link_reg() {
            assemble!(instrs, ldr_x_post, REGS.link_reg, REGS.stack_reg, Offset(16));
        }
    }
}


fn save_restore(
    instrs:                         &mut Vec<Instr>,
    pair:                           &[MachineReg],
    pair_op:                        &'static isa::Code,
    single_op:                      &'static isa::Code,
    offset_reg:                     MachineReg,
    offset:                         i32) {
    match *pair {
        [a, b]  => assemble_expr!(instrs, pair_op, a, b, offset_reg, Offset(offset)),
        [a]     => assemble_expr!(instrs, single_op, a, offset_reg, Offset(offset)),
        _       => unreachable!()
    }
}


//-------------------------------------------------------------------------------------------------

fn emit_glue(argument_types: &[Type], return_types: &[Type], instrs: &mut Vec<Instr>) {
    // Move the input buffer pointer to x16 so it doesn't get clobbered by arguments to our function.
    // In fact, at the moment, we only have floating-point arguments, so it *won't* get clobbered,
    // but if I ever get to types and integers, then I don't want to have a mystery bug strike me
    // because I was too smart about my function glue.
    assemble!(instrs, mov_x, REGS.scratch_reg, x0);

    // Move the output buffer and the return address to the stack, since they're about to get
    // overwritten and we need them later.
    assemble!(instrs, stp_x_pre, x1, REGS.link_reg, REGS.stack_reg, Offset(-16));

    // Move the arguments from the input buffer into the argument registers.
    let (mut x_reg, mut d_reg) = (0u8, 0u8);
    for (i, &ty) in argument_types.iter().enumerate() {
        let offset = Offset(8 * i32::try_from(i)
            .expect("internal compiler error; too many return values"));
        match bank_for(ty) {
            Some(X_BANK) => {
                assert!(x_reg < 8, "internal compiler error; too many return values");
                assemble!(instrs, ldr_x_offset, Reg(x_reg), REGS.scratch_reg, offset);
                x_reg += 1;
            }
            Some(D_BANK) => {
                assert!(d_reg < 8, "internal compiler error; too many return values");
                assemble!(instrs, ldr_d_offset, Reg(d_reg), REGS.scratch_reg, offset);
                d_reg += 1;
            }
            _ => panic!("internal compiler error: no bank for return value")
        }
    }

    // Call our function
    assemble!(instrs, bl,
        Offset(-4 * i32::try_from(instrs.len())
            .expect("internal compiler error: function too long for jump")));

    // Load the output buffer in to x16 and the return address to the appropriate spot
    assemble!(instrs, ldp_x_post, REGS.scratch_reg, REGS.link_reg, REGS.stack_reg, Offset(16));

    let (mut x_reg, mut d_reg) = (0u8, 0u8);
    for (i, &ty) in return_types.iter().enumerate() {
        let offset = Offset(8 * i32::try_from(i)
            .expect("internal compiler error; too many return values"));
        match bank_for(ty) {
            Some(X_BANK) => {
                assert!(x_reg < 8, "internal compiler error; too many return values");
                assemble!(instrs, str_x_offset, Reg(x_reg), REGS.scratch_reg, offset);
                x_reg += 1;
            }
            Some(D_BANK) => {
                assert!(d_reg < 8, "internal compiler error; too many return values");
                assemble!(instrs, str_d_offset, Reg(d_reg), REGS.scratch_reg, offset);
                d_reg += 1;
            }
            _ => panic!("internal compiler error: no bank for return value")
        }
    }

    assemble!(instrs, ret);
}


//-------------------------------------------------------------------------------------------------
// Text output for assembler

#[cfg(feature = "dogfood")]
mod display {
    use std::fmt;
    use super::*;
    use crate::core::{Styleable, LineStyle};

    impl Styleable for Block {
        fn write<W: LineStyle>(&self, f: &mut fmt::Formatter, indent: u16, writer: &W) -> fmt::Result {
            let instr_words = self.instrs.len();
            let constant_start_words = instr_words + usize::from(instr_words.is_multiple_of(2));
            let function_start_words = constant_start_words + self.constants.len() * 2;
            for (i, instr) in self.instrs.iter().enumerate() {

                let operands = instr.operands.iter().map(|o|
                    match o {
                        Operand::PooledF64(c)   => i32::try_from((constant_start_words - i) * 4 + *c * 8).unwrap(),
                        Operand::Function(f)    => i32::try_from((function_start_words - i) * 4 + *f * 8).unwrap(),
                        Operand::Reg(r)         => i32::from(*r),
                        Operand::ImmU16(v)      => i32::from(*v),
                        Operand::Offset(o)      => *o,
                    }).collect::<Vec<_>>();

                let address = i32::try_from(0x1000 + i * 4).unwrap();
                writer.writeln(f, indent, instr.span, &format!("{:#06x}: {} {}",
                    address,
                    instr.code.mnemonic(),
                    (instr.code.format)(&operands, address)))?;
            }
            for (i, c) in self.constants.iter().enumerate() {
                let address = 0x1000 + constant_start_words * 4 + i * 8;
                writer.writeln(f, indent, Some(c.span), &format!("{address:#06x}: {:?}", c.value))?;
            }
            for (i, func) in self.functions.iter().enumerate() {
                let address = 0x1000 + function_start_words * 4 + i * 8;
                writer.writeln(f, indent, None, &format!("{address:#06x}: {func}"))?;
            }
            Ok(())
        }
    }

}

//-------------------------------------------------------------------------------------------------
