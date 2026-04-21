use crate::core::Span;
use super::scheduler::Constant;
use super::allocator;
use super::isa;


//-------------------------------------------------------------------------------------------------

macro_rules! asm_op {
    (Register($r:expr))     => { Operand::Register($r) };
    (Constant($i:expr))     => { Operand::Constant($i) };
    (Offset($o:expr))       => { Operand::Offset($o) };
}

macro_rules! assemble {
    ($vec:expr, $span:expr, $op:ident $(, $($operand:tt $operand_arg:expr),*)?) => {
        $vec.push(Instr {
            code: &isa::$op,
            span: $span,
            operands: vec![$($(asm_op!($operand($operand_arg))),*)?],
        })
    }
}


//-------------------------------------------------------------------------------------------------

pub(super) enum Operand {
    Register(u8),
    Constant(usize),
    Offset(i32),
}


pub(super) struct Instr {
    pub(super) code:                &'static isa::Code,
    pub(super) operands:            Vec<Operand>,
    span:                           Option<Span>,
}


pub struct Block {
    pub(super) instrs:              Vec<Instr>,
    pub(super) glue_start_words:    usize,
    pub(super) constants:           Vec<Constant>,
    pub(super) argument_count:      u8,
    pub(super) return_count:        u8,
}


//-------------------------------------------------------------------------------------------------

pub(super) fn run(
    allocated:                      Vec<allocator::Instr>,
    constants:                      &[Constant],
    argument_count:                 u8,
    return_count:                   u8,
    registers_to_save:              &[u8]) -> Block{
    let mut instrs = Vec::new();
    emit_function(allocated, &mut instrs, registers_to_save);
    let glue_start_words = instrs.len();
    emit_glue(argument_count, return_count, &mut instrs);

    Block{ instrs, glue_start_words, constants: constants.into(), argument_count, return_count }
}


fn emit_function(allocated: Vec<allocator::Instr>, instrs: &mut Vec<Instr>, registers_to_save: &[u8]) {
    // Save any callee saved registers
    for pair in registers_to_save.chunks(2) {
        match *pair {
            [a, b]  => assemble!(instrs, None, STP_PRE_F64, Register(a), Register(b), Register(31), Offset(-16)),
            [a]     => assemble!(instrs, None, STR_PRE_F64, Register(a), Register(31), Offset(-16)),
            _       => unreachable!()
        }
    }

    for ai in allocated {
        if !ai.moves.is_empty() { move_registers(&ai.moves, ai.temp_register, instrs) }

        let operands = ai.code.has_output
            .then_some(Operand::Register(ai.output_register))
            .into_iter()
            .chain(ai.operands.iter().map(|op| match op {
                allocator::Operand::Register(r)     => Operand::Register(*r),
                allocator::Operand::Constant(i)     => Operand::Constant(*i),
            }))
        .collect();

        // Restore callee saved registers before a `ret`.
        if ai.code.restore_regs {
            for pair in registers_to_save.chunks(2).rev() {
                match *pair {
                    [a, b]  => assemble!(instrs, None, LDP_POST_F64, Register(a), Register(b), Register(31), Offset(16)),
                    [a]     => assemble!(instrs, None, LDR_POST_F64, Register(a), Register(31), Offset(16)),
                    _       => unreachable!()
                }
            }
        }

        instrs.push(Instr{ code: ai.code, operands, span: Some(ai.span) });
    }
}


fn move_registers(moves: &[(u8, u8)], temp_register: u8, instrs: &mut Vec<Instr>) {
    let mut sources = [0xFFu8; 32];
    let mut destination_counts = [0u8; 32];
    for (source, destination) in moves {
        sources[usize::from(*destination)] = *source;
        destination_counts[usize::from(*source)] += 1;
    }

    let mut copies = [0xFFu8; 32];
    // Handle all the chains by starting from their ends
    for (_, destination) in moves {
        let source = sources[usize::from(*destination)];
        if source != 0xFF && destination_counts[usize::from(*destination)] == 0 {
            move_registers_backwards(*destination, &mut sources, &mut destination_counts, instrs);
            copies[usize::from(source)] = *destination;
        }
    }

    // All the remaining moves are cycles.  Do the ones where we've already got a copy and don't
    // need a temp
    for (_, destination) in moves {
        let source = sources[usize::from(*destination)];
        if source == 0xFF { continue; }
        let copy = copies[usize::from(source)];
        if copy == 0xFF { continue; }
        sources[usize::from(*destination)] = 0xFF;
        move_registers_backwards(source, &mut sources, &mut destination_counts, instrs);
        assemble!(instrs, None, FMOV, Register(*destination), Register(copy));
    }

    // Now do the ones where there's no other copy and we need a temp.
    for (_, destination) in moves {
        let source = sources[usize::from(*destination)];
        if source == 0xFF { continue; }
        assemble!(instrs, None, FMOV, Register(temp_register), Register(source));
        sources[usize::from(*destination)] = 0xFF;
        move_registers_backwards(source, &mut sources, &mut destination_counts, instrs);
        assemble!(instrs, None, FMOV, Register(*destination), Register(temp_register));
    }
}


fn move_registers_backwards(mut destination: u8, sources: &mut[u8], destination_counts: &mut[u8], instrs: &mut Vec<Instr>) {
    loop {
        let source = sources[usize::from(destination)];
        if source == 0xFF { return }
        assemble!(instrs, None, FMOV, Register(destination), Register(source));
        sources[usize::from(destination)] = 0xFF;
        destination_counts[usize::from(source)] -= 1;
        if destination_counts[usize::from(source)] > 0 { return }
        destination = source;
    }
}


fn emit_glue(argument_count: u8, return_count: u8, assembler: &mut Vec<Instr>) {
    // Move the input buffer pointer to R16, since we're about to overwrite r0 with the first
    // argument to the function we're calling
    assemble!(assembler, None, MOV_I64, Register(16), Register(0));

    // Move the output buffer and the return address to the stack, since they're about to get
    // overwritten and we need them later
    assemble!(assembler, None, STP_PRE_I64, Register(1), Register(30), Register(31), Offset(-16));

    // Put the arguments in the right place on the stack
    for i in 0..argument_count {
        assemble!(assembler, None, LDR_OFFSET_F64, Register(i), Register(16), Offset(i32::from(i) * 8));
    }

    // Call our function
    assemble!(assembler, None, BL,
        Offset(-4 * i32::try_from(assembler.len())
            .expect("internal compiler error: function too long for jump")));

    // Load the output buffer in to r16 and the return address to the appropriate spot
    assemble!(assembler, None, LDP_POST_I64, Register(16), Register(30), Register(31), Offset(16));

    for i in 0..return_count {
        assemble!(assembler, None, STR_OFFSET_F64, Register(i), Register(16), Offset(i32::from(i) * 8));
    }

    assemble!(assembler, None, RET);
}


//-------------------------------------------------------------------------------------------------
// Text output for assembler

use std::fmt;
use crate::core::{Styleable, LineStyle};

impl Styleable for Block {
    fn write<W: LineStyle>(&self, f: &mut fmt::Formatter, indent: u16, writer: &W) -> fmt::Result {
        let instr_words = self.instrs.len();
        let constant_start_words = instr_words + usize::from(instr_words.is_multiple_of(2));
        for (i, instr) in self.instrs.iter().enumerate() {

            let operands = instr.operands.iter().map(|o|
                match o {
                    Operand::Constant(c)    => i32::try_from((constant_start_words - i) * 4 + *c * 8).unwrap(),
                    Operand::Register(r)    => i32::from(*r),
                    Operand::Offset(o)      => *o,
                }).collect::<Vec<_>>();

            let address = i32::try_from(0x1000 + i * 4).unwrap();
            writer.writeln(f, indent, instr.span, &format!("{:#06x}: {} {}",
                address,
                instr.code.name,
                (instr.code.format)(&operands, address)))?;
        }
        for (i, c) in self.constants.iter().enumerate() {
            let address = 0x1000 + constant_start_words * 4 + i * 8;
            writer.writeln(f, indent, Some(c.span), &format!("{address:#06x}: {:?}", c.value))?;
        }
        Ok(())
    }
}


//-------------------------------------------------------------------------------------------------
