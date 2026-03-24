use crate::core::Span;
use super::scheduler::{Constant, Value, ValueDef};
use super::scheduler;
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
}


//-------------------------------------------------------------------------------------------------

pub(super) fn emit(arguments: &[&Value<'_>], schedule: &[&Value<'_>], constants: &[Constant], registers: &[u8]) -> Block{
    let mut instrs = Vec::new();
    emit_function(schedule, registers, &mut instrs);
    let glue_start_words = instrs.len();
    let argument_count = u8::try_from(arguments.len())
        .expect("internal compiler error: too many arguments");
    emit_glue(argument_count, 1, &mut instrs);

    Block{ instrs, glue_start_words, constants: constants.into() }
}


fn emit_function(schedule: &[&Value<'_>], registers: &[u8], instrs: &mut Vec<Instr>) {
    for value in schedule {
        let ValueDef::Instr(code) = &value.def else { continue };

        // Emit FMOVs for any operands that need to be in specific registers
        if let Some(required_regs) = &value.operand_registers {
            for (op, &required) in value.operands.iter().zip(required_regs) {
                let scheduler::Operand::Value(v) = op else { continue };
                let actual = registers[v.address];
                if actual != required {
                    assemble!(instrs, None, FMOV, Register(required), Register(actual));
                }
            }
        }

        let operands = code.has_output
            .then(|| Operand::Register(registers[value.address]))
            .into_iter()
            .chain(value.operands.iter().map(|op| match op {
                scheduler::Operand::Value(v)    => Operand::Register(registers[v.address]),
                scheduler::Operand::Constant(i) => Operand::Constant(*i),
            }))
        .collect();
        instrs.push(Instr{ code, operands, span: Some(value.span) });
    }
}


fn emit_glue(argument_count: u8, return_count: u8, assembler: &mut Vec<Instr>) {
    // Move the input buffer pointer to R16, since we're about to overwrite r0 with the first
    // argument to the function we're calling
    assemble!(assembler, None, MOV_I64, Register(16), Register(0));

    // Move the output buffer and the return address to the stack, since the're about to get
    // overwritten and we need them later
    assemble!(assembler, None, STP_PRE_I64, Register(1), Register(30), Register(31), Offset(-16));

    // Put the arguments in the right place on the stack
    for i in 0..argument_count {
        assemble!(assembler, None, LDR_REG_F64, Register(i), Register(16), Offset(i32::from(i) * 8));
    }

    // Call our function
    assemble!(assembler, None, BL,
        Offset(-4 * i32::try_from(assembler.len())
            .expect("internal compiler error: function too long for jump")));

    // Load the output buffer in to r16 and the return address to the appropriate spot
    assemble!(assembler, None, LDP_POST_I64, Register(16), Register(30), Register(31), Offset(16));

    for i in 0..return_count {
        assemble!(assembler, None, STR_REG_F64, Register(i), Register(16), Offset(i32::from(i) * 8));
    }

    assemble!(assembler, None, RET);
}


//-------------------------------------------------------------------------------------------------
// Text output for assembler

use std::fmt;
use crate::core::{Styleable, LineStyle};

impl Styleable for Block {
    fn write<W: LineStyle>(&self, f: &mut fmt::Formatter, indent: u16, writer: &W) -> fmt::Result {
        for i in &self.instrs {
            let operands = i.operands.iter().map(|o|
                match o {
                    Operand::Constant(i)    => format!("K{i}"),
                    Operand::Register(i)    => format!("d{i}"),
                    Operand::Offset(o)      => format!("#{o}"),
                }).collect::<Vec<_>>();
            writer.writeln(f, indent, i.span, &format!("{} {}", i.code.name,
                operands.join(" ")))?;
        }
        for (i, c) in self.constants.iter().enumerate() {
            writer.writeln(f, indent, Some(c.span), &format!("K{i}: {:?}", c.value))?;
        }
        Ok(())
    }
}


//-------------------------------------------------------------------------------------------------
