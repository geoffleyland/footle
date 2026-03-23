use crate::core::Span;
use super::pass::{Constant, Value, ValueDef};
use super::pass;
use super::isa;



//-------------------------------------------------------------------------------------------------

macro_rules! asm_op {
    (Register($r:expr))     => { Operand::Register($r) };
    (Constant($i:expr))     => { Operand::Constant($i) };
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

pub (super) enum Operand {
    Register(u8),
    Constant(usize),
}


pub (super) struct Instr {
    pub (super) code:               &'static isa::Code,
    pub (super) operands:           Vec<Operand>,
    span:                           Option<Span>,
}


pub struct Block {
    pub (super) assembler:          Vec<Instr>,
    pub (super) constants:          Vec<Constant>,
}


//-------------------------------------------------------------------------------------------------

pub (super) fn emit(schedule: &[&Value<'_>], constants: &[Constant], registers: &[u8]) -> Block{
    let mut assembler = Vec::new();
    emit_function(schedule, registers, &mut assembler);

    Block{ assembler, constants: constants.into() }
}


fn emit_function(schedule: &[&Value<'_>], registers: &[u8], assembler: &mut Vec<Instr>) {
    for value in schedule {
        let ValueDef::Instr(code) = &value.def else { continue };

        // Count FMOVs for any operands that need to be in specific registers
        if let Some(required_regs) = &value.operand_registers {
            for (op, &required) in value.operands.iter().zip(required_regs) {
                let pass::Operand::Value(v) = op else { continue };
                let actual = registers[v.address];
                if actual != required {
                    assemble!(assembler, None, FMOV, Register(required), Register(actual));
                }
            }
        }

        let operands = code.has_output
            .then(|| Operand::Register(registers[value.address]))
            .into_iter()
            .chain(value.operands.iter().map(|op| match op {
                pass::Operand::Value(v)    => Operand::Register(registers[v.address]),
                pass::Operand::Constant(i) => Operand::Constant(*i),
            }))
        .collect();
        assembler.push(Instr{ code, operands, span: Some(value.span) });
    }
}

//-------------------------------------------------------------------------------------------------
// Text output for assembler

use std::fmt;
use crate::core::{Styleable, LineStyle};

impl Styleable for Block {
    fn write<W: LineStyle>(&self, f: &mut fmt::Formatter, indent: u16, writer: &W) -> fmt::Result {
        for i in &self.assembler {
            let operands = i.operands.iter().map(|o|
                match o {
                    Operand::Constant(i)    => format!("K{i}"),
                    Operand::Register(i)    => format!("d{i}"),
                }).collect::<Vec<_>>();
            writer.writeln(f, indent, i.span, &format!("{} {}", i.code.name,
                operands.join(" ")))?;
        }
        for (i, c) in self.constants.iter().enumerate() {
            writer.writeln (f, indent, Some(c.span), &format!("K{i}: {:?}", c.value))?;
        }
        Ok(())
    }
}

//-------------------------------------------------------------------------------------------------
