use typed_arena::Arena;
use std::fmt;

use crate::core::{Span, Styleable, LineStyle};
use crate::vir;
use super::{scheduler, allocator, assembler, binary};


//-------------------------------------------------------------------------------------------------

pub fn run(vir_block: &vir::Block) -> binary::CompiledFn {
    let arena = Arena::<scheduler::Value>::new();
    let scheduled_block = scheduler::run(&arena, vir_block);
    let (allocated, registers_to_save) =
        allocator::run(scheduled_block.argument_count, scheduled_block.value_count, &scheduled_block.instrs);
    let assembler =
        assembler::run(allocated, &scheduled_block.constants, scheduled_block.argument_count, scheduled_block.return_count, &registers_to_save);
    binary::emit(&assembler)
}


//-------------------------------------------------------------------------------------------------
// Text output for the scheduler

enum InstrOperand {
    Constant(usize),
    Instr(usize)
}


struct Instr {
    slot:                                   usize,
    opcode:                                 String,
    operands:                               Vec<InstrOperand>,
    fixed_inputs:                           Vec<usize>,
    span:                                   Span,
}


pub struct Schedule {
    arguments:                              Vec<(usize, Span)>,
    instrs:                                 Vec<Instr>,
    constants:                              Vec<scheduler::Constant>,
}


impl Styleable for Schedule {
    fn write<W: LineStyle>(&self, f: &mut fmt::Formatter, indent: u16, writer: &W) -> fmt::Result {
        for (i, span) in &self.arguments {
            writer.writeln(f, indent, Some(*span), &format!("I{i}: argument"))?;
        }
        for instr in &self.instrs {
            let operands = instr.operands.iter().map(|o|
                match o {
                    InstrOperand::Constant(i)   => format!("K{i}"),
                    InstrOperand::Instr(i)      => format!("I{i}")
                }).collect::<Vec<_>>();
            writer.writeln(f, indent, Some(instr.span), &format!("I{}: {} {}{}",
                instr.slot, instr.opcode, operands.join(" "),
                instr.fixed_inputs.iter().map(|i| format!("I{i}")).collect::<Vec<_>>().join(" ")))?;
        }
        for (i, c) in self.constants.iter().enumerate() {
            writer.writeln(f, indent, Some(c.span), &format!("K{i}: {:?}", c.value))?;
        }
        Ok(())
    }
}


pub fn schedule(vir_block: &vir::Block) -> Schedule {
    let arena = Arena::<scheduler::Value>::new();
    let scheduled_block = scheduler::run(&arena, vir_block);

    let arguments = vir_block.arguments().iter().enumerate().map(|(i, e)| (i, *e.span())).collect::<Vec<_>>();
    let instrs = scheduled_block.instrs.iter().map(|c|
        Instr{
            slot:           c.slot,
            opcode:         c.code().expect("internal compiler error: instruction without opcode").name.to_string(),
            operands:       c.operands.iter().map(|o|
                match o {
                    scheduler::Operand::Constant(i)     => InstrOperand::Constant(*i),
                    scheduler::Operand::Value(v)        => InstrOperand::Instr(v.slot)
                }).collect(),
            fixed_inputs:   c.fixed_inputs.iter().map(|(v, _)| v.slot).collect(),
            span:           c.span})
        .collect();

    Schedule{ arguments, instrs, constants: scheduled_block.constants }
}


//-------------------------------------------------------------------------------------------------
// Text output for assembler

pub fn assemble(vir_block: &vir::Block) -> assembler::Block {
    let arena = Arena::<scheduler::Value>::new();
    let scheduled_block = scheduler::run(&arena, vir_block);
    let (allocated, registers_to_save) =
        allocator::run(scheduled_block.argument_count, scheduled_block.value_count, &scheduled_block.instrs);
    assembler::run(allocated, &scheduled_block.constants, scheduled_block.argument_count, scheduled_block.return_count, &registers_to_save)
}


//-------------------------------------------------------------------------------------------------
