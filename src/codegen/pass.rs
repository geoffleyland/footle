use typed_arena::Arena;
use std::fmt;

use crate::core::{Span, Styleable, LineStyle};
use crate::vir;
use super::{scheduler, allocator, assembler, binary};


//-------------------------------------------------------------------------------------------------

pub fn run(block: &vir::Block) -> binary::CompiledFn {
    let arena = Arena::<scheduler::Value>::new();
    let scheduler::Block { arguments, values, constants, return_count, .. } = scheduler::lower_vir(&arena, block);
    let argument_count = u8::try_from(arguments.len())
        .expect("internal compiler error: too many arguments");
    let scheduled = scheduler::schedule(&values);
    let (allocated, registers_to_save) = allocator::run(argument_count, values.len(), &scheduled);
    let assembler = assembler::run(allocated, &constants, argument_count, return_count, &registers_to_save);
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


pub fn schedule(block: &vir::Block) -> Schedule {
    let arena = Arena::<scheduler::Value>::new();
    let scheduler::Block { arguments, values, constants, .. } = scheduler::lower_vir(&arena, block);
    let schedule = scheduler::schedule(&values);

    let arguments = arguments.iter().map(|a| (a.slot, a.span)).collect::<Vec<_>>();
    let instrs = schedule.iter().map(|c|
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

    Schedule{ arguments, instrs, constants }
}


//-------------------------------------------------------------------------------------------------
// Text output for assembler

pub fn assemble(block: &vir::Block) -> assembler::Block {
    let arena = Arena::<scheduler::Value>::new();
    let scheduler::Block { arguments, values, constants, return_count, .. } = scheduler::lower_vir(&arena, block);
    let argument_count = u8::try_from(arguments.len())
        .expect("internal compiler error: too many arguments");
    let scheduled = scheduler::schedule(&values);
    let (allocated, registers_to_save) = allocator::run(argument_count, values.len(), &scheduled);
    assembler::run(allocated, &constants, argument_count, return_count, &registers_to_save)
}


//-------------------------------------------------------------------------------------------------
