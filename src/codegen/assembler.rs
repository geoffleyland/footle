use super::{Code, abi, allocator, Constant, Type, MachineReg, RegSet, RegBank, REGS};

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

pub(super) struct Offset(pub(super) i32);
impl From<Offset> for Operand {
    fn from(o: Offset) -> Self { Self::Offset(o.0) }
}

struct PooledF64(usize);
impl From<PooledF64> for Operand {
    fn from(p: PooledF64) -> Self { Self::PooledF64(p.0) }
}

impl From<MachineReg> for Operand {
    fn from(r: MachineReg) -> Self { Self::Reg(r.encoding()) }
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
pub(super) use assemble_expr;


macro_rules! assemble {
    ($instrs:expr, $op:ident $(, $($operand:expr),*)?) => {
        $crate::codegen::assembler::assemble_expr!($instrs, &$crate::codegen::isa::$op $(, $($operand),*)?)
    }
}
pub(super) use assemble;


//-------------------------------------------------------------------------------------------------

pub(super) struct Instr<OP> {
    pub(super) code:                &'static Code,
    pub(super) operands:            Vec<OP>,

    #[cfg(feature = "dogfood")]
    pub(super) span:                Option<Span>,
}


#[derive(Debug, Copy, Clone)]
pub(super) struct Layout {
    pub(super) glue_start_words:    usize,
    pub(super) constant_start_words:usize,
    pub(super) function_start_words:usize,
    pub(super) total_bytes:         usize,
    #[cfg(feature = "dogfood")]
    pub(super) instr_words:         usize,
}
impl Layout {
    fn new(glue_start_words: usize, instr_words: usize, constant_count:usize, function_count:usize) -> Self {
        let constant_start_words = instr_words.next_multiple_of(2);
        let function_start_words = constant_start_words + constant_count * 2;

        Self {
            glue_start_words, constant_start_words, function_start_words,
            total_bytes:            function_start_words * 4 + function_count * 8,
            #[cfg(feature = "dogfood")]
            instr_words,
        }
    }
}

pub struct Block {
    pub(super) layout:              Layout,
    pub(super) instrs:              Vec<Instr<i32>>,
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
    written_regs:                   RegSet,
) -> Block{
    let mut instrs = Vec::new();
    emit_function(allocated, &mut instrs, functions, written_regs);
    let glue_start_words = instrs.len();
    abi::emit_glue(&argument_types, &return_types, &mut instrs);

    let layout = Layout::new(glue_start_words, instrs.len(), constants.len(), functions.len());

    let resolved = resolve(instrs, &layout);

    Block{ layout, instrs: resolved, constants: constants.into(), functions: functions.to_vec(),
        argument_types, return_types }
}


fn emit_function(
    allocated:                      Vec<allocator::Instr>,
    instrs:                         &mut Vec<Instr<Operand>>,
    functions:                      &[String],
    written_regs:                   RegSet,
) {
    let regs_to_save = REGS.regs_to_save(written_regs);
    abi::emit_save_regs(instrs, regs_to_save);

    for ai in allocated {
        abi::emit_moves(instrs, &ai.moves);

        // Restore callee saved registers before a `ret`.
        if ai.code.is_return() { abi::emit_restore_regs(instrs, regs_to_save); }

        let operands = ai.code.has_output()
            .then(|| Operand::from(ai.result_reg
                .expect("internal compiler error: no register allocated for instruction result")))
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

        instrs.push(Instr{ code: ai.code, operands,
        #[cfg(feature = "dogfood")]
            span: Some(ai.span)
        });
    }
}


pub(super) fn split_regs_by_bank(regs: RegSet) -> (Vec<MachineReg>, Vec<MachineReg>) {
    (regs.intersection(RegBank::Int.regs()).iter().collect(),
     regs.intersection(RegBank::Float.regs()).iter().collect())
}


//-------------------------------------------------------------------------------------------------

/// Once we have ALL the instructions, resolve offsets to constants and functions into simple
/// offsets.
fn resolve(instrs: Vec<Instr<Operand>>, layout: &Layout) -> Vec<Instr<i32>> {
    use Operand::*;

    instrs.into_iter().enumerate()
        .map(|(word_index, instr)| {
            let operands: Vec<i32> = instr.operands.into_iter().map(|op| {
                match op {
                    Reg(i)                  => i32::from(i),
                    ImmU16(v)               => i32::from(v),
                    PooledF64(i)            => i32::try_from((layout.constant_start_words - word_index) * 4 + (i * 8))
                                                .expect("internal compiler error: constant offset too large"),
                    Function(i)             => i32::try_from((layout.function_start_words - word_index) * 4 + (i * 8))
                                                .expect("internal compiler error: function offset too large"),
                    Offset(o)               => o,
                }
            }).collect();
            Instr{ operands, code: instr.code,
                #[cfg(feature = "dogfood")]
                span: instr.span,
            }
        }).collect()
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
            for (i, instr) in self.instrs.iter().enumerate() {
                let address = i32::try_from(0x1000 + i * 4).unwrap();
                writer.writeln(f, indent, instr.span, &format!("{:#06x}: {}",
                    address,
                    instr.code.format_assembly(&instr.operands, address)))?;
            }
            for (i, c) in self.constants.iter().enumerate() {
                let address = 0x1000 + self.layout.constant_start_words * 4 + i * 8;
                writer.writeln(f, indent, Some(c.span), &format!("{address:#06x}: {:?}", c.value))?;
            }
            for (i, func) in self.functions.iter().enumerate() {
                let address = 0x1000 + self.layout.function_start_words * 4 + i * 8;
                writer.writeln(f, indent, None, &format!("{address:#06x}: {func}"))?;
            }
            Ok(())
        }
    }

}

//-------------------------------------------------------------------------------------------------
