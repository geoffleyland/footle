use typed_index_collections::TiVec;

use crate::core::Span;
use super::{emit, allocator,
    Code, Pool, Signature, ValueSlot, IntReg, FloatReg, MachineReg, RegSet, RegBank,
    ConstantIndex, FunctionIndex, RelativeTo, REGS};


//-------------------------------------------------------------------------------------------------

pub(super) type Operand = super::operand::Operand<MachineReg>;

impl From<i32> for Operand          { fn from(imm: i32) -> Self { Self::Imm(imm) } }
impl From<MachineReg> for Operand   { fn from(r: MachineReg) -> Self { Self::Reg(r) } }
impl From<IntReg> for Operand       { fn from(r: IntReg) -> Self { Self::Reg(MachineReg::from(r)) } }
impl From<FloatReg> for Operand     { fn from(r: FloatReg) -> Self { Self::Reg(MachineReg::from(r)) } }


macro_rules! assemble_expr {
    ($instrs:expr, $op:expr $(, $($operand:expr),*)?) => {
        $instrs.push(Instr {
            code:                   $op,
            operands:               vec![$($($operand.into()),*)?],
            span:                   None,
        })
    }
}
pub(super) use assemble_expr;


macro_rules! assemble_instr {
    ($instrs:expr, $op:ident $(, $($operand:expr),*)?) => {
        $crate::codegen::assembler::assemble_expr!($instrs, &$crate::codegen::isa::$op $(, $($operand),*)?)
    }
}
pub(super) use assemble_instr;


//-------------------------------------------------------------------------------------------------

pub(super) struct Instr<OP> {
    pub(super) code:                &'static Code,
    pub(super) operands:            Vec<OP>,
    pub(super) span:                Option<Span>,
}


#[derive(Debug, Copy, Clone)]
pub(super) struct Layout {
    #[cfg_attr(not(feature = "dogfood"), expect(dead_code, reason = "only the disassembler reads instr_words"))]
    pub(super) instr_words:         usize,
    pub(super) glue_start_words:    usize,
    pub(super) constant_start_words:usize,
    pub(super) function_start_words:usize,
    pub(super) total_bytes:         usize,
}
impl Layout {
    fn new(glue_start_words: usize, instr_words: usize, constant_count:usize, function_count:usize) -> Self {
        let constant_start_words = instr_words.next_multiple_of(2);
        let function_start_words = constant_start_words + constant_count * 2;

        Self {
            instr_words, glue_start_words, constant_start_words, function_start_words,
            total_bytes:            function_start_words * 4 + function_count * 8,
        }
    }
    fn constant_words(&self, index: ConstantIndex) -> usize { self.constant_start_words + 2 * usize::from(index) }
    fn function_words(&self, index: FunctionIndex) -> usize { self.function_start_words + 2 * usize::from(index) }
}


/// Resolved instructions have operands that are either register numbers or immediates/offsets.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(super) enum Resolved {
    Reg(MachineReg),
    Imm(i32),
}


//-------------------------------------------------------------------------------------------------

pub struct Block {
    pub(super) layout:              Layout,
    pub(super) instrs:              Vec<Instr<Resolved>>,
}


pub(super) fn run(
    allocated:                      allocator::Block,
    slot_count:                     usize,
    signature:                      &Signature,
    pool:                           &Pool,
) -> Block{
    let mut draft_instrs = Vec::new();
    let value_words = emit_function(allocated, slot_count, &mut draft_instrs);
    let glue_start_words = draft_instrs.len();
    emit::glue(signature, &mut draft_instrs);

    let layout = Layout::new(glue_start_words, draft_instrs.len(), pool.constants.len(), pool.functions.len());

    let instrs = resolve(draft_instrs, &layout, &value_words);

    Block{ layout, instrs }
}


fn emit_function(
    allocated:                      allocator::Block,
    slot_count:                     usize,
    instrs:                         &mut Vec<Instr<Operand>>,
) -> TiVec<ValueSlot, Option<usize>> {
    let mut value_words: TiVec<_, _> = vec![None; slot_count].into();

    let regs_to_save = REGS.regs_to_save(allocated.written_regs);
    emit::save_regs(instrs, regs_to_save);

    for instr in allocated.instrs {
        emit::moves(instrs, &instr.moves);

        // Restore callee saved registers before a `ret`.
        if instr.code.is_return() { emit::restore_regs(instrs, regs_to_save); }

        let operands = instr.code.has_output()
            .then(|| Operand::from(instr.result_reg
                .expect("internal compiler error: no register allocated for instruction result")))
            .into_iter()
            .chain(instr.operands.iter().copied())
            .collect();

        value_words[instr.value_slot] = Some(instrs.len());
        instrs.push(Instr{ code: instr.code, operands, span: Some(instr.span) });
    }
    value_words
}


pub(super) fn split_regs_by_bank(regs: RegSet) -> (Vec<MachineReg>, Vec<MachineReg>) {
    (regs.intersection(RegBank::Int.regs()).iter().collect(),
     regs.intersection(RegBank::Float.regs()).iter().collect())
}


//-------------------------------------------------------------------------------------------------

fn compute_offset(
    value_words:                            &TiVec<ValueSlot, Option<usize>>,
    target_word:                            usize,
    word_index:                             usize,
    relative_to:                            RelativeTo,
) -> i32 {
    let from_word = match relative_to {
        RelativeTo::ThisInstr               => word_index,
        RelativeTo::InstrOf(slot) => {
            value_words[slot]
                .expect("internal compiler error: InstrOf names a value with no instruction")
        }
    };
    i32::try_from((target_word - from_word) * 4).expect("internal compiler error: pool offset too large")
}


/// Once we have ALL the instructions, resolve offsets to constants and functions into simple
/// offsets.
fn resolve(
    instrs:                                 Vec<Instr<Operand>>,
    layout:                                 &Layout,
    value_words:                            &TiVec<ValueSlot, Option<usize>>,
) -> Vec<Instr<Resolved>> {
    use super::operand::Operand::*;

    instrs.into_iter().enumerate()
        .map(|(word_index, instr)| {
            let offset = |target_word, relative_to|
                Resolved::Imm(compute_offset(value_words, target_word, word_index, relative_to));
            let operands: Vec<_> = instr.operands.into_iter().map(|op| {
                match op {
                    Reg(r)                  => Resolved::Reg(r),
                    Imm(o)                  => Resolved::Imm(o),
                    PooledF64(i, r)         => offset(layout.constant_words(i), r),
                    Function(i, r)          => offset(layout.function_words(i), r),
                }
            }).collect();
            Instr{ operands, code: instr.code, span: instr.span }
        }).collect()
}


//-------------------------------------------------------------------------------------------------
// Text output for assembler

#[cfg(feature = "dogfood")]
mod display {
    use std::fmt;
    use super::*;
    use crate::core::{Styleable, LineStyle};

    impl Styleable for (&Block, &Pool) {
        fn write<W: LineStyle>(&self, f: &mut fmt::Formatter, indent: u16, writer: &W) -> fmt::Result {
            for (i, instr) in self.0.instrs.iter().enumerate() {
                let address = i32::try_from(0x1000 + i * 4).unwrap();
                writer.writeln(f, indent, instr.span, &format!("{:#06x}: {}", address,
                    instr.code.format_assembly(&instr.operands, address)))?;
            }
            for (i, c) in self.1.constants.iter_enumerated() {
                let address = 0x1000 + self.0.layout.constant_words(i) * 4;
                writer.writeln(f, indent, Some(c.span), &format!("{address:#06x}: {:?}", c.value))?;
            }
            for (i, func) in self.1.functions.iter_enumerated() {
                let address = 0x1000 + self.0.layout.function_words(i) * 4;
                writer.writeln(f, indent, None, &format!("{address:#06x}: {func}"))?;
            }
            Ok(())
        }
    }
}


//-------------------------------------------------------------------------------------------------
