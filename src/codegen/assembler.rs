use typed_index_collections::TiVec;

use super::{Code, abi, Pool, Signature, allocator, ValueSlot, MachineReg, RegSet, RegBank, RelativeTo, REGS};

#[cfg(feature = "dogfood")]
use crate::core::Span;


//-------------------------------------------------------------------------------------------------

pub(super) enum Operand {
    Reg(MachineReg),
    PooledF64(usize, RelativeTo),
    ImmU16(u16),
    Offset(i32),
    Function(usize, RelativeTo),
}

pub(super) struct Offset(pub(super) i32);
impl From<Offset> for Operand {
    fn from(o: Offset) -> Self { Self::Offset(o.0) }
}

impl From<MachineReg> for Operand {
    fn from(r: MachineReg) -> Self { Self::Reg(r) }
}


macro_rules! assemble_expr {
    ($instrs:expr, $op:expr $(, $($operand:expr),*)?) => {
        $instrs.push(Instr {
            code:                   $op,
            operands:               vec![$($($operand.into()),*)?],
            #[cfg(feature = "dogfood")]
            span:                   None,
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


/// Resolved instructions have operands that are either register numbers or immediates/offsets.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(super) enum Resolved {
    Reg(MachineReg),
    Imm(i32),
}

impl Resolved {
    pub(super) fn reg(self) -> MachineReg {
        if let Self::Reg(r) = self { r } else {
            panic!("internal compiler error: immediate passed as register operand")
        }
    }
    pub(super) fn imm(self) -> i32 {
        if let Self::Imm(i) = self { i } else {
            panic!("internal compiler error: register passed as immediate operand")
        }
    }
    pub(super) fn imm_bits(self) -> u32 { self.imm().cast_unsigned() }
    pub(super) fn reg_field(self, bank: RegBank) -> u32 {
        let reg = self.reg();
        assert_eq!(RegBank::of_reg(reg), bank, "internal compiler error: {reg:?} is in the wrong register bank");
        u32::from(reg.encoding())
    }
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
    let value_words = emit_function(allocated, slot_count, &mut draft_instrs, &pool.functions);
    let glue_start_words = draft_instrs.len();
    abi::emit_glue(signature, &mut draft_instrs);

    let layout = Layout::new(glue_start_words, draft_instrs.len(), pool.constants.len(), pool.functions.len());

    let instrs = resolve(draft_instrs, &layout, &value_words);

    Block{ layout, instrs }
}


fn emit_function(
    allocated:                      allocator::Block,
    slot_count:                     usize,
    instrs:                         &mut Vec<Instr<Operand>>,
    functions:                      &[String],
) -> TiVec<ValueSlot, Option<usize>> {
    let mut value_words: TiVec<_, _> = vec![None; slot_count].into();

    let regs_to_save = REGS.regs_to_save(allocated.written_regs);
    abi::emit_save_regs(instrs, regs_to_save);

    for instr in allocated.instrs {
        abi::emit_moves(instrs, &instr.moves);

        // Restore callee saved registers before a `ret`.
        if instr.code.is_return() { abi::emit_restore_regs(instrs, regs_to_save); }

        let operands = instr.code.has_output()
            .then(|| Operand::from(instr.result_reg
                .expect("internal compiler error: no register allocated for instruction result")))
            .into_iter()
            .chain(instr.operands.iter().map(|op| match op {
                allocator::Operand::Reg(r)              => (*r).into(),
                allocator::Operand::PooledF64(i, r)     => Operand::PooledF64(*i, *r),
                allocator::Operand::ImmU16(v)           => Operand::ImmU16(*v),
                allocator::Operand::Function(name, r)   => {
                    let index = functions.iter().position(|s| s == name)
                        .expect("internal compiler error: unknown function name");
                    Operand::Function(index, *r)
                }
            }))
        .collect();

        value_words[instr.value_slot] = Some(instrs.len());
        instrs.push(Instr{ code: instr.code, operands,
        #[cfg(feature = "dogfood")]
            span: Some(instr.span)
        });
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
    table_start_words:                      usize,
    entry_index:                            usize,
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
    i32::try_from((table_start_words - from_word) * 4 + (entry_index * 8))
        .expect("internal compiler error: pool offset too large")
}


/// Once we have ALL the instructions, resolve offsets to constants and functions into simple
/// offsets.
fn resolve(
    instrs:                                 Vec<Instr<Operand>>,
    layout:                                 &Layout,
    value_words:                            &TiVec<ValueSlot, Option<usize>>,
) -> Vec<Instr<Resolved>> {
    use Operand::*;

    instrs.into_iter().enumerate()
        .map(|(word_index, instr)| {
            let operands: Vec<_> = instr.operands.into_iter().map(|op| {
                match op {
                    Reg(r)                  => Resolved::Reg(r),
                    ImmU16(v)               => Resolved::Imm(v.into()),
                    PooledF64(i, r)         => Resolved::Imm(compute_offset(
                                                    value_words, layout.constant_start_words,
                                                    i, word_index, r)),
                    Function(i, r)          => Resolved::Imm(compute_offset(
                                                    value_words, layout.function_start_words,
                                                    i, word_index, r)),
                    Offset(o)               => Resolved::Imm(o),
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

    impl Styleable for (&Block, &Pool) {
        fn write<W: LineStyle>(&self, f: &mut fmt::Formatter, indent: u16, writer: &W) -> fmt::Result {
            for (i, instr) in self.0.instrs.iter().enumerate() {
                let address = i32::try_from(0x1000 + i * 4).unwrap();
                writer.writeln(f, indent, instr.span, &format!("{:#06x}: {}",
                    address,
                    instr.code.format_assembly(&instr.operands, address)))?;
            }
            for (i, c) in self.1.constants.iter().enumerate() {
                let address = 0x1000 + self.0.layout.constant_start_words * 4 + i * 8;
                writer.writeln(f, indent, Some(c.span), &format!("{address:#06x}: {:?}", c.value))?;
            }
            for (i, func) in self.1.functions.iter().enumerate() {
                let address = 0x1000 + self.0.layout.function_start_words * 4 + i * 8;
                writer.writeln(f, indent, None, &format!("{address:#06x}: {func}"))?;
            }
            Ok(())
        }
    }

}

//-------------------------------------------------------------------------------------------------
