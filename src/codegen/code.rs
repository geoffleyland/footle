use enumset::EnumSet;

use crate::codegen::{MachineReg, RankSet, RegBank, REGS, Unit, Resolved};


//-------------------------------------------------------------------------------------------------

#[derive(Debug)]
pub(super) enum InstrClass { Call, Return, Store }


#[derive(Debug)]
pub(super) struct Code {
    pub(super) pattern:                     u32,

    pub(super) instr_class:                 Option<InstrClass>,
    pub(super) operand_classes:             &'static[OperandClass],

    pub(super) latency:                     u8,
    pub(super) units:                       EnumSet<Unit>,

    #[cfg(feature = "dogfood")]
    pub(super) mnemonic:                    &'static str,
    #[cfg(feature = "dogfood")]
    pub(super) addressing_mode:             crate::codegen::isa::AddressingMode,
}


impl Code {
    pub(super) fn encode(&self, operands: &[Resolved])-> u32 {
        debug_assert_eq!(self.operand_classes.len(), operands.len());

        self.operand_classes.iter().zip(operands)
            .fold(self.pattern, |acc, (class, op)| acc | (class.place)(class.ty.bits(*op)))
    }

    pub(super) fn is_call(&self) -> bool            { matches!(self.instr_class, Some(InstrClass::Call)) }
    pub(super) fn is_return(&self) -> bool          { matches!(self.instr_class, Some(InstrClass::Return)) }
    pub(super) fn has_output(&self) -> bool         {
        !matches!(self.instr_class, Some(InstrClass::Store)) &&
            self.operand_classes.iter().any(|c| c.implies_output)
    }

    pub(super) fn clobbers_anything(&self) -> bool  { self.is_call() }
    pub(super) fn clobbers(&self, reg: MachineReg) -> bool {
        self.is_call() && !REGS.callee_saved.contains(reg)
    }
    pub(super) fn clobbered_ranks(&self) -> RankSet {
        if self.is_call() { REGS.clobbered_ranks } else { RankSet::EMPTY }
    }

    pub(super) fn latency(&self) -> u8              { self.latency }
    pub(super) fn try_pick_unit(&self, free_units: EnumSet<Unit>) -> Option<Unit> {
        (self.units & free_units).iter().next()
    }

    #[cfg(feature = "dogfood")]
    pub(super) fn mnemonic(&self) -> &str           { self.mnemonic }

    #[cfg(feature = "dogfood")]
    pub(super) fn format_assembly(&self, operands: &[Resolved], address: i32) -> String {
        debug_assert_eq!(self.operand_classes.len(), operands.len());
        if self.operand_classes.is_empty() { return self.mnemonic.to_string() }

        let bits: Vec<_> = self.operand_classes.iter().zip(operands)
            .map(|(class, op)| class.ty.value(*op))
            .collect();
        let operand_strings: Vec<_> = self.operand_classes.iter().zip(&bits)
            .map(|(class, b)| (class.format_assembly)(*b, address))
            .collect();

        crate::codegen::isa::format_assembly(self.mnemonic, self.addressing_mode, &operand_strings, &bits)
    }
}


macro_rules! define_code {
    (
        ($mnemonic:ident $($dotted:ident)* ; $($suffixes:ident)*)
        $addressing_mode:ident
        ($($operand:ident)*)
        $pattern:literal
        $($instr_class:ident)? ;
        $latency:literal
        [$($unit:ident)|+ $(|)?]
    ) => {
        ::paste::paste!(
        pub static [<$mnemonic $(_ $dotted)* $(_ $suffixes)*>]: $crate::codegen::code::Code =
            $crate::codegen::code::Code {
            pattern:            $pattern,
            instr_class:        define_code!(@instr_class $($instr_class)?),
            operand_classes:    &[$($crate::codegen::isa::$operand,)*],

            latency:            $latency,
            units:              ::enumset::enum_set!($(Unit::$unit)|*),

            #[cfg(feature = "dogfood")]
            mnemonic:           concat!(stringify!($mnemonic) $(, ".", stringify!($dotted))*),
            #[cfg(feature = "dogfood")]
            addressing_mode:    $crate::codegen::isa::AddressingMode::$addressing_mode
        };);
    };

    (@instr_class)                  => { None };
    (@instr_class $class:ident)     => { Some($crate::codegen::code::InstrClass::$class) };
}
pub(super) use define_code;


//-------------------------------------------------------------------------------------------------

#[derive(Debug, Copy, Clone)]
pub(super) enum OperandType {
    Reg(RegBank),
    Imm(i32, i32, u8),
}

pub(super) const fn iimm(width: u8, right_shift: u8) -> OperandType {
    // Shift right to avoid the overflow that would happen if we shifted left with a width of 32.
    OperandType::Imm(i32::MIN >> (32 - width), i32::MAX >> (32 - width), right_shift)
}
#[cfg_attr(not(target_arch = "aarch64"),
    expect(dead_code, reason = "only AArch64 has unsigned immediate fields"))]
pub(super) const fn uimm(width: u8, right_shift: u8) -> OperandType {
    OperandType::Imm(0, i32::MAX >> (31 - width), right_shift)
}

impl OperandType {
    pub(super) fn value(self, operand: Resolved) -> i32 {
        match (self, operand) {
            (Self::Imm(min, max, right_shift), Resolved::Imm(value)) => {
                let low_bits = (1 << right_shift) - 1;
                assert!(value & low_bits == 0,
                    "internal compiler error: {value} isn't a multiple of {}", 1 << right_shift);
                assert!((min..=max).contains(&(value >> right_shift)),
                    "internal compiler error: {value} out of range");
                value
            }
            (Self::Reg(bank), Resolved::Reg(reg)) => {
                assert_eq!(RegBank::of_reg(reg), bank,
                    "internal compiler error: {reg:?} is in the wrong register bank");
                reg.encoding().into()
            }
            _ => panic!("internal compiler error: operand type mismatch")
        }
    }

    pub(super) fn bits(self, operand: Resolved) -> u32 {
        let value = self.value(operand);
        match self {
            Self::Imm(min, max, right_shift) => {
                let mask = max.wrapping_sub(min).cast_unsigned();      // 2^width - 1; all ones for the full range
                (value >> right_shift).cast_unsigned() & mask
            }
            Self::Reg(..) => { value.cast_unsigned() }
        }
    }
}



#[derive(Debug)]
pub(super) struct OperandClass {
    pub(super) ty:                          OperandType,
    pub(super) implies_output:              bool,
    pub(super) place:                       fn(u32) -> u32,
    #[cfg(feature = "dogfood")]
    pub(super) format_assembly:             fn(i32, i32) -> String,
}


macro_rules! operand_class {
    ($name:ident, $ty:expr, $format:ident, Output, $($place:tt)*) => {
        operand_class!(@define $name, $ty, $format, true, $($place)*);
    };
    ($name:ident, $ty:expr, $format:ident, $($place:tt)*) => {
        operand_class!(@define $name, $ty, $format, false, $($place)*);
    };

    (@define $name:ident, $ty:expr, $format:ident, $output:ident, $($place:tt)*) => {
        const $name: $crate::codegen::code::OperandClass = $crate::codegen::code::OperandClass {
            ty: {
                #[allow(unused_imports)]
                use $crate::codegen::{code::{OperandType::*, uimm, iimm}, RegBank::*};
                $ty
            },
            implies_output:                 $output,
            place:                          $($place)*,
            #[cfg(feature = "dogfood")]
            format_assembly:                $format,
        };
    }
}
pub(super) use operand_class;


//-------------------------------------------------------------------------------------------------
