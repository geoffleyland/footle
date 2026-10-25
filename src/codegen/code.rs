use enumset::EnumSet;

use crate::codegen::{MachineReg, RankSet, RegBank, REGS, Unit, Resolved};


//-------------------------------------------------------------------------------------------------

#[derive(Debug)]
pub(super) enum InstrClass { Call, Return, Store }


#[derive(Debug)]
pub(super) struct Code {
    pub(super) encode:                      fn(&[u32]) -> u32,

    pub(super) instr_class:                 Option<InstrClass>,
    pub(super) operand_classes:             &'static[OperandClass],

    pub(super) latency:                     u8,
    pub(super) units:                       EnumSet<Unit>,

    #[cfg(feature = "dogfood")]
    pub(super) mnemonic:                    &'static str,
    #[cfg(feature = "dogfood")]
    pub(super) format_assembly:             fn(&str, &[String], &[u32]) -> String,
}


impl Code {
    pub(super) fn encode(&self, operands: &[Resolved])-> u32 {
        debug_assert_eq!(self.operand_classes.len(), operands.len());

        let bits: Vec<_> = self.operand_classes.iter().zip(operands)
            .map(|(class, op)| op.bits(class.ty))
            .collect();
        (self.encode)(&bits)
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
            .map(|(class, op)| op.bits(class.ty))
            .collect();
        let operand_strings: Vec<_> = self.operand_classes.iter().zip(&bits)
            .map(|(class, b)| (class.format_assembly)(*b, address))
            .collect();

        (self.format_assembly)(self.mnemonic, &operand_strings, &bits)
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
            instr_class:        define_code!(@instr_class $($instr_class)?),
            operand_classes:    &[$($crate::codegen::isa::$operand,)*],

            encode: |operands: &[u32]| -> u32 {
                // If there's no operands (ie `ret`), _it is unused, so the _ silences a warning.
                let mut _it = operands.iter().copied();
                $pattern $(| {
                    let bits = _it.next().unwrap();
                    encode_operand!($operand, bits)
                })*
            },

            latency:            $latency,
            units:              ::enumset::enum_set!($(Unit::$unit)|*),

            #[cfg(feature = "dogfood")]
            mnemonic:           concat!(stringify!($mnemonic) $(, ".", stringify!($dotted))*),
            #[cfg(feature = "dogfood")]
            format_assembly: |mnemonic, strings, bits| {
                format_assembly(mnemonic, AddressingMode::$addressing_mode, strings, bits)
            },
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
    Imm
}

#[derive(Debug)]
pub(super) struct OperandClass {
    pub(super) ty:                          OperandType,
    pub(super) implies_output:              bool,
    #[cfg(feature = "dogfood")]
    pub(super) format_assembly:             fn(u32, i32) -> String,
}


macro_rules! operand_class {
    ($name:ident, $ty:expr, $format:ident, Output)    => { operand_class!(@define $name, $ty, $format, true); };
    ($name:ident, $ty:expr, $format:ident)            => { operand_class!(@define $name, $ty, $format, false); };
    (@define $name:ident, $ty:expr, $format:ident, $output:ident) => {
        const $name: $crate::codegen::code::OperandClass = $crate::codegen::code::OperandClass {
            ty: {
                #[allow(unused_imports)]
                use $crate::codegen::{code::OperandType::*, RegBank::*};
                $ty
            },
            implies_output:                 $output,
            #[cfg(feature = "dogfood")]
            format_assembly:                $format,
        };
    }
}
pub(super) use operand_class;


//-------------------------------------------------------------------------------------------------
