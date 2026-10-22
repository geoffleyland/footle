use enumset::EnumSet;

use crate::codegen::{MachineReg, RankSet, REGS, Unit, Resolved};


//-------------------------------------------------------------------------------------------------

#[derive(Debug)]
pub(super) struct Code {
    pub(super) encode:                      fn(&[Resolved]) -> u32,
    pub(super) is_call:                     bool,
    pub(super) is_return:                   bool,
    pub(super) has_output:                  bool,

    pub(super) latency:                     u8,
    pub(super) units:                       EnumSet<Unit>,

    #[cfg(feature = "dogfood")]
    pub(super) mnemonic:                    &'static str,
    #[cfg(feature = "dogfood")]
    pub(super) format_assembly:             fn(&str, &[Resolved], i32) -> String,
}


impl Code {
    pub(super) fn encode(&self, operands: &[Resolved])-> u32   { (self.encode)(operands) }

    pub(super) fn is_call(&self) -> bool            { self.is_call }
    pub(super) fn is_return(&self) -> bool          { self.is_return }
    pub(super) fn has_output(&self) -> bool         { self.has_output }


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
        (self.format_assembly)(self.mnemonic, operands, address)
    }
}


//-------------------------------------------------------------------------------------------------

macro_rules! define_code {
    (
        ($mnemonic:ident $($dotted:ident)* ; $($suffixes:ident)*)
        $addressing_mode:ident
        ($($reg:ident)*)
        $pattern:literal,
        $latency:literal,
        [$($unit:ident)|+ $(|)?]
    ) => {
        ::paste::paste!(
        pub static [<$mnemonic $(_ $dotted)* $(_ $suffixes)*>]: $crate::codegen::Code =
            $crate::codegen::Code {
            is_call:            is_call!($mnemonic),
            is_return:          is_return!($mnemonic),
            has_output:         has_output!($mnemonic $($reg)*),

            encode: |operands: &[$crate::codegen::Resolved]| -> u32 {
                // If there's no operands (ie `ret`), _it is unused, so the _ silences a warning.
                let mut _it = operands.iter().copied();
                $pattern $(| { let value = _it.next().unwrap(); encode_operand!($reg, value) })*
            },

            latency:            $latency,
            units:              ::enumset::enum_set!($(Unit::$unit)|*),

            #[cfg(feature = "dogfood")]
            mnemonic:           concat!(stringify!($mnemonic) $(, ".", stringify!($dotted))*),
            #[cfg(feature = "dogfood")]
            format_assembly: |mnemonic, operands, address|
                format_assembly(mnemonic, AddressingMode::$addressing_mode, operands, address, &[$(format_operand!($reg)),*]),
        };);
    }
}
pub(super) use define_code;


//-------------------------------------------------------------------------------------------------
