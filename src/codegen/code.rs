use enumset::EnumSet;

use crate::codegen::{MachineReg, RankSet, REGS, Unit};


//-------------------------------------------------------------------------------------------------

#[derive(Debug)]
pub struct Code {
    pub(super) encode:                      fn(&[i32]) -> u32,
    pub(super) is_call:                     bool,
    pub(super) is_return:                   bool,
    pub(super) has_output:                  bool,

    pub(super) latency:                     u8,
    pub(super) units:                       EnumSet<Unit>,

    #[cfg(any(feature = "dogfood", test))]
    pub(super) mnemonic:                    &'static str,
    #[cfg(feature = "dogfood")]
    pub(super) format_assembly:             fn(&str, &[i32], i32) -> String,
}


impl Code {
    pub fn encode(&self, args: &[i32])-> u32{ (self.encode)(args) }

    pub fn is_call(&self) -> bool           { self.is_call }
    pub fn is_return(&self) -> bool         { self.is_return }
    pub fn has_output(&self) -> bool        { self.has_output }


    pub fn clobbers_anything(&self) -> bool { self.is_call() }
    pub fn clobbers(&self, reg: MachineReg) -> bool {
        self.is_call() && !REGS.callee_saved.contains(reg)
    }
    pub fn clobbered_ranks(&self) -> RankSet {
        if self.is_call() { REGS.clobbered_ranks } else { RankSet::EMPTY }
    }

    pub fn latency(&self) -> u8             { self.latency }
    pub fn try_pick_unit(&self, free_units: EnumSet<Unit>) -> Option<Unit> {
        (self.units & free_units).iter().next()
    }

    #[cfg(any(feature = "dogfood", test))]
    pub fn mnemonic(&self) -> &str      { self.mnemonic }

    #[cfg(feature = "dogfood")]
    pub fn format_assembly(&self, operands: &[i32], address: i32) -> String {
        (self.format_assembly)(self.mnemonic, operands, address)
    }
}

//-------------------------------------------------------------------------------------------------
