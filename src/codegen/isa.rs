use enumset::{EnumSet, EnumSetType, enum_set};

#[derive(Debug, EnumSetType)]
pub enum Unit {
    LS8,
    L9,
    L10,
    FP11,
    FP12,
    FP13,
    FP14,
}


#[derive(Debug)]
pub(super) struct Code {
    pub(super) name:        &'static str,
    pub(super) has_output:  bool,
    pub(super) encode:      fn(&[u32]) -> u32,
    pub(super) latency:     u8,
    units:                  EnumSet<Unit>,
}


impl Code {
    pub fn try_pick_unit(&self, free_units: EnumSet<Unit>) -> Option<Unit> {
        (self.units & free_units).iter().next()
    }
}


pub(super) static FADD: Code = Code {
    name:                   "FADD",
    has_output:             true,
    encode:                 |regs| 0x1E60_2800 | (regs[2] << 16) | (regs[1] << 5) | regs[0],
    latency:                1,
    units:                  enum_set!(Unit::FP11 | Unit::FP12 | Unit::FP13 | Unit::FP14),
};

pub(super) static FSUB: Code = Code {
    name:                   "FSUB",
    has_output:             true,
    encode:                 |regs| 0x1E60_3800 | (regs[2] << 16) | (regs[1] << 5) | regs[0],
    latency:                1,
    units:                  enum_set!(Unit::FP11 | Unit::FP12 | Unit::FP13 | Unit::FP14),
};

pub(super) static FMUL: Code = Code {
    name:                   "FMUL",
    has_output:             true,
    encode:                 |regs| 0x1E60_0800 | (regs[2] << 16) | (regs[1] << 5) | regs[0],
    latency:                4,
    units:                  enum_set!(Unit::FP11 | Unit::FP12 | Unit::FP13 | Unit::FP14),
};

pub(super) static FDIV: Code = Code {
    name:                   "FDIV",
    has_output:             true,
    encode:                 |regs| 0x1E60_1800 | (regs[2] << 16) | (regs[1] << 5) | regs[0],
    latency:                10,
    units:                  enum_set!(Unit::FP14),
};

pub(super) static FMOV: Code = Code {
    name:                   "FMOV",
    has_output:             true,
    encode:                 |regs| 0x1E60_4000 | (regs[1] << 5) | regs[0],
    latency:                2,
    units:                  enum_set!(Unit::FP11 | Unit::FP12 | Unit::FP13 | Unit::FP14),
};

pub(super) static LDR: Code = Code {
    name:                   "LDR",
    has_output:             true,
    encode:                 |regs| 0x5C00_0000 | ((regs[1] >> 2) << 5) | regs[0],
    latency:                10,
    units:                  enum_set!(Unit::LS8 | Unit::L9 | Unit::L10),
};

pub(super) static RET: Code = Code {
    name:                   "RET",
    has_output:             false,
    encode:                 |_| 0xD65F_03C0,
    latency:                1,
    units:                  enum_set!(Unit::LS8 | Unit::L9 | Unit::L10),
};


pub(super) const REGISTER_ORDER: [u8; 24] = [
    16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,     // d16-d31
     0,  1,  2,  3,  4,  5,  6,  7,                                     // d0-d7
];


#[allow(clippy::cast_possible_truncation)]
pub(super) const REGISTER_INDEX: [u8; 32] = {
    let mut t = [255u8; 32];
    let mut i = 0;
    while i < REGISTER_ORDER.len() {
        t[REGISTER_ORDER[i] as usize] = i as u8;
        i += 1;
    }
    t
};
