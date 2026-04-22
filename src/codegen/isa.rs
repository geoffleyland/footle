#![allow(clippy::unusual_byte_groupings)]

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
    pub(super) name:            &'static str,
    pub(super) has_output:      bool,
    pub(super) encode:          fn(&[u32]) -> u32,
    pub(super) latency:         u8,
    pub(super) clobbers:        u32,
    pub(super) restore_regs:    bool,
    units:                      EnumSet<Unit>,

    pub(super) format:          fn(&[i32], i32) -> String,
}


impl Code {
    pub fn try_pick_unit(&self, free_units: EnumSet<Unit>) -> Option<Unit> {
        (self.units & free_units).iter().next()
    }
}


fn format_offset(n: i32) -> String {
    if n > -10 && n < 10 { format!("#{n}")}
    else if n < 0 { format!("#-{:#x}", -n) }
    else { format!("#{n:#x}") }
}


fn x_reg(n: i32) -> String {
    match n {
        31 => "sp".into(),
        n  => format!("x{n}"),
    }
}


fn f64_math_format(operands: &[i32], _: i32) -> String {
    format!("d{}, d{}, d{}", operands[0], operands[1], operands[2])
}


fn format_reg_offset(reg: i32, offset: i32) -> String {
    if offset == 0 {
        format!("[{}]", x_reg(reg))
    } else {
        format!("[{}, {}]", x_reg(reg), format_offset(offset))
    }
}


pub(super) static FADD: Code = Code {
    name:                   "fadd",
    has_output:             true,
    encode:                 |operands| 0x1E60_2800 | (operands[2] << 16) | (operands[1] << 5) | operands[0],
    latency:                1,
    clobbers:               0,
    restore_regs:           false,
    units:                  enum_set!(Unit::FP11 | Unit::FP12 | Unit::FP13 | Unit::FP14),
    format:                 |operands, address| f64_math_format(operands, address),
};

pub(super) static FSUB: Code = Code {
    name:                   "fsub",
    has_output:             true,
    encode:                 |operands| 0x1E60_3800 | (operands[2] << 16) | (operands[1] << 5) | operands[0],
    latency:                1,
    clobbers:               0,
    restore_regs:           false,
    units:                  enum_set!(Unit::FP11 | Unit::FP12 | Unit::FP13 | Unit::FP14),
    format:                 |operands, address| f64_math_format(operands, address),
};

pub(super) static FMUL: Code = Code {
    name:                   "fmul",
    has_output:             true,
    encode:                 |operands| 0x1E60_0800 | (operands[2] << 16) | (operands[1] << 5) | operands[0],
    latency:                4,
    clobbers:               0,
    restore_regs:           false,
    units:                  enum_set!(Unit::FP11 | Unit::FP12 | Unit::FP13 | Unit::FP14),
    format:                 |operands, address| f64_math_format(operands, address),
};

pub(super) static FDIV: Code = Code {
    name:                   "fdiv",
    has_output:             true,
    encode:                 |operands| 0x1E60_1800 | (operands[2] << 16) | (operands[1] << 5) | operands[0],
    latency:                10,
    clobbers:               0,
    restore_regs:           false,
    units:                  enum_set!(Unit::FP14),
    format:                 |operands, address| f64_math_format(operands, address),
};

pub(super) static CFDIV: Code = Code {
    name:                   "fdiv",
    has_output:             true,
    encode:                 |_operands| 0x1E60_1800 | 1 << 16,
    latency:                10,
    clobbers:               0xFFFF_00FF,
    restore_regs:           false,
    units:                  enum_set!(Unit::FP14),
    format:                 |_, _| "d0, d0, d1".to_string(),
};

pub(super) static MOV_I64: Code = Code {
    name:                   "mov",
    has_output:             true,
    encode:                 |operands| 0b1_01_01010_00_0_00000_000000_11111_00000 | (operands[1] << 16) | operands[0],
    latency:                2,
    clobbers:               0,
    restore_regs:           false,
    units:                  enum_set!(Unit::FP11 | Unit::FP12 | Unit::FP13 | Unit::FP14),
    format:                 |operands, _| format!("{}, {}", x_reg(operands[0]), x_reg(operands[1])),
};

pub(super) static FMOV: Code = Code {
    name:                   "fmov",
    has_output:             true,
    encode:                 |operands| 0x1E60_4000 | (operands[1] << 5) | operands[0],
    latency:                2,
    clobbers:               0,
    restore_regs:           false,
    units:                  enum_set!(Unit::FP11 | Unit::FP12 | Unit::FP13 | Unit::FP14),
    format:                 |operands, _| format!("d{}, d{}", operands[0], operands[1]),
};

pub(super) static LDR_PC_F64: Code = Code {
    name:                   "ldr",
    has_output:             true,
    encode:                 |operands| 0x5C00_0000 | (((operands[1] >> 2) & 0x7_FFFF) << 5) | operands[0],
    latency:                10,
    clobbers:               0,
    restore_regs:           false,
    units:                  enum_set!(Unit::LS8 | Unit::L9 | Unit::L10),
    format:                 |operands, address| format!("d{}, #{:#x}", operands[0], address + operands[1]),
};

pub(super) static LDR_OFFSET_F64: Code = Code {
    name:                   "ldr",
    has_output:             true,
    encode:                 |operands| 0b11_111_1_01_01_000000000000_00000_00000 | operands[0] | operands[1] << 5 | (((operands[2] >> 3) & 0x7FF) << 10),
    latency:                10,
    clobbers:               0,
    restore_regs:           false,
    units:                  enum_set!(Unit::LS8 | Unit::L9 | Unit::L10),
    format:                 |operands, _| format!("d{}, {}", operands[0], format_reg_offset(operands[1], operands[2])),
};

pub(super) static LDR_POST_F64: Code = Code {
    name:                   "ldr",
    has_output:             true,
    encode:                 |operands| 0b11_111_1_00_01_0_000000000_01_00000_00000 | operands[0] | operands[1] << 5 | ((operands[2] & 0x1FF) << 12),
    latency:                10,
    clobbers:               0,
    restore_regs:           false,
    units:                  enum_set!(Unit::LS8 | Unit::L9 | Unit::L10),
    format:                 |operands, _| format!("d{}, [{}], {}", operands[0], x_reg(operands[1]), format_offset(operands[2])),
};

pub(super) static LDP_POST_I64: Code = Code {
    name:                   "ldp",
    has_output:             false,
    encode:                 |operands| 0b10_101_0_001_1_0000000_00000_00000_00000 | operands[0] | operands[1] << 10 | operands[2] << 5 | ((operands[3] >> 3) & 0x7F) << 15,
    latency:                10,
    clobbers:               0,
    restore_regs:           false,
    units:                  enum_set!(Unit::LS8 | Unit::L9 | Unit::L10),
    format:                 |operands, _| format!("x{}, x{}, [{}], {}", operands[0], operands[1], x_reg(operands[2]), format_offset(operands[3])),
};

pub(super) static LDP_POST_F64: Code = Code {
    name:                   "ldp",
    has_output:             false,
    encode:                 |operands| 0b01_101_1_001_1_0000000_00000_00000_00000 | operands[0] | operands[1] << 10 | operands[2] << 5 | ((operands[3] >> 3) & 0x7F) << 15,
    latency:                10,
    clobbers:               0,
    restore_regs:           false,
    units:                  enum_set!(Unit::LS8 | Unit::L9 | Unit::L10),
    format:                 |operands, _| format!("d{}, d{}, [{}], {}", operands[0], operands[1], x_reg(operands[2]), format_offset(operands[3])),
};

pub(super) static STR_OFFSET_F64: Code = Code {
    name:                   "str",
    has_output:             false,
    encode:                 |operands| 0b11_111_1_01_00_000000000000_00000_00000 | (((operands[2] >> 3) & 0x7FF) << 10) | operands[1] << 5 | operands[0],
    latency:                10,
    clobbers:               0,
    restore_regs:           false,
    units:                  enum_set!(Unit::LS8 | Unit::L9 | Unit::L10),
    format:                 |operands, _| format!("d{}, {}", operands[0], format_reg_offset(operands[1], operands[2])),
};

pub(super) static STR_PRE_F64: Code = Code {
    name:                   "str",
    has_output:             false,
    encode:                 |operands| 0b11_111_1_00_00_0_000000000_11_00000_00000 | ((operands[2] & 0x1FF) << 12) | operands[1] << 5 | operands[0],
    latency:                10,
    clobbers:               0,
    restore_regs:           false,
    units:                  enum_set!(Unit::LS8 | Unit::L9 | Unit::L10),
    format:                 |operands, _| format!("d{}, {}!", operands[0], format_reg_offset(operands[1], operands[2])),
};

pub(super) static STP_PRE_I64: Code = Code {
    name:                   "stp",
    has_output:             false,
    encode:                 |operands| 0b10_101_0_011_0_0000000_00000_00000_00000 | operands[0] | operands[1] << 10 | operands[2] << 5 | ((operands[3] >> 3) & 0x7F) << 15,
    latency:                10,
    clobbers:               0,
    restore_regs:           false,
    units:                  enum_set!(Unit::LS8 | Unit::L9 | Unit::L10),
    format:                 |operands, _| format!("x{}, x{}, [{}, {}]!", operands[0], operands[1], x_reg(operands[2]), format_offset(operands[3])),
};

pub(super) static STP_PRE_F64: Code = Code {
    name:                   "stp",
    has_output:             false,
    encode:                 |operands| 0b01_101_1_011_0_0000000_00000_00000_00000 | operands[0] | operands[1] << 10 | operands[2] << 5 | ((operands[3] >> 3) & 0x7F) << 15,
    latency:                10,
    clobbers:               0,
    restore_regs:           false,
    units:                  enum_set!(Unit::LS8 | Unit::L9 | Unit::L10),
    format:                 |operands, _| format!("d{}, d{}, [{}, {}]!", operands[0], operands[1], x_reg(operands[2]), format_offset(operands[3])),
};

pub(super) static BL: Code = Code {
    name:                   "bl",
    has_output:             false,
    encode:                 |operands| 0b1_00_101_00000000000000000000000000 | ((operands[0] >> 2) & 0x03FF_FFFF),
    latency:                1,
    clobbers:               0xFFFF_00FF,
    restore_regs:           false,
    units:                  enum_set!(Unit::LS8 | Unit::L9 | Unit::L10),
    format:                 |operands, address| format!("#{:#x}", address + operands[0]),
};

pub(super) static RET: Code = Code {
    name:                   "ret",
    has_output:             false,
    encode:                 |_| 0xD65F_03C0,
    latency:                1,
    clobbers:               0,
    restore_regs:           true,
    units:                  enum_set!(Unit::LS8 | Unit::L9 | Unit::L10),
    format:                 |_, _| String::new(),
};


/// The order in which we want to allocate registers.
pub(super) const REG_ORDER: [u8; 32] = [
    16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,     // d16-d31
     8,  9, 10, 11, 12, 13, 14, 15,                                     // d8-d16 (callee saved)
     0,  1,  2,  3,  4,  5,  6,  7,                                     // d0-d7
];


#[allow(clippy::cast_possible_truncation)]
pub(super) const REG_INDEX: [u8; 32] = {
    let mut t = [255u8; 32];
    let mut i = 0;
    while i < REG_ORDER.len() {
        t[REG_ORDER[i] as usize] = i as u8;
        i += 1;
    }
    t
};


pub(super) const CALLEE_SAVED_REGS: u32 = 0x0000_FF00;


// For each real register dN, make a mask of the appropriate bit in REG_ORDER.
// `for` and iterators aren't allowed in const blocks, hence the weird while loop.
const CLOBBER_MASK: [u32; 32] = {
    let mut t = [0u32; 32];
    let mut i = 0;
    while i < 32 {
        t[i] = 1 << REG_INDEX[i];
        i += 1;
    }
    t
};


// Convert a mask in real registers to a mask in register order
pub(super) fn real_reg_to_ordered_reg_mask(clobbers: u32) -> u32 {
    let mut c = clobbers;
    let mut mask = 0u32;
    while c != 0 {
        let bit = c.trailing_zeros() as usize;
        mask |= CLOBBER_MASK[bit];
        c &= c - 1;
    }
    mask
}
