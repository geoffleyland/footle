#![allow(clippy::unusual_byte_groupings, non_upper_case_globals)]

use enumset::EnumSetType;

use crate::codegen::{operand_class, define_code, regs, RegFile, RegSet};
#[cfg(feature = "dogfood")]
use display::*;


//-------------------------------------------------------------------------------------------------

pub static REGS: RegFile = RegFile::new(2, 1, 31,       // sp = x2, ra = x1, scratch = t6 (x31)
    RegSet::new(0x0FFC_0300_0FFC_0300),                 // s0-s11 (x8, x9, x18-27), fs0-fs11 likewise
    [
        regs::machine_regs([10, 11, 12, 13, 14, 15, 16, 17]),      // a0-a7
        regs::machine_regs([42, 43, 44, 45, 46, 47, 48, 49]),      // fa0-fa7
    ],
    &[
         5,  6,  7, 28, 29, 30,                     // t0-t5: caller-saved temps (t6 is scratch)
         9, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, // s1-s11: callee-saved (s0 is the frame pointer)
        10, 11, 12, 13, 14, 15, 16, 17,             // a0-a7: argument registers
        32, 33, 34, 35, 36, 37, 38, 39, 60, 61, 62, 63,                 // ft0-ft11 (caller-saved)
        40, 41, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59,                 // fs0-fs11 (callee-saved)
        42, 43, 44, 45, 46, 47, 48, 49,                                 // fa0-fa7 (function args)
    ],
);


//-------------------------------------------------------------------------------------------------
// Architecture details.

// Temporary units until we target a real cpu
#[derive(Debug, EnumSetType)]
pub enum Unit { U1, U2, U3}


//-------------------------------------------------------------------------------------------------
// Operand classes  used for defining instructions

operand_class!(xd,      Reg(Int),   format_x_reg, Output);
operand_class!(xs1,     Reg(Int),   format_x_reg);
operand_class!(xs2,     Reg(Int),   format_x_reg);

operand_class!(fd,      Reg(Float), format_f_reg, Output);
operand_class!(fs1,     Reg(Float), format_f_reg);
operand_class!(fs2,     Reg(Float), format_f_reg);
operand_class!(fs12,    Reg(Float), format_f_reg);

operand_class!(imm_i,   Imm,        format_imm);
operand_class!(imm_j,   Imm,        format_imm);
operand_class!(imm_s,   Imm,        format_imm);
operand_class!(imm_u,   Imm,        format_imm_u);


//-------------------------------------------------------------------------------------------------
// The instructions!

code!(mv xd, xs1            =>  0b000000000000_00000_000_00000_0010011          ;1 [U1 | U2 | U3]);
code!(fmv.d fd, fs12        => 0b0010001_00000_00000_000_00000_1010011          ;1 [U1 | U2 | U3]);

code!(sd xs2, imm_s(xs1)    => 0b0000000_00000_00000_011_00000_0100011  Store   ;1 [U1 | U2 | U3]);
code!(fsd fs2, imm_s(xs1)   => 0b0000000_00000_00000_011_00000_0100111  Store   ;1 [U1 | U2 | U3]);

code!(ld xd, imm_i(xs1)     =>  0b000000000000_00000_011_00000_0000011          ;1 [U1 | U2 | U3]);
code!(li xd, imm_i          =>  0b000000000000_00000_000_00000_0010011          ;1 [U1 | U2 | U3]);
code!(fld fd, imm_i(xs1)    =>  0b000000000000_00000_011_00000_0000111          ;1 [U1 | U2 | U3]);

code!(auipc xd, imm_u       =>    0b00000000000000000000_00000_0010111          ;1 [U1 | U2 | U3]);
code!(addi xd, xs1, imm_i   =>  0b000000000000_00000_000_00000_0010011          ;1 [U1 | U2 | U3]);

code!(fadd.d fd, fs1, fs2   => 0b0000001_00000_00000_111_00000_1010011          ;1 [U1 | U2 | U3]);
code!(fsub.d fd, fs1, fs2   => 0b0000101_00000_00000_111_00000_1010011          ;1 [U1 | U2 | U3]);
code!(fmul.d fd, fs1, fs2   => 0b0001001_00000_00000_111_00000_1010011          ;1 [U1 | U2 | U3]);
code!(fdiv.d fd, fs1, fs2   => 0b0001101_00000_00000_111_00000_1010011          ;1 [U1 | U2 | U3]);

code!(jal imm_j             =>    0b00000000000000000000_00001_1101111  Call    ;1 [U1 | U2 | U3]);
code!(jalr xs1              =>  0b000000000000_00000_000_00001_1100111  Call    ;1 [U1 | U2 | U3]);

code!(ret                   =>  0b000000000000_00001_000_00000_1100111  Return  ;1 [U1 | U2 | U3]);

//-------------------------------------------------------------------------------------------------

// Cover the instruction operand patterns to try to figure out the addressing mode (if there is one)
macro_rules! code {
    ($mnemonic:ident $(. $dotted:ident)* $($reg:ident),* => $($rest:tt)*) => {
        define_code!(($mnemonic $($dotted)* ;) None ($($reg)*) $($rest)*);
    };
    ($mnemonic:ident $(. $dotted:ident)* $($operand:ident $(($base:ident))?),* => $($rest:tt)*) => {
        define_code!(($mnemonic $($dotted)* ;) Offset ($($($base)? $operand)*) $($rest)*);
    };
}
use code;

macro_rules! encode_operand {
    (xd, $it:expr)    => { $it << 7 };
    (xs1, $it:expr)   => { $it << 15 };
    (xs2, $it:expr)   => { $it << 20 };

    (fd, $it:expr)    => { $it << 7 };
    (fs1, $it:expr)   => { $it << 15 };
    (fs2, $it:expr)   => { $it << 20 };
    (fs12, $it:expr)  => {{ let r = $it; r << 15 | r << 20 }};

    (imm_i, $it:expr) => { ($it & 0xFFF) << 20 };            // 31:20 = imm[11:0]
    (imm_j, $it:expr) => {{
        let i = $it;
        (i >> 20 & 0x1)   << 31 |                                       // 31    = imm[20]
        (i >>  1 & 0x3FF) << 21 |                                       // 30:21 = imm[10:1]
        (i >> 11 & 0x1)   << 20 |                                       // 20    = imm[11]
        (i >> 12 & 0xFF)  << 12                                         // 19:12 = imm[19:12]
    }};
    (imm_s, $it:expr) => {{
        let i = $it;
        (i >>  5 & 0x7F)  << 25 |                                       // 31:25 = imm[11:5]
        (i       & 0x1F)  <<  7                                         // 11:7  = imm[4:0]
    }};
    // The operand is the whole PC-relative distance; the instruction after the auipc takes its
    // low 12 bits, sign-extended, so round the upper 20 bits to make up for a negative low part.
    (imm_u, $it:expr) => {{
        $it.wrapping_add(0x800) & 0xFFFF_F000                // 31:12 = imm[31:12]
    }};
}
use encode_operand;


//-------------------------------------------------------------------------------------------------

#[cfg(feature = "dogfood")]
mod display {
    #[derive(Debug, Clone, Copy)]
    pub(super) enum AddressingMode {
        None,
        Offset,
    }

    pub(super) fn format_assembly(
        mnemonic:           &str,
        mode:               AddressingMode,
        operand_strings:    &[String],
        operands:           &[u32],
    ) -> String {
        use AddressingMode::*;

        if mnemonic == "li" {
            if operands[1] == 0 { return format!("mv {}, zero", operand_strings[0]) }
            return format!("addi {}, zero, {}", operand_strings[0], operand_strings[1])
        }

        let operand_string = match (mode, operand_strings.len()) {
            (None, _)       => operand_strings.join(", "),
            (Offset, 2)     => format!("{}({})", operand_strings[1], operand_strings[0]),
            (Offset, 3)     => format!("{}, {}({})", operand_strings[0], operand_strings[2], operand_strings[1]),
            _               => panic!("internal compiler error: wrong number of operands"),
        };
        format!("{mnemonic} {operand_string}")
    }

    pub(super) fn format_imm(bits: u32, _address: i32) -> String {
        let n = bits.cast_signed();
        if (-9..=9).contains(&n) { format!("{n}") }
        else { format!("{}{:#x}", if n < 0 { "-" } else { "" }, n.unsigned_abs()) }
    }
    // auipc's operand is the whole distance, but it shows the 20-bit field it actually encodes.
    pub(super) fn format_imm_u(bits: u32, _address: i32) -> String {
        let n = bits.cast_signed();
        let field = n.cast_unsigned().wrapping_add(0x800) >> 12;
        if field <= 9 { format!("{field}") } else { format!("{field:#x}") }
    }

    // ABI names, indexed by encoding.  `encoding()` is taken modulo 32, so the index is always in
    // range.
    const X_REG_NAMES: [&str; 32] = [
        "zero", "ra",  "sp",  "gp",  "tp",  "t0",  "t1",  "t2",
        "s0",   "s1",  "a0",  "a1",  "a2",  "a3",  "a4",  "a5",
        "a6",   "a7",  "s2",  "s3",  "s4",  "s5",  "s6",  "s7",
        "s8",   "s9",  "s10", "s11", "t3",  "t4",  "t5",  "t6",
    ];
    const F_REG_NAMES: [&str; 32] = [
        "ft0",  "ft1", "ft2",  "ft3",  "ft4", "ft5", "ft6",  "ft7",
        "fs0",  "fs1", "fa0",  "fa1",  "fa2", "fa3", "fa4",  "fa5",
        "fa6",  "fa7", "fs2",  "fs3",  "fs4", "fs5", "fs6",  "fs7",
        "fs8",  "fs9", "fs10", "fs11", "ft8", "ft9", "ft10", "ft11",
    ];

    pub(super) fn format_x_reg(bits: u32, _address: i32) -> String {
        X_REG_NAMES[bits as usize % 32].into()
    }

    pub(super) fn format_f_reg(bits: u32, _address: i32) -> String {
        F_REG_NAMES[bits as usize % 32].into()
    }
}


//-------------------------------------------------------------------------------------------------
