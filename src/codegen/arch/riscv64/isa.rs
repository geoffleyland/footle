#![allow(clippy::unusual_byte_groupings, non_upper_case_globals)]

use enumset::{EnumSetType, enum_set};
use paste::paste;

use crate::codegen::{regs, Code, RegBank, RegFile, RegSet, Resolved};

#[cfg(feature = "dogfood")]
use display::*;


//-------------------------------------------------------------------------------------------------

pub static REGS: RegFile = RegFile::new(2, 1, 31,       // sp = x2, ra = x1, scratch = t6 (x31)
    RegSet::new(0x0FFC_0300_0FFC_0300),                 // s0-s11 (x8, x9, x18-27), fs0-fs11 likewise
    [
        &regs::machine_regs([10, 11, 12, 13, 14, 15, 16, 17]),      // a0-a7
        &regs::machine_regs([42, 43, 44, 45, 46, 47, 48, 49]),      // fa0-fa7
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
// The instructions!

code!(mv xd, xs1            =>  0b000000000000_00000_000_00000_0010011,     1, [U1 | U2 | U3]);
code!(fmv.d fd, fs12        => 0b0010001_00000_00000_000_00000_1010011,     1, [U1 | U2 | U3]);

code!(sd xs2, imm_s(xs1)    => 0b0000000_00000_00000_011_00000_0100011,     1, [U1 | U2 | U3]);
code!(fsd fs2, imm_s(xs1)   => 0b0000000_00000_00000_011_00000_0100111,     1, [U1 | U2 | U3]);

code!(ld xd, imm_i(xs1)     =>  0b000000000000_00000_011_00000_0000011,     1, [U1 | U2 | U3]);
code!(li xd, imm_i          =>  0b000000000000_00000_000_00000_0010011,     1, [U1 | U2 | U3]);
code!(fld fd, imm_i(xs1)    =>  0b000000000000_00000_011_00000_0000111,     1, [U1 | U2 | U3]);

code!(auipc xd, imm_u       =>    0b00000000000000000000_00000_0010111,     1, [U1 | U2 | U3]);
code!(addi xd, xs1, imm_i   =>  0b000000000000_00000_000_00000_0010011,     1, [U1 | U2 | U3]);

code!(fadd.d fd, fs1, fs2   => 0b0000001_00000_00000_111_00000_1010011,     1, [U1 | U2 | U3]);
code!(fsub.d fd, fs1, fs2   => 0b0000101_00000_00000_111_00000_1010011,     1, [U1 | U2 | U3]);
code!(fmul.d fd, fs1, fs2   => 0b0001001_00000_00000_111_00000_1010011,     1, [U1 | U2 | U3]);
code!(fdiv.d fd, fs1, fs2   => 0b0001101_00000_00000_111_00000_1010011,     1, [U1 | U2 | U3]);

code!(jal imm_j             =>    0b00000000000000000000_00001_1101111,     1, [U1 | U2 | U3]);
code!(jalr xs1              =>  0b000000000000_00000_000_00001_1100111,     1, [U1 | U2 | U3]);

code!(ret                   =>  0b000000000000_00001_000_00000_1100111,     1, [U1 | U2 | U3]);

//-------------------------------------------------------------------------------------------------

// Cover the instruction operand patterns to try to figure out the addressing mode (if there is one)
macro_rules! code {
    ($mnemonic:ident $(. $suffix:ident)* => $($rest:tt)*) => {
        _code!(None,  ($mnemonic $($suffix)*), (), $($rest)*);
    };
    ($mnemonic:ident $(. $suffix:ident)* $rd:ident, $rs1:ident => $($rest:tt)*) => {
        _code!(None,  ($mnemonic $($suffix)*), ($rd, $rs1), $($rest)*);
    };
    ($mnemonic:ident $(. $suffix:ident)* $imm:ident => $($rest:tt)*) => {
        _code!(None, ($mnemonic $($suffix)*), ($imm), $($rest)*);
    };
    ($mnemonic:ident $(. $suffix:ident)* $rd:ident, $rs1:ident, $imm:ident => $($rest:tt)*) => {
        _code!(None, ($mnemonic $($suffix)*), ($rd, $rs1, $imm), $($rest)*);
    };
    ($mnemonic:ident $(. $suffix:ident)* $imm:ident($rs1:ident) => $($rest:tt)*) => {
        _code!(Offset, ($mnemonic $($suffix)*), ($rs1, $imm), $($rest)*);
    };
    ($mnemonic:ident $(. $suffix:ident)* $rd:ident, $imm:ident($rs1:ident) => $($rest:tt)*) => {
        _code!(Offset, ($mnemonic $($suffix)*), ($rd, $rs1, $imm), $($rest)*);
    };
}
use code;


macro_rules! _code {
    (
        $addressing_mode:ident,
        ($mnemonic:ident $($suffix:ident)*),
        ($($reg:ident),* $(,)?),
        $pattern:literal,
        $latency:literal,
        [$($unit:ident)|+ $(|)?]
    ) => {
        paste!(pub static [<$mnemonic $(_ $suffix)*>]: Code = Code {
            is_call:            is_call!($mnemonic),
            is_return:          is_return!($mnemonic),
            has_output:         $( output_reg!($reg) ||)* false,

            encode: |operands: &[Resolved]| -> u32 {
                // If there's no argument (ie `ret`), _it is unused, so the _ silences a warning.
                let mut _it = operands.iter().copied();
                $pattern $(| { let value = _it.next().unwrap(); encode_operand!($reg, value) })*
            },

            latency:            $latency,
            units:              enum_set!($(Unit::$unit)|*),

            #[cfg(feature = "dogfood")]
            mnemonic:           concat!(stringify!($mnemonic) $(, ".", stringify!($suffix))*),
            #[cfg(feature = "dogfood")]
            format_assembly: |mnemonic, operands, address|
                format_assembly(mnemonic, AddressingMode::$addressing_mode, operands, address, &[$(format_operand!($reg)),*]),
            };);
    }
}
use _code;


macro_rules! encode_operand {
    (xd, $it:expr)    => { $it.reg_field(RegBank::Int) << 7 };
    (xs1, $it:expr)   => { $it.reg_field(RegBank::Int) << 15 };
    (xs2, $it:expr)   => { $it.reg_field(RegBank::Int) << 20 };

    (fd, $it:expr)    => { $it.reg_field(RegBank::Float) << 7 };
    (fs1, $it:expr)   => { $it.reg_field(RegBank::Float) << 15 };
    (fs2, $it:expr)   => { $it.reg_field(RegBank::Float) << 20 };
    (fs12, $it:expr)  => {{ let r = $it.reg_field(RegBank::Float); r << 15 | r << 20 }};

    (imm_i, $it:expr) => { ($it.imm_bits() & 0xFFF) << 20 };            // 31:20 = imm[11:0]
    (imm_j, $it:expr) => {{
        let i = $it.imm_bits();
        (i >> 20 & 0x1)   << 31 |                                       // 31    = imm[20]
        (i >>  1 & 0x3FF) << 21 |                                       // 30:21 = imm[10:1]
        (i >> 11 & 0x1)   << 20 |                                       // 20    = imm[11]
        (i >> 12 & 0xFF)  << 12                                         // 19:12 = imm[19:12]
    }};
    (imm_s, $it:expr) => {{
        let i = $it.imm_bits();
        (i >>  5 & 0x7F)  << 25 |                                       // 31:25 = imm[11:5]
        (i       & 0x1F)  <<  7                                         // 11:7  = imm[4:0]
    }};
    // The operand is the whole PC-relative distance; the instruction after the auipc takes its
    // low 12 bits, sign-extended, so round the upper 20 bits to make up for a negative low part.
    (imm_u, $it:expr) => {{
        $it.imm_bits().wrapping_add(0x800) & 0xFFFF_F000                // 31:12 = imm[31:12]
    }};
}
use encode_operand;

macro_rules! output_reg {
    (xd)                    => { true };
    (fd)                    => { true };
    ($other:tt)             => { false };
}
use output_reg;

macro_rules! is_call {
    (jal)                   => { true };
    (jalr)                  => { true };
    ($other:ident)          => { false };
}
use is_call;

macro_rules! is_return {
    (ret)                   => { true };
    ($other:ident)          => { false };
}
use is_return;


//-------------------------------------------------------------------------------------------------

#[cfg(feature = "dogfood")]
mod display {
    use crate::codegen::Resolved;

    #[derive(Debug, Clone, Copy)]
    pub(super) enum AddressingMode {
        None,
        Offset,
    }

    pub(super) fn format_assembly(
        mnemonic:           &str,
        mode:               AddressingMode,
        operands:           &[Resolved],
        address:            i32,
        formatters:         &[fn(Resolved, i32) -> String]
    ) -> String {
        use AddressingMode::*;

        if operands.is_empty() { return mnemonic.into() }

        debug_assert_eq!(operands.len(), formatters.len());

        let operand_strings: Vec<_> = formatters.iter().zip(operands)
            .map(|(f, &v)| f(v, address))
            .collect();

        if mnemonic == "li" {
            if operands[1].imm() == 0 { return format!("mv {}, zero", operand_strings[0]) }
            return format!("addi {}, zero, {}", operand_strings[0], operand_strings[1])
        }

        let operand_string = match (mode, operands.len()) {
            (None, _)       => operand_strings.join(", "),
            (Offset, 2)     => format!("{}({})", operand_strings[1], operand_strings[0]),
            (Offset, 3)     => format!("{}, {}({})", operand_strings[0], operand_strings[2], operand_strings[1]),
            _               => panic!("internal compiler error: wrong number of operands"),
        };
        format!("{mnemonic} {operand_string}")
    }

    // format_operant gets called to fill the format_assembly field in _code!
    macro_rules! format_operand {
        (xd)    => { format_x_reg };
        (xs1)   => { format_x_reg };
        (xs2)   => { format_x_reg };

        (fd)    => { format_f_reg };
        (fs1)   => { format_f_reg };
        (fs2)   => { format_f_reg };
        (fs12)  => { format_f_reg };

        (imm_i) => { format_imm };
        (imm_j) => { format_imm };
        (imm_s) => { format_imm };
        (imm_u) => { format_imm_u };
    }
    pub(super) use format_operand;

    pub(super) fn format_imm(imm: Resolved, _address: i32) -> String {
        let n = imm.imm();
        if (-9..=9).contains(&n) { format!("{n}") }
        else { format!("{}{:#x}", if n < 0 { "-" } else { "" }, n.unsigned_abs()) }
    }
    // auipc's operand is the whole distance, but it shows the 20-bit field it actually encodes.
    pub(super) fn format_imm_u(imm: Resolved, _address: i32) -> String {
        let n = imm.imm();
        let field = n.cast_unsigned().wrapping_add(0x800) >> 12;
        if field <= 9 { format!("{field}") } else { format!("{field:#x}") }
    }

    pub(super) fn format_x_reg(reg: Resolved, _address: i32) -> String {
        match reg.reg().encoding() {
             0 => "zero",  1 => "ra",   2 => "sp",   3 => "gp",
             4 => "tp",    5 => "t0",   6 => "t1",   7 => "t2",
             8 => "s0",    9 => "s1",  10 => "a0",  11 => "a1",
            12 => "a2",   13 => "a3",  14 => "a4",  15 => "a5",
            16 => "a6",   17 => "a7",  18 => "s2",  19 => "s3",
            20 => "s4",   21 => "s5",  22 => "s6",  23 => "s7",
            24 => "s8",   25 => "s9",  26 => "s10", 27 => "s11",
            28 => "t3",   29 => "t4",  30 => "t5",  31 => "t6",
            n  => unreachable!("internal compiler error: integer register {n}"),
        }.into()
    }

    pub(super) fn format_f_reg(reg: Resolved, _address: i32) -> String {
        match reg.reg().encoding() {
             0 => "ft0",   1 => "ft1",   2 => "ft2",   3 => "ft3",
             4 => "ft4",   5 => "ft5",   6 => "ft6",   7 => "ft7",
             8 => "fs0",   9 => "fs1",  10 => "fa0",  11 => "fa1",
            12 => "fa2",  13 => "fa3",  14 => "fa4",  15 => "fa5",
            16 => "fa6",  17 => "fa7",  18 => "fs2",  19 => "fs3",
            20 => "fs4",  21 => "fs5",  22 => "fs6",  23 => "fs7",
            24 => "fs8",  25 => "fs9",  26 => "fs10", 27 => "fs11",
            28 => "ft8",  29 => "ft9",  30 => "ft10", 31 => "ft11",
            n  => unreachable!("internal compiler error: float register {n}"),
        }.into()
    }
}


//-------------------------------------------------------------------------------------------------
