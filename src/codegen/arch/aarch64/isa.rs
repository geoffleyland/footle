#![allow(clippy::unusual_byte_groupings, non_upper_case_globals)]

use enumset::{EnumSet, EnumSetType, enum_set};
use paste::paste;

use crate::codegen::{regs, RegFile, RegSet, MachineReg, RankSet};

#[cfg(feature = "dogfood")]
use display::*;


//-------------------------------------------------------------------------------------------------

pub static REGS: RegFile = RegFile::new(31, 30, 16,
    RegSet::new(0x0000_FF00_1FF8_0000),
    [
        &regs::machine_regs([ 0,  1,  2,  3,  4,  5,  6,  7]),
        &regs::machine_regs([32, 33, 34, 35, 36, 37, 38, 39]),
    ],
    &[
         9, 10, 11, 12, 13, 14, 15,                 // caller-saved temps (x16-18 excluded)
        19, 20, 21, 22, 23, 24, 25, 26, 27, 28,     // callee-saved
         0,  1,  2,  3,  4,  5,  6,  7,             // argument registers
        48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, // d16-d31 (caller saved)
        40, 41, 42, 43, 44, 45, 46, 47,                                 // d8-d15 (callee saved)
        32, 33, 34, 35, 36, 37, 38, 39,                                 // d0-d7 (function args)
    ],
);


//-------------------------------------------------------------------------------------------------
// Architecture details.

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


//-------------------------------------------------------------------------------------------------
// Instruction encoding structures and macros.

#[derive(Debug)]
pub struct Code {
    encode:                     fn(&[u32]) -> u32,
    latency:                    u8,
    has_output:                 bool,
    units:                      EnumSet<Unit>,

    #[cfg(any(feature = "dogfood", test))]
    mnemonic:                   &'static str,
    #[cfg(feature = "dogfood")]
    format:                     fn(&[i32], i32) -> String,
}


impl Code {
    pub fn has_output(&self) -> bool        { self.has_output }

    pub fn latency(&self) -> u8             { self.latency }
    pub fn encode(&self, args: &[u32])-> u32{ (self.encode)(args) }

    pub fn clobbers_anything(&self) -> bool { self.is_call() }
    pub fn clobbers(&self, reg: MachineReg) -> bool {
        self.is_call() && !REGS.callee_saved.contains(reg)
    }

    pub fn clobbered_ranks(&self) -> RankSet {
        if self.is_call() { REGS.clobbered_ranks } else { RankSet::EMPTY }
    }

    pub fn is_return(&self) -> bool  { std::ptr::eq(self, &raw const ret) }
    pub fn is_call(&self) -> bool {
        std::ptr::eq(self, &raw const bl) || std::ptr::eq(self, &raw const blr)
    }

    pub fn try_pick_unit(&self, free_units: EnumSet<Unit>) -> Option<Unit> {
        (self.units & free_units).iter().next()
    }

    #[cfg(any(feature = "dogfood", test))]
    pub fn mnemonic(&self) -> &str      { self.mnemonic }

    #[cfg(feature = "dogfood")]
    pub fn format(&self, operands: &[i32], address: i32) -> String {
        (self.format)(operands, address)
    }
}


macro_rules! encode_operand {
    (dd, $it:expr)    => { $it };
    (dn, $it:expr)    => { $it << 5 };
    (dm, $it:expr)    => { $it << 16 };
    (da, $it:expr)    => { $it << 10 };
    (dt, $it:expr)    => { $it };
    (dt1, $it:expr)   => { $it };
    (dt2, $it:expr)   => { $it << 10 };
    (xd, $it:expr)    => { $it };
    (xn, $it:expr)    => { $it << 5 };
    (xm, $it:expr)    => { $it << 16 };
    (xa, $it:expr)    => { $it << 10 };
    (xt, $it:expr)    => { $it };
    (xt1, $it:expr)   => { $it };
    (xt2, $it:expr)   => { $it << 10 };
    (imm7, $it:expr)  => { (($it >> 3) & 0x7F) << 15 };
    (imm9, $it:expr)  => { ($it & 0x01FF) << 12 };
    (imm12, $it:expr) => { (($it >> 3) & 0x0FFF) << 10 };
    (imm16, $it:expr) => { ($it & 0xFFFF) << 5 };
    (imm19, $it:expr) => { (($it >> 2) & 0x7_FFFF) << 5 };
    (imm26, $it:expr) => { ($it >> 2) & 0x03FF_FFFF };
}

macro_rules! output_reg {
    (dd)    => { true };
    (dt)    => { true };
    (dt1)   => { true };
    (xd)    => { true };
    (xt)    => { true };
    (xt1)   => { true };
    ($other:tt) => { false };
}

macro_rules! has_output {
    (str, $($reg:ident),*)  => { false };
    (stp, $($reg:ident),*)  => { false };
    ($mnemonic:ident, $($reg:ident),*) => { $( output_reg!($reg) ||)* false };
}

// Cover the instruction operand patterns to try to figure out the addressing mode (if there is one)
macro_rules! code {
    ($mnemonic:ident => $($rest:tt)*) => {
        find_reg_bank!(nothing, None, $mnemonic, (), $($rest)*);
    };
    ($mnemonic:ident $rd:ident, #$imm:ident => $($rest:tt)*) => {
        find_reg_bank!($rd, @mode_suffix:_imm, None, $mnemonic, ($rd, $imm), $($rest)*);
    };
    ($mnemonic:ident $rd:ident, imm19 => $($rest:tt)*) => {
        find_reg_bank!($rd, @mode_suffix:_literal, None, $mnemonic, ($rd, imm19), $($rest)*);
    };
    ($mnemonic:ident $($rt:ident)? $(, $operands:ident)* => $($rest:tt)*) => {
        find_reg_bank!($($rt,)? None, $mnemonic, ($($rt,)? $($operands),*), $($rest)*);
    };
    ($mnemonic:ident $rt1:ident, $($rt2:ident,)? [$xn:ident, # $imm:ident]! => $($rest:tt)*) => {
        find_reg_bank!($rt1, @mode_suffix:_pre, Pre, $mnemonic, ($rt1, $($rt2,)? $xn, $imm), $($rest)*);
    };
    ($mnemonic:ident $rt1:ident, $($rt2:ident,)? [$xn:ident], # $imm:ident => $($rest:tt)*) => {
        find_reg_bank!($rt1, @mode_suffix:_post, Post, $mnemonic, ($rt1, $($rt2,)? $xn, $imm), $($rest)*);
    };
    ($mnemonic:ident $rt1:ident, $($rt2:ident,)? [$xn:ident, # $imm:ident] => $($rest:tt)*) => {
        find_reg_bank!($rt1, @mode_suffix:_offset, Offset, $mnemonic, ($rt1, $($rt2,)? $xn, $imm), $($rest)*);
    };
}


// Try to figure out if our destination is a x or d register.
macro_rules! find_reg_bank {
    (xd,    $($rest:tt)*)   => { _code!(@reg_bank:_x, $($rest)*); };
    (xt,    $($rest:tt)*)   => { _code!(@reg_bank:_x, $($rest)*); };
    (xt1,   $($rest:tt)*)   => { _code!(@reg_bank:_x, $($rest)*); };
    (dd,    $($rest:tt)*)   => { _code!(@reg_bank:_d, $($rest)*); };
    (dt,    $($rest:tt)*)   => { _code!(@reg_bank:_d, $($rest)*); };
    (dt1,   $($rest:tt)*)   => { _code!(@reg_bank:_d, $($rest)*); };
    ($other:ident, $($rest:tt)*) => { _code!($($rest)*); };
}


macro_rules! _code {
    (
        $(@reg_bank:$reg_bank:ident,)?
        $(@mode_suffix:$mode_suffix:ident,)?
        $addressing_mode:ident,
        $mnemonic:ident,
        ($($reg:ident),* $(,)?),
        $pattern:literal,
        $latency:literal,
        [$($unit:ident)|+ $(|)?]
    ) => {
        paste!(pub static [<$mnemonic $($reg_bank)? $($mode_suffix)?>]: Code = Code {
            has_output:         $( has_output!($mnemonic, $reg) ||)* false,
            latency:            $latency,
            units:              enum_set!($(Unit::$unit)|*),
            encode: |operands: &[u32]| -> u32 {
                // If there's no argument (ie `ret`), _it is unused, so the _ silences a warning.
                let mut _it = operands.iter().copied();
                $pattern $(| { let value = _it.next().unwrap(); encode_operand!($reg, value) })*
            },

            #[cfg(any(feature = "dogfood", test))]
            mnemonic:           stringify!($mnemonic),
            #[cfg(feature = "dogfood")]
            format: |operands, address|
                format_operands(AddressingMode::$addressing_mode, operands, address, &[$(format_operand!($reg)),*]),
            };);
    }
}


//-------------------------------------------------------------------------------------------------
// The instructions!

code!(fadd dd, dn, dm               => 0b0_0_0_11110_01_1_00000_0010_10_00000_00000,    1, [FP11 | FP12 | FP13 | FP14]);
code!(fsub dd, dn, dm               => 0b0_0_0_11110_01_1_00000_0011_10_00000_00000,    1, [FP11 | FP12 | FP13 | FP14]);
code!(fmul dd, dn, dm               => 0b0_0_0_11110_01_1_00000_0000_10_00000_00000,    4, [FP11 | FP12 | FP13 | FP14]);
code!(fdiv dd, dn, dm               => 0b0_0_0_11110_01_1_00000_0001_10_00000_00000,   10, [FP11 | FP12 | FP13 | FP14]);
code!(fmsub dd, dn, dm, da          => 0b0_0_0_11111_01_0_00000_1_00000_00000_00000,    4, [FP11 | FP12 | FP13 | FP14]);
code!(frintz dd, dn                 => 0b0_0_0_11110_01_1_001011_10000_00000_00000,     3, [FP11 | FP12 | FP13 | FP14]);

code!(fmov dd, dn                   => 0b0_0_0_11110_01_1_000000_10000_00000_00000,     2, [FP11 | FP12 | FP13 | FP14]);

// TODO! mov doesn't actually use a unit and has no latency.
code!(mov xd, xm                    => 0b1_01_01010_00_0_00000_000000_11111_00000,      1, [LS8 | L9 | L10]);
code!(mov xd, #imm16                => 0b1_10_100101_00_0000000000000000_00000,         1, [LS8 | L9 | L10]);

code!(ldr xd, imm19                 => 0b01_011_0_00_0000000000000000000_00000,        10, [LS8 | L9 | L10]);
code!(ldr dd, imm19                 => 0b01_011_1_00_0000000000000000000_00000,        10, [LS8 | L9 | L10]);

code!(ldr xt, [xn, #imm12]          => 0b11_111_0_01_01_000000000000_00000_00000,      10, [LS8 | L9 | L10]);
code!(ldr xt, [xn], #imm9           => 0b11_111_0_00_01_0_000000000_01_00000_00000,    10, [LS8 | L9 | L10]);
code!(ldr dt, [xn, #imm12]          => 0b11_111_1_01_01_000000000000_00000_00000,      10, [LS8 | L9 | L10]);
code!(ldr dt, [xn], #imm9           => 0b11_111_1_00_01_0_000000000_01_00000_00000,    10, [LS8 | L9 | L10]);

code!(ldp xt1, xt2, [xn], #imm7     => 0b10_101_0_001_1_0000000_00000_00000_00000,     10, [LS8 | L9 | L10]);
code!(ldp dt1, dt2, [xn], #imm7     => 0b01_101_1_001_1_0000000_00000_00000_00000,     10, [LS8 | L9 | L10]);

code!(str xt, [xn, #imm12]          => 0b11_111_0_01_00_000000000000_00000_00000,      10, [LS8 | L9 | L10]);
code!(str xt, [xn, #imm9]!          => 0b11_111_0_00_00_0_000000000_11_00000_00000,    10, [LS8 | L9 | L10]);
code!(str dt, [xn, #imm12]          => 0b11_111_1_01_00_000000000000_00000_00000,      10, [LS8 | L9 | L10]);
code!(str dt, [xn, #imm9]!          => 0b11_111_1_00_00_0_000000000_11_00000_00000,    10, [LS8 | L9 | L10]);

code!(stp xt1, xt2, [xn, #imm7]!    => 0b10_101_0_011_0_0000000_00000_00000_00000,     10, [LS8 | L9 | L10]);
code!(stp dt1, dt2, [xn, #imm7]!    => 0b01_101_1_011_0_0000000_00000_00000_00000,     10, [LS8 | L9 | L10]);

code!(bl imm26                      => 0b1_00_101_00000000000000000000000000,           1, [LS8 | L9 | L10]);
code!(blr xn                        => 0b110_101_1_0_0_01_11111_0000_0_0_00000_00000,   1, [LS8 | L9 | L10]);
code!(ret                           => 0b110_101_1_0_0_10_11111_0000_0_0_11110_00000,   1, [LS8 | L9 | L10]);

//-------------------------------------------------------------------------------------------------

#[cfg(feature = "dogfood")]
mod display {
    #[derive(Debug, Clone, Copy)]
    pub(super) enum AddressingMode {
        None,
        Pre,
        Post,
        Offset,
    }

    pub(super) fn format_operands(
        addressing_mode:    AddressingMode,
        operands:           &[i32],
        address:            i32,
        formatters:         &[fn(i32, i32, AddressingMode) -> String]
    ) -> String {
        debug_assert_eq!(operands.len(), formatters.len());
        formatters.iter().zip(operands)
            .map(|(f, &v)| f(v, address, addressing_mode))
            .fold(String::new(), |mut acc, piece| {
                if !acc.is_empty() && !piece.starts_with(']') {
                    acc.push_str(", ");
                }
                acc.push_str(&piece);
                acc
            })
    }

    macro_rules! format_operand {
        (dd)    => { format_d_reg };
        (dn)    => { format_d_reg };
        (dm)    => { format_d_reg };
        (da)    => { format_d_reg };
        (dt)    => { format_d_reg };
        (dt1)   => { format_d_reg };
        (dt2)   => { format_d_reg };
        (xd)    => { format_x_reg };
        (xn)    => { format_xn };
        (xm)    => { format_x_reg };
        (xa)    => { format_x_reg };
        (xt)    => { format_x_reg };
        (xt1)   => { format_x_reg };
        (xt2)   => { format_x_reg };
        (imm7)  => { format_imm };
        (imm9)  => { format_imm };
        (imm12) => { format_imm12 };
        (imm16) => { format_imm };
        (imm19) => { format_address };
        (imm26) => { format_address };
    }
    pub(super) use format_operand;


    pub(super) fn format_xn(n: i32, _address: i32, mode: AddressingMode) -> String {
        let reg = x_reg(n);
        match mode {
            AddressingMode::Pre | AddressingMode::Offset => format!("[{reg}"),
            AddressingMode::Post => format!("[{reg}]"),
            AddressingMode::None => reg
        }
    }
    pub(super) fn format_d_reg(n: i32, _address: i32, _mode: AddressingMode) -> String { format!("d{n}") }
    pub(super) fn format_x_reg(n: i32, _address: i32, _mode: AddressingMode) -> String  { x_reg(n) }
    pub(super) fn format_address(n: i32, address: i32, _mode: AddressingMode) -> String  { format!("#{:#x}", address + n) }
    pub(super) fn format_imm(n: i32, _address: i32, mode: AddressingMode) -> String
    {
        let offset = if n > -10 && n < 10 { format!("#{n}")}
            else if n < 0 { format!("#-{:#x}", -n) }
            else { format!("#{n:#x}") };
        match mode {
            AddressingMode::Pre => format!("{offset}]!"),
            _ => offset
        }
    }
    pub(super) fn format_imm12(n: i32, _address: i32, _mode: AddressingMode) -> String {
        if n == 0                   { "]".to_string() }
        else if n > -10 && n < 10   { format!("#{n}]")}
        else if n < 0               { format!("#-{:#x}]", -n) }
        else                        { format!("#{n:#x}]") }
    }

    pub(super) fn x_reg(n: i32) -> String {
        match n {
            31 => "sp".into(),
            n  => format!("x{n}"),
        }
    }
}


//-------------------------------------------------------------------------------------------------
