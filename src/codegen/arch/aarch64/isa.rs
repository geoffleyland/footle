#![allow(clippy::unusual_byte_groupings, non_upper_case_globals)]

use enumset::EnumSetType;

use crate::codegen::{operand_class, define_code, regs, RegBank, RegFile, RegSet};
#[cfg(feature = "dogfood")]
use display::*;


//-------------------------------------------------------------------------------------------------

pub static REGS: RegFile = RegFile::new(31, 30, 16,
    RegSet::new(0x0000_FF00_1FF8_0000),
    [
        regs::machine_regs([ 0,  1,  2,  3,  4,  5,  6,  7]),
        regs::machine_regs([32, 33, 34, 35, 36, 37, 38, 39]),
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
// Operand classes  used for defining instructions

operand_class!(xd       Output  format_x_reg);
operand_class!(xt       Output  format_x_reg);
operand_class!(xt1      Output  format_x_reg);
operand_class!(xt2              format_x_reg);
operand_class!(xn               format_x_reg);
operand_class!(xm               format_x_reg);

operand_class!(dd       Output  format_d_reg);
operand_class!(dt       Output  format_d_reg);
operand_class!(dt1      Output  format_d_reg);
operand_class!(dt2              format_d_reg);
operand_class!(dn               format_d_reg);
operand_class!(dm               format_d_reg);

operand_class!(imm7             format_imm);
operand_class!(imm9             format_imm);
operand_class!(imm12            format_imm);
operand_class!(imm16            format_imm);
operand_class!(imm19            format_address);
operand_class!(imm26            format_address);


//-------------------------------------------------------------------------------------------------
// The instructions!

code!(fadd dd, dn, dm               => 0b0_0_0_11110_01_1_00000_0010_10_00000_00000             ; 1 [FP11 | FP12 | FP13 | FP14]);
code!(fsub dd, dn, dm               => 0b0_0_0_11110_01_1_00000_0011_10_00000_00000             ; 1 [FP11 | FP12 | FP13 | FP14]);
code!(fmul dd, dn, dm               => 0b0_0_0_11110_01_1_00000_0000_10_00000_00000             ; 4 [FP11 | FP12 | FP13 | FP14]);
code!(fdiv dd, dn, dm               => 0b0_0_0_11110_01_1_00000_0001_10_00000_00000             ;10 [FP11 | FP12 | FP13 | FP14]);

code!(fmov dd, dn                   => 0b0_0_0_11110_01_1_000000_10000_00000_00000              ; 2 [FP11 | FP12 | FP13 | FP14]);

// TODO! mov doesn't actually use a unit and has no latency.
code!(mov xd, xm                    => 0b1_01_01010_00_0_00000_000000_11111_00000               ; 1 [LS8 | L9 | L10]);
code!(mov xd, #imm16                => 0b1_10_100101_00_0000000000000000_00000                  ; 1 [LS8 | L9 | L10]);

code!(ldr xd, imm19                 => 0b01_011_0_00_0000000000000000000_00000                  ;10 [LS8 | L9 | L10]);
code!(ldr dd, imm19                 => 0b01_011_1_00_0000000000000000000_00000                  ;10 [LS8 | L9 | L10]);

code!(ldr xt, [xn, #imm12]          => 0b11_111_0_01_01_000000000000_00000_00000                ;10 [LS8 | L9 | L10]);
code!(ldr xt, [xn], #imm9           => 0b11_111_0_00_01_0_000000000_01_00000_00000              ;10 [LS8 | L9 | L10]);
code!(ldr dt, [xn, #imm12]          => 0b11_111_1_01_01_000000000000_00000_00000                ;10 [LS8 | L9 | L10]);
code!(ldr dt, [xn], #imm9           => 0b11_111_1_00_01_0_000000000_01_00000_00000              ;10 [LS8 | L9 | L10]);

code!(ldp xt1, xt2, [xn], #imm7     => 0b10_101_0_001_1_0000000_00000_00000_00000               ;10 [LS8 | L9 | L10]);
code!(ldp dt1, dt2, [xn], #imm7     => 0b01_101_1_001_1_0000000_00000_00000_00000               ;10 [LS8 | L9 | L10]);

code!(str xt, [xn, #imm12]          => 0b11_111_0_01_00_000000000000_00000_00000        Store   ;10 [LS8 | L9 | L10]);
code!(str xt, [xn, #imm9]!          => 0b11_111_0_00_00_0_000000000_11_00000_00000      Store   ;10 [LS8 | L9 | L10]);
code!(str dt, [xn, #imm12]          => 0b11_111_1_01_00_000000000000_00000_00000        Store   ;10 [LS8 | L9 | L10]);
code!(str dt, [xn, #imm9]!          => 0b11_111_1_00_00_0_000000000_11_00000_00000      Store   ;10 [LS8 | L9 | L10]);

code!(stp xt1, xt2, [xn, #imm7]!    => 0b10_101_0_011_0_0000000_00000_00000_00000       Store   ;10 [LS8 | L9 | L10]);
code!(stp dt1, dt2, [xn, #imm7]!    => 0b01_101_1_011_0_0000000_00000_00000_00000       Store   ;10 [LS8 | L9 | L10]);

code!(bl imm26                      => 0b1_00_101_00000000000000000000000000            Call    ; 1 [LS8 | L9 | L10]);
code!(blr xn                        => 0b110_101_1_0_0_01_11111_0000_0_0_00000_00000    Call    ; 1 [LS8 | L9 | L10]);
code!(ret                           => 0b110_101_1_0_0_10_11111_0000_0_0_11110_00000    Return  ; 1 [LS8 | L9 | L10]);

//-------------------------------------------------------------------------------------------------

// Cover the instruction operand patterns to try to figure out the addressing mode (if there is one)
macro_rules! code {
    ($mnemonic:ident => $($rest:tt)*) => {
        code!(@reg_bank nothing ($mnemonic;) None () $($rest)*);
    };
    ($mnemonic:ident $rd:ident, #$imm:ident => $($rest:tt)*) => {
        code!(@reg_bank $rd ($mnemonic; imm) None ($rd $imm) $($rest)*);
    };
    ($mnemonic:ident $rd:ident, imm19 => $($rest:tt)*) => {
        code!(@reg_bank $rd ($mnemonic; literal) None ($rd imm19) $($rest)*);
    };
    ($mnemonic:ident $rt:ident $(, $operands:ident)* => $($rest:tt)*) => {
        code!(@reg_bank $rt ($mnemonic;) None ($rt $($operands)*) $($rest)*);
    };
    ($mnemonic:ident $rt1:ident, $($rt2:ident,)? [$xn:ident, # $imm:ident]! => $($rest:tt)*) => {
        code!(@reg_bank $rt1 ($mnemonic; pre) Pre ($rt1 $($rt2)? $xn $imm) $($rest)*);
    };
    ($mnemonic:ident $rt1:ident, $($rt2:ident,)? [$xn:ident], # $imm:ident => $($rest:tt)*) => {
        code!(@reg_bank $rt1 ($mnemonic; post) Post ($rt1 $($rt2)? $xn $imm) $($rest)*);
    };
    ($mnemonic:ident $rt1:ident, $($rt2:ident,)? [$xn:ident, # $imm:ident] => $($rest:tt)*) => {
        code!(@reg_bank $rt1 ($mnemonic; offset) Offset ($rt1 $($rt2)? $xn $imm) $($rest)*);
    };

    (@reg_bank xd     $($rest:tt)*)   => { code!(@insert_reg_bank x $($rest)*); };
    (@reg_bank xt     $($rest:tt)*)   => { code!(@insert_reg_bank x $($rest)*); };
    (@reg_bank xt1    $($rest:tt)*)   => { code!(@insert_reg_bank x $($rest)*); };
    (@reg_bank dd     $($rest:tt)*)   => { code!(@insert_reg_bank d $($rest)*); };
    (@reg_bank dt     $($rest:tt)*)   => { code!(@insert_reg_bank d $($rest)*); };
    (@reg_bank dt1    $($rest:tt)*)   => { code!(@insert_reg_bank d $($rest)*); };
    (@reg_bank $i:ident $($rest:tt)*) => { define_code!($($rest)*); };

    (@insert_reg_bank $bank:ident ($mnemonic:ident $($dotted:ident)*; $($suffixes:ident)*) $($rest:tt)*) => {
        define_code!(($mnemonic $($dotted)*; $bank $($suffixes)*) $($rest)*);
    };
}
use code;

macro_rules! encode_operand {
    (xd, $it:expr)    => { $it.reg_field(RegBank::Int) << 0 };
    (xn, $it:expr)    => { $it.reg_field(RegBank::Int) << 5 };
    (xm, $it:expr)    => { $it.reg_field(RegBank::Int) << 16 };
    (xt, $it:expr)    => { $it.reg_field(RegBank::Int) << 0 };
    (xt1, $it:expr)   => { $it.reg_field(RegBank::Int) << 0 };
    (xt2, $it:expr)   => { $it.reg_field(RegBank::Int) << 10 };

    (dd, $it:expr)    => { $it.reg_field(RegBank::Float) << 0 };
    (dn, $it:expr)    => { $it.reg_field(RegBank::Float) << 5 };
    (dm, $it:expr)    => { $it.reg_field(RegBank::Float) << 16 };
    (dt, $it:expr)    => { $it.reg_field(RegBank::Float) << 0 };
    (dt1, $it:expr)   => { $it.reg_field(RegBank::Float) << 0 };
    (dt2, $it:expr)   => { $it.reg_field(RegBank::Float) << 10 };

    (imm7, $it:expr)  => { (($it.imm_bits() >> 3) & 0x7F) << 15 };
    (imm9, $it:expr)  => { ($it.imm_bits() & 0x01FF) << 12 };
    (imm12, $it:expr) => { (($it.imm_bits() >> 3) & 0x0FFF) << 10 };
    (imm16, $it:expr) => { ($it.imm_bits() & 0xFFFF) << 5 };
    (imm19, $it:expr) => { (($it.imm_bits() >> 2) & 0x7_FFFF) << 5 };
    (imm26, $it:expr) => { ($it.imm_bits() >> 2) & 0x03FF_FFFF };
}
use encode_operand;


//-------------------------------------------------------------------------------------------------

#[cfg(feature = "dogfood")]
mod display {
    use crate::codegen::Resolved;

    #[derive(Debug, Clone, Copy)]
    pub(super) enum AddressingMode {
        None,
        Offset,
        Pre,
        Post,
    }

    pub(super) fn format_assembly(
        mnemonic:           &str,
        mode:               AddressingMode,
        operands:           &[Resolved],
        address:            i32,
        formatters:         &[fn(Resolved, i32) -> String]
    ) -> String {
        use AddressingMode::*;
        if operands.is_empty() { return mnemonic.to_string() }

        debug_assert_eq!(operands.len(), formatters.len());
        let operand_strings: Vec<_> = formatters.iter().zip(operands)
            .map(|(f, &v)| f(v, address))
            .collect();

        let operand_string = match (mode, operand_strings.as_slice()) {
            (Offset,    [head @ .., base, _]) if operands.last() == Some(&Resolved::Imm(0))
                                                    => format!("{}, [{base}]", head.join(", ")),
            (Offset,    [head @ .., base, offset])  => format!("{}, [{base}, {offset}]", head.join(", ")),
            (Pre,       [head @ .., base, offset])  => format!("{}, [{base}, {offset}]!", head.join(", ")),
            (Post,      [head @ .., base, offset])  => format!("{}, [{base}], {offset}", head.join(", ")),
            (None,      all)                        => all.join(", "),
            _ => unreachable!("internal compiler error: memory operand without a base and offset"),
        };
        format!("{mnemonic} {operand_string}")
    }

    pub(super) fn format_x_reg(reg: Resolved, _address: i32) -> String {
        match reg.reg().encoding() {
            31 => "sp".into(),
            n  => format!("x{n}"),
        }
    }
    pub(super) fn format_d_reg(reg: Resolved, _address: i32) -> String {
        format!("d{}", reg.reg().encoding())
    }
    pub(super) fn format_imm(imm: Resolved, _address: i32) -> String {
        let n = imm.imm();
        if (-9..=9).contains(&n) { format!("#{n}") }
        else { format!("#{}{:#x}", if n < 0 { "-" } else { "" }, n.unsigned_abs()) }
    }
    pub(super) fn format_address(addr: Resolved, address: i32) -> String {
        format!("#{:#x}", address + addr.imm())
    }
}


//-------------------------------------------------------------------------------------------------
