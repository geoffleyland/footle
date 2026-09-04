#![allow(clippy::unusual_byte_groupings, non_upper_case_globals)]

use enumset::{EnumSet, EnumSetType, enum_set};
use paste::paste;

#[derive(Debug, EnumSetType)]
pub(super) enum Unit {
    LS8,
    L9,
    L10,
    FP11,
    FP12,
    FP13,
    FP14,
}

#[derive(Debug, Clone, Copy)]
pub(super) enum AddressingMode {
    None,
    Pre,
    Post,
    Offset,
}


pub(super) const STACK_REG: u8 = 31;
pub(super) const LINK_REG: u8 = 30;

#[derive(Debug)]
pub(super) struct Code {
    name:                       &'static str,
    pub(super) encode:          fn(&[u32]) -> u32,
    pub(super) latency:         u8,
    has_output:                 bool,
    units:                      EnumSet<Unit>,

    pub(super) format:          fn(&[i32], i32) -> String,
}


impl Code {
    pub fn name(&self) -> &str          { self.name }
    pub fn has_output(&self) -> bool    { self.has_output }
    pub fn clobbers(&self) -> u32       { if self.save_link_reg() { 0xFFFF_00FF} else { 0 }}
    pub fn restore_regs(&self) -> bool  { std::ptr::eq(self, &raw const ret) }
    pub fn save_link_reg(&self) -> bool {
        std::ptr::eq(self, &raw const bl) || std::ptr::eq(self, &raw const blr)
    }
    pub fn try_pick_unit(&self, free_units: EnumSet<Unit>) -> Option<Unit> {
        (self.units & free_units).iter().next()
    }
}


fn format_operands(
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
    (imm19) => { format_address };
    (imm26) => { format_address };
}


fn format_xn(n: i32, _address: i32, mode: AddressingMode) -> String {
    let reg = x_reg(n);
    match mode {
        AddressingMode::Pre | AddressingMode::Offset => format!("[{reg}"),
        AddressingMode::Post => format!("[{reg}]"),
        AddressingMode::None => reg
    }
}
fn format_d_reg(n: i32, _address: i32, _mode: AddressingMode) -> String { format!("d{n}") }
fn format_x_reg(n: i32, _address: i32, _mode: AddressingMode) -> String  { x_reg(n) }
fn format_address(n: i32, address: i32, _mode: AddressingMode) -> String  { format!("#{:#x}", address + n) }
fn format_imm(n: i32, _address: i32, mode: AddressingMode) -> String
{
    let offset = if n > -10 && n < 10 { format!("#{n}")}
        else if n < 0 { format!("#-{:#x}", -n) }
        else { format!("#{n:#x}") };
    match mode {
        AddressingMode::Pre => format!("{offset}]!"),
        _ => offset
    }
}
fn format_imm12(n: i32, _address: i32, _mode: AddressingMode) -> String {
    if n == 0                   { "]".to_string() }
    else if n > -10 && n < 10   { format!("#{n}]")}
    else if n < 0               { format!("#-{:#x}]", -n) }
    else                        { format!("#{n:#x}]") }
}

fn x_reg(n: i32) -> String {
    match n {
        31 => "sp".into(),
        n  => format!("x{n}"),
    }
}


macro_rules! reg {
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
    (imm19, $it:expr) => { (($it >> 2) & 0x7_FFFF) << 5 };
    (imm26, $it:expr) => { ($it >> 2) & 0x03FF_FFFF };
}

macro_rules! output_reg {
    (dd) => { true };
    (dt) => { true };
    (dt1) => { true };
    (xd) => { true };
    (xt) => { true };
    (xt1) => { true };
    ($other:tt) => { false };
}

macro_rules! code {
    ($name:ident => $($rest:tt)*) => {
        find_reg_bank!(nothing, None, $name, (), $($rest)*);
    };
    ($name:ident $rd:ident, imm19 => $($rest:tt)*) => {
        find_reg_bank!($rd, @mode_suffix:_literal, None, $name, ($rd, imm19), $($rest)*);
    };
    ($name:ident $($rt:ident)? $(, $operands:ident)* => $($rest:tt)*) => {
        find_reg_bank!($($rt,)? None, $name, ($($rt,)? $($operands),*), $($rest)*);
    };
    ($name:ident $rt1:ident, $($rt2:ident,)? [$xn:ident, # $imm:ident]! => $($rest:tt)*) => {
        find_reg_bank!($rt1, @mode_suffix:_pre, Pre, $name, ($rt1, $($rt2,)? $xn, $imm), $($rest)*);
    };
    ($name:ident $rt1:ident, $($rt2:ident,)? [$xn:ident], # $imm:ident => $($rest:tt)*) => {
        find_reg_bank!($rt1, @mode_suffix:_post, Post, $name, ($rt1, $($rt2,)? $xn, $imm), $($rest)*);
    };
    ($name:ident $rt1:ident, $($rt2:ident,)? [$xn:ident, # $imm:ident] => $($rest:tt)*) => {
        find_reg_bank!($rt1, @mode_suffix:_offset, Offset, $name, ($rt1, $($rt2,)? $xn, $imm), $($rest)*);
    };
}


macro_rules! find_reg_bank {
    (xd, $($rest:tt)*) => { _code!(@reg_bank:_x, $($rest)*); };
    (xt, $($rest:tt)*) => { _code!(@reg_bank:_x, $($rest)*); };
    (xt1, $($rest:tt)*) => { _code!(@reg_bank:_x, $($rest)*); };
    (dd, $($rest:tt)*) => { _code!(@reg_bank:_d, $($rest)*); };
    (dt, $($rest:tt)*) => { _code!(@reg_bank:_d, $($rest)*); };
    (dt1, $($rest:tt)*) => { _code!(@reg_bank:_d, $($rest)*); };
    ($other:ident, $($rest:tt)*) => { _code!($($rest)*); };
}


macro_rules! _code {
    (
        $(@reg_bank:$reg_bank:ident,)?
        $(@mode_suffix:$mode_suffix:ident,)?
        $addressing_mode:ident,
        $name:ident,
        ($($reg:ident),* $(,)?),
        $latency:literal,
        [$($unit:ident)|+ $(|)?],
        $pattern:literal
    ) => {
        paste!(pub(super) static [<$name $($reg_bank)? $($mode_suffix)?>]: Code = Code {
            name:               stringify!($name),
            has_output:         $( output_reg!($reg) ||)* false,
            latency:            $latency,
            units:              enum_set!($(Unit::$unit)|*),
            encode: |operands: &[u32]| -> u32 {
                // If there's no argument (ie `ret`), _it is unused, so the _ silences a warning.
                let mut _it = operands.iter().copied();
                $pattern $(| reg!($reg, _it.next().unwrap()))*
            },
            format: |operands, address|
                format_operands(AddressingMode::$addressing_mode, operands, address, &[$(format_operand!($reg)),*]),
            };);
    }
}


code!(fadd dd, dn, dm               =>  1, [FP11 | FP12 | FP13 | FP14], 0x1E60_2800);
code!(fsub dd, dn, dm               =>  1, [FP11 | FP12 | FP13 | FP14], 0x1E60_3800);
code!(fmul dd, dn, dm               =>  4, [FP11 | FP12 | FP13 | FP14], 0x1E60_0800);
code!(fdiv dd, dn, dm               => 10, [FP11 | FP12 | FP13 | FP14], 0x1E60_1800);
code!(fmsub dd, dn, dm, da          =>  4, [FP11 | FP12 | FP13 | FP14], 0x1F40_8000);
code!(frintz dd, dn                 =>  3, [FP11 | FP12 | FP13 | FP14], 0x1E65_C000);

code!(fmov dd, dn                   =>  2, [FP11 | FP12 | FP13 | FP14], 0x1E60_4000);
code!(mov xd, xm                    =>  2, [FP11 | FP12 | FP13 | FP14], 0b1_01_01010_00_0_00000_000000_11111_00000);

code!(ldr xd, imm19                 => 10, [LS8 | L9 | L10],            0b01_011_0_00_0000000000000000000_00000);
code!(ldr dd, imm19                 => 10, [LS8 | L9 | L10],            0x5C00_0000);

code!(ldr xt, [xn], #imm9           => 10, [LS8 | L9 | L10],            0b11_111_0_00_01_0_000000000_01_00000_00000);
code!(ldr dt, [xn, #imm12]          => 10, [LS8 | L9 | L10],            0b11_111_1_01_01_000000000000_00000_00000);
code!(ldr dt, [xn], #imm9           => 10, [LS8 | L9 | L10],            0b11_111_1_00_01_0_000000000_01_00000_00000);

code!(ldp xt1, xt2, [xn], #imm7     => 10, [LS8 | L9 | L10],            0b10_101_0_001_1_0000000_00000_00000_00000);
code!(ldp dt1, dt2, [xn], #imm7     => 10, [LS8 | L9 | L10],            0b01_101_1_001_1_0000000_00000_00000_00000);

code!(str dt, [xn, #imm12]          => 10, [LS8 | L9 | L10],            0b11_111_1_01_00_000000000000_00000_00000);
code!(str xt, [xn, #imm9]!          => 10, [LS8 | L9 | L10],            0b11_111_0_00_00_0_000000000_11_00000_00000);
code!(str dt, [xn, #imm9]!          => 10, [LS8 | L9 | L10],            0b11_111_1_00_00_0_000000000_11_00000_00000);

code!(stp xt1, xt2, [xn, #imm7]!    => 10, [LS8 | L9 | L10],            0b10_101_0_011_0_0000000_00000_00000_00000);
code!(stp dt1, dt2, [xn, #imm7]!    => 10, [LS8 | L9 | L10],            0b01_101_1_011_0_0000000_00000_00000_00000);

code!(bl imm26                      =>  1, [LS8 | L9 | L10],            0b1_00_101_00000000000000000000000000);
code!(blr xn                        =>  1, [LS8 | L9 | L10],            0b110_101_1_0_0_01_11111_0000_0_0_00000_00000);
code!(ret                           =>  1, [LS8 | L9 | L10],            0xD65F_03C0);


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
