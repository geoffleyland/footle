#![allow(clippy::unusual_byte_groupings, non_upper_case_globals)]

use enumset::{EnumSet, EnumSetType, enum_set};
use paste::paste;


//-------------------------------------------------------------------------------------------------
// Not really architecture specific stuff (maybe it'll move if we ever get to a second arch)

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(super) struct MachineReg(u8);

impl MachineReg {
    pub(super) const fn new(index: u8) -> Self {
        debug_assert!(index < 32);
        Self(index)
    }
}

impl From<MachineReg> for u8    { fn from(m: MachineReg) -> Self  { m.0 } }
impl From<MachineReg> for u32   { fn from(m: MachineReg) -> Self  { m.0.into() } }
impl From<MachineReg> for i32   { fn from(m: MachineReg) -> Self  { m.0.into() } }
impl From<MachineReg> for usize { fn from(m: MachineReg) -> Self  { m.0.into() } }


impl TryFrom<u8> for MachineReg {
    type Error = ();
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        if value < 32 { Ok(Self(value)) } else { Err(()) }
    }
}

impl TryFrom<usize> for MachineReg {
    type Error = ();
    fn try_from(value: usize) -> Result<Self, Self::Error> {
        let v = u8::try_from(value).map_err(|_| ())?;
        if value < 32 { Ok(Self(v)) } else { Err(()) }
    }
}


#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct RegRank(u8);

impl RegRank {
    #[allow(clippy::cast_possible_truncation)]
    const fn new(index: usize) -> Self {
        debug_assert!(index < 32);
        Self(index as u8)
    }
}


//-------------------------------------------------------------------------------------------------
// Register details

pub(super) const STACK_REG: MachineReg = MachineReg::new(31);
pub(super) const LINK_REG: MachineReg = MachineReg::new(30);
pub(super) const SCRATCH_REG: MachineReg = MachineReg::new(16);


/// Information about a bank of registers (int or FP)  Possibly the structure is cross-platform?
pub (super) struct RegBank<const N: usize> {
    order:              [MachineReg; N],        // Order in which we allocate registers
    rank:               [Option<RegRank>; 32],  // Rank (in `order`) of a register.  `None` if we
                                                // never allocate that register.
    callee_saved:       u32,                    // Bitmask of registers we have to save in our
                                                // prologue and epilogue (if we use them)
    clobber_rank_mask:  [u32; 32],              // In register order, bitmask of whether this reg
                                                // is clobbered.
}

impl<const N:usize> RegBank<N> {
    #[allow(clippy::cast_possible_truncation)]
    const fn new(u8_order: [u8; N], callee_saved: u32) -> Self {
        let mut order = [MachineReg::new(0); N];
        let mut rank = [None; 32];
        let mut i = 0;
        while i < N {
            order[i] = MachineReg::new(u8_order[i]);
            rank[u8_order[i] as usize] = Some(RegRank::new(i));
            i += 1;
        }
        let mut clobber_rank_mask = [0u32; 32];
        let mut r = 0;
        while r < 32 {
            if let Some(rank) = rank[r] { clobber_rank_mask[r] = 1 << rank.0; }
            r += 1;
        }
        Self { order, rank, callee_saved, clobber_rank_mask }
    }

    /// Pick a register from `available` (a bitmask of ranks).  If `preferred` is available, use it —
    /// this just avoids an extra move later, it's not required for correctness (the move machinery
    /// will fix up the register either way).
    pub(super) fn best_reg(&self, available: u32, preferred_reg: Option<MachineReg>) -> MachineReg {
        if let Some(p) = preferred_reg {
            let rank = self.rank[usize::from(p)]
                .expect("internal compiler error: trying to use system register");
            if (available >> rank.0) & 1 == 1 { return p; }
        }
        self.order[available.trailing_zeros() as usize]
    }

    pub(super) fn get_rank_bits(&self, reg: MachineReg) -> u32 {
        let r = self.rank[usize::from(reg)]
            .expect("internal compiler error: trying to use system register");
        1 << r.0
    }

    pub(super) fn is_callee_saved(&self, maybe_reg: Option<MachineReg>) -> Option<MachineReg> {
        maybe_reg.filter(|reg| self.callee_saved & (1 << reg.0) != 0)
    }

    pub(super) fn real_reg_to_ranked_reg_mask(&self, clobbers: u32) -> u32 {
        let mut c = clobbers;
        let mut mask = 0u32;
        while c != 0 {
            let bit = c.trailing_zeros() as usize;
            mask |= self.clobber_rank_mask[bit];
            c &= c - 1;
        }
        mask
    }
}

pub(super) const D_BANK: RegBank<32> = RegBank::new(
    [
        16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,     // d16-d31 (caller saved)
         8,  9, 10, 11, 12, 13, 14, 15,                                     // d8-d16 (callee saved)
         0,  1,  2,  3,  4,  5,  6,  7,                                     // d0-d7 (function args)
    ],
    0x0000_FF00
);


//-------------------------------------------------------------------------------------------------
// Architecture details.

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


//-------------------------------------------------------------------------------------------------
// Instruction encoding structures and macros.

#[derive(Debug)]
pub(super) struct Code {
    mnemonic:                   &'static str,
    pub(super) encode:          fn(&[u32]) -> u32,
    pub(super) latency:         u8,
    has_output:                 bool,
    units:                      EnumSet<Unit>,

    pub(super) format:          fn(&[i32], i32) -> String,
}


impl Code {
    pub fn mnemonic(&self) -> &str      { self.mnemonic }
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
        $latency:literal,
        [$($unit:ident)|+ $(|)?],
        $pattern:literal
    ) => {
        paste!(pub(super) static [<$mnemonic $($reg_bank)? $($mode_suffix)?>]: Code = Code {
            mnemonic:           stringify!($mnemonic),
            has_output:         $( has_output!($mnemonic, $reg) ||)* false,
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


//-------------------------------------------------------------------------------------------------
// The instructions!

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

//-------------------------------------------------------------------------------------------------
