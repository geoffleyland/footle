#![allow(clippy::unusual_byte_groupings, non_upper_case_globals)]

use enumset::{EnumSet, EnumSetType, enum_set};
use paste::paste;

use super::scheduler::Type;

#[cfg(feature = "dogfood")]
use display::*;


//-------------------------------------------------------------------------------------------------
// Not really architecture specific stuff (maybe it'll move if we ever get to a second arch)

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(super) struct MachineReg(pub(super) u8);

impl MachineReg {
    pub(super) const fn new(index: u8) -> Self {
        debug_assert!((index as usize) < RegFile::REG_COUNT);
        Self(index)
    }
    pub(super) fn encoding(self) -> u8 { self.0 & 0x1F }
}

impl From<MachineReg> for u8    { fn from(m: MachineReg) -> Self  { m.0 } }
impl From<MachineReg> for u32   { fn from(m: MachineReg) -> Self  { m.0.into() } }
impl From<MachineReg> for i32   { fn from(m: MachineReg) -> Self  { m.0.into() } }
impl From<MachineReg> for usize { fn from(m: MachineReg) -> Self  { m.0.into() } }


impl TryFrom<u8> for MachineReg {
    type Error = ();
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        if (value as usize) < RegFile::REG_COUNT { Ok(Self(value)) } else { Err(()) }
    }
}

impl TryFrom<usize> for MachineReg {
    type Error = ();
    fn try_from(value: usize) -> Result<Self, Self::Error> {
        let v = u8::try_from(value).map_err(|_| ())?;
        if value < RegFile::REG_COUNT { Ok(Self(v)) } else { Err(()) }
    }
}


#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(super) struct RegRank(u8);

impl RegRank {
    #[allow(clippy::cast_possible_truncation)]
    const fn new(index: usize) -> Self {
        debug_assert!(index < RegFile::REG_COUNT);
        Self(index as u8)
    }
}


//-------------------------------------------------------------------------------------------------
// Register details

type SetBits = u64;

#[derive(Debug, Copy, Clone)]
pub(super) struct RankSet(SetBits);

impl RankSet {
    pub(super) const EMPTY: Self = Self(0);

    pub(super) fn contains(self, rank: RegRank) -> bool     { self.0 >> rank.0 & 1 != 0 }
    /// Remove every rank in `other` from this set.
    pub(super) fn remove(&mut self, other: Self)            { self.0 &= !other.0; }
    pub(super) fn remove_reg(&mut self, reg: MachineReg) {
        self.remove(REGS.get_rank_bits(reg));
    }

    pub(super) const fn union(self, other: Self) -> Self    { Self(self.0 | other.0) }
    pub(super) const fn intersection(self, other: Self) -> Self    { Self(self.0 & other.0) }
    const fn with(self, rank: RegRank) -> Self { Self(self.0 | 1 << rank.0) }
}

#[derive(Debug, Copy, Clone)]
pub(super) struct RegSet(SetBits);

impl RegSet {
    fn contains(self, reg: MachineReg) -> bool { self.0 >> reg.0 & 1 != 0 }
}


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum Bank { X = 0, D = 1 }

impl Bank {
    const COUNT: usize = Self::D as usize + 1;

    pub(super) const fn of_reg(reg: MachineReg) -> Self { if reg.0 < 32 { Self::X } else { Self::D } }
    pub(super) const fn of_type(ty: Type) -> Option<Self> {
        match ty {
            Type::F64                               => Some(Self::D),
            Type::I64 | Type::FunctionPointer       => Some(Self::X),
            Type::None                              => None,
        }
    }
    pub(super) const fn index(self) -> usize { self as usize }
}


#[derive(Debug)]
pub(super) struct RegFile {
    pub(super) stack_reg:       MachineReg,
    pub(super) link_reg:        MachineReg,
    pub(super) scratch_reg:     MachineReg,
    callee_saved:               RegSet,             // Registers we have to save in our prologue
                                                    // and restore in our epilogue (if we use them)
    argument_regs:              [&'static [MachineReg]; Bank::COUNT],
                                                    // Argument registers in order.
    order:                      [MachineReg; Self::REG_COUNT],
                                                    // Order in which we allocate registers
    rank:                       [Option<RegRank>; Self::REG_COUNT],
                                                    // Rank (in `order`) of a register.  `None`
                                                    // if we never allocate that register.
    available_ranks:            [RankSet; Bank::COUNT],
                                                    // Available registers by rank.
    clobbered_ranks:            RankSet,            // The register ranks a bl[r] will clobber.
}


impl RegFile {
    pub(super) const REG_COUNT: usize = SetBits::BITS as usize;

    const fn new(
        stack_reg:          u8,
        link_reg:           u8,
        scratch_reg:        u8,
        callee_saved:       RegSet,
        argument_regs:      [&'static [MachineReg]; Bank::COUNT],
        u8_order:           &[u8],
    ) -> Self {
        let mut order = [MachineReg::new(0); Self::REG_COUNT];
        let mut rank = [None; Self::REG_COUNT];
        let mut available_ranks = [RankSet::EMPTY; Bank::COUNT];
        let mut i = 0;
        while i < u8_order.len() {
            order[i] = MachineReg::new(u8_order[i]);
            let r = RegRank::new(i);
            rank[u8_order[i] as usize] = Some(r);
            let bank = Bank::of_reg(order[i]).index();
            available_ranks[bank] = available_ranks[bank].with(r);
            i += 1;
        }
        // Ideally we'd use bit_indices here, but it's not const.
        let mut c = !callee_saved.0;
        let mut clobbered_ranks = RankSet::EMPTY;
        while c != 0 {
            let reg = c.trailing_zeros() as usize;
            if let Some(rank) = rank[reg] {
                clobbered_ranks = clobbered_ranks.with(rank);
            }
            c &= c - 1;
        }

        Self {
            stack_reg:          MachineReg::new(stack_reg),
            link_reg:           MachineReg::new(link_reg),
            scratch_reg:        MachineReg::new(scratch_reg),
            callee_saved, argument_regs, order, rank, available_ranks, clobbered_ranks
        }
    }

    /// Pick a register from `available` (a bitmask of ranks).  If `preferred` is available, use it —
    /// this just avoids an extra move later, it's not required for correctness (the move machinery
    /// will fix up the register either way).
    pub(super) fn best_reg(&self, available: RankSet, preferred: Option<MachineReg>) -> MachineReg {
        if let Some(p) = preferred {
            let rank = self.rank[usize::from(p)]
                .expect("internal compiler error: trying to use system register");
            if available.contains(rank) { return p; }
        }
        self.order[available.0.trailing_zeros() as usize]
    }

    pub(super) fn is_callee_saved(&self, reg: MachineReg) -> bool { self.callee_saved.contains(reg) }
    fn get_rank_bits(&self, reg: MachineReg) -> RankSet {
        let r = self.rank[usize::from(reg)]
            .expect("internal compiler error: trying to use system register");
        RankSet(1 << r.0)
    }

    pub(super) fn ranks_for_type(&self, ty: Type) -> RankSet {
        Bank::of_type(ty).map_or(RankSet::EMPTY, |b| self.available_ranks[b.index()])
    }
    pub(super) fn ranks_for_reg(&self, reg: MachineReg) -> RankSet {
        self.available_ranks[Bank::of_reg(reg).index()]
    }

    pub(super) fn abi_regs(&self, types: impl IntoIterator<Item = Type>) -> impl Iterator<Item = MachineReg> {
        let mut used = [0usize; Bank::COUNT];           // registers used so far, per class
        types.into_iter().map(move |ty| {
            let b = Bank::of_type(ty).expect("internal compiler error: no bank for type").index();
            let reg = *self.argument_regs[b].get(used[b])
                .expect("internal compiler error: too many values in one register class");
            used[b] += 1;
            reg
        })
    }
}


const fn regs<const N: usize>(numbers: [u8; N]) -> [MachineReg; N] {
    let mut out = [MachineReg::new(0); N];
    let mut i = 0;
    while i < N { out[i] = MachineReg::new(numbers[i]); i += 1; }
    out
}


pub(super) static REGS: RegFile = RegFile::new(31, 30, 16,
    RegSet(0x0000_FF00_1FF8_0000),
    [
        &regs([ 0,  1,  2,  3,  4,  5,  6,  7]),
        &regs([32, 33, 34, 35, 36, 37, 38, 39]),
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
pub(super) enum Unit {
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
pub(super) struct Code {
    pub(super) encode:          fn(&[u32]) -> u32,
    pub(super) latency:         u8,
    has_output:                 bool,
    units:                      EnumSet<Unit>,

    #[cfg(any(feature = "dogfood", test))]
    mnemonic:                   &'static str,
    #[cfg(feature = "dogfood")]
    pub(super) format:          fn(&[i32], i32) -> String,
}


impl Code {
    pub fn has_output(&self) -> bool        { self.has_output }

    pub fn clobbers_anything(&self) -> bool { self.save_link_reg() }
    pub fn clobbers(&self, reg: MachineReg) -> bool {
        self.save_link_reg() && !REGS.callee_saved.contains(reg)
    }

    pub fn clobbered_ranks(&self) -> RankSet {
        if self.save_link_reg() { REGS.clobbered_ranks } else { RankSet::EMPTY }
    }

    pub fn restore_regs(&self) -> bool  { std::ptr::eq(self, &raw const ret) }
    pub fn save_link_reg(&self) -> bool {
        std::ptr::eq(self, &raw const bl) || std::ptr::eq(self, &raw const blr)
    }

    pub fn try_pick_unit(&self, free_units: EnumSet<Unit>) -> Option<Unit> {
        (self.units & free_units).iter().next()
    }

    #[cfg(any(feature = "dogfood", test))]
    pub fn mnemonic(&self) -> &str      { self.mnemonic }
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
        $latency:literal,
        [$($unit:ident)|+ $(|)?],
        $pattern:literal
    ) => {
        paste!(pub(super) static [<$mnemonic $($reg_bank)? $($mode_suffix)?>]: Code = Code {
            has_output:         $( has_output!($mnemonic, $reg) ||)* false,
            latency:            $latency,
            units:              enum_set!($(Unit::$unit)|*),
            encode: |operands: &[u32]| -> u32 {
                // If there's no argument (ie `ret`), _it is unused, so the _ silences a warning.
                let mut _it = operands.iter().copied();
                $pattern $(| reg!($reg, _it.next().unwrap()))*
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

code!(fadd dd, dn, dm               =>  1, [FP11 | FP12 | FP13 | FP14], 0x1E60_2800);
code!(fsub dd, dn, dm               =>  1, [FP11 | FP12 | FP13 | FP14], 0x1E60_3800);
code!(fmul dd, dn, dm               =>  4, [FP11 | FP12 | FP13 | FP14], 0x1E60_0800);
code!(fdiv dd, dn, dm               => 10, [FP11 | FP12 | FP13 | FP14], 0x1E60_1800);
code!(fmsub dd, dn, dm, da          =>  4, [FP11 | FP12 | FP13 | FP14], 0x1F40_8000);
code!(frintz dd, dn                 =>  3, [FP11 | FP12 | FP13 | FP14], 0x1E65_C000);

code!(fmov dd, dn                   =>  2, [FP11 | FP12 | FP13 | FP14], 0x1E60_4000);

// TODO! mov doesn't actually use a unit and has no latency.
code!(mov xd, xm                    =>  1, [LS8 | L9 | L10],            0b1_01_01010_00_0_00000_000000_11111_00000);
code!(mov xd, #imm16                =>  1, [LS8 | L9 | L10],            0b1_10_100101_00_0000000000000000_00000);

code!(ldr xd, imm19                 => 10, [LS8 | L9 | L10],            0b01_011_0_00_0000000000000000000_00000);
code!(ldr dd, imm19                 => 10, [LS8 | L9 | L10],            0x5C00_0000);

code!(ldr xt, [xn, #imm12]          => 10, [LS8 | L9 | L10],            0b11_111_0_01_01_000000000000_00000_00000);
code!(ldr xt, [xn], #imm9           => 10, [LS8 | L9 | L10],            0b11_111_0_00_01_0_000000000_01_00000_00000);
code!(ldr dt, [xn, #imm12]          => 10, [LS8 | L9 | L10],            0b11_111_1_01_01_000000000000_00000_00000);
code!(ldr dt, [xn], #imm9           => 10, [LS8 | L9 | L10],            0b11_111_1_00_01_0_000000000_01_00000_00000);

code!(ldp xt1, xt2, [xn], #imm7     => 10, [LS8 | L9 | L10],            0b10_101_0_001_1_0000000_00000_00000_00000);
code!(ldp dt1, dt2, [xn], #imm7     => 10, [LS8 | L9 | L10],            0b01_101_1_001_1_0000000_00000_00000_00000);

code!(str xt, [xn, #imm12]          => 10, [LS8 | L9 | L10],            0b11_111_0_01_00_000000000000_00000_00000);
code!(str xt, [xn, #imm9]!          => 10, [LS8 | L9 | L10],            0b11_111_0_00_00_0_000000000_11_00000_00000);
code!(str dt, [xn, #imm12]          => 10, [LS8 | L9 | L10],            0b11_111_1_01_00_000000000000_00000_00000);
code!(str dt, [xn, #imm9]!          => 10, [LS8 | L9 | L10],            0b11_111_1_00_00_0_000000000_11_00000_00000);

code!(stp xt1, xt2, [xn, #imm7]!    => 10, [LS8 | L9 | L10],            0b10_101_0_011_0_0000000_00000_00000_00000);
code!(stp dt1, dt2, [xn, #imm7]!    => 10, [LS8 | L9 | L10],            0b01_101_1_011_0_0000000_00000_00000_00000);

code!(bl imm26                      =>  1, [LS8 | L9 | L10],            0b1_00_101_00000000000000000000000000);
code!(blr xn                        =>  1, [LS8 | L9 | L10],            0b110_101_1_0_0_01_11111_0000_0_0_00000_00000);
code!(ret                           =>  1, [LS8 | L9 | L10],            0xD65F_03C0);

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
