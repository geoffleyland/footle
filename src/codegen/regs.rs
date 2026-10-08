use super::Type;

//-------------------------------------------------------------------------------------------------

/// Representation of a machine register - just a number from 0-63.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(super) struct MachineReg(u8);

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


//-------------------------------------------------------------------------------------------------

type SetBits = u64;

/// A set of register ranks.
#[derive(Debug, Copy, Clone)]
pub(super) struct RegSet(SetBits);

impl RegSet {
    pub(super) const EMPTY: Self = Self(0);

    pub(super) const fn new(regs: u64) -> Self              { Self(regs) }

    #[cfg_attr(not(target_arch = "riscv64"), expect(dead_code, reason = "only riscv64's frame layout uses it"))]
    pub(super) const fn len(self) -> usize                  { self.0.count_ones() as usize }
    #[cfg_attr(not(target_arch = "riscv64"), expect(dead_code, reason = "only riscv64's frame layout uses it"))]
    pub(super) const fn is_empty(self) -> bool              { self.0 == 0 }

    pub(super) fn insert(&mut self, reg: MachineReg)        { self.0 |= 1 << reg.0 }
    pub(super) const fn intersection(self, other: Self) -> Self
                                                            { Self(self.0 & other.0) }
    pub(super) fn contains(self, reg: MachineReg) -> bool   { self.0 >> reg.0 & 1 != 0 }

    pub(super) fn iter(self) -> impl Iterator<Item = MachineReg> {
        let mut bits = self.0;
        std::iter::from_fn(move || {
            if bits == 0 { return None }
            let reg = MachineReg::try_from(bits.trailing_zeros() as usize)
                .expect("internal compiler error: register out of range");
            bits &= bits - 1;                           // clear the lowest set bit
            Some(reg)
        })
    }
}


//-------------------------------------------------------------------------------------------------

/// The "rank" of a register - again, a number from 1-64.
///
/// The is the order in which we wish to use registers - lowest first
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(super) struct RegRank(u8);

impl RegRank {
    #[allow(clippy::cast_possible_truncation)]
    const fn new(index: usize) -> Self {
        debug_assert!(index < RegFile::REG_COUNT);
        Self(index as u8)
    }
}


#[derive(Debug, Copy, Clone)]
pub(super) struct RankSet(SetBits);

impl RankSet {
    pub(super) const EMPTY: Self = Self(0);

    pub(super) fn contains(self, rank: RegRank) -> bool     { self.0 >> rank.0 & 1 != 0 }
    /// Remove every rank in `other` from this set.
    pub(super) fn remove(&mut self, other: Self)            { self.0 &= !other.0; }

    pub(super) const fn union(self, other: Self) -> Self    { Self(self.0 | other.0) }
    pub(super) const fn intersection(self, other: Self) -> Self
                                                            { Self(self.0 & other.0) }
    const fn with(self, rank: RegRank) -> Self { Self(self.0 | 1 << rank.0) }
}


//-------------------------------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum RegBank { Int = 0, Float = 1 }

impl RegBank {
    const COUNT: usize = Self::Float as usize + 1;

    pub(super) const fn of_reg(reg: MachineReg) -> Self { if reg.0 < 32 { Self::Int } else { Self::Float } }
    pub(super) const fn of_type(ty: Type) -> Option<Self> {
        match ty {
            Type::I64 | Type::Pointer       => Some(Self::Int),
            Type::F64                       => Some(Self::Float),
            Type::NoValue                   => None,
        }
    }
    pub(super) const fn index(self) -> usize { self as usize }
    pub(super) const fn regs(self) -> RegSet {
        match self {
            Self::Int                       => RegSet(0x0000_0000_FFFF_FFFF),
            Self::Float                     => RegSet(0xFFFF_FFFF_0000_0000),
        }
    }
}


//-------------------------------------------------------------------------------------------------

#[derive(Debug)]
pub(super) struct RegFile {
    pub(super) stack_reg:       MachineReg,
    pub(super) link_reg:        MachineReg,
    pub(super) scratch_reg:     MachineReg,
    pub(super) callee_saved:    RegSet,             // Registers we have to save in our prologue
                                                    // and restore in our epilogue (if we use them)
    argument_regs:              [&'static [MachineReg]; RegBank::COUNT],
                                                    // Argument registers in order.
    order:                      [MachineReg; Self::REG_COUNT],
                                                    // Order in which we allocate registers
    rank:                       [Option<RegRank>; Self::REG_COUNT],
                                                    // Rank (in `order`) of a register.  `None`
                                                    // if we never allocate that register.
    available_ranks:            [RankSet; RegBank::COUNT],
                                                    // Available registers by rank.
    pub(super) clobbered_ranks: RankSet,            // The register ranks a call will clobber.
}


impl RegFile {
    pub(super) const REG_COUNT: usize = SetBits::BITS as usize;

    pub(super) const fn new(
        stack_reg:          u8,
        link_reg:           u8,
        scratch_reg:        u8,
        callee_saved:       RegSet,
        argument_regs:      [&'static [MachineReg]; RegBank::COUNT],
        u8_order:           &[u8],
    ) -> Self {
        let mut order = [MachineReg::new(0); Self::REG_COUNT];
        let mut rank = [None; Self::REG_COUNT];
        let mut available_ranks = [RankSet::EMPTY; RegBank::COUNT];
        let mut i = 0;
        while i < u8_order.len() {
            order[i] = MachineReg::new(u8_order[i]);
            let r = RegRank::new(i);
            rank[u8_order[i] as usize] = Some(r);
            let bank = RegBank::of_reg(order[i]).index();
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

    pub(super) fn regs_to_save(&self, written: RegSet) -> RegSet {
        let mut must_save_if_used = self.callee_saved;
        must_save_if_used.insert(self.link_reg);
        written.intersection(must_save_if_used)
    }
    pub(super) fn rank_bits(&self, reg: MachineReg) -> RankSet {
        let r = self.rank[usize::from(reg)]
            .expect("internal compiler error: trying to use system register");
        RankSet(1 << r.0)
    }

    pub(super) fn ranks_for_type(&self, ty: Type) -> RankSet {
        RegBank::of_type(ty).map_or(RankSet::EMPTY, |b| self.available_ranks[b.index()])
    }
    pub(super) fn ranks_for_reg(&self, reg: MachineReg) -> RankSet {
        self.available_ranks[RegBank::of_reg(reg).index()]
    }

    pub(super) fn abi_regs(&self, types: impl IntoIterator<Item = Type>) -> impl Iterator<Item = MachineReg> {
        let mut used = [0usize; RegBank::COUNT];           // registers used so far, per bank
        types.into_iter().map(move |ty| {
            let b = RegBank::of_type(ty).expect("internal compiler error: no bank for type").index();
            let reg = *self.argument_regs[b].get(used[b])
                .expect("internal compiler error: too many arguments/return values in one register bank");
            used[b] += 1;
            reg
        })
    }
}


//-------------------------------------------------------------------------------------------------

pub(super) const fn machine_regs<const N: usize>(numbers: [u8; N]) -> [MachineReg; N] {
    let mut out = [MachineReg::new(0); N];
    let mut i = 0;
    while i < N { out[i] = MachineReg::new(numbers[i]); i += 1; }
    out
}


//-------------------------------------------------------------------------------------------------
