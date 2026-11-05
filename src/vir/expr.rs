use std::{rc::Rc, cmp::Ordering};

use crate::core::{BinaryOperator, Span};
use super::PoolIndex;


//-------------------------------------------------------------------------------------------------

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, derive_more::From)]
pub struct CallId(usize);

#[derive(Debug, Clone)]
pub enum InstrKind {
    Number(f64),
    Bool(bool),
    Binary(BinaryOperator, ExprUse, ExprUse),
    Call(String, Vec<ExprUse>, Option<CallId>),
}

impl Ord for InstrKind {
    fn cmp(&self, other: &Self) -> Ordering {
        use InstrKind::*;
        match (self, other) {
            (Number(a), Number(b))      => a.to_bits().cmp(&b.to_bits()),
            (Bool(a), Bool(b))          => a.cmp(b),
            (Binary(op1, lhs1, rhs1), Binary(op2, lhs2, rhs2)) =>
                (op1, lhs1.pool_index(), rhs1.pool_index())
                    .cmp(&(op2, lhs2.pool_index(), rhs2.pool_index())),
            // If the name and arguments are the same, and there's no id (the function is pure),
            // then they're the same.  Otherwise not.
            (Call(name1, exprs1, id1), Call(name2, exprs2, id2)) =>
                name1.cmp(name2)
                    .then_with(|| exprs1.iter().map(ExprUse::pool_index)
                        .cmp(exprs2.iter().map(ExprUse::pool_index)))
                    .then_with(|| id1.cmp(id2)),
            _                           => self.rank().cmp(&other.rank()),
        }
    }
}
impl PartialOrd for InstrKind {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> { Some(self.cmp(other)) }
}
impl PartialEq for InstrKind {
    fn eq(&self, other: &Self) -> bool { self.cmp(other) == Ordering::Equal }
}
impl Eq for InstrKind {}

impl InstrKind {
    fn is_constant(&self) -> bool {
        matches!(self, Self::Number(..) | Self::Bool(..))
    }

    fn rank(&self) -> u8 {
        match self {
            Self::Number(..)    => 0,
            Self::Bool(..)      => 1,
            Self::Binary(..)    => 2,
            Self::Call(..)      => 3,
        }
    }
}


//-------------------------------------------------------------------------------------------------

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq)]
pub enum ExprKind {
    Arg(usize),
    Instr(InstrKind),
}

impl ExprKind {
    pub(super) fn instr(&self) -> Option<&InstrKind> {
        if let Self::Instr(i) = self { Some(i) } else { None }
    }

    fn is_constant(&self) -> bool {
        self.instr().is_some_and(InstrKind::is_constant)
    }
}

impl From<InstrKind> for ExprKind { fn from(i: InstrKind) -> Self { Self::Instr(i) } }


//-------------------------------------------------------------------------------------------------

/// What we intern when we intern an Expression.
///
/// Expressions are the expression itself (the `ExprKind`) and supporting information.  If you
/// think about CSE, then some information is "per expression" that is, if the same expression
/// appears twice, the same information applies, where as other stuff (like the source span where
/// the expression appears) are per appearance.  An `ExprEntry` is the per-expression information
/// that we intern.
#[derive(Debug, Clone)]
pub struct ExprEntry {
    kind:                                   ExprKind,
    pool_index:                             PoolIndex,
    definition_span:                        Span,
}


impl ExprEntry {
    pub fn new(kind: ExprKind, pool_index: PoolIndex, definition_span: Span) -> Self {
        Self{ kind, pool_index, definition_span }
    }
    pub fn used_at(self: &Rc<Self>, use_span: Span) -> ExprUse {
        ExprUse { expr: Rc::clone(self), use_span }
    }

    pub fn kind(&self) -> &ExprKind         { &self.kind }
    pub fn pool_index(&self) -> PoolIndex   { self.pool_index }
    pub fn is_constant(&self) -> bool       { self.kind().is_constant() }

    pub fn definition_span(&self) -> Span   { self.definition_span }
}


//-------------------------------------------------------------------------------------------------

pub type Expr = Rc<ExprEntry>;

#[derive(Debug, Clone)]
pub struct ExprUse {
    expr:                                   Expr,
    use_span:                               Span,
}


impl ExprUse {
    pub fn kind(&self) -> &ExprKind         { self.expr.kind() }
    pub fn pool_index(&self) -> PoolIndex   { self.expr.pool_index() }
    pub fn is_constant(&self) -> bool       { self.expr.is_constant() }

    pub fn use_span(&self) -> Span          { self.use_span }

    pub fn expr(&self) -> &Expr              { &self.expr }
    pub fn into_expr(self) -> Expr          { self.expr }
}


//-------------------------------------------------------------------------------------------------

/// What we pass to codegen as instrs.
///
/// We don't pass arguments as instrs (since codegen wants to treat arguments and instructions
/// differently) so we flatten the exprs into instrs and remove the `ExprKind` wrapper that holds
/// `Arg`s.
#[derive(Debug)]
pub struct Instr {
    pub kind:                               InstrKind,
    pub pool_index:                         PoolIndex,
    pub definition_span:                    Span,
}


#[derive(Debug)]
pub struct Arg {
    pub pool_index:                         PoolIndex,
    pub definition_span:                    Span,
}


//-------------------------------------------------------------------------------------------------
