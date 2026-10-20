use std::hash::{Hash, Hasher};
use std::rc::Rc;

use crate::core::{BinaryOperator, Span};


//-------------------------------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub enum InstrKind {
    Number(f64),
    Bool(bool),
    Binary(BinaryOperator, Expr, Expr),
    Call(String, Vec<Expr>),
}

impl Hash for InstrKind {
    fn hash<H: Hasher>(&self, state: &mut H) {
        use InstrKind::*;
        match self {
            Number(value)               => { value.to_bits().hash(state); }
            Bool(value)                 => { value.hash(state); }
            Binary(op, lhs, rhs) => {
                op.hash(state);
                Rc::as_ptr(&lhs.entry).hash(state);
                Rc::as_ptr(&rhs.entry).hash(state);
            }
            Call(name, exprs) => {
                name.hash(state);
                for e in exprs { Rc::as_ptr(&e.entry).hash(state); }
            }
        }
    }
}

impl PartialEq for InstrKind {
    fn eq(&self, other: &Self) -> bool {
        use InstrKind::*;
        match (self, other) {
            (Number(a), Number(b))      => { a.to_bits() == b.to_bits() }
            (Bool(a), Bool(b))          => { a == b }
            (Binary(op1, lhs1, rhs1), Binary(op2, lhs2, rhs2)) => {
                op1 == op2 &&
                Rc::ptr_eq(&lhs1.entry, &lhs2.entry) &&
                Rc::ptr_eq(&rhs1.entry, &rhs2.entry)
            }
            // We don't do CSE on calls (until we can work out if functions are pure)
            _ => false
        }
    }
}
impl Eq for InstrKind {}

impl InstrKind {
    fn is_constant(&self) -> bool {
        matches!(self, Self::Number(..) | Self::Bool(..))
    }
}


#[derive(Debug, Clone, Hash, PartialEq, Eq)]
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
    pool_index:                             usize,
    span:                                   Span,
}


impl ExprEntry {
    pub fn new(
        kind:                               ExprKind,
        pool_index:                         usize,
        span:                               Span
    ) -> Self {
        Self{ kind, pool_index, span }
    }
    pub fn kind(&self) -> &ExprKind         { &self.kind }
    pub fn pool_index(&self) -> usize       { self.pool_index }

    pub fn span(&self) -> Span              { self.span }
}


//-------------------------------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub struct Expr {
    entry:                                  Rc<ExprEntry>,
    use_span:                               Span,
}


impl Expr {
    pub fn new(entry: Rc<ExprEntry>, use_span: Span) -> Self { Self { entry, use_span } }
    pub fn used_at(&self, use_span: Span) -> Self { Self { entry: self.entry.clone(), use_span } }

    pub fn kind(&self) -> &ExprKind         { self.entry.kind() }
    pub fn pool_index(&self) -> usize       { self.entry.pool_index() }
    pub fn is_constant(&self) -> bool       { self.entry.kind().is_constant() }

    pub fn use_span(&self) -> Span          { self.use_span }
    pub fn definition_span(&self) -> Span   { self.entry.span() }
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
    pub pool_index:                         usize,
    pub definition_span:                    Span,
}


#[derive(Debug)]
pub struct Arg {
    pub pool_index:                         usize,
    pub definition_span:                    Span,
}


//-------------------------------------------------------------------------------------------------
