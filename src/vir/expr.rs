use std::hash::{Hash, Hasher};
use std::rc::Rc;

use crate::core::{BinaryOperator, Span};


//-------------------------------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub enum ExprKind {
    Binary(BinaryOperator, Expr, Expr),
    Number(f64),
    Argument(usize, String),
    Call(String, Vec<Expr>),
}


impl Hash for ExprKind {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Self::Binary(op, lhs, rhs) => {
                op.hash(state);
                Rc::as_ptr(&lhs.entry).hash(state);
                Rc::as_ptr(&rhs.entry).hash(state);
            }
            Self::Number(value) => {
                value.to_bits().hash(state);
            }
            Self::Argument(index, ..) => {
                index.hash(state);
            }
            Self::Call(name, exprs) => {
                name.hash(state);
                for e in exprs { Rc::as_ptr(&e.entry).hash(state); }
            }
        }
    }
}


impl PartialEq for ExprKind {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Binary(op1, lhs1, rhs1), Self::Binary(op2, lhs2, rhs2)) => {
                op1 == op2 &&
                Rc::ptr_eq(&lhs1.entry, &lhs2.entry) &&
                Rc::ptr_eq(&rhs1.entry, &rhs2.entry)
            }
            (Self::Number(value1), Self::Number(value2)) => { value1 == value2 }
            (Self::Argument(index1, ..), Self::Argument(index2, ..)) => { index1 == index2 }
            // We don't do CSE on calls (until we can work out if functions are pure)
            _ => false
        }
    }
}
impl Eq for ExprKind {}


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
    pub fn new(kind: ExprKind, pool_index: usize, span: Span) -> Self {
        Self{kind, pool_index, span}
    }
    pub fn kind(&self) -> &ExprKind         { &self.kind }
    pub fn pool_index(&self) -> usize       { self.pool_index }
    pub fn span(&self) -> &Span             { &self.span }
}


//-------------------------------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub struct Expr {
    entry:                                  Rc<ExprEntry>,
}


impl Expr {
    pub fn new(entry: Rc<ExprEntry>) -> Self {
        Self { entry }
    }
    pub fn kind(&self) -> &ExprKind         { self.entry.kind() }
    pub fn pool_index(&self) -> usize       { self.entry.pool_index() }
    pub fn span(&self) -> &Span             { self.entry.span() }
}


//-------------------------------------------------------------------------------------------------
