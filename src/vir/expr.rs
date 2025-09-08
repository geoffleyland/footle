use std::fmt;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

use crate::core::{BinaryOperator, Span};
use super::variable::Binding;


//-------------------------------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub enum ExprKind {
    Binary(BinaryOperator, Expr, Expr),
    Number(f64),
    Identifier(Rc<Binding>),
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
            Self::Identifier(binding) => {
                Rc::as_ptr(binding).hash(state);
            }
        }
    }
}


impl PartialEq for ExprKind {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Binary(op1, lhs1, rhs1), Self::Binary(op2, lhs2, rhs2)) => {
                op1 == op2 && Rc::ptr_eq(&lhs1.entry, &lhs2.entry) && Rc::ptr_eq(&rhs1.entry, &rhs2.entry)
            }
            (Self::Number(value1), Self::Number(value2)) => { value1 == value2 }
            (Self::Identifier(version1), Self::Identifier(version2)) => {
                Rc::ptr_eq(version1, version2)
            }
            _ => false
        }
    }
}
impl Eq for ExprKind {}


impl fmt::Display for ExprKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ExprKind::*;
        match self {
            Binary(op, lhs, rhs)            => write!(f, "({lhs} {op} {rhs})"),
            Number(value)                   => write!(f, "{value}"),
            Identifier(binding)             => write!(f, "{binding}"),
        }
    }
}


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
    index:                                  usize,
    span:                                   Span,
}


impl ExprEntry {
    pub fn new(kind: ExprKind, index: usize, span: Span) -> Self {
        Self{kind, index, span}
    }
    pub fn kind(&self) -> &ExprKind         { &self.kind }
    pub fn index(&self) -> usize            { self.index }
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
    pub fn index(&self) -> usize            { self.entry.index() }
    pub fn span(&self) -> &Span             { self.entry.span() }
}


impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.kind().fmt(f)
    }
}


//-------------------------------------------------------------------------------------------------
