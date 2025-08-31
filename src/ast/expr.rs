use std::fmt;

use crate::core::{BinaryOperator, Span};


//-------------------------------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub enum ExprKind {
    Binary(BinaryOperator, Box<Expr>, Box<Expr>),
    Number(f64),
    Identifier(String),
}


impl fmt::Display for ExprKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ExprKind::*;
        match self {
            Binary(op, lhs, rhs)                    => write!(f, "({lhs} {op} {rhs})"),
            Number(value)                           => write!(f, "{value}"),
            Identifier(name)                        => write!(f, "{name}"),
        }
    }
}


#[derive(Debug, Clone)]
pub struct Expr {
    pub kind:                                       ExprKind,
    span:                                           Span,
}


impl Expr {
    pub fn kind(&self) -> &ExprKind                 { &self.kind }
    pub fn span(&self) -> &Span                     { &self.span }

    pub fn binary(op: BinaryOperator, lhs: Self, rhs: Self) -> Self {
        let span = lhs.span().union(rhs.span());
        Self { kind: ExprKind::Binary(op, Box::new(lhs), Box::new(rhs)), span }
    }
    pub fn number(value: f64, span: Span) -> Self { Self{ kind: ExprKind::Number(value), span } }
    pub fn identifier(n: String, span: Span) -> Self { Self{ kind: ExprKind::Identifier(n), span } }
}


impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { self.kind().fmt(f) }
}


//-------------------------------------------------------------------------------------------------
