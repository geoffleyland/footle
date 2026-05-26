use crate::core::{Nev, Span};
use super::expr::Expr;


//-------------------------------------------------------------------------------------------------

#[derive(Debug)]
pub enum StmtKind {
    Return(Nev<Expr>),
}


#[derive(Debug)]
pub struct Stmt {
    pub kind:                               StmtKind,
    pub span:                               Span,
}


impl Stmt {
    pub fn return_stmt(exprs: Nev<Expr>, span: Span) -> Self {
        Self{span, kind: StmtKind::Return(exprs)}
    }
}

//-------------------------------------------------------------------------------------------------
