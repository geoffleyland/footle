use crate::core::Nev;
use super::expr::Expr;

use crate::core::Span;


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
    pub fn return_stmt(
        exprs:                              Nev<Expr>,
        span:                               Span,
    ) -> Self {
        Self{kind: StmtKind::Return(exprs), span }
    }
}

//-------------------------------------------------------------------------------------------------
