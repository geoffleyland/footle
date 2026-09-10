use crate::core::Nev;
use super::expr::Expr;

#[cfg(any(feature = "dogfood", test))]
use crate::core::Span;


//-------------------------------------------------------------------------------------------------

#[derive(Debug)]
pub enum StmtKind {
    Return(Nev<Expr>),
}


#[derive(Debug)]
pub struct Stmt {
    pub kind:                               StmtKind,
    #[cfg(any(feature = "dogfood", test))]
    pub span:                               Span,
}


impl Stmt {
    pub fn return_stmt(
        exprs:                              Nev<Expr>,
        #[cfg(any(feature = "dogfood", test))]
        span:                               Span,
    ) -> Self {
        Self{kind: StmtKind::Return(exprs),
            #[cfg(any(feature = "dogfood", test))]
            span
        }
    }
}

//-------------------------------------------------------------------------------------------------
