use std::fmt;

use crate::core::{BinaryOperator, Nev, Span, Styleable, LineStyle};
use crate::lex::Token;
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


pub enum InstrKind {
    Argument(),
    Number(f64),
    Binary(BinaryOperator, usize, usize),
    Return(Nev<usize>),
}


pub struct Instr {
    pub kind:                               InstrKind,
    pub address:                            usize,
    pub span:                               Span,
}


impl Styleable for Instr {
    fn write<W: LineStyle>(&self, f: &mut fmt::Formatter, indent: u16, writer: &W) -> fmt::Result {
        use InstrKind::*;
        let address = self.address;
        let line = match &self.kind {
            Argument()                          => format!("{} I{address}", Token::Argument),
            Number(value)                       => format!("{} I{address} = {value}", Token::Local),
            Binary(op, lhs, rhs)                => format!("{} I{address} = I{lhs} {op} I{rhs}", Token::Local),
            Return(addresses)                   => format!("{} {}", Token::Return,
                addresses.iter().map(|a| format!("I{a}")).collect::<Vec<_>>().join(", "))

        };
        writer.write(f, indent, Some(self.span), &line)
    }
}


impl std::fmt::Display for Instr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_styled(f)
    }
}


//-------------------------------------------------------------------------------------------------
