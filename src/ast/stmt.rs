use std::fmt;

use super::expr::Expr;
use crate::core::{Declaration, Nev, LineStyle, Span, Styleable};
use crate::lex::Token;


//-------------------------------------------------------------------------------------------------

#[derive(Debug)]
pub struct Assignment {
    pub declaration:        Declaration,
    pub assignments:        Vec<(String, Span, Expr)>,
    pub stmts:              Vec<Stmt>,
}


#[derive(Debug)]
pub enum StmtKind {
    Arguments(Nev<(String, Span)>),
    Assignment(Assignment),
    Exprs(Nev<Expr>),
    Return(Nev<Expr>),
}


//-------------------------------------------------------------------------------------------------

#[derive(Debug)]
pub struct Stmt {
    pub kind:               StmtKind,
    pub span:               Span,
}


impl Stmt {
    pub fn arguments(names: Nev<(String, Span)>, span: Span) -> Self {
        Self { span, kind: StmtKind::Arguments(names) }
    }

    pub fn block(
        declaration:        Declaration,
        assignments:        Vec<(String, Span, Expr)>,
        stmts:              Vec<Self>,
        span:               Span) -> Self {
        Self { span, kind: StmtKind::Assignment(Assignment { declaration, assignments, stmts }) }
    }

    pub fn exprs(exprs: Nev<Expr>, span: Span) -> Self {
        Self { span, kind: StmtKind::Exprs(exprs) }
    }

    pub fn return_stmt(exprs: Nev<Expr>, span: Span) -> Self {
        Self { span, kind: StmtKind::Return(exprs) }
    }
}


//-------------------------------------------------------------------------------------------------

pub fn join_format<T: fmt::Display>(v: &[T]) -> String {
    v.iter().map(|v| format!("{v}")).collect::<Vec<_>>().join(", ")
}

pub fn join_field_format<T1, T2: fmt::Display, F: Fn(&T1) -> &T2>(v: &[T1], f: F) -> String {
    v.iter().map(|e| format!("{}", f(e))).collect::<Vec<_>>().join(", ")
}


//-------------------------------------------------------------------------------------------------

impl Styleable for Stmt {
    fn write<S: LineStyle>(&self, f: &mut fmt::Formatter, indent: u16, style: &S) -> fmt::Result {
        use StmtKind::*;
        let span = self.span;
        match &self.kind {
            Return(values) => style.write(f, indent, Some(span),
                &format!("{} {}", Token::Return, join_format(values))
            ),
            Exprs(values) => style.write(f, indent, Some(span), &join_format(values)),
            Arguments(names) => style.write(f, indent, Some(span),
                &format!("{} {}", Token::Argument, join_field_format(names, |(n, _)| n)),
            ),
            Assignment(a) => {
                let (lhs, rhs) = if a.assignments.is_empty() {
                    (String::new(), String::new())
                } else {
                    (
                        format!("{}{} = ", a.declaration, join_field_format(&a.assignments, |e| &e.0)),
                        join_field_format(&a.assignments, |e| &e.2),
                    )
                };

                if a.stmts.is_empty() {
                    style.write(f, indent, Some(span), &format!("{lhs}{rhs}"))
                } else {
                    style.writeln(f, indent, Some(span), &format!("{lhs}{}", Token::Begin))?;
                    for stmt in &a.stmts {
                        stmt.write(f, indent + 1, style)?;
                        writeln!(f)?;
                    }
                    if !a.assignments.is_empty() {
                        let span = a.assignments.first().unwrap().2.span()
                            .union(a.assignments.last().unwrap().2.span());
                        style.writeln(f, indent + 1, Some(span), &rhs)?;
                    }
                    let end_string = format!("{}", Token::End);
                    let end_span: Span = (span.end() - end_string.len(), span.end()).into();
                    style.write(f, indent, Some(end_span), &end_string)
                }
            }
        }
    }
}


impl std::fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_styled(f)
    }
}


//-------------------------------------------------------------------------------------------------
