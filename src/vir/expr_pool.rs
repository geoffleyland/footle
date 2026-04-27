use std::collections::HashMap;
use std::rc::Rc;

use crate::core::Span;
use super::expr::{ExprKind, ExprEntry, Expr};


//-------------------------------------------------------------------------------------------------

pub struct ExprPool {
    exprs:                      HashMap<ExprKind, Rc<ExprEntry>>,
}


impl ExprPool {
    pub fn new() -> Self {
        Self {
            exprs:              HashMap::new(),
        }
    }


    pub(super) fn intern(&mut self, kind: ExprKind, span: Span) -> Expr {
        let index = self.exprs.len();
        Expr::new(self.exprs.entry(kind.clone())
            .or_insert_with(|| Rc::new(ExprEntry::new(kind, index, span))).clone())
    }

    pub(super) fn number(&mut self, value: f64, span: Span) -> Expr {
        self.intern(ExprKind::Number(value), span)
    }


    pub(super) fn argument(&mut self, index: usize, name: &str, span: Span) -> Expr {
        self.intern(ExprKind::Argument(index, name.to_string()), span)
    }
}


//-------------------------------------------------------------------------------------------------
