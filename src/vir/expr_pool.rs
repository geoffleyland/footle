use std::collections::{HashMap, hash_map::Entry};
use std::rc::Rc;

use crate::core::Span;
use super::expr::{InstrKind, ExprKind, ExprEntry, Expr};


//-------------------------------------------------------------------------------------------------

pub struct ExprPool {
    exprs:                      HashMap<ExprKind, Rc<ExprEntry>>,
    ordered:                    Vec<Expr>
}


impl ExprPool {
    pub fn new() -> Self {
        Self {
            exprs:              HashMap::new(),
            ordered:            vec![],
        }
    }

    pub(super) fn iter(&self) -> std::slice::Iter<'_, Expr>
        { self.ordered.iter() }

    pub(super) fn intern(
        &mut self,
        kind:                   ExprKind,
        span:                   Span
    ) -> Expr {
        match self.exprs.entry(kind) {
            Entry::Occupied(existing) => Expr::new(existing.get().clone(), span),
            Entry::Vacant(vacant) => {
                let entry = Rc::new(ExprEntry::new(vacant.key().clone(), self.ordered.len(), span));
                vacant.insert(entry.clone());
                let expr = Expr::new(entry, span);
                self.ordered.push(expr.clone());
                expr
            }
        }
    }

    pub(super) fn number(
        &mut self,
        value:                  f64,
        span:                   Span
    ) -> Expr {
        self.intern(InstrKind::Number(value).into(), span)
    }

    pub(super) fn bool(
        &mut                    self,
        value:                  bool,
        span:                   Span
    ) -> Expr {
        self.intern(InstrKind::Bool(value).into(), span)
    }

    pub(super) fn arg(
        &mut                    self,
        index:                  usize,
        span:                   Span,
    ) -> Expr {
        self.intern(ExprKind::Arg(index), span)
    }
}


//-------------------------------------------------------------------------------------------------
