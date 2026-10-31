use std::collections::{HashMap, hash_map::Entry};
use std::rc::Rc;

use typed_index_collections::TiVec;

use crate::core::Span;
use super::expr::{InstrKind, ExprKind, ExprEntry, Expr, ExprUse};


//-------------------------------------------------------------------------------------------------

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::From, derive_more::Into)]
pub struct PoolIndex(usize);


#[derive(Debug, Default)]
pub struct ExprPool {
    exprs:                      HashMap<ExprKind, Expr>,
    ordered:                    TiVec<PoolIndex, Expr>,
}


impl ExprPool {
    pub(super) fn len(&self) -> usize                                   { self.ordered.len() }
    pub(super) fn iter(&self) -> std::slice::Iter<'_, Expr>             { self.ordered.iter() }

    pub(super) fn intern(&mut self, kind: ExprKind, span: Span) -> ExprUse {
        match self.exprs.entry(kind) {
            Entry::Occupied(existing) => existing.get().used_at(span),
            Entry::Vacant(vacant) => {
                let expr = Rc::new(ExprEntry::new(vacant.key().clone(), self.ordered.next_key(), span));
                vacant.insert(expr.clone());
                self.ordered.push(expr.clone());
                expr.used_at(span)
            }
        }
    }

    pub(super) fn number(&mut self, value: f64, span: Span) -> ExprUse {
        self.intern(InstrKind::Number(value).into(), span)
    }

    pub(super) fn bool(&mut self, value: bool, span: Span) -> ExprUse {
        self.intern(InstrKind::Bool(value).into(), span)
    }

    pub(super) fn arg(&mut self, index: usize, span: Span) -> Expr {
        self.intern(ExprKind::Arg(index), span).into_expr()
    }
}


//-------------------------------------------------------------------------------------------------
