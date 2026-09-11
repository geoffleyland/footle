use std::collections::HashMap;
use std::rc::Rc;

use crate::core::Span;
use super::expr::{ExprKind, ExprEntry, Expr};


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

    pub(super) fn len(&self) -> usize   { self.exprs.len() }
    pub(super) fn iter(&self) -> std::slice::Iter<'_, Expr>
        { self.ordered.iter() }

    pub(super) fn intern(
        &mut self,
        kind:                   ExprKind,
        span:                   Span
    ) -> Expr {
        let index = self.exprs.len();
        let entry = ExprEntry::new(kind.clone(), index, span);
        let expr =Expr::new(self.exprs.entry(kind).or_insert_with(|| Rc::new(entry)).clone());
        self.ordered.push(expr.clone());
        expr
    }

    pub(super) fn number(
        &mut self,
        value:                  f64,
        span:                   Span
    ) -> Expr {
        self.intern(ExprKind::Number(value), span)
    }

    pub(super) fn bool(
        &mut                    self,
        value:                  bool,
        span:                   Span
    ) -> Expr {
        self.intern(ExprKind::Bool(value), span)
    }

    pub(super) fn argument(
        &mut                    self,
        index:                  usize,
        span:                   Span,
        #[cfg(any(feature = "dogfood", test))]
        name:                   &str,
    ) -> Expr {
        #[cfg(any(feature = "dogfood", test))]
        let e = self.intern(ExprKind::Argument(index, name.to_string()), span);
        #[cfg(not(any(feature = "dogfood", test)))]
        let e = self.intern(ExprKind::Argument(index), span);
        e
    }
}


//-------------------------------------------------------------------------------------------------
