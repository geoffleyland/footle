use std::collections::HashMap;
use std::rc::Rc;

use super::expr::{ExprKind, ExprEntry, Expr};

#[cfg(any(feature = "dogfood", test))]
use crate::core::Span;


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


    pub(super) fn intern(
        &mut self,
        kind:                   ExprKind,
        #[cfg(any(feature = "dogfood", test))]
        span:                   Span
    ) -> Expr {
        let index = self.exprs.len();
        let entry = ExprEntry::new(kind.clone(), index,
            #[cfg(any(feature = "dogfood", test))]
            span
        );
        Expr::new(self.exprs.entry(kind).or_insert_with(|| Rc::new(entry)).clone())
    }

    pub(super) fn number(
        &mut self,
        value:                  f64,
        #[cfg(any(feature = "dogfood", test))]
        span:                   Span
    ) -> Expr {
        self.intern(ExprKind::Number(value),
            #[cfg(any(feature = "dogfood", test))]
            span
        )
    }

    pub(super) fn argument(
        &mut                    self,
        index:                  usize,
        #[cfg(any(feature = "dogfood", test))]
        name:                   &str,
        #[cfg(any(feature = "dogfood", test))]
        span:                   Span
    ) -> Expr {
        #[cfg(any(feature = "dogfood", test))]
        let e = self.intern(ExprKind::Argument(index, name.to_string()), span);
        #[cfg(not(any(feature = "dogfood", test)))]
        let e = self.intern(ExprKind::Argument(index));
        e
    }
}


//-------------------------------------------------------------------------------------------------
