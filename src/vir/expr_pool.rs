use std::collections::HashMap;
use std::mem::swap;
use std::rc::Rc;

use crate::core::{BinaryOperator, Span};
use super::expr::{ExprKind, ExprEntry, Expr};
use super::variable::Binding;


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


    fn intern(&mut self, kind: ExprKind, span: Span) -> Expr {
        let index = self.exprs.len();
        Expr::new(self.exprs.entry(kind.clone())
            .or_insert_with(|| Rc::new(ExprEntry::new(kind, index, span))).clone())
    }

    /// Intern a binary expression.
    ///
    /// If the left-hand-side and right-hand-side are both constants, then fold the constant, and
    /// intern the result as a constant.
    /// Alternatively one or both must be a variable or expression, and it'll help CSE to have
    /// them in a standard form (so it thinks A + B is the same as B + A).  So:
    ///  * try to organise comparisons into Less and Less or Equal (rather that Greater)
    ///  * if the operator is commutable then:
    ///    * if there's a constant, try to get it on the right
    ///    * if they're both expressions, put the one with the lower index on the left.
    pub fn binary(&mut self, op: BinaryOperator, lhs: Expr, rhs: Expr, span: Span) -> Expr {
        // Get comparison operators the standard way around.
        let (op, mut reverse) = op.should_reverse().map_or(
            (op, false),
            |reverse_op| (reverse_op, true));

        if let &ExprKind::Number(mut lhs_value) = lhs.kind() {
            if let &ExprKind::Number(mut rhs_value) = rhs.kind() {
                // Both operands are constants: fold them.
                if reverse { swap(&mut lhs_value, &mut rhs_value); }
                return self.intern(ExprKind::Number(op.eval_constants(lhs_value, rhs_value)), span)
            }
            // LHS is a constant, and RHS is a variable/expression - try to get the RHS first.
            if op.is_commutable() { reverse = !reverse; }

        } else if !matches!(rhs.kind(), ExprKind::Number(_))
                && op.is_commutable()
                && rhs.index() < lhs.index() {
            // Both operands are variables/expressions - get the lowest-indexed one first.
            reverse = !reverse;
        }
        // Turns out it's quite hard to swap lhs and rhs, so just do it this way.
        if reverse  { self.intern(ExprKind::Binary(op, rhs, lhs), span) }
        else        { self.intern(ExprKind::Binary(op, lhs, rhs), span) }
    }


    pub fn number(&mut self, value: f64, span: Span) -> Expr {
        self.intern(ExprKind::Number(value), span)
    }


    pub fn identifier(&mut self, binding: Rc<Binding>, span: Span) -> Expr {
        self.intern(ExprKind::Identifier(binding), span)
    }
}


//-------------------------------------------------------------------------------------------------
