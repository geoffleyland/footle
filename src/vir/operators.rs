use crate::core::BinaryOperator;
use super::expr::ExprKind;

#[allow(clippy::float_cmp)]
pub fn fold_constants(op: BinaryOperator, lhs_expr: &ExprKind, rhs_expr: &ExprKind) -> Option<ExprKind> {
    use BinaryOperator::*;
    match (lhs_expr, rhs_expr) {
        (ExprKind::Number(lhs), ExprKind::Number(rhs)) => Some(match op {
            // Math operators
            Add                             => ExprKind::Number(lhs + rhs),
            Subtract                        => ExprKind::Number(lhs - rhs),
            Multiply                        => ExprKind::Number(lhs * rhs),
            Divide                          => ExprKind::Number(lhs / rhs),
            Power                           => ExprKind::Number(f64::powf(*lhs, *rhs)),
            Modulo                          => ExprKind::Number(lhs % rhs),

            // Comparison operators
            Equal                           => ExprKind::Number(if lhs == rhs {1.0} else {0.0}),
            NotEqual                        => ExprKind::Number(if lhs == rhs {0.0} else {1.0}),
            LessThan                        => ExprKind::Number(if lhs < rhs {1.0} else {0.0}),
            LessEqual                       => ExprKind::Number(if lhs <= rhs {1.0} else {0.0}),
            GreaterThan                     => ExprKind::Number(if lhs > rhs {1.0} else {0.0}),
            GreaterEqual                    => ExprKind::Number(if lhs >= rhs {1.0} else {0.0}),
        }),
        _ => None,
    }
}


//-------------------------------------------------------------------------------------------------

#[cfg(test)]
mod test {
    use super::*;
    use crate::core::BinaryOperator;

    #[test]
    fn test_fold_constants() {
        use BinaryOperator::*;

        let n = ExprKind::Number;

        // Math operators
        assert_eq!(fold_constants(Add, &n(3.0), &n(4.0)),      Some(n(7.0)));
        assert_eq!(fold_constants(Subtract, &n(3.0), &n(4.0)), Some(n(-1.0)));
        assert_eq!(fold_constants(Multiply, &n(3.0), &n(4.0)), Some(n(12.0)));
        assert_eq!(fold_constants(Divide, &n(9.0), &n(3.0)),   Some(n(3.0)));
        assert_eq!(fold_constants(Power, &n(2.0), &n(10.0)),   Some(n(1024.0)));
        assert_eq!(fold_constants(Modulo, &n(3.0), &n(2.0)),   Some(n(1.0)));

        // Comparison operators on numbers — now fold to Bool, not 1.0/0.0
        assert_eq!(fold_constants(Equal, &n(1.0), &n(1.0)),        Some(n(1.0)));
        assert_eq!(fold_constants(Equal, &n(1.0), &n(2.0)),        Some(n(0.0)));
        assert_eq!(fold_constants(NotEqual, &n(1.0), &n(2.0)),     Some(n(1.0)));
        assert_eq!(fold_constants(NotEqual, &n(1.0), &n(1.0)),     Some(n(0.0)));
        assert_eq!(fold_constants(LessThan, &n(1.0), &n(2.0)),     Some(n(1.0)));
        assert_eq!(fold_constants(LessThan, &n(2.0), &n(1.0)),     Some(n(0.0)));
        assert_eq!(fold_constants(LessEqual, &n(1.0), &n(1.0)),    Some(n(1.0)));
        assert_eq!(fold_constants(LessEqual, &n(2.0), &n(1.0)),    Some(n(0.0)));
        assert_eq!(fold_constants(GreaterThan, &n(2.0), &n(1.0)),  Some(n(1.0)));
        assert_eq!(fold_constants(GreaterThan, &n(1.0), &n(2.0)),  Some(n(0.0)));
        assert_eq!(fold_constants(GreaterEqual, &n(1.0), &n(1.0)), Some(n(1.0)));
        assert_eq!(fold_constants(GreaterEqual, &n(1.0), &n(2.0)), Some(n(0.0)));

        // a > b  ≡  b < a  (not b <= a)
        assert_eq!(fold_constants(LessThan, &n(2.0), &n(1.0)), Some(n(0.0)));
        assert_eq!(fold_constants(LessThan, &n(1.0), &n(2.0)), Some(n(1.0)));
        assert_eq!(fold_constants(LessThan, &n(1.0), &n(1.0)), Some(n(0.0)));

        // a >= b  ≡  b <= a  (not b < a)
        assert_eq!(fold_constants(LessEqual, &n(2.0), &n(1.0)), Some(n(0.0)));
        assert_eq!(fold_constants(LessEqual, &n(1.0), &n(2.0)), Some(n(1.0)));
        assert_eq!(fold_constants(LessEqual, &n(1.0), &n(1.0)), Some(n(1.0)));
    }
}
