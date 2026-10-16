use crate::core::BinaryOperator;
use super::expr::InstrKind;

#[allow(clippy::float_cmp)]
pub fn fold_constants(op: BinaryOperator, lhs_i: &InstrKind, rhs_i: &InstrKind) -> Option<InstrKind> {
    use BinaryOperator::*;
    use InstrKind::*;
    match (lhs_i, rhs_i) {
        (Number(lhs), Number(rhs)) => Some(match op {
            // Math operators
            Add                             => Number(lhs + rhs),
            Subtract                        => Number(lhs - rhs),
            Multiply                        => Number(lhs * rhs),
            Divide                          => Number(lhs / rhs),
            Power                           => Number(f64::powf(*lhs, *rhs)),
            Modulo                          => Number(lhs % rhs),

            // Comparison operators
            Equal                           => Bool(lhs == rhs),
            NotEqual                        => Bool(lhs != rhs),
            LessThan                        => Bool(lhs < rhs),
            LessEqual                       => Bool(lhs <= rhs),
            GreaterThan                     => Bool(lhs > rhs),
            GreaterEqual                    => Bool(lhs >= rhs),
        }),
        (Bool(l), Bool(r)) => match op {
            Equal    => Some(Bool(l == r)),
            NotEqual => Some(Bool(l != r)),
            _        => None,
        },
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

        let n = InstrKind::Number;
        let b = InstrKind::Bool;

        // Math operators
        assert_eq!(fold_constants(Add, &n(3.0), &n(4.0)),      Some(n(7.0)));
        assert_eq!(fold_constants(Subtract, &n(3.0), &n(4.0)), Some(n(-1.0)));
        assert_eq!(fold_constants(Multiply, &n(3.0), &n(4.0)), Some(n(12.0)));
        assert_eq!(fold_constants(Divide, &n(9.0), &n(3.0)),   Some(n(3.0)));
        assert_eq!(fold_constants(Power, &n(2.0), &n(10.0)),   Some(n(1024.0)));
        assert_eq!(fold_constants(Modulo, &n(3.0), &n(2.0)),   Some(n(1.0)));

        // Comparison operators on numbers — now fold to Bool, not 1.0/0.0
        assert_eq!(fold_constants(Equal, &n(1.0), &n(1.0)),        Some(b(true)));
        assert_eq!(fold_constants(Equal, &n(1.0), &n(2.0)),        Some(b(false)));
        assert_eq!(fold_constants(NotEqual, &n(1.0), &n(2.0)),     Some(b(true)));
        assert_eq!(fold_constants(NotEqual, &n(1.0), &n(1.0)),     Some(b(false)));
        assert_eq!(fold_constants(LessThan, &n(1.0), &n(2.0)),     Some(b(true)));
        assert_eq!(fold_constants(LessThan, &n(2.0), &n(1.0)),     Some(b(false)));
        assert_eq!(fold_constants(LessEqual, &n(1.0), &n(1.0)),    Some(b(true)));
        assert_eq!(fold_constants(LessEqual, &n(2.0), &n(1.0)),    Some(b(false)));
        assert_eq!(fold_constants(GreaterThan, &n(2.0), &n(1.0)),  Some(b(true)));
        assert_eq!(fold_constants(GreaterThan, &n(1.0), &n(2.0)),  Some(b(false)));
        assert_eq!(fold_constants(GreaterEqual, &n(1.0), &n(1.0)), Some(b(true)));
        assert_eq!(fold_constants(GreaterEqual, &n(1.0), &n(2.0)), Some(b(false)));

        // a > b  ≡  b < a  (not b <= a)
        assert_eq!(fold_constants(LessThan, &n(2.0), &n(1.0)), Some(b(1.0_f64 > 2.0)));
        assert_eq!(fold_constants(LessThan, &n(1.0), &n(2.0)), Some(b(2.0_f64 > 1.0)));
        assert_eq!(fold_constants(LessThan, &n(1.0), &n(1.0)), Some(b(1.0_f64 > 1.0)));

        // a >= b  ≡  b <= a  (not b < a)
        assert_eq!(fold_constants(LessEqual, &n(2.0), &n(1.0)), Some(b(1.0_f64 >= 2.0)));
        assert_eq!(fold_constants(LessEqual, &n(1.0), &n(2.0)), Some(b(2.0_f64 >= 1.0)));
        assert_eq!(fold_constants(LessEqual, &n(1.0), &n(1.0)), Some(b(1.0_f64 >= 1.0)));

        // Bool operands — coverage eval_constants never had
        assert_eq!(fold_constants(Equal, &b(true), &b(false)),    Some(b(false)));
        assert_eq!(fold_constants(Equal, &b(true), &b(true)),     Some(b(true)));
        assert_eq!(fold_constants(NotEqual, &b(true), &b(false)), Some(b(true)));
        assert_eq!(fold_constants(NotEqual, &b(true), &b(true)),  Some(b(false)));

        // Operators that don't apply to bools: decline to fold, don't panic
        assert_eq!(fold_constants(Add, &b(true), &b(false)), None);

        // Mismatched operand kinds: decline to fold, let the type pass catch it
        assert_eq!(fold_constants(Equal, &n(1.0), &b(true)), None);
    }
}
