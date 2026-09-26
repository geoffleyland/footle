use std::fmt;


pub fn join_format<T>(v: &[T], sep: &str) -> String
where T: fmt::Display {
    v.iter().map(|v| format!("{v}")).collect::<Vec<_>>().join(sep)
}


#[cfg(any(feature = "dogfood", test))]
pub fn join_field_format<T1, T2, F>(v: &[T1], f: F, sep: &str) -> String
where
    T2:fmt::Display,
    F: Fn(&T1) -> &T2
{
    v.iter().map(|e| format!("{}", f(e))).collect::<Vec<_>>().join(sep)
}
