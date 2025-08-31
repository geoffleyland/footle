//! A cheap and nasty non-empty Vector implmentation
//!
//! This one only does the things I need in order to not have to unwrap `first` and `last` and play
//! nice with vectors and slices.
//! I'd much rather use someone else's crate, but the ones I found didn't implement a Deref into a
//! "normal" slice.  If I find such a crate, I'll replace this (maybe, since it's nice having few
//! dependencies)

use std::ops::Deref;

#[derive(Debug)]
pub struct Nev<T> {
    v:          Vec<T>,
}

impl<T> Nev<T> {
    pub fn new(v: T) -> Self { Self { v: vec![v] } }
    pub fn first(&self) -> &T { unsafe { self.v.first().unwrap_unchecked() } }
    pub fn last(&self) -> &T { unsafe { self.v.last().unwrap_unchecked() } }
    pub fn push(&mut self, v: T) { self.v.push(v); }
    pub fn iter(&self) -> std::slice::Iter<'_, T> { self.v.iter() }
    pub fn retain<F: FnMut(&T) -> bool>(&mut self, f: F) -> Result<(), ()> {
        self.v.retain(f);
        if self.v.is_empty() { Err(()) } else { Ok(()) }
    }
}


impl<T> TryFrom<Vec<T>> for Nev<T> {
    type Error = &'static str;

    fn try_from(v: Vec<T>) -> Result<Self, Self::Error> {
        if v.is_empty() {
            Err("Can't construct a nonempty vector from an empty Vec")
        } else {
            Ok(Self { v })
        }
    }
}


impl<T> From<Nev<T>> for Vec<T> {
    fn from(v: Nev<T>) -> Self { v.v }
}


impl<T> IntoIterator for Nev<T> {
    type IntoIter = <Vec<T> as IntoIterator>::IntoIter;
    type Item = T;

    fn into_iter(self) -> Self::IntoIter { self.v.into_iter() }
}


impl<'a, T> IntoIterator for &'a Nev<T> {
    type IntoIter = std::slice::Iter<'a, T>;
    type Item = &'a T;

    fn into_iter(self) -> Self::IntoIter { self.v.iter() }
}


impl<T> Deref for Nev<T> {
    type Target = [T];

    fn deref(&self) -> &[T] { self.v.as_slice() }
}


#[macro_export]
macro_rules! nev {
    () => {compile_error!("A nev cannot be empty")};
    ($h:expr, $( $x:expr ),* $(,)?) => {{
        let mut v = $crate::nonempty::Nev::new($h);
        $( v.push($x); )*
        v
    }};
    ($h:expr) => {
        $crate::nonempty::Nev::new($h)
    }
}
