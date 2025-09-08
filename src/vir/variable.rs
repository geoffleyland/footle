use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use crate::core::{Nev, Span};
use super::expr::Expr;

//-------------------------------------------------------------------------------------------------

#[derive(Debug)]
pub struct Binding {
    name:                                   String,
    name_version:                           usize,
    value_version:                          usize,
    assignment_span:                        Span,
    assignment_scope:                       usize,
    values:                                 Vec<Expr>,
}


impl Binding {
    fn new_first(
        name:                               &str,
        name_version:                       usize,
        assignment_span:                    Span,
        assignment_scope:                   usize,
        values:                             Vec<Expr>) -> Self {
        Self{ name: name.into(), name_version,
            value_version: 1, assignment_span, assignment_scope, values}
    }

    fn new_next(
        v:                                  &Variable,
        value_version:                      usize,
        assignment_span:                    Span,
        assignment_scope:                   usize,
        values:                             Vec<Expr>) -> Self {
        Self{ name: v.name().into(), name_version: v.name_version(),
            value_version, assignment_span, assignment_scope, values}
    }

    pub fn name(&self) -> &str              { &self.name }
    fn name_version(&self) -> usize         { self.name_version }
    fn value_version(&self) -> usize        { self.value_version }
    fn assignment_scope(&self) -> usize     { self.assignment_scope }
    pub fn assignment_span(&self) -> Span   { self.assignment_span }
    pub fn values(&self) -> &Vec<Expr>      { &self.values }
}


impl fmt::Display for Binding {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.name_version() == 1usize && self.value_version() == 1usize {
            write!(f, "{}", self.name())
        } else {
            write!(f, "{}@{}_{}", self.name(), self.name_version(), self.value_version())
        }
    }
}


//-------------------------------------------------------------------------------------------------

#[derive(Debug)]
pub struct Variable {
    mutable:                                bool,
    name:                                   String,
    name_version:                           usize,
    declaration_span:                       Span,
    value_version_count:                    RefCell<usize>,
    versions:                               RefCell<Nev<Rc<Binding>>>,
}


impl Variable {
    pub fn new(
            mutable:                        bool,
            name:                           &str,
            name_version:                   usize,
            declaration_span:               Span,
            declaration_scope:              usize,
            values:                         Vec<Expr>) -> Self {
        Self{mutable, name: name.into(), name_version, declaration_span,
            value_version_count: RefCell::new(1),
            versions: RefCell::new(Nev::new(Rc::new(Binding::new_first(name, name_version,
                declaration_span, declaration_scope, values))))
        }
    }

    fn name(&self) -> &str                  { &self.name }
    fn name_version(&self) -> usize         { self.name_version }
    pub fn declaration_span(&self) -> Span  { self.declaration_span }
    fn declaration_scope(&self) -> usize    { self.versions.borrow().first().assignment_scope }
    pub fn current_version(&self) -> Rc<Binding>
                                            { self.versions.borrow().last().clone() }
//    pub fn current_values(&self) -> &Vec<Expr> { &self.versions.borrow().last().values }

    pub fn matches(&self, n: &str) -> bool  { self.name() == n }
    pub fn next_name_version(&self) -> usize{ self.name_version() + 1 }
    pub fn live_at(&self, s: usize) -> bool { self.declaration_scope() <= s }

    pub fn try_push(&self, scope_depth: usize, span: Span, values: Vec<Expr>) -> Result<(), ()> {
        if self.mutable {
            *self.value_version_count.borrow_mut() += 1;
            let binding = Binding::new_next(self, *self.value_version_count.borrow(), span, scope_depth, values);
            self.versions.borrow_mut().push(Rc::new(binding));
            Ok(())
        } else { Err(()) }
    }

    pub fn pop_scope(&self, scope_index: usize) -> Option<(Span, Vec<Expr>)> {
        let cv = self.versions.borrow().last().clone();
        if cv.assignment_scope() == scope_index {
            self.versions.borrow_mut().retain(|v| v.assignment_scope < scope_index)
                .expect("internal compiler error: no value versions left for live variable");
            Some((cv.assignment_span(), cv.values().clone()))
        } else { None }
    }
}


//-------------------------------------------------------------------------------------------------
