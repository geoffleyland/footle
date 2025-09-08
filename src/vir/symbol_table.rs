use std::rc::Rc;

use crate::core::Span;
use super::expr::Expr;
use super::variable::{Binding, Variable};


//-------------------------------------------------------------------------------------------------

/// A list of all the variables in scope.
///
/// We search the list backwards to look for a variable so the most recent ones come out first.
#[derive(Debug)]
pub struct SymbolTable {
    variables:                              Vec<Rc<Variable>>,
    scope_depth:                            usize,
}


pub enum AssignmentError {
    NoSuchVariable,
    Immutable(Span)
}


impl SymbolTable {
    pub const fn new() -> Self {
        Self {
            variables:                      vec![],
            scope_depth:                    0,
        }
    }


    pub fn scope_depth(&self) -> usize      { self.scope_depth }


    pub fn insert(&mut self, mutable: bool, name: &str, span: Span, values: Vec<Expr>) {
        // println!("SymbolTable::insert({self:p}, {name})");
        let name_version = self.variables.iter().rev().find(|v| v.matches(name))
            .map_or(1, |v| v.next_name_version());
        let v = Rc::new(Variable::new(mutable, name, name_version, span, self.scope_depth, values));
        self.variables.push(v);
        // println!("SymbolTable::insert({self:p}, {name}): {self:?}");
    }


    /// Open a new scope.  All we do is increment the scope depth.
    pub const fn push_scope(&mut self) {
        self.scope_depth += 1;
    }


    /// Pop a scope.
    ///
    /// Clear out all the versions of variables that were assigned in this scope, and return a list
    /// of all variables from parent scopes that were assigned to, and their final version.
    pub fn pop_scope(&mut self) -> Vec<(Rc<Variable>, Span, Vec<Expr>)> {
        // println!("SymbolTable::pop_scope({self:p})");
        self.scope_depth -= 1;
        self.variables.retain(|v| v.live_at(self.scope_depth));
        // println!("SymbolTable::pop_scope({self:p}): {self:?}");
        self.variables.iter_mut()
            .filter_map(|v| v.pop_scope(self.scope_depth + 1)
                .map(|(span, values)| (v.clone(), span, values)))
            .collect()
    }


    pub fn try_push(&mut self, name: &str, span: Span, values: Vec<Expr>) -> Result<(), AssignmentError> {
        if let Some(v) = self.variables.iter_mut().rev().find(|v| v.matches(name)) {
            v.try_push(self.scope_depth, span, values).map_err(
                |()| AssignmentError::Immutable(v.declaration_span()))
        } else { Err(AssignmentError::NoSuchVariable) }
    }


    /// Find the latest version of a variable with the given name.
    pub fn find(&self, name: &str) -> Option<Rc<Binding>> {
        self.variables.iter().rev().find(|v| v.matches(name)).map(|v| v.current_version())
    }
}


//-------------------------------------------------------------------------------------------------
