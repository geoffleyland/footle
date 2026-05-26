use std::collections::{HashMap, HashSet};
use std::fmt;
use std::mem::swap;

use crate::env::{Env, FunctionDef};
use crate::core::{BinaryOperator, ParseError, Span, Styleable, LineStyle};
use crate::{ast, vir, nev};
use crate::lex::Token;
use super::symbol_table::{AssignmentError, SymbolTable};
use super::expr_pool::ExprPool;
use super::expr::ExprKind;
use crate::parse_error;


//-------------------------------------------------------------------------------------------------

pub fn run(env: &Env, stmts: &[ast::Stmt]) -> (Block, Vec<ParseError>) {
    let mut p = Pass::new();

    for stmt in stmts {
        p.transform_stmt(env, stmt);
    }
    (p.flatten(), p.errors)
}


//-------------------------------------------------------------------------------------------------

pub struct Block {
    pub argument_count:     usize,
    pub instrs:             Vec<vir::Expr>,
    pub return_values:      Vec<vir::Expr>,
    pub return_span:        Span,
}


//------------------------------------------------------------------------------------------------

struct Pass {
    arguments:                  Vec<vir::Expr>, // Only contains vir::ExprKind::Arguments.
    stmts:                      Vec<vir::Stmt>,
    symbols:                    SymbolTable,
    exprs:                      ExprPool,
    errors:                     Vec<ParseError>,
}


impl Pass {
    fn new() -> Self {
        Self {
            arguments:          vec![],
            stmts:              vec![],
            symbols:            SymbolTable::new(),
            exprs:              ExprPool::new(),
            errors:             vec![],
        }
    }

    fn push_error(&mut self, error: ParseError) { self.errors.push(error); }

    fn transform_stmt(&mut self, env: &Env, stmt: &ast::Stmt) {
        match &stmt.kind {
            ast::StmtKind::Arguments(names) => {
                for (name, span) in names {
                    let expr = self.exprs.argument(self.arguments.len(), name, *span);
                    self.symbols.insert(false, name, *span, nev![expr.clone()]);
                    self.arguments.push(expr);
                }
            }
            ast::StmtKind::Return(exprs) => {
                let maybe_exprs = exprs.into_iter()
                    .map(|expr| self.transform_expr(env, expr).ok())
                    .collect::<Option<Vec<_>>>();
                if let Some(exprs) = maybe_exprs {
                    self.stmts.push(vir::Stmt::return_stmt(exprs.try_into().unwrap(), stmt.span));
                }
            }
            ast::StmtKind::Assignment(assignment) => {
                if !assignment.stmts.is_empty() {
                    self.symbols.push_scope();
                    for stmt in &assignment.stmts {
                        self.transform_stmt(env, stmt);
                    }
                }
                // Transform all the right-hand-sides before we execute any of the assignments,
                // otherwise, if it's a multiple assignment, later assignments will pick up earlier
                // values, and things like `a, b = b, a` won't work.  (the `b = a` bit will think
                // that `a` already equals `b` and the new `a` and `b` will be `b`)
                let transformed_assignments = assignment.assignments.iter()
                    .map(|(name, span, value)| (name, span, self.transform_expr(env, value)))
                    .collect::<Vec<_>>();

                if !assignment.stmts.is_empty() {
                    for (variable, span, values) in self.symbols.pop_scope() {
                        if !assignment.assignments.iter().any(|(name, ..)| variable.matches(name)) {
                            variable.try_push(self.symbols.scope_depth(), span, values)
                                .expect("internal compiler error: internal block assignment to immutable variable");
                        }
                    }
                }

                for (name, span, value) in transformed_assignments {
                    if let Ok(v) = value {
                        if assignment.declaration.is_declaring() {
                            self.symbols.insert(assignment.declaration.is_mutable(), name, *span,
                                nev![v]);
                        } else {
                            match self.symbols.try_push(name, *span, nev![v]) {
                                Err(AssignmentError::NoSuchVariable) => {
                                    parse_error!(self,
                                        format!("cannot find value '{name}' in this scope"), *span);
                                }
                                Err(AssignmentError::Immutable(declaration_span)) => {
                                    parse_error!(self,
                                        format!("cannot assign twice to the immutable variable '{name}'"), *span,
                                        format!("the declaration of '{name}' is here:"), declaration_span);
                                }
                                Ok(()) => {}
                            }
                        }
                    }
                }
            }
            ast::StmtKind::Exprs(..) => {}
        }
    }


    fn transform_expr(&mut self, env: &Env, expr: &ast::Expr) -> Result<vir::Expr, ()> {
        match expr.kind() {
            ast::ExprKind::Number(value) => {
                Ok(self.exprs.number(*value, *expr.span()))
            }
            ast::ExprKind::Bool(value) => {
                Ok(self.exprs.bool(*value, *expr.span()))
            }
            ast::ExprKind::Identifier(name) => {
                self.symbols.find(name).map_or_else(|| {
                    parse_error!(self, format!("cannot find value '{name}' in this scope"), *expr.span());
                    Err(())
                },
                    // FIXME: will need phi node handling when if/else is added
                    |binding| Ok(binding.values()[0].clone()))
            }
            ast::ExprKind::Binary(op, lhs, rhs) => {
                let span = lhs.span().union(rhs.span());
                let lhs = self.transform_expr(env, lhs)?;
                let rhs = self.transform_expr(env, rhs)?;
                Ok(self.exprs.intern(fold_binary(*op, lhs, rhs), span))
            }
            ast::ExprKind::Call(name, exprs) => {
                let Some(def) = env.module.functions.get(name) else {
                    parse_error!(self, format!("cannot find function '{name}' in this scope"), *expr.span());
                    return Err(())
                };
                if def.arguments as usize != exprs.len() {
                    parse_error!(self,
                        format!("function '{name}' called with {} arguments, expected {}",
                            exprs.len(),
                            env.module.functions[name].arguments,
                        ),
                        *expr.span());
                    return Err(())
                }
                let exprs = exprs.iter()
                    .map(|e| self.transform_expr(env, e).ok())
                    .collect::<Option<Vec<_>>>()
                    .ok_or(())?;
                // Ok(self.exprs.call(name, exprs.ok_or(())?, *expr.span()))
                Ok(self.exprs.intern(fold_call(name, def, exprs), *expr.span()))
            }
        }
    }

    pub fn flatten(&self) -> Block {
        let mut instrs = vec![];
        let mut emitted = HashSet::<usize>::new();

        for expr in &self.arguments {
            emit_expr(expr, &mut instrs, &mut emitted);
        }
        let (return_values, return_span) = match self.stmts.last() {
            Some(vir::Stmt{ span, kind: vir::StmtKind::Return(exprs)}) => {
                for expr in exprs { emit_expr(expr, &mut instrs, &mut emitted); }
                (exprs.to_vec(), *span)
            }
            _ => (vec![], Span::from((0, 0))),
        };
        Block { instrs, return_values, return_span, argument_count: self.arguments.len() }
    }
}


//-------------------------------------------------------------------------------------------------

/// Fold a binary expression.
///
/// If the left-hand-side and right-hand-side are both constants, then fold the constant, and
/// intern the result as a constant.
/// Alternatively one or both must be a variable or expression, and it'll help CSE to have
/// them in a standard form (so it thinks A + B is the same as B + A).  So:
///  * try to organise comparisons into Less and Less or Equal (rather that Greater)
///  * if the operator is commutable then:
///    * if there's a constant, try to get it on the right
///    * if they're both expressions, put the one with the lower index on the left.
fn fold_binary(op: BinaryOperator, lhs: vir::Expr, rhs: vir::Expr) -> ExprKind {
    // Get comparison operators the standard way around.
    let (op, mut reverse) = op.should_reverse().map_or(
        (op, false),
        |reverse_op| (reverse_op, true));

    if let &ExprKind::Number(mut lhs_value) = lhs.kind() {
        if let &ExprKind::Number(mut rhs_value) = rhs.kind() {
            // Both operands are constants: fold them.
            if reverse { swap(&mut lhs_value, &mut rhs_value); }
            return ExprKind::Number(op.eval_constants(lhs_value, rhs_value))
        }
        // LHS is a constant, and RHS is a variable/expression - try to get the RHS first.
        if op.is_commutable() { reverse = !reverse; }

    } else if !matches!(rhs.kind(), ExprKind::Number(_))
            && op.is_commutable()
            && rhs.pool_index() < lhs.pool_index() {
        // Both operands are variables/expressions - get the lowest-indexed one first.
        reverse = !reverse;
    }
    // Turns out it's quite hard to swap lhs and rhs, so just do it this way.
    if reverse  { ExprKind::Binary(op, rhs, lhs) }
    else        { ExprKind::Binary(op, lhs, rhs) }
}


fn all_constants(exprs: &[vir::Expr]) -> Option<Vec<f64>> {
    exprs.iter()
        .map(|e| if let ExprKind::Number(v) = e.kind() { Some(*v) } else { None })
        .collect()
}


fn fold_call(name: &str, def: &FunctionDef, exprs: Vec<vir::Expr>) -> ExprKind {
    if let Some(fold) = def.const_fold &&
        let Some(args) = all_constants(&exprs) {
        ExprKind::Number((fold)(&args))
    } else {
        ExprKind::Call(name.to_string(), exprs)
    }
}


//-------------------------------------------------------------------------------------------------
// Emit expressions for flattening

fn emit_expr(expr: &vir::Expr, instrs: &mut Vec<vir::Expr>, emitted: &mut HashSet<usize>) {
    if !emitted.contains(&expr.pool_index()) {
        match expr.kind() {
            vir::ExprKind::Binary(_, lhs, rhs) => {
                emit_expr(lhs, instrs, emitted);
                emit_expr(rhs, instrs, emitted);
            }
            vir::ExprKind::Call(_, exprs) => {
                for e in exprs { emit_expr(e, instrs, emitted); }
            }
            _ => {}
        }
        emitted.insert(expr.pool_index());
        instrs.push(expr.clone());
    }
}


//-------------------------------------------------------------------------------------------------
// Text output support

impl Styleable for Block {
    fn write<W: LineStyle>(&self, f: &mut fmt::Formatter, indent: u16, writer: &W) -> fmt::Result {
        let mut address_map = HashMap::<usize, usize>::new();
        for (address, expr) in self.instrs.iter().enumerate() {
            use ExprKind::*;
            address_map.insert(expr.pool_index(), address);
            let line = match &expr.kind() {
                Argument(..)                    => format!("{} I{address}", Token::Argument),
                Number(value)                   => format!("{} I{address} = {value}", Token::Local),
                Bool(value)                     => format!("{} I{address} = {value}", Token::Local),
                Binary(op, lhs, rhs)            => format!("{} I{address} = I{} {op} I{}",
                    Token::Local, address_map[&lhs.pool_index()], address_map[&rhs.pool_index()]),
                Call(name, exprs)               => format!("{} I{address} = {name}({})", Token::Local,
                    exprs.iter().map(|e| format!("I{}", address_map[&e.pool_index()])).collect::<Vec<_>>().join(", ")),
            };
            writer.writeln(f, indent, Some(*expr.span()), &line)?;
        }
        let line = format!("{} {}", Token::Return,
            self.return_values.iter().map(|e| format!("I{}", address_map[&e.pool_index()])).collect::<Vec<_>>().join(", "));
        writer.write(f, indent, Some(self.return_span), &line)
    }
}


impl std::fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_styled(f)
    }
}


//-------------------------------------------------------------------------------------------------
