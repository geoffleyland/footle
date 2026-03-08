use std::collections::HashMap;

use crate::core::ParseError;
use crate::ast;
use crate::vir;
use super::symbol_table::{AssignmentError, SymbolTable};
use super::expr_pool::ExprPool;
use crate::parse_error;


//-------------------------------------------------------------------------------------------------

pub struct Block {
    arguments:              Vec<vir::Expr>,
    stmts:                  Vec<vir::Stmt>,
}


impl Block {
    fn new() -> Self {
        Self { arguments: vec![], stmts: vec![] }
    }
    pub fn stmts(&self) -> &[vir::Stmt] { &self.stmts }
}


//-------------------------------------------------------------------------------------------------

pub fn run(stmts: &[ast::Stmt]) -> (Block, Vec<ParseError>) {
    let mut p = Pass::new();

    for stmt in stmts {
        p.transform_stmt(stmt);
    }
    (p.block, p.errors)
}


//-------------------------------------------------------------------------------------------------

struct Pass {
    block:                      Block,
    symbols:                    SymbolTable,
    exprs:                      ExprPool,
    errors:                     Vec<ParseError>,
}


impl Pass {
    fn new() -> Self {
        Self {
            block:              Block::new(),
            symbols:            SymbolTable::new(),
            exprs:              ExprPool::new(),
            errors:             vec![],
        }
    }

    fn push_error(&mut self, error: ParseError) { self.errors.push(error); }

    fn transform_stmt(&mut self, stmt: &ast::Stmt) {
        match &stmt.kind {
            ast::StmtKind::Arguments(names) => {
                for (name, span) in names {
                    let expr = self.exprs.argument(self.block.arguments.len(), *span);
                    self.symbols.insert(false, name, *span, vec![expr.clone()]);
                    self.block.arguments.push(expr);
                }
            }
            ast::StmtKind::Return(exprs) => {
                let maybe_exprs = exprs.into_iter()
                    .map(|expr| self.transform_expr(expr).ok())
                    .collect::<Option<Vec<_>>>();
                if let Some(exprs) = maybe_exprs {
                    self.block.stmts.push(vir::Stmt::return_stmt(exprs.try_into().unwrap(), stmt.span));
                }
            }
            ast::StmtKind::Assignment(assignment) => {
                if !assignment.stmts.is_empty() {
                    self.symbols.push_scope();
                    for stmt in &assignment.stmts {
                        self.transform_stmt(stmt);
                    }
                }
                // Transform all the right-hand-sides before we execute any of the assignments,
                // otherwise, if it's a multiple assignment, later assignments will pick up earlier
                // values, and things like `a, b = b, a` won't work.  (the `b = a` bit will think
                // that `a` already equals `b` and the new `a` and `b` will be `b`)
                let transformed_assignments = assignment.assignments.iter()
                    .map(|(name, span, value)| (name, span, self.transform_expr(value)))
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
                                vec![v]);
                        } else {
                            match self.symbols.try_push(name, *span, vec![v]) {
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


    fn transform_expr(&mut self, expr: &ast::Expr) -> Result<vir::Expr, ()> {
        match expr.kind() {
            ast::ExprKind::Number(value) => {
                Ok(self.exprs.number(*value, *expr.span()))
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
                let lhs = self.transform_expr(lhs)?;
                let rhs = self.transform_expr(rhs)?;
                Ok(self.exprs.binary(*op, lhs, rhs, span))
            }
        }
    }
}


pub fn instructions(block: &Block) -> Vec<vir::Instr> {
    let mut instrs = vec![];
    let mut address_map = HashMap::<usize, usize>::new();
    for stmt in &block.stmts {
        match &stmt.kind {
            vir::StmtKind::Return(exprs) => {
                for expr in exprs {
                    emit_expr(expr, &mut instrs, &mut address_map);
                }
                let addresses = exprs.iter()
                    .map(|e| address_map[&e.pool_index()]).collect::<Vec<_>>().try_into().unwrap();
                instrs.push(vir::Instr{
                    kind: vir::InstrKind::Return(addresses), address: instrs.len(), span: stmt.span});
            }
        }
    }
    instrs
}


fn emit_expr(expr: &vir::Expr, instrs: &mut Vec<vir::Instr>, address_map: &mut HashMap<usize, usize>) {
    if !address_map.contains_key(&expr.pool_index()) {
        if let vir::ExprKind::Binary(_, lhs, rhs) = expr.kind() {
            emit_expr(lhs, instrs, address_map);
            emit_expr(rhs, instrs, address_map);
        }
        let address = instrs.len();
        address_map.insert(expr.pool_index(), address);
        let kind = match expr.kind() {
            vir::ExprKind::Number(v) => vir::InstrKind::Number(*v),
            vir::ExprKind::Argument(..) => vir::InstrKind::Argument,
            vir::ExprKind::Binary(op, lhs, rhs) =>
                vir::InstrKind::Binary(*op, address_map[&lhs.pool_index()], address_map[&rhs.pool_index()])
        };
        instrs.push(vir::Instr{kind, address, span: *expr.span()});
    }
}


//-------------------------------------------------------------------------------------------------
