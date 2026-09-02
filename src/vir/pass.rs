use std::collections::{HashMap, HashSet};
use std::fmt;

use crate::env::{Env, FunctionDef};
use crate::core::{BinaryOperator, ParseError, Span, Styleable, LineStyle};
use crate::{ast, vir, nev};
use crate::lex::Token;
use super::symbol_table::{AssignmentError, SymbolTable};
use super::expr_pool::ExprPool;
use super::expr::ExprKind;
use super::operators::fold_constants;
use crate::parse_error;


//-------------------------------------------------------------------------------------------------

pub fn run(env: &Env, stmts: &[ast::Stmt]) -> (Block, Vec<ParseError>) {
    let mut p = Pass::new();

    for stmt in stmts {
        p.transform_stmt(env, stmt);
    }
    let block = p.flatten();
    if p.errors.is_empty() {
        let _types = p.types();
    }
    (block, p.errors)
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
    reassignments:              Vec<(String, vir::Expr, vir::Expr, Span)>,
}


impl Pass {
    fn new() -> Self {
        Self {
            arguments:          vec![],
            stmts:              vec![],
            symbols:            SymbolTable::new(),
            exprs:              ExprPool::new(),
            errors:             vec![],
            reassignments:      vec![],
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
                            let new_value = values[0].clone();
                            let initial_values = variable.try_push(self.symbols.scope_depth(), span, values)
                                .expect("internal compiler error: internal block assignment to immutable variable");
                            self.reassignments.push((variable.name().into(), initial_values[0].clone(), new_value, span));
                        }
                    }
                }

                for (name, span, value) in transformed_assignments {
                    if let Ok(v) = value {
                        if assignment.declaration.is_declaring() {
                            self.symbols.insert(assignment.declaration.is_mutable(), name, *span,
                                nev![v]);
                        } else {
                            match self.symbols.try_push(name, *span, nev![v.clone()]) {
                                Err(AssignmentError::NoSuchVariable) => {
                                    parse_error!(self,
                                        format!("cannot find value '{name}' in this scope"), *span);
                                }
                                Err(AssignmentError::Immutable(declaration_span)) => {
                                    parse_error!(self,
                                        format!("cannot assign twice to the immutable variable '{name}'"), *span,
                                        format!("the declaration of '{name}' is here:"), declaration_span);
                                }
                                Ok(initial_values) => {
                                    self.reassignments.push((name.into(), initial_values[0].clone(), v, *span));
                                }
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


    fn types(&mut self) -> Vec<TypeInfo> {
        let mut typer = Typer::new(self.exprs.len());
        let instrs: Vec<vir::Expr> = self.exprs.iter().cloned().collect();
        for instr in instrs {
            if let Err(TypeConflict{expected, expected_span, found, found_span}) =
                typer.type_instr(&instr) {
                parse_error!(self,
                    format!("Expected `{expected}`, got `{found}`"),
                    *instr.span(),
                    format!("`{expected}` was set here:"),
                    expected_span,
                    format!("`{found}` was set here:"),
                    found_span
                );
            }
        }

        for (name, old, new, span) in self.reassignments.clone() {
            if let Err(TypeConflict{expected, expected_span, found, found_span}) =
                typer.type_union(old.pool_index(), new.pool_index(), &span) {
                    parse_error!(self,
                        format!("Reassignment of `{name}` from `{expected}` to `{found}`"),
                        span,
                        format!("`{name}` was `{expected}` here:"),
                        expected_span,
                        format!("The rhs is `{found}` here:"),
                        found_span
                    );
                }
        }
        typer.extract_types()
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
    // See if we can fold to a constant
    if let Some(value) = fold_constants(op, lhs.kind(), rhs.kind()) { return value }

    // Get comparison operators the standard way around.
    let (op, mut reverse) = op.should_reverse().map_or(
        (op, false),
        |reverse_op| (reverse_op, true));

    // LHS is a constant, and RHS is a variable/expression - try to get the RHS first.
    if lhs.is_constant() && !rhs.is_constant() && op.is_commutable() { reverse = !reverse }

    if !lhs.is_constant() && !rhs.is_constant() && op.is_commutable()
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
// Type Figurer-outer

#[derive(Debug, Copy, Clone, PartialEq)]
enum TypeInfo { Unknown, F64, Bool }

impl fmt::Display for TypeInfo {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            Self::Unknown           => "unknown",
            Self::F64               => "float",
            Self::Bool              => "bool"
        };
        write!(fmt, "{s}")
    }
}


#[derive(Debug, Copy, Clone)]
enum TypeNode { Root(TypeInfo, u8), Pointer(usize) }

impl TypeNode {
    fn new() -> Self { Self::Root(TypeInfo::Unknown, 0) }
}

#[derive(Debug, Copy, Clone)]
struct TypeRecord {
    node:           TypeNode,
    first_span:     Option<Span>
}

impl TypeRecord {
    fn new() -> Self { Self { node: TypeNode::new(), first_span: None }}
}


struct TypeConflict {
    expected:       TypeInfo,
    expected_span:  Span,
    found:          TypeInfo,
    found_span:     Span
}

impl TypeConflict {
    fn new(expected: TypeInfo, expected_span: Span, found: TypeInfo, found_span: Span) -> Self {
        Self{ expected, expected_span, found, found_span }
    }
}


struct Typer {
    records:        Vec<TypeRecord>
}

impl Typer {
    fn new(len: usize) -> Self { Self { records: vec![TypeRecord::new(); len]}}

    fn extract_types(&mut self) -> Vec<TypeInfo> {
        (1..self.records.len()).map(|i| {
            let root = self.find_root(i);
            match self.records[root].node {
                TypeNode::Pointer(..)   => panic!("internal compiler error: not all types were resolved"),
                TypeNode::Root(t, ..)   => t
            }
        }).collect()
    }

    fn type_instr(&mut self,  instr: &vir::Expr) -> Result<(), TypeConflict> {
        let pool_index = instr.pool_index();
        let span = instr.span();
        match instr.kind() {
            ExprKind::Number(..)            => self.set_type(pool_index, TypeInfo::F64, span)?,
            ExprKind::Bool(..)              => self.set_type(pool_index, TypeInfo::Bool, span)?,
            ExprKind::Argument(..) |
            ExprKind::Call(..)              => {},
            ExprKind::Binary(op, lhs, rhs)  => {
                match op {
                    BinaryOperator::Add | BinaryOperator::Subtract |
                    BinaryOperator::Multiply | BinaryOperator::Divide |
                    BinaryOperator::Modulo | BinaryOperator::Power => {
                        self.set_type(pool_index, TypeInfo::F64, span)?;
                        self.set_type(lhs.pool_index(), TypeInfo::F64, span)?;
                        self.set_type(rhs.pool_index(), TypeInfo::F64, span)?;
                        self.type_union(pool_index, lhs.pool_index(), span)?;
                        self.type_union(pool_index, rhs.pool_index(), span)?;
                    }
                    BinaryOperator::LessEqual | BinaryOperator::LessThan |
                    BinaryOperator::GreaterEqual | BinaryOperator::GreaterThan => {
                        self.set_type(lhs.pool_index(), TypeInfo::F64, span)?;
                        self.set_type(rhs.pool_index(), TypeInfo::F64, span)?;
                        self.set_type(pool_index, TypeInfo::Bool, span)?;
                        self.type_union(lhs.pool_index(), rhs.pool_index(), span)?;
                    }
                    BinaryOperator::Equal | BinaryOperator::NotEqual => {
                        self.set_type(pool_index, TypeInfo::Bool, span)?;
                        self.type_union(lhs.pool_index(), rhs.pool_index(), span)?;
                    }
                }
            }
        }
        Ok(())
    }


    fn type_union(&mut self, index_1: usize, index_2: usize, span: &Span) -> Result<(), TypeConflict> {
        use TypeNode::*;
        self.records[index_1].first_span.get_or_insert(*span);
        self.records[index_2].first_span.get_or_insert(*span);
        let root_1 = self.find_root(index_1);
        let root_2 = self.find_root(index_2);
        if root_1 == root_2 { return Ok(()) }
        match (self.records[root_1].node, self.records[root_2].node) {
            (Root(type_1, rank_1), Root(type_2, rank_2)) if type_1 == type_2 => {
                if rank_1 > rank_2 {
                    self.records[root_2].node = Pointer(root_1);
                } else {
                    self.records[root_1].node = Pointer(root_2);
                    self.records[root_2].node = Root(type_2, rank_2 + u8::from(rank_1 == rank_2));
                }
            }
            (Root(..), Root(TypeInfo::Unknown, ..)) => {
                self.records[root_2].node = Pointer(root_1);
            }
            (Root(TypeInfo::Unknown, ..), Root(..)) => {
                self.records[root_1].node = Pointer(root_2);
            }
            (Pointer(_), _) | (_, Pointer(_)) => {
                panic!("internal compiler error: find_root did not find a root")
            }
            (Root(type_1, ..), Root(type_2, ..)) => {
                return Err(TypeConflict::new(
                    type_1, self.records[index_1].first_span.expect("internal compiler error"),
                    type_2, self.records[index_2].first_span.expect("internal compiler error")))
            }
        }
        Ok(())
    }


    fn set_type(
        &mut self,
        index:          usize,
        the_type:       TypeInfo,
        span:           &Span
    ) -> Result<(), TypeConflict> {
        self.records[index].first_span.get_or_insert(*span);
        let root = self.find_root(index);
        let current_type_node = self.records[root].node;
        match current_type_node {
            TypeNode::Pointer(..) => panic!("internal compiler error: find_root did not find a root"),
            TypeNode::Root(current_type, rank) => {
                match (current_type, the_type) {
                    (TypeInfo::Unknown, _) => {
                        self.records[root].node = TypeNode::Root(the_type, rank);
                    }
                    (x, y) if x == y => {}
                    _ => { return Err(TypeConflict::new(
                        current_type, self.records[index].first_span.expect("internal compiler error"),
                        the_type, *span))
                    }
                }
            }
        }
        Ok(())
    }


    fn find_root(&mut self, index: usize) -> usize {
        if let TypeRecord{ node: TypeNode::Pointer(next), .. } = self.records[index] {
            let root = self.find_root(next);
            self.records[index].node = TypeNode::Pointer(root);
            root
        } else { index }
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
