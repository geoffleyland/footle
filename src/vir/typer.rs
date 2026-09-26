use std::fmt;

use crate::core::{BinaryOperator, ParseError};
use crate::env::Env;
use crate::vir;
use super::expr::ExprKind;
use crate::parse_error;
use crate::core::Span;

//-------------------------------------------------------------------------------------------------

struct TypeErrors(Vec<ParseError>);

impl TypeErrors {
    fn push_error(&mut self, error: ParseError) { self.0.push(error); }
}


pub fn infer_types(
    block:              &vir::Block,
    argument_types:     &[TypeInfo],
    env:                &Env,
) -> Result<Vec<TypeInfo>, Vec<ParseError>> {
    let mut errors = TypeErrors(vec![]);
    let mut typer = Typer::new(block.exprs.len());

    for (argument, ty) in block.arguments.iter().zip(argument_types) {
        assert!(typer.set_type(argument.pool_index(), *ty, argument.span()).is_ok(),
            "internal compiler error: type conflict setting argument type");
    }

    for expr in &block.exprs {
        if let Err(TypeConflict{expected, expected_span, found, found_span}) =
            typer.type_instr(expr, env) {
            parse_error!(errors,
                format!("Expected `{expected}`, got `{found}`"),
                *expr.span(),
                format!("`{expected}` was set here:"),
                expected_span,
                format!("`{found}` was set here:"),
                found_span
            );
        }
    }

    for (name, old, new, span) in &block.reassignments {
        if let Err(TypeConflict{expected, expected_span, found, found_span}) =
            typer.type_union(old.pool_index(), new.pool_index(), span) {
                parse_error!(errors,
                    format!("Reassignment of `{name}` from `{expected}` to `{found}`"),
                    *span,
                    format!("`{name}` was `{expected}` here:"),
                    expected_span,
                    format!("The rhs is `{found}` here:"),
                    found_span
                );
            }
    }

    if errors.0.is_empty() { Ok(typer.extract_types()) } else { Err(errors.0) }
}


//-------------------------------------------------------------------------------------------------
// Type Figurer-outer

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum TypeInfo { Unknown, F64, Bool }

impl fmt::Display for TypeInfo {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            Self::Unknown           => "unknown",
            Self::F64               => "f64",
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
        (0..self.records.len()).map(|i| {
            let root = self.find_root(i);
            match self.records[root].node {
                TypeNode::Pointer(..)   => panic!("internal compiler error: not all types were resolved"),
                TypeNode::Root(t, ..)   => t
            }
        }).collect()
    }

    fn type_instr(&mut self,  instr: &vir::Expr, env: &Env) -> Result<(), TypeConflict> {
        let pool_index = instr.pool_index();
        let span = instr.span();
        match instr.kind() {
            ExprKind::Number(..)            => self.set_type(pool_index, TypeInfo::F64, span)?,
            ExprKind::Bool(..)              => self.set_type(pool_index, TypeInfo::Bool, span)?,
            ExprKind::Argument(..)          => {}
            ExprKind::Call(name, exprs)=> {
                // vir already checked the function exists and the arguments list is the right
                // length
                let Some(def) = env.module.functions.get(name) else {
                    panic!("internal compiler error: unknown function `{name}`")
                };
                for (e, ty) in exprs.iter().zip(&def.argument_types) {
                    self.set_type(e.pool_index(), *ty, e.span())?;
                }
                // We only cope with one result at the moment.
                assert_eq!(def.result_types.len(), 1);
                self.set_type(pool_index, def.result_types[0], span)?;
            },
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
