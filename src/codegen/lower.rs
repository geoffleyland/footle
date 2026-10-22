use std::collections::HashMap;

use crate::core::Span;
use crate::vir;
use super::{Code, Instr, Arg, Value, ValueSlot, value::Operand, Type, Constant, MachineReg, REGS};


//-------------------------------------------------------------------------------------------------

/// The function signature: established in lower and passed right through to the compiled function
pub struct Signature {
    pub(super) args:                        Vec<Arg>,
    pub(super) return_types:                Vec<Type>,
}

/// The constant pool - floats and function pointers.
pub struct Pool {
    pub(super) constants:                   Vec<Constant>,
    pub(super) functions:                   Vec<String>,
}

pub(super) fn run(
    input:                                  &vir::Block,
    types:                                  &[vir::TypeInfo],
) -> (Vec<Instr>, usize, Signature, Pool) {
    let mut builder = Builder::default();
    builder.lower_args(input, types);
    for instr in &input.instrs {
        // Call arch-specific instruction selection.
        let value = builder.select_instr(instr, types);
        builder.operand_map.insert(instr.pool_index, value.into_operand(&builder));
    }
    builder.lower_return(input, types);

    (builder.instrs, builder.slot_count,
    Signature {
        args:                               builder.args,
        return_types:                       builder.return_types,
    },
    Pool {
        constants:                          builder.constants,
        functions:                          builder.functions.into_iter().map(|(n, _)| n).collect()
    })
}


//-------------------------------------------------------------------------------------------------

pub(super) trait IntoOperand { fn into_operand(self, builder: &Builder) -> Operand; }

impl IntoOperand for Value {
    fn into_operand(self, _: &Builder) -> Operand { Operand::Reg(self) }
}

impl IntoOperand for &vir::Expr {
    fn into_operand(self, builder: &Builder) -> Operand {
        builder.operand_map[&self.pool_index()].clone()
    }
}

impl IntoOperand for Operand {
    fn into_operand(self, _: &Builder) -> Self { self }
}

macro_rules! operands {
    ($builder:expr, $( $op:expr ),* $(,)?) => {
        vec![$( $crate::codegen::lower::IntoOperand::into_operand($op, $builder) ),*]
    };
}
pub(super) use operands;


//-------------------------------------------------------------------------------------------------

#[derive(Default)]
pub struct Builder {
    args:                                   Vec<Arg>,
    instrs:                                 Vec<Instr>,
    slot_count:                             usize,
    pub(super) return_types:                Vec<Type>,
    pub(super) constants:                   Vec<Constant>,
    operand_map:                            HashMap<usize, Operand>,
    pub(super) functions:                   Vec<(String, Value)>,
}

impl Builder {
    fn new_slot(&mut self) -> ValueSlot {
        let slot = ValueSlot::from(self.slot_count);
        self.slot_count += 1;
        slot
    }

    fn lower_args(&mut self, input: &vir::Block, types: &[vir::TypeInfo]) {
        for vir_arg in &input.args {
            let ty = Type::from(types[vir_arg.pool_index]);
            let slot = self.new_slot();
            self.args.push(Arg{ slot, ty, span: vir_arg.definition_span });
            let operand = Operand::Reg(Value::Arg(slot, ty));
            self.operand_map.insert(vir_arg.pool_index, operand);
        }
    }

    pub(super) fn emit_instr(
        &mut self,
        ty:                                     Type,
        code:                                   &'static Code,
        operands:                               Vec<Operand>,
        span:                                   Span,
    ) -> Value {
        self.emit_abi_instr(ty, code, operands, vec![], None, span)
    }

    pub(super) fn emit_abi_instr(
        &mut self,
        ty:                                     Type,
        code:                                   &'static Code,
        operands:                               Vec<Operand>,
        abi_args:                               Vec<(Value, MachineReg)>,
        abi_output:                             Option<MachineReg>,
        span:                                   Span,
    ) -> Value {
        let slot = self.new_slot();
        self.instrs.push(Instr::new(slot, Some(ty), code, operands, abi_args, abi_output, span));
        Value::Instr(slot, ty)
    }

    pub fn lower_return(&mut self, input: &vir::Block, types: &[vir::TypeInfo]) {
        let return_values = self.exprs_to_abi_regs(&input.return_values, types);
        self.return_types = return_values.iter().map(|(v, _)| v.ty()).collect();
        let slot = self.new_slot();
        self.instrs.push(Instr::new(slot, None, Self::RETURN, vec![], return_values, None, input.return_span));
    }

    pub(super) fn exprs_to_abi_regs(
        &self,
        exprs:                                  &[vir::Expr],
        types:                                  &[vir::TypeInfo],
    ) -> Vec<(Value, MachineReg)> {
        exprs.iter()
            .zip(REGS.abi_regs(exprs.iter().map(|expr| Type::from(types[expr.pool_index()]))))
            .map(|(expr, reg)| (
                if let Operand::Reg(v) = self.operand_map[&expr.pool_index()] { v } else {
                    panic!("internal compiler error: constant as an ABI argument")
                },
                reg)
            ).collect()
    }

    pub(super) fn lower_call(
        &mut self,
        name:                                   &str,
        ty:                                     Type,
        arg_exprs:                              &[vir::Expr],
        types:                                  &[vir::TypeInfo],
        span:                                   Span,
    ) -> Value {
        let function_address =
            if let Some(&(_, a)) = self.functions.iter().find(|(n, _)| n == name) { a } else {
            let a = self.load_function_address(name, span);
            self.functions.push((name.into(), a));
            a
        };
        let args = self.exprs_to_abi_regs(arg_exprs, types);
        let output_reg = REGS.abi_result_reg(ty);
        self.select_call(ty, function_address, args, output_reg, span)
    }
}


//-------------------------------------------------------------------------------------------------
