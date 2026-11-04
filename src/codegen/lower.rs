use typed_index_collections::{ti_vec, TiVec};

use crate::core::Span;
use crate::vir::{self, PoolIndex};
use super::{Code, Instr, Arg, Value, ValueSlot, value::Operand, Type, Constant, ConstantIndex, MachineReg, REGS};


//-------------------------------------------------------------------------------------------------

/// The function signature: established in lower and passed right through to the compiled function
pub struct Signature {
    pub(super) args:                        Vec<Arg>,
    pub(super) return_types:                Vec<Type>,
}

/// The constant pool - floats and function pointers.
pub struct Pool {
    pub(super) constants:                   TiVec<ConstantIndex, Constant>,
    pub(super) functions:                   Vec<String>,
}

pub(super) fn run(
    input:                                  &vir::Block,
    types:                                  &vir::Types,
) -> (Vec<Instr>, usize, Signature, Pool) {
    let mut builder = Builder{
        value_map:                          ti_vec![None; input.exprs.len()],
        ..Default::default()
    };
    builder.lower_args(input, types);
    for instr in &input.instrs {
        // Call arch-specific instruction selection.
        let value = builder.select_instr(instr, types);
        builder.value_map.insert(instr.pool_index, Some(value));
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
impl IntoOperand for Operand { fn into_operand(self, _: &Builder) -> Self { self } }

impl IntoOperand for Value {
    fn into_operand(self, _: &Builder) -> Operand { Operand::Reg(self) }
}

impl IntoOperand for &vir::ExprUse {
    fn into_operand(self, builder: &Builder) -> Operand { builder.value(self.pool_index()).into_operand(builder) }
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
    pub(super) constants:                   TiVec<ConstantIndex, Constant>,
    value_map:                              TiVec<PoolIndex, Option<Value>>,
    pub(super) functions:                   Vec<(String, Value)>,
}

impl Builder {
    fn value(&self, index: PoolIndex) -> Value {
        self.value_map[index].expect("internal compiler error: value used before it was lowered")
    }

    fn new_slot(&mut self) -> ValueSlot {
        let slot = ValueSlot::from(self.slot_count);
        self.slot_count += 1;
        slot
    }

    fn lower_args(&mut self, input: &vir::Block, types: &vir::Types) {
        for vir_arg in &input.args {
            let ty = Type::from(types[vir_arg.pool_index]);
            let slot = self.new_slot();
            self.args.push(Arg{ slot, ty, span: vir_arg.definition_span });
            self.value_map.insert(vir_arg.pool_index, Some(Value::Arg(slot, ty)));
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

    fn lower_return(&mut self, input: &vir::Block, types: &vir::Types) {
        let return_values = self.exprs_to_abi_regs(
            input.return_values.iter().map(|e| e.pool_index()), types);
        self.return_types = return_values.iter().map(|(v, _)| v.ty()).collect();
        let slot = self.new_slot();
        self.instrs.push(Instr::new(slot, None, Self::RETURN, vec![], return_values, None, input.return_span));
    }

    pub(super) fn exprs_to_abi_regs(
        &self,
        pool_indices:                           impl Iterator<Item = PoolIndex> + Clone,
        types:                                  &vir::Types,
    ) -> Vec<(Value, MachineReg)> {
        let regs = REGS.abi_regs(pool_indices.clone().map(|index| Type::from(types[index])));
        pool_indices.zip(regs).map(|(index, reg)| (self.value(index), reg)).collect()
    }

    pub(super) fn lower_call(
        &mut self,
        name:                                   &str,
        ty:                                     Type,
        arg_exprs:                              &[vir::ExprUse],
        types:                                  &vir::Types,
        span:                                   Span,
    ) -> Value {
        let function_address =
            if let Some(&(_, a)) = self.functions.iter().find(|(n, _)| n == name) { a } else {
            let a = self.load_function_address(name, span);
            self.functions.push((name.into(), a));
            a
        };
        let args = self.exprs_to_abi_regs(arg_exprs.iter().map(vir::ExprUse::pool_index), types);
        let output_reg = REGS.abi_result_reg(ty);
        self.emit_abi_instr(ty, Self::CALL_REG, operands!(self, function_address), args, Some(output_reg), span)
    }
}


//-------------------------------------------------------------------------------------------------
