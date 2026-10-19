use std::collections::HashMap;

use typed_arena::Arena;

use crate::vir;
use super::{Code, Instr, Arg, Value, ValueSlot, value::Operand, Type, Constant, MachineReg, REGS};

#[cfg(feature = "dogfood")]
use crate::core::Span;


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

pub(super) fn run<'arena>(
    arena:                                  &'arena Arena<Instr>,
    input:                                  &vir::Block,
    types:                                  &[vir::TypeInfo],
) -> (Vec<&'arena Instr>, usize, Signature, Pool) {
    let mut builder = Builder::new(arena);
    builder.lower_args(input, types);
    // Call arch-specific instruction selection.
    builder.select_instrs(input, types);

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

pub(super) trait IntoOperand<'arena> {
    fn into_operand(self, builder: &Builder<'arena>) -> Operand;
}

impl<'arena> IntoOperand<'arena> for &'arena Instr {
    fn into_operand(self, _: &Builder<'arena>) -> Operand {
        Operand::Reg(Value::Instr(self.slot, self.ty))
    }
}

impl<'arena> IntoOperand<'arena> for &vir::Expr {
    fn into_operand(self, builder: &Builder<'arena>) -> Operand {
        builder.operand_map[&self.pool_index()].clone()
    }
}

impl<'arena> IntoOperand<'arena> for Operand {
    fn into_operand(self, _: &Builder<'arena>) -> Self {
        self
    }
}

macro_rules! operands {
    ($builder:expr, $( $op:expr ),* $(,)?) => {
        vec![$( $crate::codegen::lower::IntoOperand::into_operand($op, $builder) ),*]
    };
}
pub(super) use operands;


//-------------------------------------------------------------------------------------------------

pub struct Builder<'arena> {
    arena:                                  &'arena Arena<Instr>,
    args:                                   Vec<Arg>,
    instrs:                                 Vec<&'arena Instr>,
    slot_count:                             usize,
    pub(super) return_types:                Vec<Type>,
    pub(super) constants:                   Vec<Constant>,
    operand_map:                            HashMap<usize, Operand>,
    pub(super) functions:                   Vec<(String, &'arena Instr)>,
}

impl<'arena> Builder<'arena> {
    fn new(
        arena:                              &'arena Arena<Instr>
    ) -> Self {
        Self { arena,
            args: vec![], instrs: vec![], slot_count: 0, return_types: vec![],
            constants: vec![], operand_map: HashMap::new(), functions: vec![] }
    }

    fn new_slot(&mut self) -> ValueSlot {
        let slot = ValueSlot::from(self.slot_count);
        self.slot_count += 1;
        slot
    }


    fn lower_args(&mut self, input: &vir::Block, types: &[vir::TypeInfo]) {
        for vir_arg in &input.args {
            let ty = Type::from(types[vir_arg.pool_index]);
            let slot = self.new_slot();
            self.args.push(Arg{ slot, ty,
                #[cfg(feature = "dogfood")]
                span: vir_arg.definition_span,
            });
            let operand = Operand::Reg(Value::Arg(slot, ty));
            self.operand_map.insert(vir_arg.pool_index, operand);
        }
    }

    pub(super) fn lower_instr(
        &mut self,
        ty:                                     Type,
        code:                                   &'static Code,
        operands:                               Vec<Operand>,
        instr:                                  &vir::Instr,
    ) -> &'arena Instr {
        self.lower_fixed_instr(ty, code, operands, vec![], None, instr)
    }

    pub(super) fn lower_fixed_instr(
        &mut self,
        ty:                                     Type,
        code:                                   &'static Code,
        operands:                               Vec<Operand>,
        fixed_inputs:                           Vec<(Value, MachineReg)>,
        fixed_output:                           Option<MachineReg>,
        vir_instr:                              &vir::Instr,
    ) -> &'arena Instr {
        let instr = self.make_fixed_instr(ty, code, operands, fixed_inputs, fixed_output,
            #[cfg(feature = "dogfood")]
            vir_instr.definition_span
        );
        let operand = instr.into_operand(self);
        self.operand_map.insert(vir_instr.pool_index, operand);
        instr
    }

    pub(super) fn make_instr(
        &mut self,
        ty:                                     Type,
        code:                                   &'static Code,
        operands:                               Vec<Operand>,
        #[cfg(feature = "dogfood")]
        span:                                   Span,
    ) -> &'arena Instr  {
        self.make_fixed_instr(ty, code, operands, vec![], None,
            #[cfg(feature = "dogfood")]
            span
        )
    }

    pub(super) fn make_fixed_instr(
        &mut self,
        ty:                                     Type,
        code:                                   &'static Code,
        operands:                               Vec<Operand>,
        fixed_inputs:                           Vec<(Value, MachineReg)>,
        fixed_output:                           Option<MachineReg>,
        #[cfg(feature = "dogfood")]
        span:                                   Span,
    ) -> &'arena Instr  {
        let instr = self.arena.alloc(Instr::new(self.new_slot(), ty, code,
            operands, fixed_inputs, fixed_output,
            #[cfg(feature = "dogfood")]
            span
        ));
        self.instrs.push(instr);
        instr
    }


    pub(super) fn exprs_to_fixed_inputs(
        &self,
        exprs:                                  &[vir::Expr],
        types:                                  &[vir::TypeInfo],
    ) -> Vec<(Value, MachineReg)> {
        exprs.iter()
            .zip(REGS.abi_regs(exprs.iter().map(|expr| Type::from(types[expr.pool_index()]))))
            .map(|(expr, reg)| (
                if let Operand::Reg(v) = self.operand_map[&expr.pool_index()] { v } else {
                    panic!("internal compiler error: constant as a fixed input")
                },
                reg)
            ).collect()
    }
}


//-------------------------------------------------------------------------------------------------
