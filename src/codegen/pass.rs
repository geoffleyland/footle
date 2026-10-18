use typed_arena::Arena;

use crate::{vir, runtime::Observer};
use super::{lower, scheduler, allocator, assembler, binary, Instr, Arg};


//-------------------------------------------------------------------------------------------------

pub fn run<O:Observer>(
    vir_block:          &vir::Block,
    types:              &[vir::TypeInfo],
    observer:           &mut O,
) -> binary::CompiledFn {
    let arg_arean = Arena::<Arg>::new();
    let instr_arena = Arena::<Instr>::new();

    let lowered_block = lower::run(&arg_arean, &instr_arena, vir_block, types);

    let scheduled_block = scheduler::run(lowered_block);
    observer.schedule(&scheduled_block);
    let arg_types = scheduled_block.args.iter().map(|a| a.ty).collect();

    let (allocated, written_regs) =
        allocator::run(scheduled_block.slot_count, &scheduled_block.args,
            &scheduled_block.instrs);
    let assembly =
        assembler::run(allocated, &scheduled_block.constants, &scheduled_block.functions,
            arg_types, scheduled_block.return_types, written_regs);
    observer.assembly(&assembly);
    let func = binary::emit(&assembly);
    observer.func(&func);
    func
}


//-------------------------------------------------------------------------------------------------
