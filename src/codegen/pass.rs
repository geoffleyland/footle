use typed_arena::Arena;

use crate::{vir, runtime::Observer};
use super::{lower, scheduler, allocator, assembler, binary, Instr, Arg};


//-------------------------------------------------------------------------------------------------

pub fn run<O:Observer>(
    vir_block:          &vir::Block,
    types:              &[vir::TypeInfo],
    observer:           &mut O,
) -> binary::CompiledFn {
    let arg_arena = Arena::<Arg>::new();
    let instr_arena = Arena::<Instr>::new();

    let (lowered_block, pool) = lower::run(&arg_arena, &instr_arena, vir_block, types);
    let arg_types = lowered_block.args.iter().map(|a| a.ty).collect();

    let scheduled_block = scheduler::run(lowered_block);
    observer.schedule(&scheduled_block, &pool);

    let (allocated, written_regs) = allocator::run(&scheduled_block);
    let assembly =
        assembler::run(allocated, arg_types, scheduled_block.return_types, written_regs, &pool);
    observer.assembly(&assembly, &pool);
    let func = binary::emit(&assembly, &pool);
    observer.func(&func);
    func
}


//-------------------------------------------------------------------------------------------------
