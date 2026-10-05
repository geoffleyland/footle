use typed_arena::Arena;

use crate::{vir, runtime::Observer};
use super::{lower, scheduler, allocator, assembler, binary, Value};


//-------------------------------------------------------------------------------------------------

pub fn run<O:Observer>(
    vir_block:          &vir::Block,
    types:              &[vir::TypeInfo],
    observer:           &mut O,
) -> binary::CompiledFn {
    let arena = Arena::<Value>::new();

    let lowered_block = lower::run(&arena, vir_block, types);

    let scheduled_block = scheduler::run(lowered_block);
    observer.schedule(&scheduled_block);
    let argument_types = scheduled_block.arguments.iter().map(|a| a.ty).collect();

    let (allocated, written_regs) =
        allocator::run(scheduled_block.value_count, &scheduled_block.arguments,
            &scheduled_block.instrs);
    let assembly =
        assembler::run(allocated, &scheduled_block.constants, &scheduled_block.functions,
            argument_types, scheduled_block.return_types, written_regs);
    observer.assembly(&assembly);
    let func = binary::emit(&assembly);
    observer.func(&func);
    func
}


//-------------------------------------------------------------------------------------------------
