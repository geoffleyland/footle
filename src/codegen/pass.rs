use crate::{vir, runtime::Observer};
use super::{lower, scheduler, allocator, assembler, binary};


//-------------------------------------------------------------------------------------------------

pub fn run<O:Observer>(
    vir_block:          &vir::Block,
    types:              &[vir::TypeInfo],
    observer:           &mut O,
) -> binary::CompiledFn {
    let (lowered, slot_count, signature, pool) = lower::run(vir_block, types);

    let scheduled = scheduler::run(&lowered, slot_count);
    observer.schedule(&scheduled, &signature, &pool);

    let (allocated, written_regs) = allocator::run(&scheduled, slot_count, &signature);
    let assembly = assembler::run(allocated, written_regs, &signature, &pool);
    observer.assembly(&assembly, &pool);
    let func = binary::emit(&assembly, signature, &pool);
    observer.func(&func);
    func
}


//-------------------------------------------------------------------------------------------------
