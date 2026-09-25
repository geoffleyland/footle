use typed_arena::Arena;

use crate::vir;
use crate::runtime::Observer;
use super::{scheduler, allocator, assembler, binary};


//-------------------------------------------------------------------------------------------------

pub fn run<O:Observer>(
    vir_block:          &vir::Block,
    types:              &[vir::TypeInfo],
    observer:           &mut O,
) -> binary::CompiledFn {
    let arena = Arena::<scheduler::Value>::new();
    let scheduled_block = scheduler::run(&arena, vir_block, types);
    observer.schedule(&scheduled_block);
    let argument_types = scheduled_block.arguments.iter().map(|a| a.ty).collect();

    let (allocated, registers_to_save) =
        allocator::run(scheduled_block.value_count, &scheduled_block.arguments,
            &scheduled_block.instrs);
    let assembler =
        assembler::run(allocated, &scheduled_block.constants, &scheduled_block.functions,
            argument_types, scheduled_block.return_types, &registers_to_save);
    observer.assembler(&assembler);
    let func = binary::emit(&assembler);
    observer.func(&func);
    func
}


//-------------------------------------------------------------------------------------------------
