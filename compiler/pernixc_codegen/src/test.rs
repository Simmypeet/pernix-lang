use inkwell::{
    llvm_sys::core::{LLVMBuildAlloca, LLVMBuildStore, LLVMGetUndef},
    types::AsTypeRef,
};

#[test]
fn test_struct() {
    let context = inkwell::context::Context::create();
    let module = context.create_module("test");

    let function = context.void_type().fn_type(&[], false);
    let func_val = module.add_function("test", function, None);

    let builder = context.create_builder();
    let entry = context.append_basic_block(func_val, "entry");

    builder.position_at_end(entry);

    unsafe {
        let alloca = LLVMBuildAlloca(
            builder.as_mut_ptr(),
            context.void_type().as_type_ref(),
            c"test".as_ptr().cast(),
        );

        let undef = LLVMGetUndef(context.void_type().as_type_ref());

        LLVMBuildStore(builder.as_mut_ptr(), undef, alloca);
    }

    builder.build_return(None).unwrap();
    module.verify().unwrap();
    module.print_to_stderr();
}
