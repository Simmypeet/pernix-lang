use std::marker::PhantomData;

use llvm_sys::{
    core::{LLVMDumpModule, LLVMModuleCreateWithNameInContext},
    prelude::LLVMModuleRef,
};

use crate::context::Context;

/// Represent a wrapper around an LLVM module.
pub struct Module<'ctx> {
    llvm_module: LLVMModuleRef,
    _phantom: PhantomData<&'ctx Context>,
}

impl<'ctx> Module<'ctx> {
    /// Create a new LLVM module.
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        Self {
            llvm_module: unsafe {
                LLVMModuleCreateWithNameInContext(
                    module_name.as_ptr() as *const i8,
                    context.get_context(),
                )
            },
            _phantom: PhantomData,
        }
    }

    /// Print the LLVM IR dump of the module.
    pub fn print_ir(&self) {
        unsafe {
            LLVMDumpModule(self.llvm_module);
        }
    }

    pub fn add_function(&self, bound_function: BoundFunction) -> bool {}
}

impl<'ctx> Drop for Module<'ctx> {
    fn drop(&mut self) {
        unsafe { llvm_sys::core::LLVMDisposeModule(self.llvm_module) };
    }
}
