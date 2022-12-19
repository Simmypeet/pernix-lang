use llvm_sys::{
    core::{LLVMContextCreate, LLVMContextDispose},
    prelude::LLVMContextRef,
};

/// Represent a wrapper around an LLVM context. All the entieis related to
/// LLVM code generation are created using this context.
pub struct Context {
    context: LLVMContextRef,
}

impl Drop for Context {
    fn drop(&mut self) {
        unsafe { LLVMContextDispose(self.context) };
    }
}

impl Context {
    /// Create a new LLVM context.
    pub fn new() -> Self {
        Self {
            context: unsafe { LLVMContextCreate() },
        }
    }

    /// Get the underlying LLVM context.
    pub unsafe fn get_context(&self) -> LLVMContextRef {
        self.context
    }
}
