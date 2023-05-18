//! Contains the definition of the [`Context`] type, a wrapper over `LLVMContextRef`.

use std::sync::Arc;

use llvm_sys::prelude::LLVMContextRef;

/// Represents a wrapper over `LLVMContextRef`, the parent container of all LLVM resources.
#[derive(Debug)]
pub struct Context {
    llvm_context: LLVMContextRef,
}

/// The numeric type used for lengths and indices in LLVM.
pub type LenTy = u32;

impl Context {
    /// Creates a new `Context`.
    #[must_use]
    pub fn new() -> Arc<Self> {
        let context = unsafe { llvm_sys::core::LLVMContextCreate() };
        Arc::new(Self {
            llvm_context: context,
        })
    }

    /// Returns the `LLVMContextRef` wrapped by this `Context`.
    ///
    /// # Safety
    /// This function is unsafe because it returns a raw LLVM context pointer.
    #[must_use]
    pub unsafe fn llvm_context(&self) -> LLVMContextRef { self.llvm_context }
}

impl Drop for Context {
    fn drop(&mut self) {
        unsafe {
            llvm_sys::core::LLVMContextDispose(self.llvm_context);
        }
    }
}

/// Represents a child of a [`Context`]
pub trait Child: 'static {
    /// Returns the [`Context`] of this child.
    fn parent_context(&self) -> &Arc<Context>;
}
