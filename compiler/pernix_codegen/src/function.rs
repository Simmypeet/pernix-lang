use llvm_sys::prelude::{LLVMTypeRef, LLVMValueRef};
use pernix_analyzer::binding::bound_declaration::BoundFunction;

/// Represents a bound function that has been added to the LLVM module and
/// is ready to be called.
pub struct Function<'table, 'ast> {
    bound_function: BoundFunction<'table, 'ast>,
    llvm_type: LLVMTypeRef,
    llvm_function: LLVMValueRef,
}

impl<'table, 'ast> Function<'table, 'ast> {
    pub(super) fn new(
        bound_function: BoundFunction<'table, 'ast>,
        llvm_type: LLVMTypeRef,
        llvm_function: LLVMValueRef,
    ) -> Self {
        Self {
            bound_function,
            llvm_type,
            llvm_function,
        }
    }

    /// Return a reference to the bound function of this [`Function`].
    pub fn bound_function(&self) -> &BoundFunction<'table, 'ast> {
        &self.bound_function
    }

    /// Return the llvm type of this [`Function`].
    pub unsafe fn llvm_type(&self) -> LLVMTypeRef {
        self.llvm_type
    }

    /// Returns the llvm function of this [`Function`].
    pub unsafe fn llvm_function(&self) -> LLVMValueRef {
        self.llvm_function
    }
}
