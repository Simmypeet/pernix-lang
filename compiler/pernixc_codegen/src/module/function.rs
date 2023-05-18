//! Contains the definition of [`Function`] struct.

use std::collections::HashMap;

use getset::{CopyGetters, Getters};
use llvm_sys::prelude::{LLVMTypeRef, LLVMValueRef};
use pernixc_semantic::{
    hir::Hir,
    symbol::{OverloadID, ParameterID},
};

use crate::context::LenTy;

/// Contains the LLVM function value and type of a particular overload in Pernix.
#[derive(Debug, Getters, CopyGetters)]
pub struct Function {
    pub(super) llvm_value: LLVMValueRef,
    pub(super) llvm_type: LLVMTypeRef,

    /// The [`OverloadID`] that this [`Function`] represents.
    #[get_copy = "pub"]
    pub(super) overload_id: OverloadID,

    /// Maps between [`ParameterID`]s and their indices in the LLVM function.
    #[get = "pub"]
    pub(super) parameter_indices_by_parameter_id: HashMap<ParameterID, LenTy>,

    /// The intermediate representation of this function.
    #[get = "pub"]
    pub(super) hir: Hir,
}

impl Function {
    /// Returns the llvm value of this [`Function`].
    ///
    /// # Safety
    /// The returned `LLVMValueRef` is only valid as long as the [`Function`] is valid.
    #[must_use]
    pub unsafe fn llvm_value(&self) -> LLVMValueRef { self.llvm_value }

    /// Returns the llvm type of this [`Function`].
    ///
    /// # Safety
    /// The returned `LLVMTypeRef` is only valid as long as the [`Function`] is valid.
    #[must_use]
    pub unsafe fn llvm_type(&self) -> LLVMTypeRef { self.llvm_type }
}
