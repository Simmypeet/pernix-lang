//! Contains the definition of the structs related to LLVM type system.

use std::collections::HashMap;

use getset::{CopyGetters, Getters};
use llvm_sys::prelude::LLVMTypeRef;
use pernixc_semantic::symbol::{FieldID, StructID};

use crate::context::LenTy;

/// Represents a struct created in the LLVM context.
#[derive(Debug, Getters, CopyGetters)]
pub struct Struct {
    pub(super) llvm_type: LLVMTypeRef,

    /// The [`StructID`] that this [`Struct`] represents.
    #[get_copy = "pub"]
    pub(super) struct_id: StructID,

    /// Maps the [`FieldID`]s of the fields of this [`Struct`] to their indices in the LLVM struct.
    #[get = "pub"]
    pub(super) field_indices_by_field_id: HashMap<FieldID, LenTy>,
}

impl Struct {
    /// Returns the llvm type of this [`Struct`].
    ///
    /// # Safety
    /// The returned `LLVMTypeRef` is only valid as long as the [`Struct`] is valid.
    #[must_use]
    pub unsafe fn llvm_type(&self) -> LLVMTypeRef { self.llvm_type }
}
