//! Contains the definition of [`Generator`] struct.

use std::{collections::HashMap, ffi::CString};

use llvm_sys::prelude::{LLVMBasicBlockRef, LLVMValueRef};
use pernixc_semantic::{cfg::BasicBlockID, hir::value::VariableID, symbol::OverloadID};
use pernixc_system::arena::InvalidIDError;

use crate::{
    context::Child,
    module::{function::Function, Module},
};

/// Is an LLVM-IR generator, by translating the Pernixc's [`pernixc_semantic::hir::Hir`] into
/// LLVM-IR.
#[derive(Debug)]
pub(super) struct Generator<'module> {
    target_function: &'module Function,
    llvm_basic_blocks_by_basic_block_id: HashMap<BasicBlockID, LLVMBasicBlockRef>,
    module: &'module Module,

    allocas_by_variable_id: HashMap<VariableID, LLVMValueRef>,
    current_basic_block_id: BasicBlockID,
}

impl Module {
    /// Creates a new [`Generator`] for the given [`OverloadID`].
    pub(super) fn create_generator(
        &self,
        target_overload_id: OverloadID,
    ) -> Result<Generator, InvalidIDError> {
        let llvm_ir_builder = unsafe {
            llvm_sys::core::LLVMCreateBuilderInContext(self.parent_context().llvm_context())
        };
        let target_function = self
            .functions_by_overload_id()
            .get(&target_overload_id)
            .ok_or(InvalidIDError)?;

        // creates LLVM-BasicBlock for each Pernixc's BasicBlock
        let mut llvm_basic_blocks_by_basic_block_id = HashMap::new();
        llvm_basic_blocks_by_basic_block_id.insert(
            target_function.hir().control_flow_graph().entry_block(),
            unsafe {
                let cstring = CString::new("entry").unwrap();
                llvm_sys::core::LLVMAppendBasicBlockInContext(
                    self.parent_context().llvm_context(),
                    target_function.llvm_value(),
                    cstring.as_ptr(),
                )
            },
        );
        for basic_block_sym in target_function.hir().control_flow_graph().basic_blocks() {
            if basic_block_sym.id() == target_function.hir().control_flow_graph().entry_block() {
                continue;
            };

            let llvm_basic_block = unsafe {
                llvm_sys::core::LLVMAppendBasicBlockInContext(
                    self.parent_context().llvm_context(),
                    target_function.llvm_value(),
                    b"\0".as_ptr().cast::<i8>(),
                )
            };

            llvm_basic_blocks_by_basic_block_id.insert(basic_block_sym.id(), llvm_basic_block);
        }

        // position the builder at the entry block
        unsafe {
            llvm_sys::core::LLVMPositionBuilderAtEnd(
                llvm_ir_builder,
                llvm_basic_blocks_by_basic_block_id
                    .get(&target_function.hir().control_flow_graph().entry_block())
                    .copied()
                    .unwrap(),
            );
        };

        let mut allocas_by_variable_id = HashMap::new();

        // creates an alloca instructions for every paramter and variable
        for alloca_sym in target_function.hir().allocas().values() {
            // skip alloca for void type
            if alloca_sym.ty().is_void() {
                continue;
            }

            let alloca_name_cstring =
                CString::new(alloca_sym.identifier_token().span.str()).unwrap();

            let alloca_value = unsafe {
                llvm_sys::core::LLVMBuildAlloca(
                    llvm_ir_builder,
                    self.resolve_type(alloca_sym.ty()).unwrap(),
                    alloca_name_cstring.as_ptr(),
                )
            };

            allocas_by_variable_id.insert(VariableID::AllocaID(alloca_sym.id()), alloca_value);
        }
        for parameter_sym in self
            .table()
            .get_overload(target_overload_id)
            .unwrap()
            .parameter_order()
            .iter()
            .map(|x| self.table().get_parameter(*x).unwrap())
        {
            if parameter_sym.type_binding().ty.is_void() {
                continue;
            }

            let parameter_name_cstring = CString::new(parameter_sym.name().clone()).unwrap();

            let parameter_value = unsafe {
                llvm_sys::core::LLVMBuildAlloca(
                    llvm_ir_builder,
                    self.resolve_type(parameter_sym.type_binding().ty).unwrap(),
                    parameter_name_cstring.as_ptr(),
                )
            };

            allocas_by_variable_id
                .insert(VariableID::ParameterID(parameter_sym.id()), parameter_value);

            // store the parameter value in the alloca
            unsafe {
                llvm_sys::core::LLVMBuildStore(
                    llvm_ir_builder,
                    llvm_sys::core::LLVMGetParam(
                        target_function.llvm_value(),
                        target_function
                            .parameter_indices_by_parameter_id()
                            .get(&parameter_sym.id())
                            .copied()
                            .unwrap(),
                    ),
                    parameter_value,
                );
            }
        }

        Ok(Generator {
            target_function,
            llvm_basic_blocks_by_basic_block_id,
            module: self,
            allocas_by_variable_id,
            current_basic_block_id: target_function.hir().control_flow_graph().entry_block(),
        })
    }
}
