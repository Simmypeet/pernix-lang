//! Contains the definition of the [`Module`] type, a wrapper over `LLVMModuleRef`.

use std::{
    collections::HashMap,
    ffi::{CStr, CString},
    sync::Arc,
};

use getset::Getters;
use llvm_sys::{
    analysis::LLVMVerifierFailureAction,
    prelude::{LLVMModuleRef, LLVMTypeRef},
};
use pernixc_semantic::{
    hir::{Hir, Target},
    symbol::{
        table::Table,
        ty::{PrimitiveType, Type as TypeSymbol},
        OverloadID, ParameterID, StructID, TypedID,
    },
};
use pernixc_system::arena::InvalidIDError;

use self::{function::Function, ty::Struct};
use crate::context::{Child, Context, LenTy};

pub mod function;
pub mod ty;

/// Represents a [`Target`] wrapper over a `LLVMModuleRef`.
#[derive(Debug, Getters)]
pub struct Module {
    parent_context: Arc<Context>,
    llvm_module: LLVMModuleRef,

    /// The table that was used in building the [`Target`] of the module.
    #[get = "pub"]
    table: Arc<Table>,

    /// Maps between a [`StructID`] to its [`Struct`] LLVM wrapper equivalent.
    #[get = "pub"]
    structs_by_struct_id: HashMap<StructID, Struct>,

    /// Maps between a [`OverloadID`] to its [`Function`] LLVM wrapper equivalent.
    #[get = "pub"]
    functions_by_overload_id: HashMap<OverloadID, Function>,
}

impl Context {
    /// Creates a new module in the context.
    #[must_use]
    pub fn create_module(self: &Arc<Self>, name: String, target: Target) -> Arc<Module> {
        let name_cstring = CString::new(name).unwrap();
        let llvm_module = unsafe {
            llvm_sys::core::LLVMModuleCreateWithNameInContext(
                name_cstring.as_ptr(),
                self.llvm_context(),
            )
        };

        let (table, hirs_by_overload_id) = target.dissolve();

        let mut module = Module {
            parent_context: self.clone(),
            llvm_module,
            table,
            structs_by_struct_id: HashMap::new(),
            functions_by_overload_id: HashMap::new(),
        };

        module.populate_structs();
        module.populate_functions(hirs_by_overload_id);

        Arc::new(module)
    }
}

impl Module {
    /// Gets an `LLVMTypeRef` equivalent to the given [`TypeSymbol`].
    ///
    /// # Errors
    /// Returns an [`InvalidIDError`] if the given [`TypeSymbol`] is invalid or not from the same
    /// [`Table`] as this [`Module`].
    ///
    /// # Safety
    /// The returned [`LLVMTypeRef`] is only valid for the lifetime of this [`Module`].
    pub unsafe fn resolve_type(
        &self,
        type_symbol: TypeSymbol,
    ) -> Result<LLVMTypeRef, InvalidIDError> {
        match type_symbol {
            TypeSymbol::PrimitiveType(ty) => Ok(match ty {
                PrimitiveType::Void => {
                    llvm_sys::core::LLVMVoidTypeInContext(self.parent_context.llvm_context())
                }
                PrimitiveType::Bool => {
                    llvm_sys::core::LLVMInt1TypeInContext(self.parent_context.llvm_context())
                }
                PrimitiveType::Float32 => {
                    llvm_sys::core::LLVMFloatTypeInContext(self.parent_context.llvm_context())
                }
                PrimitiveType::Float64 => {
                    llvm_sys::core::LLVMDoubleTypeInContext(self.parent_context.llvm_context())
                }
                PrimitiveType::Int8 | PrimitiveType::Uint8 => {
                    llvm_sys::core::LLVMInt8TypeInContext(self.parent_context.llvm_context())
                }
                PrimitiveType::Int16 | PrimitiveType::Uint16 => {
                    llvm_sys::core::LLVMInt16TypeInContext(self.parent_context.llvm_context())
                }
                PrimitiveType::Int32 | PrimitiveType::Uint32 => {
                    llvm_sys::core::LLVMInt32TypeInContext(self.parent_context.llvm_context())
                }
                PrimitiveType::Int64 | PrimitiveType::Uint64 => {
                    llvm_sys::core::LLVMInt64TypeInContext(self.parent_context.llvm_context())
                }
            }),
            TypeSymbol::TypedID(ty) => match ty {
                TypedID::Enum(..) => Ok(llvm_sys::core::LLVMInt32TypeInContext(
                    self.parent_context.llvm_context(),
                )),
                TypedID::Struct(st) => {
                    let llvm_struct = self.structs_by_struct_id.get(&st).ok_or(InvalidIDError)?;
                    Ok(llvm_struct.llvm_type)
                }
            },
        }
    }

    #[allow(clippy::too_many_lines)]
    fn populate_functions(&mut self, mut hirs_by_overload_id: HashMap<OverloadID, Hir>) {
        for overload_symbol in self.table.overloads() {
            // return type of the function
            let return_type = unsafe { self.resolve_type(overload_symbol.return_type()).unwrap() };

            // parameter type of the function
            let mut llvm_parameter_types = Vec::new();
            let mut parameter_indices_by_parameter_id: HashMap<ParameterID, LenTy> = HashMap::new();

            for parameter_symbol in overload_symbol
                .parameter_order()
                .iter()
                .map(|x| self.table.get_parameter(*x).unwrap())
            {
                // skip void parameter
                if parameter_symbol.type_binding().ty.is_void() {
                    continue;
                }

                let parameter_type = unsafe {
                    self.resolve_type(parameter_symbol.type_binding().ty)
                        .unwrap()
                };

                parameter_indices_by_parameter_id.insert(
                    parameter_symbol.id(),
                    llvm_parameter_types
                        .len()
                        .try_into()
                        .expect("too many parameter"),
                );
                llvm_parameter_types.push(parameter_type);
            }

            let llvm_type = unsafe {
                llvm_sys::core::LLVMFunctionType(
                    return_type,
                    llvm_parameter_types.as_mut_ptr(),
                    llvm_parameter_types
                        .len()
                        .try_into()
                        .expect("too many parameters"),
                    0,
                )
            };

            // creates mangled name
            let mangled_name_cstring = {
                let mut mangled_name = self
                    .table
                    .get_full_name_of(overload_symbol.parent_overload_set_id())
                    .unwrap();

                for parameter_symbol in overload_symbol
                    .parameter_order()
                    .iter()
                    .map(|x| self.table.get_parameter(*x).unwrap())
                {
                    mangled_name.push('$');
                    match parameter_symbol.type_binding().ty {
                        TypeSymbol::PrimitiveType(ty) => {
                            let name = match ty {
                                PrimitiveType::Void => "void",
                                PrimitiveType::Bool => "bool",
                                PrimitiveType::Float32 => "float32",
                                PrimitiveType::Float64 => "float64",
                                PrimitiveType::Int8 => "int8",
                                PrimitiveType::Int16 => "int16",
                                PrimitiveType::Int32 => "int32",
                                PrimitiveType::Int64 => "int64",
                                PrimitiveType::Uint8 => "uint8",
                                PrimitiveType::Uint16 => "uint16",
                                PrimitiveType::Uint32 => "uint32",
                                PrimitiveType::Uint64 => "uint64",
                            };
                            mangled_name.push_str(name);
                        }
                        TypeSymbol::TypedID(ty_id) => {
                            mangled_name.push_str(&self.table.get_full_name_of(ty_id).unwrap());
                        }
                    }
                }

                CString::new(mangled_name).unwrap()
            };

            let llvm_value = unsafe {
                llvm_sys::core::LLVMAddFunction(
                    self.llvm_module,
                    mangled_name_cstring.as_ptr(),
                    llvm_type,
                )
            };

            // set the name for each parameter
            for parameter_sym in overload_symbol
                .parameter_order()
                .iter()
                .map(|x| self.table.get_parameter(*x).unwrap())
            {
                if parameter_sym.type_binding().ty.is_void() {
                    continue;
                }

                unsafe {
                    llvm_sys::core::LLVMSetValueName2(
                        llvm_sys::core::LLVMGetParam(
                            llvm_value,
                            parameter_indices_by_parameter_id[&parameter_sym.id()],
                        ),
                        parameter_sym.name().as_ptr().cast::<i8>(),
                        parameter_sym.name().len(),
                    );
                }
            }

            self.functions_by_overload_id
                .insert(overload_symbol.id(), Function {
                    llvm_value,
                    llvm_type,
                    overload_id: overload_symbol.id(),
                    parameter_indices_by_parameter_id,
                    hir: hirs_by_overload_id.remove(&overload_symbol.id()).unwrap(),
                });
        }
    }

    fn populate_structs(&mut self) {
        // Maps between pernixc's struct IDs and LLVM's struct types.
        for struct_symbol in self.table.structs() {
            let name_cstring =
                CString::new(self.table.get_full_name_of(struct_symbol.id()).unwrap()).unwrap();

            let llvm_type = unsafe {
                llvm_sys::core::LLVMStructCreateNamed(
                    self.parent_context.llvm_context(),
                    name_cstring.as_ptr(),
                )
            };

            self.structs_by_struct_id
                .insert(struct_symbol.id(), Struct {
                    llvm_type,
                    struct_id: struct_symbol.id(),
                    field_indices_by_field_id: HashMap::new(),
                });
        }

        // Populates struct fields
        for struct_symbol in self.table.structs() {
            let mut llvm_struct_field_types = Vec::new();

            for field in struct_symbol
                .field_order()
                .iter()
                .map(|x| self.table.get_field(*x).unwrap())
            {
                // skip void field
                if field.ty().is_void() {
                    continue;
                }

                let llvm_field_type = unsafe { self.resolve_type(field.ty()).unwrap() };

                let llvm_struct = self
                    .structs_by_struct_id
                    .get_mut(&struct_symbol.id())
                    .unwrap();

                // translate field type to index offset
                llvm_struct.field_indices_by_field_id.insert(
                    field.id(),
                    llvm_struct_field_types
                        .len()
                        .try_into()
                        .expect("too many fields in struct"),
                );
                llvm_struct_field_types.push(llvm_field_type);
            }

            let llvm_struct = self
                .structs_by_struct_id
                .get_mut(&struct_symbol.id())
                .unwrap();

            unsafe {
                llvm_sys::core::LLVMStructSetBody(
                    llvm_struct.llvm_type,
                    llvm_struct_field_types.as_mut_ptr(),
                    llvm_struct_field_types
                        .len()
                        .try_into()
                        .expect("too many fields in struct"),
                    0,
                );
            };
        }
    }

    /// Dumps the LLVM-Module information to stdout.
    pub fn dump_module(&self) {
        unsafe {
            llvm_sys::core::LLVMDumpModule(self.llvm_module);
        }
    }

    /// Validates the LLVM-Module.
    ///
    /// # Errors
    /// Returns an error message if the module is ill-formed.
    pub fn validate(&self) -> Result<(), String> {
        let mut error = std::ptr::null_mut();
        let result = unsafe {
            llvm_sys::analysis::LLVMVerifyModule(
                self.llvm_module,
                LLVMVerifierFailureAction::LLVMReturnStatusAction,
                &mut error,
            )
        };

        if result == 1 {
            let error_message = unsafe { CStr::from_ptr(error) }
                .to_str()
                .unwrap()
                .to_owned();
            unsafe {
                llvm_sys::core::LLVMDisposeMessage(error);
            }
            Err(error_message)
        } else {
            Ok(())
        }
    }
}

impl Drop for Module {
    fn drop(&mut self) {
        unsafe {
            llvm_sys::core::LLVMDisposeModule(self.llvm_module);
        }
    }
}

impl Child for Module {
    fn parent_context(&self) -> &Arc<Context> { &self.parent_context }
}

#[cfg(test)]
mod tests;
