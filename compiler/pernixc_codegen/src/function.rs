//! Contains the logic related to translate the Pernix's functions to LLVM's
//! functions.

use std::collections::HashMap;

use pernixc_table::GlobalID;
use pernixc_term::instantiation::Instantiation;

use crate::{context::Context, Model};

/// Represents an instantiation of a function.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Key {
    /// Available kinds are:
    ///
    /// - [`SymbolKind::Function`]
    /// - [`SymbolKind::ExternFunction`]: External Linkage
    /// - [`SymbolKind::AdtImplementationFunction`]
    /// - [`SymbolKind::TraitImplementationFunction`]
    pub callable_id: GlobalID,

    /// The instantiation of the function.
    pub instantiation: Instantiation<Model>,
}

/// Mpas between the function from Pernix to the LLVM function.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Map<'ctx> {
    llvm_functions_by_key: HashMap<Key, inkwell::values::FunctionValue<'ctx>>,
}

impl Context<'_> {
    /// Gets the function from the map.
    pub fn get_function(
        &mut self,
        key: &Key,
    ) -> inkwell::values::FunctionValue {
        let function_value =
            self.module().add_function(todo!(), todo!(), todo!());

        let builder = self.context().create_builder();

        self.context().append_basic_block(function_value, "any");
    }
}
