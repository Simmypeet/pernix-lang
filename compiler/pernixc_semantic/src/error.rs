//! Contains all the definition of errors that can be emitted by the semantic analyzer.

use pernixc_base::source_file::Span;

use crate::{
    arena::ID,
    symbol::{Module, ModuleMemberID},
};

/// The item symbol with the same name already exists in the given scope.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ItemDuplication {
    /// The ID of the existing symbol.
    pub existing_symbol: ModuleMemberID,

    /// The ID of the new symbol.
    pub new_symbol: ModuleMemberID,

    /// The scope in which the duplication occurred.
    pub scope: ID<Module>,
}

/// The module with the given name was not found in the given scope.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModuleNotFound {
    /// The name of the module that was not found.
    pub module_path: Span,

    /// In which module the submodule was searched. If `None`, the root module was searched.
    pub searched_module_id: Option<ID<Module>>,
}

/// An enumeration containing all kinds of errors that can be emitted by the semantic analyzer.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Error {
    GlobalSymbolDuplication(ItemDuplication),
    ModuleNotFound(ModuleNotFound),
}
