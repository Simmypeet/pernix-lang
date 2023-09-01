use pernixc_source::Span;
use pernixc_system::arena;

use crate::symbol::{self, Module};

/// After resolved the module path, the found symbol was not a `module` but something else.
#[derive(Debug, Clone)]
pub struct ModuleExpected {
    /// The span of the module path that was resolved into the found symbol.
    pub module_path_span: Span,

    /// The span of the found symbol.
    pub found_symbol_id: symbol::ID,
}

/// The submodule with the given name was not found in the module with the given ID.
#[derive(Debug, Clone)]
pub struct ModuleNotFound {
    /// The span of the module path that was attempted to be resolved.
    pub module_path_span: Span,

    /// The ID of the module that was searched.
    ///
    /// If `None`, the search was started from the root module.
    pub serached_module_id: Option<arena::ID<Module>>,
}

/// The module was already used in the `using` statement.
#[derive(Debug, Clone)]
pub struct UsingDuplication {
    /// Span of the `using` statement that caused the duplication.
    pub duplicate_span: Span,

    /// Span of the `using` statement that was already declared.
    pub existing_using_span: Span,
}

/// The symbol with the given name was already declared in the module.
#[derive(Debug, Clone)]
pub struct SymbolDuplication {
    /// Span of the symbol that caused the duplication.
    pub duplicate_span: Span,

    /// Span of the symbol that was already declared.
    pub existing_symbol_id: symbol::ID,
}

#[derive(Debug)]
pub enum Error {
    UsingDuplication(UsingDuplication),
    ModuleNotFound(ModuleNotFound),
    ModuleExpected(ModuleExpected),
    SymbolDuplication(SymbolDuplication),
}
