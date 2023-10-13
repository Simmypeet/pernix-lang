use std::collections::HashMap;

use pernixc_base::source_file::Span;

use crate::arena::ID;

/// Represents an accessibility of a symbol.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Accessibility {
    /// The symbol is accessible from anywhere.
    Public,

    /// The symbol is accessible only from the same module and its submodules.
    Private,

    /// The symbol is accessible only from the same target.
    Internal,
}

/// An ID to all kinds of symbols that can be defined in a module.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum ModuleChildID {
    Module(ID<Module>),
}

/// Represents a module declaration.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    /// The name of the module declaration.
    pub name: String,

    /// The accessibility of the module.
    pub accessibility: Accessibility,

    /// The ID of the parent module.
    ///
    /// If this module is a root module, then this field is `None`.
    pub parent: Option<ID<Module>>,

    /// Maps the name of the module child to its ID.
    pub module_child_ids_by_name: HashMap<String, ModuleChildID>,

    /// Location of where the module is declared.
    pub span: Option<Span>,
}
