use pernixc_source::Span;

use crate::ModuleID;

/// No target was found with the given name.
#[derive(Debug, Clone)]
pub struct TargetNotFound {
    /// The span of the unknown target name.
    pub unknown_target_span: Span,
}

/// No module was found with the given name in the given module.
#[derive(Debug, Clone)]
pub struct ModuleNotFound {
    /// The module where the modules were searched for.
    pub in_module_id: ModuleID,

    /// The span of the unknown module name.
    pub unknown_module_span: Span,
}

/// A using statement was found that is a duplicate of a previous using statement.
#[derive(Debug, Clone)]
pub struct UsingDuplication {
    /// The span of the previous using statement.
    pub previous_using_span: Span,

    /// The span of the duplicate using statement.
    pub duplicate_using_span: Span,
}

/// A using statement was found that is using a module that is the same as the module that it is in.
#[derive(Debug, Clone)]
pub struct UsingOwnModule {
    /// The module that is being used.
    pub module_id: ModuleID,

    /// The span of the using statement.
    pub using_span: Span,
}

/// Is an eumeration of all errors occuring during the symbol resolution/analysis.
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub enum Error {
    TargetNotFound(TargetNotFound),
    ModuleNotFound(ModuleNotFound),
    UsingDuplication(UsingDuplication),
    UsingOwnModule(UsingOwnModule),
}
