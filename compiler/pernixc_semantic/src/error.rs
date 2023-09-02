use enum_as_inner::EnumAsInner;
use pernixc_source::Span;
use pernixc_system::arena;

use crate::symbol::{self, ConstantParameterRef, Module};

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

/// The using statement uses the module that is using itself.
#[derive(Debug, Clone)]
pub struct ModuleUsingItself {
    /// Span of the `using` statement that caused the error.
    pub using_span: Span,
}

/// The symbol with the given name was already declared in the module.
#[derive(Debug, Clone)]
pub struct SymbolDuplication {
    /// Span of the symbol that caused the duplication.
    pub duplicate_span: Span,

    /// Span of the symbol that was already declared.
    pub existing_symbol_id: symbol::ID,
}

/// The constant parameter with the given name was already declared in the generic parameters.
#[derive(Debug, Clone)]
pub struct ConstantParameterDuplication {
    /// Span of the constant parameter that caused the duplication.
    pub duplicate_span: Span,

    /// The reference to the existing constant parameter with the same name.
    pub existing_constant_parameter_ref: ConstantParameterRef,
}

/// The type parameter with the given name was already declared in the generic parameters.
#[derive(Debug, Clone)]
pub struct TypeParameterDuplication {
    /// Span of the type parameter that caused the duplication.
    pub duplicate_span: Span,

    /// The reference to the existing type parameter with the same name.
    pub existing_type_parameter_ref: symbol::TypeParameterRef,
}

/// The lifetime parameter with the given name was already declared in the generic parameters.
#[derive(Debug, Clone)]
pub struct LifetimeParameterDuplication {
    /// Span of the lifetime parameter that caused the duplication.
    pub duplicate_span: Span,

    /// The reference to the existing lifetime parameter with the same name.
    pub existing_lifetime_parameter_ref: symbol::LifetimeParameterRef,
}

/// The lifetime parameter was declared after the type or constant parameter.
#[derive(Debug, Clone)]
pub struct LifetimeParameterDeclaredAfterTypeOrConstantParameter {
    /// Span of the lifetime parameter that caused the error.
    pub lifetime_parameter_span: Span,
}

/// The type parameter was declared after the constant parameter.
#[derive(Debug, Clone)]
pub struct TypeParameterDeclaredAfterConstantParameter {
    /// Span of the type parameter that caused the error.
    pub type_parameter_span: Span,
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum Error {
    UsingDuplication(UsingDuplication),
    ModuleNotFound(ModuleNotFound),
    ModuleExpected(ModuleExpected),
    SymbolDuplication(SymbolDuplication),
    ConstantParameterDuplication(ConstantParameterDuplication),
    TypeParameterDuplication(TypeParameterDuplication),
    LifetimeParameterDuplication(LifetimeParameterDuplication),
    ModuleUsingItself(ModuleUsingItself),
    LifetimeParameterDeclaredAfterTypeOrConstantParameter(
        LifetimeParameterDeclaredAfterTypeOrConstantParameter,
    ),
    TypeParameterDeclaredAfterConstantParameter(TypeParameterDeclaredAfterConstantParameter),
}
