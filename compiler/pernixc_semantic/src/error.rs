use enum_as_inner::EnumAsInner;
use pernixc_source::Span;
use pernixc_system::arena;

use crate::symbol::Module;

/// After resolved the module path, the found symbol was not a `module` but something else.
#[derive(Debug, Clone)]
pub struct ModuleExpected {
    /// The span of the module path that was resolved into the found symbol.
    pub module_path_span: Span,
}

/// After resolved the trait path, the found symbol was not a `trait` but something else.
#[derive(Debug, Clone)]
pub struct TraitExpected {
    /// The span of the trait path that was resolved into the found symbol.
    pub trait_path_span: Span,
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
    pub existing_symbol_span: Span,
}

/// The constant parameter with the given name was already declared in the generic parameters.
#[derive(Debug, Clone)]
pub struct ConstantParameterDuplication {
    /// Span of the constant parameter that caused the duplication.
    pub duplicate_span: Span,

    /// The span of the existing constant parameter with the same name.
    pub existing_constant_parameter: Span,
}

/// The type parameter with the given name was already declared in the generic parameters.
#[derive(Debug, Clone)]
pub struct TypeParameterDuplication {
    /// Span of the type parameter that caused the duplication.
    pub duplicate_span: Span,

    /// The span of the existing type parameter with the same name.
    pub existing_type_parameter: Span,
}

/// The lifetime parameter with the given name was already declared in the generic parameters.
#[derive(Debug, Clone)]
pub struct LifetimeParameterDuplication {
    /// Span of the lifetime parameter that caused the duplication.
    pub duplicate_span: Span,

    /// The span of the existing lifetime parameter with the same name.
    pub existing_lifetime_parameter_span: Span,
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

/// The variant was already declared in the enum.
#[derive(Debug, Clone)]
pub struct VariantDuplication {
    /// Span of the variant that caused the duplication.
    pub duplication_span: Span,

    /// The span of the existing variant with the same name.
    pub existing_variant_span: Span,
}

/// The trait member was already declared in the trait.
#[derive(Debug, Clone)]
pub struct TraitMemberDuplication {
    /// Span of the trait member that caused the duplication.
    pub duplication_span: Span,

    /// The span of the existing trait member with the same name.
    pub existing_trait_member_span: Span,
}

/// The resolution of the identifier was ambiguous.
#[derive(Debug, Clone)]
pub struct ResolutionAmbiguity {
    /// Span of the identifier that caused the ambiguity.
    pub identifier_span: Span,

    /// The qualified names of the candidates.
    pub candidate_qualified_names: Vec<String>,
}

/// The symbol was not found in the current scope.
#[derive(Debug, Clone)]
pub struct SymbolNotFound {
    /// The span of the symbol reference.
    pub symbol_reference_span: Span,
}

/// The symbol did not require generic arguments but the generic arguments were provided.
#[derive(Debug, Clone)]
pub struct NoGenericArgumentsRequired {
    /// The span of the generic arguments.
    pub generic_arguments_span: Span,

    /// The qualified name of the symbol.
    pub qualified_name: String,
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum Error {
    VariantDuplication(VariantDuplication),
    TraitMemberDuplication(TraitMemberDuplication),
    UsingDuplication(UsingDuplication),
    ModuleNotFound(ModuleNotFound),
    ModuleExpected(ModuleExpected),
    SymbolDuplication(SymbolDuplication),
    NoGenericArgumentsRequired(NoGenericArgumentsRequired),
    ConstantParameterDuplication(ConstantParameterDuplication),
    TypeParameterDuplication(TypeParameterDuplication),
    LifetimeParameterDuplication(LifetimeParameterDuplication),
    ModuleUsingItself(ModuleUsingItself),
    ResolutionAmbiguity(ResolutionAmbiguity),
    SymbolNotFound(SymbolNotFound),
    TraitExpected(TraitExpected),
    LifetimeParameterDeclaredAfterTypeOrConstantParameter(
        LifetimeParameterDeclaredAfterTypeOrConstantParameter,
    ),
    TypeParameterDeclaredAfterConstantParameter(TypeParameterDeclaredAfterConstantParameter),
}
