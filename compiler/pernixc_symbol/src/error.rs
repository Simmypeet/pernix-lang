//! Contains the definition of all semantic errors that can occur during the symbol
//! resolution/analysis.

use pernixc_source::Span;
use pernixc_system::arena;

use crate::{
    Field, GlobalID, LifetimeParameter, Module, Parameter, ScopedID, Struct, TraitMemberID,
    TypeParameter, ID,
};

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
    pub in_module_id: arena::ID<Module>,

    /// The span of the unknown module name.
    pub unknown_module_span: Span,
}

/// A using statement was found duplicatig a previous using statement.
#[derive(Debug, Clone)]
pub struct UsingDuplication {
    /// The span of the previous using statement.
    pub previous_using_span: Span,

    /// The span of the duplicate using statement.
    pub duplicate_using_span: Span,
}

/// A using statement was found using a module that is the same as the module that it is in.
#[derive(Debug, Clone)]
pub struct UsingOwnModule {
    /// The module that is being used.
    pub module_id: arena::ID<Module>,

    /// The span of the using statement.
    pub using_span: Span,
}

/// A lifetime parameter was found to be declared after a type parameter.
#[derive(Debug, Clone)]
pub struct LifetimeParameterMustBeDeclaredPriotToTypeParameter {
    /// The span of the lifetime parameter.
    pub lifetime_parameter_span: Span,
}

/// Symbol redefinition error.
#[derive(Debug, Clone)]
pub struct SymbolRedefinition<T> {
    /// The id of the symbol that is being redefined.
    pub previous_definition_id: T,

    /// Span to the syntax node that is redefining the symbol.
    pub redefinition_span: Span,
}

/// The struct filed is more accessible than the struct itself.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FieldMoreAccessibleThanStruct {
    /// The id of the field.
    pub field_id: arena::ID<Field>,

    /// The id of the struct.
    pub struct_id: arena::ID<Struct>,
}

/// There is a cyclic dependency between the symbols.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CyclicDependency {
    /// The list of symbols that are involved in the cycle.
    pub participants: Vec<ID>,
}

/// Multiples symbol candidates were found for the same symbol reference.
#[derive(Debug, Clone)]
pub struct ResolutionAmbiguity {
    /// The span of the symbol reference.
    pub span: Span,

    /// The list of candidates.
    pub candidates: Vec<GlobalID>,
}

/// Symbol with the given name was not found.
#[derive(Debug, Clone)]
pub struct SymbolNotFound {
    /// In which scope the symbol was searched.
    pub searched_scoped_id: ScopedID,

    /// The span of the symbol reference.
    pub span: Span,
}

/// Found a qualified identifier can't be used as a path to a trait.
///
/// This error occurs when resolving the trait symbol of a qualified identifier in the `implements`
/// block.
#[derive(Debug, Clone)]
pub struct InvalidTraitPath {
    /// The span of the qualified identifier.
    pub span: Span,
}

/// Expects the symbol to be module but found something else.
#[derive(Debug, Clone)]
pub struct ModuleExpected {
    /// The span of the symbol reference.
    pub span: Span,

    /// The symbol that was found instead.
    pub found: GlobalID,
}

/// Expects the symbol to be a trait but found something else.
#[derive(Debug, Clone)]
pub struct TraitExpected {
    /// The span of the symbol reference.
    pub span: Span,

    /// The symbol that was found instead.
    pub found: GlobalID,
}

/// Lifetime parameter shadowing is not allowed.
#[derive(Debug, Clone)]
pub struct LifetimeParameterShadowing {
    /// The span of the lifetime parameter.
    pub span: Span,

    /// The ID of the shadowed lifetime parameter.
    pub shadowed_lifetime_parameter_id: arena::ID<LifetimeParameter>,
}

/// Type parameter shadowing is not allowed.
#[derive(Debug, Clone)]
pub struct TypeParameterShadowing {
    /// The span of the type parameter.
    pub span: Span,

    /// The ID of the shadowed type parameter.
    pub shadowed_type_parameter_id: arena::ID<TypeParameter>,
}

/// Is an eumeration of all errors occuring during the symbol resolution/analysis.
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub enum Error {
    TargetNotFound(TargetNotFound),
    ModuleNotFound(ModuleNotFound),
    UsingDuplication(UsingDuplication),
    UsingOwnModule(UsingOwnModule),
    SymbolRedefinition(SymbolRedefinition<GlobalID>),
    LifetimeParameterRedefinition(SymbolRedefinition<arena::ID<LifetimeParameter>>),
    TypeParameterRedefinition(SymbolRedefinition<arena::ID<TypeParameter>>),
    LifetimeParameterMustBeDeclaredPriotToTypeParameter(
        LifetimeParameterMustBeDeclaredPriotToTypeParameter,
    ),
    ParameterRedefinition(SymbolRedefinition<arena::ID<Parameter>>),
    FieldRedefinition(SymbolRedefinition<arena::ID<Field>>),
    FieldMoreAccessibleThanStruct(FieldMoreAccessibleThanStruct),
    TraitMemberRedefinition(SymbolRedefinition<TraitMemberID>),
    CyclicDependency(CyclicDependency),
    ResolutionAmbiguity(ResolutionAmbiguity),
    SymbolNotFound(SymbolNotFound),
    InvalidTraitPath(InvalidTraitPath),
    ModuleExpected(ModuleExpected),
    TraitExpected(TraitExpected),
    LifetimeParameterShadowing(LifetimeParameterShadowing),
    TypeParameterShadowing(TypeParameterShadowing),
}
