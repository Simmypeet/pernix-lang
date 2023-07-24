//! Contains the definition of all semantic errors that can occur during the symbol
//! resolution/analysis.

use pernixc_print::LogSeverity;
use pernixc_source::Span;
use pernixc_system::arena;

use crate::{
    ty, Field, Function, GlobalID, ImplementsFunction, LifetimeArgument, LifetimeParameter, Module,
    Parameter, Struct, TraitBound, TraitFunction, TraitMemberID, TypeParameter, ID,
};

/// No target was found with the given name.
#[derive(Debug, Clone)]
pub struct TargetNotFound {
    /// The span of the unknown target name.
    pub unknown_target_span: Span,
}

impl TargetNotFound {
    /// Prints the error message to the stdout.
    pub fn print(&self) {
        pernixc_print::print(
            LogSeverity::Error,
            format!(
                "the target name `{}` didn't exist",
                self.unknown_target_span.str()
            )
            .as_str(),
        );
    }
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
pub struct LifetimeParameterMustBeDeclaredPriorToTypeParameter {
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

/// No lifetime with the given name was found.
#[derive(Debug, Clone)]
pub struct LifetimeNotFound {
    /// The span of the unknown lifetime name.
    pub unknown_lifetime_span: Span,
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
    pub searched_global_id: GlobalID,

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

/// Found lifetime arguments after type arguments.
#[derive(Debug, Clone)]
pub struct LifetimeArgumentMustBeSuppliedPriorToTypeArgument {
    /// The span of the type argument.
    pub lifetime_argument_span: Span,
}

/// The number of supplied lifetime arguments does not match the number of expected lifetime
#[derive(Debug, Clone)]
pub struct LifetimeArgumentCountMismatch {
    /// The number of supplied lifetime arguments.
    pub supplied: usize,

    /// The number of expected lifetime arguments.
    pub expected: usize,

    /// The span to the generic arguments that contain the lifetime arguments.
    pub generic_arguments_span: Span,
}

/// The number of supplied type arguments does not match the number of expected type arguments.
#[derive(Debug, Clone)]
pub struct TypeArgumentCountMismatch {
    /// The number of supplied type arguments.
    pub supplied: usize,

    /// The number of expected type arguments.
    pub expected: usize,

    /// The span to the generic arguments that contain the type arguments.
    pub generic_arguments_span: Span,
}

/// Lifetime arguments must be supplied to the type in this context.
#[derive(Debug, Clone)]
pub struct LifetimeArgumentsRequired {
    /// The span of the type with missing lifetime arguments.
    pub span: Span,
}

/// There is no member on this type.
#[derive(Debug, Clone)]
pub struct NoMemberOnThisType {
    /// The span of the member name, trying to access.
    pub span: Span,

    /// The type that does not have the member.
    pub ty: ty::Type,
}

/// Trait resolution is not allowed in this context.
#[derive(Debug, Clone, Copy)]
pub struct TraitResolutionNotAllowed;

/// The given symbol can't be used as a type.
#[derive(Debug, Clone)]
pub struct TypeExpected {
    /// The span of the symbol reference.
    pub span: Span,
}

/// The symbol is not accessible from the given referring site.
#[derive(Debug, Clone)]
pub struct SymbolIsNotAccessible {
    /// The span of the symbol reference.
    pub span: Span,

    /// Where the symbol is being referred from.
    pub referring_site: ID,

    /// The symbol that is not accessible.
    pub referred: GlobalID,
}

/// The symbol does not require any generic arguments but some were supplied.
#[derive(Debug, Clone)]
pub struct NoGenericArgumentsRequired {
    /// The span of the generic arguments.
    pub span: Span,
}

/// The more private symbol is being exposed in a more public scope.
#[derive(Debug, Clone)]
pub struct PrivateSymbolLeakage {
    /// The span of the symbol reference.
    pub span: Span,
}

/// The supplied lifetime argument does not outlive the required lifetime.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LifetimeDoesNotOutlive {
    /// The supplied lifetime that does not outlive the required lifetime.
    pub passed_lifetime_parameter: arena::ID<LifetimeParameter>,

    /// The lifetime that the supplied argument must outlive.
    pub required_lifetime_argument: LifetimeArgument,
}

/// The required trait bound is not satisfied.
#[derive(Debug, Clone)]
pub struct TraitBoundNotSatisfied {
    /// The required trait bound that is not satisfied.
    pub required_trait_bound: TraitBound,
}

/// Is an enumeration of all errors occurring during the symbol resolution/analysis.
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub enum Error {
    TypeExpected(TypeExpected),
    TargetNotFound(TargetNotFound),
    ModuleNotFound(ModuleNotFound),
    UsingDuplication(UsingDuplication),
    UsingOwnModule(UsingOwnModule),
    SymbolRedefinition(SymbolRedefinition<GlobalID>),
    LifetimeParameterRedefinition(SymbolRedefinition<arena::ID<LifetimeParameter>>),
    TypeParameterRedefinition(SymbolRedefinition<arena::ID<TypeParameter>>),
    LifetimeArgumentMustBeSuppliedPriorToTypeArgument(
        LifetimeArgumentMustBeSuppliedPriorToTypeArgument,
    ),
    LifetimeParameterMustBeDeclaredPriorToTypeParameter(
        LifetimeParameterMustBeDeclaredPriorToTypeParameter,
    ),
    FunctionParameterRedefinition(SymbolRedefinition<arena::ID<Parameter<Function>>>),
    TraitFunctionParameterRedefinition(SymbolRedefinition<arena::ID<Parameter<TraitFunction>>>),
    ImplementsFunctionParameterRedefinition(
        SymbolRedefinition<arena::ID<Parameter<ImplementsFunction>>>,
    ),
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
    LifetimeNotFound(LifetimeNotFound),
    LifetimeArgumentCountMismatch(LifetimeArgumentCountMismatch),
    TypeArgumentCountMismatch(TypeArgumentCountMismatch),
    LifetimeArgumentsRequired(LifetimeArgumentsRequired),
    NoMemberOnThisType(NoMemberOnThisType),
    TraitResolutionNotAllowed(TraitResolutionNotAllowed),
    SymbolIsNotAccessible(SymbolIsNotAccessible),
    NoGenericArgumentsRequired(NoGenericArgumentsRequired),
    PrivateSymbolLeakage(PrivateSymbolLeakage),
    LifetimeDoesNotOutlive(LifetimeDoesNotOutlive),
    TraitBoundNotSatisfied(TraitBoundNotSatisfied),
}
