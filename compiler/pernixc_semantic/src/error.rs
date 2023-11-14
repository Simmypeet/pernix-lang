//! Contains all the definition of errors that can be emitted by the semantic analyzer.

use pernixc_base::source_file::Span;

use crate::{
    arena::ID,
    symbol::{GlobalID, Module, ModuleMemberID},
};

/// The item symbol with the same name already exists in the given scope.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ItemDuplication {
    /// The ID of the existing symbol
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

/// Expected a module in the module path, but found other kind of symbol.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModuleExpected {
    /// The module path that was expected to be a module.
    pub module_path: Span,

    /// The ID of the symbol that was found instead of a module.
    pub found_id: GlobalID,
}

/// The symbol is not accessible from the referring site.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SymbolIsNotAccessible {
    /// [`GlobalID`] where the [`Self::referred`] is referred.
    pub referring_site: GlobalID,

    /// The symbol that was referred and is not accessible.
    pub referred: GlobalID,

    /// The span where the [`Self::referred`] is referred from.
    pub referred_span: Span,
}

/// The symbol resolution resulted in multiple candidates.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ResolutionAmbiguity {
    /// The span where the resolution occurred.
    pub resolution_span: Span,

    /// The candidates that were found.
    pub candidates: Vec<GlobalID>,
}

/// The symbol was not found in the given scope.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SymbolNotFound {
    /// The [`GlobalID`] where the symbol was searched in. If `None`, the root module was searched.
    pub searched_global_id: Option<GlobalID>,

    /// The span where the symbol was searched from.
    pub resolution_span: Span,
}

/// The symbol doesn't require any generic arguments but some were supplied.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NoGenericArgumentsRequired {
    /// The symbol that  was supplied with generic arguments.
    pub global_id: GlobalID,

    /// The span where the generic arguments were supplied.
    pub generic_argument_span: Span,
}

/// The implementation is expected to implement a trait, but the trait was not found.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitExpectedInImplemenation {
    /// The ID of the symbol that was not a trait.
    pub found_id: GlobalID,

    /// The span of the trait path.
    pub trait_path: Span,
}

/// The cyclic dependency was found in the given set of symbols.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CyclicDependency {
    /// List of symbols that are involved in the cycle.
    pub participants: Vec<GlobalID>,
}

/// The generic arguments were supplied in the wrong order.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MisorderedGenericArguments {
    /// The span of the generic argument.
    pub generic_argument: Span,
}

/// Enumeration of all kinds of generic parameters.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum GenericParameterKind {
    Type,
    Lifetime,
    Constant,
}

/// Generic arguments count mismatch.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GenericArgumentCountMismatch {
    /// The kind of the generic parameter.
    pub generic_parameter_kind: GenericParameterKind,

    /// Span where mismatch occurred.
    pub generic_identifier_span: Span,

    /// Expected count of generic arguments.
    pub expected_count: usize,

    /// Supplied count of generic arguments.
    pub supplied_count: usize,
}

/// The lifetime parameter was not found in the given scope.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LifetimeParameterNotFound {
    /// The span where the lifetime parameter was referred from.
    pub referred_span: Span,

    /// The [`GlobalID`] where the referenced occurred from.
    pub referring_site: GlobalID,
}

/// A lifetime wasn't supplied in the reference.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LifetimeExpected {
    /// The span where the lifetime was expected.
    pub expected_span: Span,
}

/// The tuple type contains more than one unpacked type.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MoreThanOneUnpackedInTupleType {
    /// The span where the illegal tuple type was found.
    pub illegal_tuple_type_span: Span,
}

/// The type was expected but the non-type symbol was found.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeExpected {
    /// The span where the non-type symbol was found.
    pub non_type_symbol_span: Span,
}

/// Trying to access a member on a type that is not a struct or an enum.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MemberAccessOnType {
    /// The span where the member access occurred.
    pub access_span: Span,
}

/// Trying to access a member on the expression
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MemberAccessOnExpression {
    /// The span where the member access occurred.
    pub access_span: Span,
}

/// An enumeration containing all kinds of errors that can be emitted by the semantic analyzer.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Error {
    GlobalSymbolDuplication(ItemDuplication),
    ModuleNotFound(ModuleNotFound),
    ModuleExpected(ModuleExpected),
    SymbolIsNotAccessible(SymbolIsNotAccessible),
    ResolutionAmbiguity(ResolutionAmbiguity),
    SymbolNotFound(SymbolNotFound),
    NoGenericArgumentsRequired(NoGenericArgumentsRequired),
    TraitExpectedInImplemenation(TraitExpectedInImplemenation),
    CyclicDependency(CyclicDependency),
    MisorderedGenericArguments(MisorderedGenericArguments),
    GenericArgumentCountMismatch(GenericArgumentCountMismatch),
    LifetimeParameterNotFound(LifetimeParameterNotFound),
    LifetimeExpected(LifetimeExpected),
    MoreThanOneUnpackedInTupleType(MoreThanOneUnpackedInTupleType),
    TypeExpected(TypeExpected),
    MemberAccessOnType(MemberAccessOnType),
    MemberAccessOnExpression(MemberAccessOnExpression),
}
