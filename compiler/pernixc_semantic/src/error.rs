//! Contains the definition of the semantic errors.

use enum_as_inner::EnumAsInner;
use pernixc_source::Span;

use crate::{
    symbol::{
        ConstantParameterRef, GlobalItemRef, LifetimeParameterRef, ModuleMemberRef, ModuleRef,
        TraitAssociatedRef, TypeParameterRef, VariantRef,
    },
    ty,
};

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

    /// The index of the module that doesn't contain the submodule with the given name.
    pub searched_module_ref: Option<ModuleRef>,
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

    /// The reference to the existing symbol with the same name.
    pub existing_module_member_ref: ModuleMemberRef,
}

/// The constant parameter with the given name was already declared in the generic parameters.
#[derive(Debug, Clone)]
pub struct ConstantParameterDuplication {
    /// Span of the constant parameter that caused the duplication.
    pub duplicate_span: Span,

    /// The reference to the already existing constant parameter with the same name.
    pub existing_constant_parameter_ref: ConstantParameterRef,
}

/// The type parameter with the given name was already declared in the generic parameters.
#[derive(Debug, Clone)]
pub struct TypeParameterDuplication {
    /// Span of the type parameter that caused the duplication.
    pub duplicate_span: Span,

    /// The reference to the already existing type parameter with the same name.
    pub existing_type_parameter_ref: TypeParameterRef,
}

/// The lifetime parameter with the given name was already declared in the generic parameters.
#[derive(Debug, Clone)]
pub struct LifetimeParameterDuplication {
    /// Span of the lifetime parameter that caused the duplication.
    pub duplicate_span: Span,

    /// The reference to the already existing lifetime parameter with the same name.
    pub existing_lifetime_parameter_ref: LifetimeParameterRef,
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

    /// The reference to the existing variant with the same name.
    pub existing_variant_ref: VariantRef,
}

/// The trait member was already declared in the trait.
#[derive(Debug, Clone)]
pub struct TraitMemberDuplication {
    /// Span of the trait member that caused the duplication.
    pub duplication_span: Span,

    /// The reference to the existing trait member with the same name.
    pub existing_trait_member_ref: TraitAssociatedRef,
}

/// The resolution of the identifier was ambiguous.
#[derive(Debug, Clone)]
pub struct ResolutionAmbiguity {
    /// Span of the identifier that caused the ambiguity.
    pub identifier_span: Span,

    /// The candidates that caused the ambiguity.
    pub candidates: Vec<GlobalItemRef>,
}

/// The symbol was not found in the current scope.
#[derive(Debug, Clone)]
pub struct SymbolNotFound {
    /// The span of the symbol reference.
    pub symbol_reference_span: Span,
}

/// The symbol doesn not require generic arguments but the generic arguments were provided.
#[derive(Debug, Clone)]
pub struct NoGenericArgumentsRequired {
    /// The span of the generic arguments.
    pub generic_arguments_span: Span,

    /// The reference to the symbol that doesn't require generic arguments.
    pub global_item_ref: GlobalItemRef,
}

/// Couldn't refer to the target symbol because the target symbol was not found.
#[derive(Debug, Clone)]
pub struct TargetNotFound {
    /// The span of the target symbol reference.
    pub target_name_span: Span,
}

/// The symbol was not accessible from the current scope.
#[derive(Debug, Clone)]
pub struct SymbolNotAccessible {
    /// The span of the symbol reference.
    pub reference_span: Span,

    /// The reference to the site that referred to the symbol.
    pub referring_site_ref: GlobalItemRef,

    /// The reference to the symbol that was not accessible.
    pub referred_ref: GlobalItemRef,
}

/// The expression doesn't have the member with the given name.
#[derive(Debug, Clone)]
pub struct NoMemberOnExpression {
    /// The span of the path that tried to access the member on the expression.
    pub member_reference_span: Span,
}

/// Cyclic dependency was detected.
#[derive(Debug, Clone)]
pub struct CyclicDependency {
    /// Span to the symbol declaration that participated in the cycle.
    pub participant_refs: Vec<GlobalItemRef>,
}

/// Trait associated items are expected in associated bounds.
#[derive(Debug, Clone)]
pub struct TraitAssociatedItemExpected {
    /// The span of the item that was expected to be a trait associated item.
    pub expected_span: Span,
}

/// Trait associated bounds kind mismatched.
#[derive(Debug, Clone)]
pub struct MismatchedTraitAssociatedBound {
    /// The span of the trait associated bound that caused the error.
    pub trait_associated_bound_span: Span,
}

/// No member with the given name was found on the type.
#[derive(Debug, Clone)]
pub struct NoMemberOnType {
    /// The type that was attempted to access the member.
    pub ty: ty::Type,

    /// The span of the member access.
    pub member_access_span: Span,
}

/// The number of lifetime arguments supplied does not match the number of lifetime parameters.
#[derive(Debug, Clone)]
pub struct LifetimeArgumentCountMismatch {
    /// The number of expected lifetime arguments.
    pub expected_count: usize,

    /// The number of supplied lifetime arguments.
    pub actual_count: usize,

    /// The span of the generic identifier containing mismatched lifetime argument count.
    pub generic_identifier_span: Span,
}

/// The number of type arguments supplied does not match the number of type parameters.
#[derive(Debug, Clone)]
pub struct TypeArgumentCountMismatch {
    /// The number of expected type arguments.
    pub expected_count: usize,

    /// The number of supplied type arguments.
    pub actual_count: usize,

    /// The span of the generic identifier containing mismatched type argument count.
    pub generic_identifier_span: Span,
}

/// The number of constant arguments supplied does not match the number of constant parameters.
#[derive(Debug, Clone)]
pub struct ConstantArgumentCountMismatch {
    /// The number of expected constant arguments.
    pub expected_count: usize,

    /// The number of supplied constant arguments.
    pub actual_count: usize,

    /// The span of the generic identifier containing mismatched constant argument count.
    pub generic_identifier_span: Span,
}

/// Type arguments must be supplied prior to constant arguments.
#[derive(Debug, Clone)]
pub struct TypeArgumentSuppliedAfterConstantArgument {
    /// The span of the type argument.
    pub type_argument_span: Span,
}

/// Lifetime arguments must be supplied to this item.
#[derive(Debug, Clone)]
pub struct LifetimeArgumentsRequired {
    /// The span of the generic identifier missing the lifetime arguments.
    pub generic_identifier_span: Span,
}

/// Lifetime arguments must be supplied prior to constant or type arguments.
#[derive(Debug, Clone)]
pub struct LifetimeArgumentSuppliedAfterConstantOrTypeArgument {
    /// The span of the lifetime argument.
    pub lifetime_argument_span: Span,
}

/// Lifetime parameter with the given name cannot be found.
#[derive(Debug, Clone)]
pub struct LifetimeParameterNotFound {
    /// The span of the lifetime parameter.
    pub unknown_lifetime_parameter_span: Span,
}

/// Enumeration containing all possible semantic errors that can occur in this phase.
#[derive(Debug, Clone, EnumAsInner)]
#[allow(missing_docs)]
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
    NoMemberOnExpression(NoMemberOnExpression),
    ModuleUsingItself(ModuleUsingItself),
    ResolutionAmbiguity(ResolutionAmbiguity),
    SymbolNotAccessible(SymbolNotAccessible),
    SymbolNotFound(SymbolNotFound),
    TargetNotFound(TargetNotFound),
    TraitExpected(TraitExpected),
    LifetimeParameterDeclaredAfterTypeOrConstantParameter(
        LifetimeParameterDeclaredAfterTypeOrConstantParameter,
    ),
    CyclicDependency(CyclicDependency),
    TypeParameterDeclaredAfterConstantParameter(TypeParameterDeclaredAfterConstantParameter),
    MismatchedTraitAssociatedBound(MismatchedTraitAssociatedBound),
    TraitAssociatedItemExpected(TraitAssociatedItemExpected),
    NoMemberOnType(NoMemberOnType),
    TypeArgumentSuppliedAfterConstantArgument(TypeArgumentSuppliedAfterConstantArgument),
    LifetimeArgumentSuppliedAfterConstantOrTypeArgument(
        LifetimeArgumentSuppliedAfterConstantOrTypeArgument,
    ),
    LifetimeArgumentsRequired(LifetimeArgumentsRequired),
    LifetimeParameterNotFound(LifetimeParameterNotFound),
    LifetimeArgumentCountMismatch(LifetimeArgumentCountMismatch),
    TypeArgumentCountMismatch(TypeArgumentCountMismatch),
    ConstantArgumentCountMismatch(ConstantArgumentCountMismatch),
}
