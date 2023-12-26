//! Contains all the definition of errors that can be emitted by the semantic analyzer.

use std::{
    any::Any,
    fmt::{self, Display},
};

use pernixc_base::{
    log::{
        formatting::{Style, WithStyle},
        Message, Severity, SourceCodeDisplay,
    },
    source_file::Span,
};

use crate::{
    arena::ID,
    semantic::{predicate::Predicate, term::r#type},
    symbol::{
        semantic::Symbolic, GenericID, GenericKind, GlobalID, LocalGenericParameterID,
        TraitImplementation, TraitImplementationKindID, TraitImplementationMemberID, TraitMemberID,
    },
    table::{Index, Suboptimal, Table},
};

/// Contains both error and the table in which the error occurred.
///
/// Primiraly used for implementing [`std::fmt::Display`] trait.
#[derive(Debug, Clone, Copy)]
pub struct WithTable<'a, Error> {
    /// The table in which the error occurred.
    pub table: &'a Table<Suboptimal>,

    /// The error that occurred.
    pub error: &'a Error,
}

/// The global symbol with the same name already exists in the given scope.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GlobalRedefinition {
    /// The ID of the existing symbol.
    pub existing_global_id: GlobalID,

    /// The ID of the new symbol.
    pub new_global_id: GlobalID,

    /// The scope in which the duplication occurred.
    pub in_global_id: GlobalID,
}

impl Display for WithTable<'_, GlobalRedefinition> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (Some(existing_symbol), Some(new_symbol), Some(scope_qualified_name)) = (
            self.table.get_global(self.error.existing_global_id),
            self.table.get_global(self.error.new_global_id),
            self.table.get_qualified_name(self.error.in_global_id),
        ) else {
            return Err(fmt::Error);
        };

        write!(f, "{}", Message {
            severity: pernixc_base::log::Severity::Error,
            display: format!(
                "The symbol `{}` is already defined in `{}`",
                existing_symbol.name(),
                scope_qualified_name
            ),
        })?;

        if let Some(existing_span) = existing_symbol.span() {
            write!(f, "\n{}", SourceCodeDisplay {
                span: &existing_span,
                help_display: Some("previously defined here"),
            })?;
        }

        if let Some(new_span) = new_symbol.span() {
            write!(f, "\n{}", SourceCodeDisplay {
                span: &new_span,
                help_display: Some("redefined here"),
            })?;
        }

        Ok(())
    }
}

/// Expected a module in the module path, but found other kind of symbol.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModuleExpected {
    /// The module path that was expected to be a module.
    pub module_path: Span,

    /// The ID of the symbol that was found instead of a module.
    pub found_id: GlobalID,
}

impl GlobalID {
    /// Returns the kind of the symbol description as a string.
    #[must_use]
    pub fn kind_str(&self) -> &'static str {
        match self {
            Self::Module(_) => "module",
            Self::Type(_) => "type",
            Self::Constant(_) => "constant",
            Self::Trait(_) => "trait",
            Self::TraitImplementation(_) => "trait implementation",
            Self::TraitFunction(_) => "trait function",
            Self::TraitType(_) => "trait type",
            Self::TraitConstant(_) => "trait constant",
            Self::TraitImplementationFunction(_) => "trait implementation function",
            Self::TraitImplementationType(_) => "trait implementation type",
            Self::TraitImplementationConstant(_) => "trait implementation constant",
            Self::Struct(_) => "struct",
            Self::Enum(_) => "enum",
            Self::Function(_) => "function",
            Self::Variant(_) => "enum variant",
            Self::NegativeTraitImplementation(_) => "negative implementation",
            Self::AdtImplementation(_) => "implementation",
            Self::AdtImplementationType(_) => "implementation type",
            Self::AdtImplementationFunction(_) => "implementation function",
            Self::AdtImplementationConstant(_) => "implementation constant",
        }
    }
}

impl Display for WithTable<'_, ModuleExpected> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let found_symbol_qualified_name = self
            .table
            .get_qualified_name(self.error.found_id)
            .ok_or(fmt::Error)?;

        write!(f, "{}", Message {
            severity: Severity::Error,
            display: format!(
                "Expected a module in the module path, but found `{} {}`",
                self.error.found_id.kind_str(),
                found_symbol_qualified_name
            ),
        })?;

        write!(f, "\n{}", SourceCodeDisplay {
            span: &self.error.module_path,
            help_display: Option::<i32>::None,
        })?;

        Ok(())
    }
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

impl Display for WithTable<'_, SymbolIsNotAccessible> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let referring_site_qualified_name = self
            .table
            .get_qualified_name(self.error.referring_site)
            .ok_or(fmt::Error)?;

        let referred_qualified_name = self
            .table
            .get_qualified_name(self.error.referred)
            .ok_or(fmt::Error)?;

        write!(f, "{}", Message {
            severity: Severity::Error,
            display: format!(
                "The symbol `{referred_qualified_name}` is not accessible from \
                 `{referring_site_qualified_name}`",
            ),
        })?;

        write!(f, "\n{}", SourceCodeDisplay {
            span: &self.error.referred_span,
            help_display: Option::<i32>::None,
        })?;

        Ok(())
    }
}

/// The symbol resolution resulted in multiple candidates.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ResolutionAmbiguity {
    /// The span where the resolution occurred.
    pub resolution_span: Span,

    /// The candidates that were found.
    pub candidates: Vec<GlobalID>,
}

impl Display for WithTable<'_, ResolutionAmbiguity> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", Message {
            severity: Severity::Error,
            display: "The symbol resolution resulted in multiple candidates",
        })?;
        for candidate in &self.error.candidates {
            let candidate_qualified_name = self
                .table
                .get_qualified_name(*candidate)
                .ok_or(fmt::Error)?;

            write!(f, "\n  - {}", WithStyle {
                style: Style::Bold,
                display: candidate_qualified_name,
            })?;
        }

        write!(f, "\n{}", SourceCodeDisplay {
            span: &self.error.resolution_span,
            help_display: Option::<i32>::None,
        })?;

        Ok(())
    }
}

/// The symbol was not found in the given scope.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SymbolNotFound {
    /// The [`GlobalID`] where the symbol was searched in. If `None`, the root module was searched.
    pub searched_global_id: Option<GlobalID>,

    /// The span where the symbol was searched from.
    pub resolution_span: Span,
}

impl Display for WithTable<'_, SymbolNotFound> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(searched_in_module_id) = self.error.searched_global_id {
            let qualified_name = self
                .table
                .get_qualified_name(searched_in_module_id)
                .ok_or(fmt::Error)?;

            write!(f, "{}", Message {
                severity: Severity::Error,
                display: format!(
                    "The symbol named `{}` does not exist in `{}`",
                    self.error.resolution_span.str(),
                    qualified_name
                ),
            })?;
        } else {
            write!(f, "{}", Message {
                severity: Severity::Error,
                display: format!(
                    "The symbol `{}` does not exist",
                    self.error.resolution_span.str(),
                ),
            })?;
        }

        write!(f, "\n{}", SourceCodeDisplay {
            span: &self.error.resolution_span,
            help_display: Option::<i32>::None
        })?;

        Ok(())
    }
}

/// The symbol doesn't require any generic arguments but some were supplied.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NoGenericArgumentsRequired {
    /// The symbol that  was supplied with generic arguments.
    pub global_id: GlobalID,

    /// The span where the generic arguments were supplied.
    pub generic_argument_span: Span,
}

impl Display for WithTable<'_, NoGenericArgumentsRequired> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let qualified_name = self
            .table
            .get_qualified_name(self.error.global_id)
            .ok_or(fmt::Error)?;

        write!(f, "{}", Message {
            severity: Severity::Error,
            display: format!("The symbol `{qualified_name}` doesn't require any generic arguments"),
        })?;

        write!(f, "\n{}", SourceCodeDisplay {
            span: &self.error.generic_argument_span,
            help_display: Some("found supplied here"),
        })?;

        Ok(())
    }
}

/// The implementation is expected to implement a trait, but the trait was not found.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitExpectedInImplemenation {
    /// The ID of the symbol that was not a trait.
    pub found_id: GlobalID,

    /// The span of the trait path.
    pub trait_path: Span,
}

impl Display for WithTable<'_, TraitExpectedInImplemenation> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let found_symbol_qualified_name = self
            .table
            .get_qualified_name(self.error.found_id)
            .ok_or(fmt::Error)?;

        write!(f, "{}", Message {
            severity: Severity::Error,
            display: format!(
                "Expected a trait in the trait path, but found `{} {}`",
                self.error.found_id.kind_str(),
                found_symbol_qualified_name
            ),
        })?;

        write!(f, "\n{}", SourceCodeDisplay {
            span: &self.error.trait_path,
            help_display: Option::<i32>::None,
        })?;

        Ok(())
    }
}

/// The cyclic dependency was found in the given set of symbols.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CyclicDependency {
    /// List of symbols that are involved in the cycle.
    pub participants: Vec<GlobalID>,
}

impl Display for WithTable<'_, CyclicDependency> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let symbol_list = self
            .error
            .participants
            .iter()
            .map(|&symbol| {
                let qualified_name = self.table.get_qualified_name(symbol).ok_or(fmt::Error)?;

                Ok(format!("`{qualified_name}`"))
            })
            .collect::<Result<Vec<_>, _>>()?
            .join(", ");

        write!(f, "{}", Message {
            severity: Severity::Error,
            display: format!(
                "The cyclic dependency was found in the given set of symbols: {symbol_list}"
            ),
        })?;

        for participant in &self.error.participants {
            if let Some(participant_span) = self
                .table
                .get_global(*participant)
                .ok_or(fmt::Error)?
                .span()
            {
                write!(f, "\n{}", SourceCodeDisplay {
                    span: &participant_span,
                    help_display: Option::<i32>::None,
                })?;
            }
        }

        Ok(())
    }
}

/// The generic arguments were supplied in the wrong order.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MisorderedGenericArgument {
    /// The kind of the misordered generic argument.
    pub generic_kind: GenericKind,

    /// The span of the generic argument.
    pub generic_argument: Span,
}

impl<'a> Display for WithTable<'a, MisorderedGenericArgument> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", Message {
            severity: Severity::Error,
            display: "The generic argument was supplied in the wrong order",
        })?;

        let help_display = match self.error.generic_kind {
            GenericKind::Type => Some("can't be supplied after constant arguments"),
            GenericKind::Lifetime => Some("can't be supplied after type or constant arguments"),
            GenericKind::Constant => None,
        };

        write!(f, "\n{}", SourceCodeDisplay {
            span: &self.error.generic_argument,
            help_display,
        })?;

        Ok(())
    }
}

/// The generic parameter was declared in the wrong order.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MisorderedGenericParameter {
    /// The kind of the misordered generic parameter.
    pub generic_kind: GenericKind,

    /// The span of the generic parameter.
    pub generic_parameter_span: Span,
}

impl Display for WithTable<'_, MisorderedGenericParameter> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", Message {
            severity: Severity::Error,
            display: "The generic parameter was declared in the wrong order",
        })?;

        let help_display = match self.error.generic_kind {
            GenericKind::Type => Some("can't be declared after constant parameters"),
            GenericKind::Lifetime => Some("can't be declared after type or constant parameters"),
            GenericKind::Constant => None,
        };

        write!(f, "\n{}", SourceCodeDisplay {
            span: &self.error.generic_parameter_span,
            help_display,
        })?;

        Ok(())
    }
}

/// An entity was exposed to the public interface but it's accessbility is less permissive than the
/// public interface.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PrivateEntityLeakedToPublicInterface<Entity> {
    /// The entity that was leaked.
    pub entity: Entity,

    /// The span where the entity was leaked.
    pub leaked_span: Span,

    /// The ID of the public interface that contains the leaked entity.
    pub public_interface_id: GlobalID,
}

/// Generic arguments count mismatch.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GenericArgumentCountMismatch {
    /// The kind of the generic parameter.
    pub generic_kind: GenericKind,

    /// Span where mismatch occurred.
    pub generic_identifier_span: Span,

    /// Expected count of generic arguments.
    pub expected_count: usize,

    /// Supplied count of generic arguments.
    pub supplied_count: usize,
}

impl Display for WithTable<'_, GenericArgumentCountMismatch> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let generic_kind = match self.error.generic_kind {
            GenericKind::Type => "type",
            GenericKind::Lifetime => "lifetime",
            GenericKind::Constant => "constant",
        };

        write!(f, "{}", Message {
            severity: Severity::Error,
            display: format!(
                "Expected {} {} arguments, but {} were supplied",
                self.error.expected_count, generic_kind, self.error.supplied_count,
            ),
        })?;

        write!(f, "\n{}", SourceCodeDisplay {
            span: &self.error.generic_identifier_span,
            help_display: Option::<i32>::None,
        })?;

        Ok(())
    }
}

/// The lifetime parameter was not found in the given scope.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LifetimeParameterNotFound {
    /// The span where the lifetime parameter was referred from.
    pub referred_span: Span,

    /// The [`GlobalID`] where the referenced occurred from.
    pub referring_site: GlobalID,
}

impl Display for WithTable<'_, LifetimeParameterNotFound> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let referring_site_qualified_name = self
            .table
            .get_qualified_name(self.error.referring_site)
            .ok_or(fmt::Error)?;

        write!(f, "{}", Message {
            severity: Severity::Error,
            display: format!(
                "The lifetime parameter `{}` was not found in `{referring_site_qualified_name}`",
                self.error.referred_span.str()
            ),
        })?;

        write!(f, "\n{}", SourceCodeDisplay {
            span: &self.error.referred_span,
            help_display: Option::<i32>::None,
        })?;

        Ok(())
    }
}

/// A lifetime wasn't supplied in the reference.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LifetimeExpected {
    /// The span where the lifetime was expected.
    pub expected_span: Span,
}

impl Display for WithTable<'_, LifetimeExpected> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", Message {
            severity: Severity::Error,
            display: "A lifetime was expected",
        })?;

        write!(f, "\n{}", SourceCodeDisplay {
            span: &self.error.expected_span,
            help_display: Option::<i32>::None,
        })?;

        Ok(())
    }
}

/// The tuple type contains more than one unpacked type.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MoreThanOneUnpackedInTupleType {
    /// The span where the illegal tuple type was found.
    pub illegal_tuple_type_span: Span,
}

impl Display for WithTable<'_, MoreThanOneUnpackedInTupleType> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", Message {
            severity: Severity::Error,
            display: "The tuple type contains more than one unpacked type",
        })?;

        write!(f, "\n{}", SourceCodeDisplay {
            span: &self.error.illegal_tuple_type_span,
            help_display: Option::<i32>::None,
        })?;

        Ok(())
    }
}

/// The type was expected but the non-type symbol was found.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeExpected {
    /// The span where the non-type symbol was found.
    pub non_type_symbol_span: Span,

    /// The [`GlobalID`] where the non-type symbol was found.
    pub resolved_global_id: GlobalID,
}

impl Display for WithTable<'_, TypeExpected> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let qualified_name = self
            .table
            .get_qualified_name(self.error.resolved_global_id)
            .ok_or(fmt::Error)?;

        write!(f, "{}", Message {
            severity: Severity::Error,
            display: format!(
                "The type was expected but found {} `{qualified_name}`",
                self.error.resolved_global_id.kind_str(),
            ),
        })?;

        write!(f, "\n{}", SourceCodeDisplay {
            span: &self.error.non_type_symbol_span,
            help_display: Option::<i32>::None,
        })?;

        Ok(())
    }
}

/// The generic parameter with the same name already exists in the given scope.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GenericParameterDuplication<ID> {
    /// The ID of the existing generic parameter.
    pub existing_generic_parameter_id: ID,

    /// The ID of the new generic parameter.
    pub duplicating_generic_parameter_span: Span,
}

/// The default generic parameter must be trailing.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DefaultGenericParameterMustBeTrailing {
    /// The span of the generic parameter.
    pub invalid_generic_default_parameter_spans: Vec<Span>,
}

/// The given type can't be used as an operand for outlives predicate.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InvalidTypeInOutlivesPredicate {
    /// The span to the predicate that contains the invalid type as its operand.
    pub outlive_predicate_span: Span,

    /// The invalid type
    pub invalid_type: r#type::Type<Symbolic>,
}

/// The given type can't be used as an operand for constant type predicate.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InvalidTypeInConstantTypePredicate {
    /// The span to the predicate that contains the invalid type as its operand.
    pub constant_type_predicate_span: Span,

    /// The invalid type
    pub invalid_type: r#type::Type<Symbolic>,
}

/// The trait member was expected but the non-trait member was found.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitMemberExpected {
    /// The span where the non-trait member was found.
    pub non_trait_member_span: Span,
}

/// Trait member bound argument mismatched.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitMemberBoundArgumentMismatched {
    /// The span where the trait member bound argument was found.
    pub trait_member_bound_argument_span: Span,
}

/// The trait member is not implemented in the implementation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitMemberNotImplemented {
    /// The ID of the trait member that is not implemented.
    pub trait_member_id: TraitMemberID,

    /// The ID of the implementation in which the trait member is not implemented.
    pub implementation_id: ID<TraitImplementation>,
}

impl Display for WithTable<'_, TraitMemberNotImplemented> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let trait_name = self
            .table
            .get_qualified_name(self.error.trait_member_id.into())
            .ok_or(fmt::Error)?;

        write!(f, "{}", Message {
            severity: Severity::Error,
            display: format!("The trait member `{trait_name}` is not implemented"),
        })?;

        if let Some(trait_member_span) = self
            .table
            .get_global(self.error.trait_member_id.into())
            .ok_or(fmt::Error)?
            .span()
        {
            write!(f, "\n{}", SourceCodeDisplay {
                span: &trait_member_span,
                help_display: Some("is required to be implemented")
            })?;
        }

        if let Some(implementation_span) = self
            .table
            .get_global(self.error.implementation_id.into())
            .ok_or(fmt::Error)?
            .span()
        {
            write!(f, "\n{}", SourceCodeDisplay {
                span: &implementation_span,
                help_display: Some("doesn't implement the above trait member")
            })?;
        }

        Ok(())
    }
}

/// The trait member and the implementation member have different types.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitMemberAndImplementationMemberMismatched {
    /// The ID of the trait member that is not implemented.
    pub trait_member_id: TraitMemberID,

    /// The ID of the implementation member that is not implemented.
    pub implementation_member_id: TraitImplementationMemberID,
}

/// Generic parameter is unused in the implementation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UnusedGenericParameterInImplementation {
    /// The ID of the unused generic parameter.
    pub generic_parameter_id: LocalGenericParameterID,

    /// The ID of the implementation in which the generic parameter is unused.
    pub implementation_kind_id: TraitImplementationKindID,
}

impl Display for WithTable<'_, UnusedGenericParameterInImplementation> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let generic_id: GenericID = self.error.implementation_kind_id.into();
        let generic_symbol = self.table.get_generic(generic_id).ok_or(fmt::Error)?;

        let (span, name, kind) = {
            match self.error.generic_parameter_id {
                LocalGenericParameterID::Lifetime(lifetime_parameter_id) => {
                    let lifetime_param = generic_symbol
                        .generic_declaration()
                        .parameters
                        .lifetimes
                        .get(lifetime_parameter_id)
                        .ok_or(fmt::Error)?;

                    (
                        lifetime_param.span.as_ref(),
                        lifetime_param.name.as_deref().unwrap_or("`{unknown}"),
                        "lifetime parameter",
                    )
                }
                LocalGenericParameterID::Type(type_parameter_id) => {
                    let type_param = generic_symbol
                        .generic_declaration()
                        .parameters
                        .types
                        .get(type_parameter_id)
                        .ok_or(fmt::Error)?;

                    (
                        type_param.span.as_ref(),
                        type_param.name.as_str(),
                        "type parameter",
                    )
                }
                LocalGenericParameterID::Constant(constant_parameter_id) => {
                    let constant_param = generic_symbol
                        .generic_declaration()
                        .parameters
                        .constants
                        .get(constant_parameter_id)
                        .ok_or(fmt::Error)?;

                    (
                        constant_param.span.as_ref(),
                        constant_param.name.as_str(),
                        "constant parameter",
                    )
                }
            }
        };

        write!(f, "{}", Message {
            severity: Severity::Error,
            display: format!("The {kind} `{name}` is unused in the implementation",),
        })?;

        if let Some(span) = span {
            write!(f, "\n{}", SourceCodeDisplay {
                span,
                help_display: Some("consider removing it")
            })?;
        }

        Ok(())
    }
}

/// Two implementations have the same speciality order.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AmbiguousImplementation {
    /// The ID of the first implementation.
    pub first_implementation_id: TraitImplementationKindID,

    /// The ID of the second implementation.
    pub second_implementation_id: TraitImplementationKindID,
}
impl Display for WithTable<'_, AmbiguousImplementation> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let trait_name = self
            .table
            .get_qualified_name(self.error.first_implementation_id.into())
            .ok_or(fmt::Error)?;

        write!(f, "{}", Message {
            severity: Severity::Error,
            display: format!("The implementations of the trait `{trait_name}` are ambiguous"),
        })?;

        if let Some(first_implementation_span) = self
            .table
            .get_global(self.error.first_implementation_id.into())
            .ok_or(fmt::Error)?
            .span()
        {
            write!(f, "\n{}", SourceCodeDisplay {
                span: &first_implementation_span,
                help_display: Option::<i32>::None
            })?;
        }

        if let Some(second_implementation_span) = self
            .table
            .get_global(self.error.second_implementation_id.into())
            .ok_or(fmt::Error)?
            .span()
        {
            write!(f, "\n{}", SourceCodeDisplay {
                span: &second_implementation_span,
                help_display: Option::<i32>::None
            })?;
        }

        Ok(())
    }
}

/// The higher-ranked lifetime with the same name already exists in the given scope.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HigherRankedLifetimeRedeclaration {
    /// The span of the redeclaration.
    pub redeclaration_span: Span,
}

impl Display for WithTable<'_, HigherRankedLifetimeRedeclaration> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", Message {
            severity: Severity::Error,
            display: "The higher-ranked lifetime with the same name already exists in the given \
                      scope",
        })?;

        write!(f, "\n{}", SourceCodeDisplay {
            span: &self.error.redeclaration_span,
            help_display: Option::<i32>::None,
        })?;

        Ok(())
    }
}

/// The trait member and the implementation member have different number of generic parameters.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MismatchedGenericParameterCountInImplementation {
    /// The ID of the implementation member
    pub implementation_member_id: TraitImplementationMemberID,

    /// The ID of the trait member
    pub trait_member_id: TraitMemberID,

    /// Expected count of generic parameters
    pub expected_count: usize,

    /// Number of generic parameters declared in the implementation
    pub declared_count: usize,

    /// The kind of the generic parameter that has mismatched count
    pub generic_kind: GenericKind,
}

impl Display for WithTable<'_, MismatchedGenericParameterCountInImplementation> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let generic_kind = match self.error.generic_kind {
            GenericKind::Type => "type",
            GenericKind::Lifetime => "lifetime",
            GenericKind::Constant => "constant",
        };

        write!(f, "{}", Message {
            severity: Severity::Error,
            display: format!(
                "The implementation member has {} {generic_kind} parameters, but the trait member \
                 has {}",
                self.error.declared_count, self.error.expected_count
            ),
        })?;

        if let Some(implementation_member_span) = self
            .table
            .get_global(self.error.implementation_member_id.into())
            .ok_or(fmt::Error)?
            .span()
        {
            write!(f, "\n{}", SourceCodeDisplay {
                span: &implementation_member_span,
                help_display: Some("implemented here")
            })?;
        }

        if let Some(trait_member_span) = self
            .table
            .get_global(self.error.trait_member_id.into())
            .ok_or(fmt::Error)?
            .span()
        {
            write!(f, "\n{}", SourceCodeDisplay {
                span: &trait_member_span,
                help_display: Some("trait member declared here")
            })?;
        }

        Ok(())
    }
}

/// The type of the constant parameter in the implementation doesn't match the type of the constant
/// parameter in the trait.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MismatchedImplementationConstantTypeParameter {
    /// The ID of the implementation member
    pub implementation_member_id: TraitImplementationMemberID,

    /// The ID of the trait member
    pub trait_member_id: TraitMemberID,

    /// In which generic parameter the mismatch occurred
    pub constant_parameter_index: usize,
}

impl Display for WithTable<'_, MismatchedImplementationConstantTypeParameter> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", Message {
            severity: Severity::Error,
            display: "The type of the constant parameter in the implementation doesn't match the \
                      type of the constant parameter in the trait",
        })?;

        let trait_member_span = match self.error.trait_member_id {
            TraitMemberID::Type(trait_type_id) => self
                .table
                .get(trait_type_id)
                .ok_or(fmt::Error)?
                .generic_declaration
                .parameters
                .constants
                .get(ID::new(self.error.constant_parameter_index))
                .ok_or(fmt::Error)?
                .span
                .clone(),
            TraitMemberID::Function(trait_function_id) => self
                .table
                .get(trait_function_id)
                .ok_or(fmt::Error)?
                .generic_declaration
                .parameters
                .constants
                .get(ID::new(self.error.constant_parameter_index))
                .ok_or(fmt::Error)?
                .span
                .clone(),
            TraitMemberID::Constant(_) => return Err(fmt::Error),
        };

        let implementation_member_span = match self.error.implementation_member_id {
            TraitImplementationMemberID::Type(implementation_type_id) => self
                .table
                .get(implementation_type_id)
                .ok_or(fmt::Error)?
                .generic_declaration
                .parameters
                .constants
                .get(ID::new(self.error.constant_parameter_index))
                .ok_or(fmt::Error)?
                .span
                .clone(),
            TraitImplementationMemberID::Function(implementation_function_id) => self
                .table
                .get(implementation_function_id)
                .ok_or(fmt::Error)?
                .generic_declaration
                .parameters
                .constants
                .get(ID::new(self.error.constant_parameter_index))
                .ok_or(fmt::Error)?
                .span
                .clone(),
            TraitImplementationMemberID::Constant(_) => return Err(fmt::Error),
        };

        if let Some(span) = implementation_member_span {
            write!(f, "\n{}", SourceCodeDisplay {
                span: &span,
                help_display: Option::<i32>::None
            })?;
        }

        if let Some(span) = trait_member_span {
            write!(f, "\n{}", SourceCodeDisplay {
                span: &span,
                help_display: Option::<i32>::None
            })?;
        }

        Ok(())
    }
}

/// The where clause predicate is not satisfied.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct WhereClausePredicateNotSatisfied {
    /// The predicate that is not satisfied.
    pub predicate: Predicate<Symbolic>,

    /// The span where the predicate check occurred.
    pub span: Span,
}

impl Display for WithTable<'_, WhereClausePredicateNotSatisfied> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let predicate = &self.error.predicate;

        write!(f, "{}", Message {
            severity: Severity::Error,
            display: format!("The where clause predicate `{predicate:#?}` is not satisfied",),
        })?;

        write!(f, "\n{}", SourceCodeDisplay {
            span: &self.error.span,
            help_display: Option::<i32>::None,
        })?;

        Ok(())
    }
}

/// Implemented by all semantic errors.
pub trait Error: Any
where
    for<'a> WithTable<'a, Self>: Display,
{
    /// Returns the error as a [`dyn Any`].
    fn as_any(&self) -> &dyn Any;

    /// Returns the error as a mutable [`dyn Any`].
    fn as_any_mut(&mut self) -> &mut dyn Any;
}

impl<T: Any> Error for T
where
    for<'a> WithTable<'a, T>: Display,
{
    fn as_any(&self) -> &dyn Any { self }

    fn as_any_mut(&mut self) -> &mut dyn Any { self }
}
