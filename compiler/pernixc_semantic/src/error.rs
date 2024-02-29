//! Contains all the definition of errors that can be emitted by the semantic
//! analyzer.

use std::fmt::{self, Debug, Display};

use pernixc_base::{
    log::{
        formatting::{Style, WithStyle},
        Message, Severity, SourceCodeDisplay,
    },
    source_file::Span,
};

use crate::{
    arena::ID,
    semantic::predicate::Predicate,
    symbol::{
        Accessibility, GenericID, GenericKind, Global, GlobalID,
        LocalGenericParameterID, Module, Trait, TraitImplementation,
        TraitImplementationKindID, TraitImplementationMemberID, TraitMemberID,
    },
    table::{Index, Suboptimal, Table},
};

/// Contains both error and the table in which the error occurred.
///
/// Primarily used for implementing [`std::fmt::Display`] trait.
#[derive(Debug, Clone, Copy)]
pub struct WithTable<'a, Error: ?Sized> {
    /// The table in which the error occurred.
    pub table: &'a Table<Suboptimal>,

    /// The error that occurred.
    pub error: &'a Error,
}

impl<'a, Error: DisplayWithTable + ?Sized> Display for WithTable<'a, Error> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.error.fmt(self.table, f)
    }
}

/// The global symbol with the same name already exists in the given scope.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RedefinedGlobal {
    /// The ID of the existing symbol.
    pub existing_global_id: GlobalID,

    /// The ID of the new symbol.
    pub new_global_id: GlobalID,

    /// The scope in which the duplication occurred.
    pub in_global_id: GlobalID,
}

/// Similar to [`std::fmt::Display`] but with the table in which the error
/// occurred.
pub trait DisplayWithTable {
    #[allow(missing_docs, clippy::missing_errors_doc)]
    fn fmt(
        &self,
        table: &Table<Suboptimal>,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result;
}

impl DisplayWithTable for RedefinedGlobal {
    fn fmt(
        &self,
        table: &Table<Suboptimal>,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        let (
            Some(existing_symbol),
            Some(new_symbol),
            Some(scope_qualified_name),
        ) = (
            table.get_global(self.existing_global_id),
            table.get_global(self.new_global_id),
            table.get_qualified_name(self.in_global_id),
        )
        else {
            return Err(fmt::Error);
        };

        write!(f, "{}", Message {
            severity: pernixc_base::log::Severity::Error,
            display: format!(
                "the symbol `{}` is already defined in `{}`",
                existing_symbol.name(),
                scope_qualified_name
            ),
        })?;

        if let Some(existing_span) = existing_symbol.span() {
            write!(f, "\n{}", SourceCodeDisplay {
                span: existing_span,
                help_display: Some("previously defined here"),
            })?;
        }

        if let Some(new_span) = new_symbol.span() {
            write!(f, "\n{}", SourceCodeDisplay {
                span: new_span,
                help_display: Some("redefined here"),
            })?;
        }

        Ok(())
    }
}

/// The symbol is more accessible than the parent symbol.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SymbolIsMoreAccessibleThanParent {
    /// The ID of the symbol that is more accessible than the parent symbol.
    pub symbol_id: GlobalID,

    /// The ID of the parent symbol.
    pub parent_id: GlobalID,
}

impl DisplayWithTable for SymbolIsMoreAccessibleThanParent {
    fn fmt(
        &self,
        table: &Table<Suboptimal>,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        let (Some(symbol_name), Some(parent_qualified_name)) = (
            table.get_global(self.symbol_id).map(Global::name),
            table.get_qualified_name(self.parent_id),
        ) else {
            return Err(fmt::Error);
        };

        let (Some(symbol_accessibility), Some(parent_accessibility)) = (
            table.get_accessibility(self.symbol_id),
            table.get_accessibility(self.parent_id),
        ) else {
            return Err(fmt::Error);
        };

        let (Some(symbol_span), Some(parent_span)) = (
            table.get_global(self.symbol_id).map(Global::span),
            table.get_global(self.parent_id).map(Global::span),
        ) else {
            return Err(fmt::Error);
        };

        write!(f, "{}", Message {
            severity: Severity::Error,
            display: format!(
                "the symbol `{symbol_name}` in `{parent_qualified_name}` is \
                 more accessible than the parent symbol"
            ),
        })?;

        if let Some(symbol_span) = symbol_span {
            write!(f, "\n{}", SourceCodeDisplay {
                span: symbol_span,
                help_display: Some(format!(
                    "the symbol `{symbol_name}` is {symbol_accessibility}",
                )),
            })?;
        }

        if let Some(parent_span) = parent_span {
            write!(f, "\n{}", SourceCodeDisplay {
                span: parent_span,
                help_display: Some(format!(
                    "the parent symbol is {parent_accessibility}",
                )),
            })?;
        }

        Ok(())
    }
}

/// The symbol in the implementation is not trait, struct, or enum.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InvalidSymbolInImplementation {
    /// The ID of the symbol that was found in the implementation.
    pub invalid_global_id: GlobalID,

    /// The span where the invalid symbol was found.
    pub qualified_identifier_span: Span,
}

impl DisplayWithTable for InvalidSymbolInImplementation {
    fn fmt(
        &self,
        table: &Table<Suboptimal>,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        let qualified_name = table
            .get_qualified_name(self.invalid_global_id)
            .ok_or(fmt::Error)?;

        write!(f, "{}", Message {
            severity: Severity::Error,
            display: format!(
                "the symbol `{qualified_name}` is not a trait, struct, or enum"
            ),
        })?;

        write!(f, "\n{}", SourceCodeDisplay {
            span: &self.qualified_identifier_span,
            help_display: Option::<i32>::None,
        })?;

        Ok(())
    }
}

/// Expected a module in the module path, but found other kind of symbol.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExpectModule {
    /// The module path that was expected to be a module.
    pub module_path: Span,

    /// The ID of the symbol that was found instead of a module.
    pub found_id: GlobalID,
}

impl GlobalID {
    /// Returns the kind of the symbol description as a string.
    #[must_use]
    pub const fn kind_str(&self) -> &'static str {
        match self {
            Self::Module(_) => "module",
            Self::Type(_) => "type",
            Self::Constant(_) => "constant",
            Self::Trait(_) => "trait",
            Self::TraitImplementation(_) => "trait implementation",
            Self::TraitFunction(_) => "trait function",
            Self::TraitType(_) => "trait type",
            Self::TraitConstant(_) => "trait constant",
            Self::TraitImplementationFunction(_) => {
                "trait implementation function"
            }
            Self::TraitImplementationType(_) => "trait implementation type",
            Self::TraitImplementationConstant(_) => {
                "trait implementation constant"
            }
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

impl DisplayWithTable for ExpectModule {
    fn fmt(
        &self,
        table: &Table<Suboptimal>,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        let found_symbol_qualified_name =
            table.get_qualified_name(self.found_id).ok_or(fmt::Error)?;

        write!(f, "{}", Message {
            severity: Severity::Error,
            display: format!(
                "expected a module in the module path, but found `{} {}`",
                self.found_id.kind_str(),
                found_symbol_qualified_name
            ),
        })?;

        write!(f, "\n{}", SourceCodeDisplay {
            span: &self.module_path,
            help_display: Option::<i32>::None,
        })?;

        Ok(())
    }
}

/// The module cannot have a `using` statement that uses itself.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SelfModuleUsing {
    /// The module that was found to be using itself.
    pub module_id: ID<Module>,

    /// The span where the `using` statement was found.
    pub using_span: Span,
}

impl DisplayWithTable for SelfModuleUsing {
    fn fmt(
        &self,
        table: &Table<Suboptimal>,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        let module_qualified_name = table
            .get_qualified_name(self.module_id.into())
            .ok_or(fmt::Error)?;

        write!(f, "{}", Message {
            severity: Severity::Error,
            display: format!(
                "the module `{module_qualified_name}` was found to have a \
                 `using` statement that uses itself",
            ),
        })?;

        write!(f, "\n{}", SourceCodeDisplay {
            span: &self.using_span,
            help_display: Option::<i32>::None,
        })?;

        Ok(())
    }
}

/// The module was found having a duplicate `using` statement.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DuplicatedUsing {
    /// The module name that was found to have a duplicate `using` statement.
    pub used_in_module_id: ID<Module>,

    /// The module name that was used more than once in the `using` statement.
    pub already_used_module_id: ID<Module>,

    /// The span where the `using` statement was found.
    pub using_span: Span,
}

impl DisplayWithTable for DuplicatedUsing {
    fn fmt(
        &self,
        table: &Table<Suboptimal>,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        let used_in_module_qualified_name = table
            .get_qualified_name(self.used_in_module_id.into())
            .ok_or(fmt::Error)?;

        let already_used_module_qualified_name = table
            .get_qualified_name(self.already_used_module_id.into())
            .ok_or(fmt::Error)?;

        write!(f, "{}", Message {
            severity: Severity::Error,
            display: format!(
                "the module `{used_in_module_qualified_name}` was found to \
                 have a duplicate `using` statement",
            ),
        })?;

        write!(f, "\n{}", SourceCodeDisplay {
            span: &self.using_span,
            help_display: Some(format!(
                "the module `{already_used_module_qualified_name}` was used \
                 more than once"
            )),
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

impl DisplayWithTable for SymbolIsNotAccessible {
    fn fmt(
        &self,
        table: &Table<Suboptimal>,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        let referring_site_qualified_name =
            table.get_qualified_name(self.referring_site).ok_or(fmt::Error)?;

        let referred_qualified_name =
            table.get_qualified_name(self.referred).ok_or(fmt::Error)?;

        write!(f, "{}", Message {
            severity: Severity::Error,
            display: format!(
                "the symbol `{referred_qualified_name}` is not accessible \
                 from `{referring_site_qualified_name}`",
            ),
        })?;

        write!(f, "\n{}", SourceCodeDisplay {
            span: &self.referred_span,
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

impl DisplayWithTable for ResolutionAmbiguity {
    fn fmt(
        &self,
        table: &Table<Suboptimal>,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        write!(f, "{}", Message {
            severity: Severity::Error,
            display: "the symbol resolution resulted in multiple candidates",
        })?;
        for candidate in &self.candidates {
            let candidate_qualified_name =
                table.get_qualified_name(*candidate).ok_or(fmt::Error)?;

            write!(f, "\n  - {}", WithStyle {
                style: Style::Bold,
                display: candidate_qualified_name,
            })?;
        }

        write!(f, "\n{}", SourceCodeDisplay {
            span: &self.resolution_span,
            help_display: Option::<i32>::None,
        })?;

        Ok(())
    }
}

/// The symbol was not found in the given scope.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SymbolNotFound {
    /// The [`GlobalID`] where the symbol was searched in. If `None`, the root
    /// module was searched.
    pub searched_global_id: Option<GlobalID>,

    /// The span where the symbol was searched from.
    pub resolution_span: Span,
}

impl DisplayWithTable for SymbolNotFound {
    fn fmt(
        &self,
        table: &Table<Suboptimal>,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        if let Some(searched_in_module_id) = self.searched_global_id {
            let qualified_name = table
                .get_qualified_name(searched_in_module_id)
                .ok_or(fmt::Error)?;

            write!(f, "{}", Message {
                severity: Severity::Error,
                display: format!(
                    "the symbol named `{}` does not exist in `{}`",
                    self.resolution_span.str(),
                    qualified_name
                ),
            })?;
        } else {
            write!(f, "{}", Message {
                severity: Severity::Error,
                display: format!(
                    "the symbol `{}` does not exist",
                    self.resolution_span.str(),
                ),
            })?;
        }

        write!(f, "\n{}", SourceCodeDisplay {
            span: &self.resolution_span,
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

impl DisplayWithTable for NoGenericArgumentsRequired {
    fn fmt(
        &self,
        table: &Table<Suboptimal>,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        let qualified_name =
            table.get_qualified_name(self.global_id).ok_or(fmt::Error)?;

        write!(f, "{}", Message {
            severity: Severity::Error,
            display: format!(
                "the symbol `{qualified_name}` doesn't require any generic \
                 arguments"
            ),
        })?;

        write!(f, "\n{}", SourceCodeDisplay {
            span: &self.generic_argument_span,
            help_display: Some("found supplied here"),
        })?;

        Ok(())
    }
}

/// The implementation is expected to implement a trait, but the trait was not
/// found.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExpectTraitInImplementation {
    /// The ID of the symbol that was not a trait.
    pub found_id: GlobalID,

    /// The span of the trait path.
    pub trait_path: Span,
}

impl DisplayWithTable for ExpectTraitInImplementation {
    fn fmt(
        &self,
        table: &Table<Suboptimal>,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        let found_symbol_qualified_name =
            table.get_qualified_name(self.found_id).ok_or(fmt::Error)?;

        write!(f, "{}", Message {
            severity: Severity::Error,
            display: format!(
                "txpected a trait in the trait path, but found `{} {}`",
                self.found_id.kind_str(),
                found_symbol_qualified_name
            ),
        })?;

        write!(f, "\n{}", SourceCodeDisplay {
            span: &self.trait_path,
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

impl DisplayWithTable for CyclicDependency {
    fn fmt(
        &self,
        table: &Table<Suboptimal>,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        let symbol_list = self
            .participants
            .iter()
            .map(|&symbol| {
                let qualified_name =
                    table.get_qualified_name(symbol).ok_or(fmt::Error)?;

                Ok(format!("`{qualified_name}`"))
            })
            .collect::<Result<Vec<_>, _>>()?
            .join(", ");

        write!(f, "{}", Message {
            severity: Severity::Error,
            display: format!(
                "the cyclic dependency was found in the given set of symbols: \
                 {symbol_list}"
            ),
        })?;

        for participant in &self.participants {
            if let Some(participant_span) =
                table.get_global(*participant).ok_or(fmt::Error)?.span()
            {
                write!(f, "\n{}", SourceCodeDisplay {
                    span: participant_span,
                    help_display: Option::<i32>::None,
                })?;
            }
        }

        Ok(())
    }
}

/// The generic arguments were supplied in the wrong order.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MisOrderedGenericArgument {
    /// The kind of the mis-ordered generic argument.
    pub generic_kind: GenericKind,

    /// The span of the generic argument.
    pub generic_argument: Span,
}

impl<'a> Display for WithTable<'a, MisOrderedGenericArgument> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", Message {
            severity: Severity::Error,
            display: "the generic argument was supplied in the wrong order",
        })?;

        let help_display = match self.error.generic_kind {
            GenericKind::Type => {
                Some("can't be supplied after constant arguments")
            }
            GenericKind::Lifetime => {
                Some("can't be supplied after type or constant arguments")
            }
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
pub struct MisOrderedGenericParameter {
    /// The kind of the mis-ordered generic parameter.
    pub generic_kind: GenericKind,

    /// The span of the generic parameter.
    pub generic_parameter_span: Span,
}

impl DisplayWithTable for MisOrderedGenericParameter {
    fn fmt(
        &self,
        _: &Table<Suboptimal>,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        write!(f, "{}", Message {
            severity: Severity::Error,
            display: "the generic parameter was declared in the wrong order",
        })?;

        let help_display = match self.generic_kind {
            GenericKind::Type => {
                Some("can't be declared after constant parameters")
            }
            GenericKind::Lifetime => {
                Some("can't be declared after type or constant parameters")
            }
            GenericKind::Constant => None,
        };

        write!(f, "\n{}", SourceCodeDisplay {
            span: &self.generic_parameter_span,
            help_display,
        })?;

        Ok(())
    }
}

/// An entity was exposed to the public interface but it's accessability is less
/// permissive than the public interface.
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
pub struct MismatchedGenericArugmentCount {
    /// The kind of the generic parameter.
    pub generic_kind: GenericKind,

    /// Span where mismatch occurred.
    pub generic_identifier_span: Span,

    /// Expected count of generic arguments.
    pub expected_count: usize,

    /// Supplied count of generic arguments.
    pub supplied_count: usize,
}

impl DisplayWithTable for MismatchedGenericArugmentCount {
    fn fmt(
        &self,
        _: &Table<Suboptimal>,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        let generic_kind = match self.generic_kind {
            GenericKind::Type => "type",
            GenericKind::Lifetime => "lifetime",
            GenericKind::Constant => "constant",
        };

        write!(f, "{}", Message {
            severity: Severity::Error,
            display: format!(
                "expected {} {} arguments, but {} were supplied",
                self.expected_count, generic_kind, self.supplied_count,
            ),
        })?;

        write!(f, "\n{}", SourceCodeDisplay {
            span: &self.generic_identifier_span,
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

impl DisplayWithTable for LifetimeParameterNotFound {
    fn fmt(
        &self,
        table: &Table<Suboptimal>,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        let referring_site_qualified_name =
            table.get_qualified_name(self.referring_site).ok_or(fmt::Error)?;

        write!(f, "{}", Message {
            severity: Severity::Error,
            display: format!(
                "the lifetime parameter `{}` was not found in \
                 `{referring_site_qualified_name}`",
                self.referred_span.str()
            ),
        })?;

        write!(f, "\n{}", SourceCodeDisplay {
            span: &self.referred_span,
            help_display: Option::<i32>::None,
        })?;

        Ok(())
    }
}

/// A lifetime wasn't supplied in the reference.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExpectLifetime {
    /// The span where the lifetime was expected.
    pub expected_span: Span,
}

impl DisplayWithTable for ExpectLifetime {
    fn fmt(
        &self,
        _: &Table<Suboptimal>,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        write!(f, "{}", Message {
            severity: Severity::Error,
            display: "a lifetime was expected",
        })?;

        write!(f, "\n{}", SourceCodeDisplay {
            span: &self.expected_span,
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

impl DisplayWithTable for MoreThanOneUnpackedInTupleType {
    fn fmt(
        &self,
        _: &Table<Suboptimal>,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        write!(f, "{}", Message {
            severity: Severity::Error,
            display: "the tuple type contains more than one unpacked type",
        })?;

        write!(f, "\n{}", SourceCodeDisplay {
            span: &self.illegal_tuple_type_span,
            help_display: Option::<i32>::None,
        })?;

        Ok(())
    }
}

/// The type was expected but the non-type symbol was found.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExpectType {
    /// The span where the non-type symbol was found.
    pub non_type_symbol_span: Span,

    /// The [`GlobalID`] where the non-type symbol was found.
    pub resolved_global_id: GlobalID,
}

impl DisplayWithTable for ExpectType {
    fn fmt(
        &self,
        table: &Table<Suboptimal>,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        let qualified_name = table
            .get_qualified_name(self.resolved_global_id)
            .ok_or(fmt::Error)?;

        write!(f, "{}", Message {
            severity: Severity::Error,
            display: format!(
                "the type was expected but found {} `{qualified_name}`",
                self.resolved_global_id.kind_str(),
            ),
        })?;

        write!(f, "\n{}", SourceCodeDisplay {
            span: &self.non_type_symbol_span,
            help_display: Option::<i32>::None,
        })?;

        Ok(())
    }
}

/// The generic parameter with the same name already exists in the given scope.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DuplicatedGenericParameter<ID> {
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

/// An enumeration of all kinds of symbols in the implemetation.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    derive_more::Display,
)]
#[allow(missing_docs)]
pub enum TraitMemberKind {
    #[display(fmt = "function")]
    Function,
    #[display(fmt = "type")]
    Type,
    #[display(fmt = "constant")]
    Constant,
}

/// The trait member and the implementation member have different types.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MismatchedTraitMemberAndImplementationMember {
    /// The ID of the trait member that is not implemented.
    pub trait_member_id: TraitMemberID,

    /// The implementation member kind
    pub found_kind: TraitMemberKind,

    /// The span of the implementation member's identifier.
    pub implementation_member_identifer_span: Span,
}

impl DisplayWithTable for MismatchedTraitMemberAndImplementationMember {
    fn fmt(
        &self,
        table: &Table<Suboptimal>,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        let trait_member_qualified_identifier = table
            .get_qualified_name(self.trait_member_id.into())
            .ok_or(fmt::Error)?;
        let trait_member_sym =
            table.get_global(self.trait_member_id.into()).ok_or(fmt::Error)?;

        let trait_member_kind = match self.trait_member_id {
            TraitMemberID::Type(_) => TraitMemberKind::Type,
            TraitMemberID::Function(_) => TraitMemberKind::Function,
            TraitMemberID::Constant(_) => TraitMemberKind::Constant,
        };

        write!(f, "{}", Message {
            severity: Severity::Error,
            display: format!(
                "the trait member `{trait_member_qualified_identifier}` is of \
                 kind `{trait_member_kind}` but the implementation member is \
                 of kind `{}`",
                self.found_kind
            ),
        })?;

        write!(f, "\n{}", SourceCodeDisplay {
            span: &self.implementation_member_identifer_span,
            help_display: Option::<i32>::None,
        })?;

        if let Some(span) = trait_member_sym.span() {
            write!(f, "\n{}", SourceCodeDisplay {
                span,
                help_display: Some("the trait member is defined here"),
            })?;
        }

        Ok(())
    }
}

/// Generic parameter is unused in the implementation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UnusedGenericParameterInImplementation {
    /// The ID of the unused generic parameter.
    pub generic_parameter_id: LocalGenericParameterID,

    /// The ID of the implementation in which the generic parameter is unused.
    pub implementation_kind_id: TraitImplementationKindID,
}

impl DisplayWithTable for UnusedGenericParameterInImplementation {
    fn fmt(
        &self,
        table: &Table<Suboptimal>,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        let generic_id: GenericID = self.implementation_kind_id.into();
        let generic_symbol = table.get_generic(generic_id).ok_or(fmt::Error)?;

        let (span, name, kind) = {
            match self.generic_parameter_id {
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
            display: format!(
                "The {kind} `{name}` is unused in the implementation",
            ),
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

/// Two implementations have the same specialty order.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AmbiguousImplementation {
    /// The ID of the first implementation.
    pub first_implementation_id: TraitImplementationKindID,

    /// The ID of the second implementation.
    pub second_implementation_id: TraitImplementationKindID,
}

impl DisplayWithTable for AmbiguousImplementation {
    fn fmt(
        &self,
        table: &Table<Suboptimal>,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        let trait_name = table
            .get_qualified_name(self.first_implementation_id.into())
            .ok_or(fmt::Error)?;

        write!(f, "{}", Message {
            severity: Severity::Error,
            display: format!(
                "the implementations of the trait `{trait_name}` are ambiguous"
            ),
        })?;

        if let Some(first_implementation_span) = table
            .get_global(self.first_implementation_id.into())
            .ok_or(fmt::Error)?
            .span()
        {
            write!(f, "\n{}", SourceCodeDisplay {
                span: first_implementation_span,
                help_display: Option::<i32>::None
            })?;
        }

        if let Some(second_implementation_span) = table
            .get_global(self.second_implementation_id.into())
            .ok_or(fmt::Error)?
            .span()
        {
            write!(f, "\n{}", SourceCodeDisplay {
                span: second_implementation_span,
                help_display: Option::<i32>::None
            })?;
        }

        Ok(())
    }
}

/// The higher-ranked lifetime with the same name already exists in the given
/// scope.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RedefinedHigherRankedLifetime {
    /// The span of the redefinition.
    pub redefinition_span: Span,
}

impl DisplayWithTable for RedefinedHigherRankedLifetime {
    fn fmt(
        &self,
        _: &Table<Suboptimal>,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        write!(f, "{}", Message {
            severity: Severity::Error,
            display: "the higher-ranked lifetime with the same name already \
                      exists in the given scope",
        })?;

        write!(f, "\n{}", SourceCodeDisplay {
            span: &self.redefinition_span,
            help_display: Option::<i32>::None,
        })?;

        Ok(())
    }
}

/// The trait member and the implementation member have different number of
/// generic parameters.
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

impl DisplayWithTable for MismatchedGenericParameterCountInImplementation {
    fn fmt(
        &self,
        table: &Table<Suboptimal>,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        let generic_kind = match self.generic_kind {
            GenericKind::Type => "type",
            GenericKind::Lifetime => "lifetime",
            GenericKind::Constant => "constant",
        };

        write!(f, "{}", Message {
            severity: Severity::Error,
            display: format!(
                "the implementation member has {} {generic_kind} parameters, \
                 but the trait member has {}",
                self.declared_count, self.expected_count
            ),
        })?;

        if let Some(implementation_member_span) = table
            .get_global(self.implementation_member_id.into())
            .ok_or(fmt::Error)?
            .span()
        {
            write!(f, "\n{}", SourceCodeDisplay {
                span: implementation_member_span,
                help_display: Some("implemented here")
            })?;
        }

        if let Some(trait_member_span) = table
            .get_global(self.trait_member_id.into())
            .ok_or(fmt::Error)?
            .span()
        {
            write!(f, "\n{}", SourceCodeDisplay {
                span: trait_member_span,
                help_display: Some("trait member declared here")
            })?;
        }

        Ok(())
    }
}

/// The implementation contains a member that is not a member of the trait.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UnknownTraitImplementationMember {
    /// The span to the identifier of the unknown member.
    pub identifier_span: Span,

    /// The ID of the trait implementation.
    pub trait_id: ID<Trait>,
}

impl DisplayWithTable for UnknownTraitImplementationMember {
    fn fmt(
        &self,
        table: &Table<Suboptimal>,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        let trait_qualified_name =
            table.get_qualified_name(self.trait_id.into()).ok_or(fmt::Error)?;
        let trait_span =
            table.get_global(self.trait_id.into()).ok_or(fmt::Error)?.span();

        write!(f, "{}", Message {
            severity: Severity::Error,
            display: format!(
                "the symbol named `{}` is not a member of the trait \
                 `{trait_qualified_name}`",
                self.identifier_span.str(),
            )
        })?;

        write!(f, "\n{}", SourceCodeDisplay {
            span: &self.identifier_span,
            help_display: Option::<i32>::None,
        })?;

        if let Some(span) = trait_span {
            write!(f, "\n{}", SourceCodeDisplay {
                span,
                help_display: Some("trait declared here"),
            })?;
        }

        Ok(())
    }
}

/// The trait member is already implemented in the implementation.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AlreadyImplementedTraitMember {
    /// The trait member that is being implemented more than once.
    pub trait_member_id: TraitMemberID,

    /// The ID of the existing trait implementation member.   
    pub implemented_id: TraitImplementationMemberID,

    /// The span where the re-implementation occurred.
    pub new_implementation_span: Span,
}

impl DisplayWithTable for AlreadyImplementedTraitMember {
    fn fmt(
        &self,
        table: &Table<Suboptimal>,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        let trait_member_qualified_name = table
            .get_qualified_name(self.trait_member_id.into())
            .ok_or(fmt::Error)?;

        write!(f, "{}", Message {
            severity: Severity::Error,
            display: format!(
                "the trait member `{trait_member_qualified_name}` is already \
                 implemented"
            ),
        })?;

        write!(f, "\n{}", SourceCodeDisplay {
            span: &self.new_implementation_span,
            help_display: Option::<i32>::None,
        })?;

        if let Some(span) = table
            .get_global(self.implemented_id.into())
            .ok_or(fmt::Error)?
            .span()
        {
            write!(f, "\n{}", SourceCodeDisplay {
                span,
                help_display: Some(
                    "the trait member is already implemented here"
                ),
            })?;
        }

        Ok(())
    }
}

/// The trait member is implemented with different accessibility in the
/// implementation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MismatchedTraitMemberAndImplementationMemberAccessibility {
    /// The ID of the trait member.
    pub trait_member_id: TraitMemberID,

    /// The accessibility defined in the implementation.
    pub implementation_member_accessibility: Accessibility,

    /// The trait implementation member that has different accessibility.
    pub implementation_member_id: TraitImplementationMemberID,
}

impl DisplayWithTable
    for MismatchedTraitMemberAndImplementationMemberAccessibility
{
    fn fmt(
        &self,
        table: &Table<Suboptimal>,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        let trait_member_qualified_name = table
            .get_qualified_name(self.trait_member_id.into())
            .ok_or(fmt::Error)?;
        let trait_member_accessibility = table
            .get_accessibility(self.trait_member_id.into())
            .ok_or(fmt::Error)?;

        write!(f, "{}", Message {
            severity: Severity::Error,
            display: format!(
                "the trait member `{trait_member_qualified_name}` is defined \
                 with accessibility `{trait_member_accessibility}` but the \
                 implementation member is defined with accessibility `{}`",
                self.implementation_member_accessibility
            ),
        })?;

        if let Some(span) = table
            .get_global(self.implementation_member_id.into())
            .ok_or(fmt::Error)?
            .span()
        {
            write!(f, "\n{}", SourceCodeDisplay {
                span,
                help_display: Some("the implementation member is defined here"),
            })?;
        }

        if let Some(span) = table
            .get_global(self.trait_member_id.into())
            .ok_or(fmt::Error)?
            .span()
        {
            write!(f, "\n{}", SourceCodeDisplay {
                span,
                help_display: Some("the trait member is defined here"),
            })?;
        }

        Ok(())
    }
}

/// The type of the constant parameter in the implementation doesn't match the
/// type of the constant parameter in the trait.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MismatchedImplementationConstantTypeParameter {
    /// The ID of the implementation member
    pub implementation_member_id: TraitImplementationMemberID,

    /// The ID of the trait member
    pub trait_member_id: TraitMemberID,

    /// In which generic parameter the mismatch occurred
    pub constant_parameter_index: usize,
}

impl DisplayWithTable for MismatchedImplementationConstantTypeParameter {
    fn fmt(
        &self,
        table: &Table<Suboptimal>,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        write!(f, "{}", Message {
            severity: Severity::Error,
            display: "the type of the constant parameter in the \
                      implementation doesn't match the type of the constant \
                      parameter in the trait",
        })?;

        let trait_member_span = match self.trait_member_id {
            TraitMemberID::Type(trait_type_id) => table
                .get(trait_type_id)
                .ok_or(fmt::Error)?
                .generic_declaration
                .parameters
                .constants
                .get(ID::new(self.constant_parameter_index))
                .ok_or(fmt::Error)?
                .span
                .clone(),
            TraitMemberID::Function(trait_function_id) => table
                .get(trait_function_id)
                .ok_or(fmt::Error)?
                .generic_declaration
                .parameters
                .constants
                .get(ID::new(self.constant_parameter_index))
                .ok_or(fmt::Error)?
                .span
                .clone(),
            TraitMemberID::Constant(_) => return Err(fmt::Error),
        };

        let implementation_member_span = match self.implementation_member_id {
            TraitImplementationMemberID::Type(implementation_type_id) => table
                .get(implementation_type_id)
                .ok_or(fmt::Error)?
                .generic_declaration
                .parameters
                .constants
                .get(ID::new(self.constant_parameter_index))
                .ok_or(fmt::Error)?
                .span
                .clone(),
            TraitImplementationMemberID::Function(
                implementation_function_id,
            ) => table
                .get(implementation_function_id)
                .ok_or(fmt::Error)?
                .generic_declaration
                .parameters
                .constants
                .get(ID::new(self.constant_parameter_index))
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

/// Not all trait members are implemented in the implementation.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UnimplementedTraitMembers {
    /// The list of trait members that are not implemented.
    pub unimplemented_trait_member_ids: Vec<TraitMemberID>,

    /// The ID of the implementation in which the trait members are not
    pub implementation_id: ID<TraitImplementation>,
}

impl DisplayWithTable for UnimplementedTraitMembers {
    fn fmt(
        &self,
        table: &Table<Suboptimal>,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        let trait_member_qualified_names = self
            .unimplemented_trait_member_ids
            .iter()
            .map(|&trait_member_id| {
                table
                    .get_qualified_name(trait_member_id.into())
                    .ok_or(fmt::Error)
            })
            .collect::<Result<Vec<_>, _>>()?;

        write!(f, "{}", Message {
            severity: Severity::Error,
            display: "mot all trait members are implemented in the \
                      implementation",
        })?;

        if let Some(span) = table
            .get_global(self.implementation_id.into())
            .ok_or(fmt::Error)?
            .span()
        {
            write!(f, "\n{}", SourceCodeDisplay {
                span,
                help_display: Some("the implementation is declared here"),
            })?;
        }

        write!(f, "\n{}", Message {
            severity: Severity::Info,
            display: format!(
                "the following trait members are not implemented: {}",
                trait_member_qualified_names.join(", ")
            ),
        })?;

        Ok(())
    }
}

/// The where clause predicate is not satisfied.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UnsatisfiedWhereClausePredicate {
    /// The predicate that is not satisfied.
    pub predicate: Predicate,

    /// The span where the predicate check occurred.
    pub span: Span,
}

impl DisplayWithTable for UnsatisfiedWhereClausePredicate {
    fn fmt(
        &self,
        _: &Table<Suboptimal>,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        let predicate = &self.predicate;

        write!(f, "{}", Message {
            severity: Severity::Error,
            display: format!(
                "the where clause predicate `{predicate:#?}` is not satisfied",
            ),
        })?;

        write!(f, "\n{}", SourceCodeDisplay {
            span: &self.span,
            help_display: Option::<i32>::None,
        })?;

        Ok(())
    }
}

/// Implemented by all semantic errors.
pub trait Error: Debug + DisplayWithTable + Send + Sync + 'static {
    #[allow(missing_docs, clippy::missing_errors_doc)]
    fn as_display_with_table(&self) -> &dyn DisplayWithTable;
}

impl<T: Debug + DisplayWithTable + Send + Sync + 'static> Error for T
where
    for<'a> WithTable<'a, T>: Display,
{
    fn as_display_with_table(&self) -> &dyn DisplayWithTable { self }
}
