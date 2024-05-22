//! Contains all the definition of errors that can be emitted by the semantic
//! analyzer.

use std::{
    collections::HashSet,
    fmt::{self, Debug},
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
    semantic::{model::Model, predicate::Predicate, term::r#type::Type},
    symbol::{
        table::{
            representation::Index, Display, DisplayObject, State, Suboptimal,
            Table,
        },
        Accessibility, AdtID, ConstantParameterID, Field, GenericID,
        GenericKind, GenericParameter, GlobalID, LocalGenericParameterID,
        MemberID, Module, PositiveTraitImplementation, Struct, Trait,
        TraitImplementationID, TraitImplementationMemberID, TraitMemberID,
    },
};

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

impl<T: State> Display<T> for RedefinedGlobal {
    fn fmt(&self, table: &Table<T>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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

impl<T: State> Table<T> {
    fn accessibility_description(
        &self,
        accessibility: Accessibility,
    ) -> Result<String, fmt::Error> {
        match accessibility {
            Accessibility::Public => Ok("publicly accessible".to_owned()),
            Accessibility::Scoped(module_id) => {
                let module_qualified_name = self
                    .get_qualified_name(module_id.into())
                    .ok_or(fmt::Error)?;

                Ok(format!("accessible in `{module_qualified_name}`"))
            }
        }
    }
}

impl<T: State> Display<T> for SymbolIsMoreAccessibleThanParent {
    fn fmt(&self, table: &Table<T>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (Some(symbol_name), Some(parent_qualified_name)) = (
            table.get_global(self.symbol_id).map(|x| x.name().to_owned()),
            table.get_qualified_name(self.parent_id),
        ) else {
            return Err(fmt::Error);
        };

        let accessibility_description =
            |accessibility: Accessibility| -> Result<String, fmt::Error> {
                match accessibility {
                    Accessibility::Public => {
                        Ok("publicly accessibility".to_owned())
                    }
                    Accessibility::Scoped(module_id) => {
                        let module_qualified_name = table
                            .get_qualified_name(module_id.into())
                            .ok_or(fmt::Error)?;

                        Ok(format!("accessible in `{module_qualified_name}`"))
                    }
                }
            };

        let (Some(symbol_accessibility), Some(parent_accessibility)) = (
            table.get_accessibility(self.symbol_id),
            table.get_accessibility(self.parent_id),
        ) else {
            return Err(fmt::Error);
        };

        let (Some(symbol_span), Some(parent_span)) = (
            table.get_global(self.symbol_id).map(|x| x.span().cloned()),
            table.get_global(self.parent_id).map(|x| x.span().cloned()),
        ) else {
            return Err(fmt::Error);
        };

        let symbol_accessibility_description =
            accessibility_description(symbol_accessibility)?;

        let parent_accessibility_description =
            accessibility_description(parent_accessibility)?;

        write!(f, "{}", Message {
            severity: Severity::Error,
            display: format!(
                "the symbol `{symbol_name}` in `{parent_qualified_name}` is \
                 more accessible than the parent symbol"
            ),
        })?;

        if let Some(symbol_span) = symbol_span {
            write!(f, "\n{}", SourceCodeDisplay {
                span: &symbol_span,
                help_display: Some(format!(
                    "the symbol `{symbol_name}` is \
                     {symbol_accessibility_description}",
                )),
            })?;
        }

        if let Some(parent_span) = parent_span {
            write!(f, "\n{}", SourceCodeDisplay {
                span: &parent_span,
                help_display: Some(format!(
                    "the parent symbol is {parent_accessibility_description}",
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

impl<T: State> Display<T> for InvalidSymbolInImplementation {
    fn fmt(&self, table: &Table<T>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
            Self::PositiveTraitImplementation(_) => "trait implementation",
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

impl<T: State> Display<T> for ExpectModule {
    fn fmt(&self, table: &Table<T>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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

/// Calculating the specialization between the implementations was undecidable.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UndecidableImplementationSpecialization {
    /// The id of the first implementation.
    pub first_implementation_id: TraitImplementationID,

    /// The id of the second implementation.
    pub second_implementation_id: TraitImplementationID,
}

impl<T: State> Display<T> for UndecidableImplementationSpecialization {
    fn fmt(&self, table: &Table<T>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let first_sym = table
            .get_global(self.first_implementation_id.into())
            .ok_or(fmt::Error)?;
        let second_sym = table
            .get_global(self.second_implementation_id.into())
            .ok_or(fmt::Error)?;

        write!(f, "{}", Message {
            severity: Severity::Error,
            display: "overflow calculating the specialization between these \
                      two implementations",
        })?;

        if let Some(first_span) = first_sym.span() {
            write!(f, "\n{}", SourceCodeDisplay {
                span: first_span,
                help_display: Some("first implementation defined here"),
            })?;
        }

        if let Some(second_span) = second_sym.span() {
            write!(f, "\n{}", SourceCodeDisplay {
                span: second_span,
                help_display: Some("second implementation defined here"),
            })?;
        }

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

impl<T: State> Display<T> for SelfModuleUsing {
    fn fmt(&self, table: &Table<T>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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

impl<T: State> Display<T> for DuplicatedUsing {
    fn fmt(&self, table: &Table<T>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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

impl<T: State> Display<T> for SymbolIsNotAccessible {
    fn fmt(&self, table: &Table<T>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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

impl<T: State> Display<T> for ResolutionAmbiguity {
    fn fmt(&self, table: &Table<T>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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

impl<T: State> Display<T> for SymbolNotFound {
    fn fmt(&self, table: &Table<T>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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

impl<T: State> Display<T> for NoGenericArgumentsRequired {
    fn fmt(&self, table: &Table<T>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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

/// Expects a trait symbol but found other kind of symbol.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExpectTrait {
    /// The ID of the symbol that was not a trait.
    pub found_id: GlobalID,

    /// The span of the trait path.
    pub trait_path: Span,
}

impl<T: State> Display<T> for ExpectTrait {
    fn fmt(&self, table: &Table<T>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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

impl<T: State> Display<T> for CyclicDependency {
    fn fmt(&self, table: &Table<T>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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

impl<T: State> Display<T> for MisOrderedGenericArgument {
    fn fmt(&self, _: &Table<T>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", Message {
            severity: Severity::Error,
            display: "the generic argument was supplied in the wrong order",
        })?;

        let help_display = match self.generic_kind {
            GenericKind::Type => {
                Some("can't be supplied after constant arguments")
            }
            GenericKind::Lifetime => {
                Some("can't be supplied after type or constant arguments")
            }
            GenericKind::Constant => None,
        };

        write!(f, "\n{}", SourceCodeDisplay {
            span: &self.generic_argument,
            help_display,
        })?;

        Ok(())
    }
}

/// The negative implementation was used on the ADT.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NegativeImplementationOnAdt {
    /// The span where the negative implementation was used.
    pub negative_implementation_span: Span,

    /// The ID of the ADT where the negative implementation was used.
    pub adt_id: AdtID,
}

impl<T: State> Display<T> for NegativeImplementationOnAdt {
    fn fmt(&self, table: &Table<T>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let adt_qualified_name =
            table.get_qualified_name(self.adt_id.into()).ok_or(fmt::Error)?;

        write!(f, "{}", Message {
            severity: Severity::Error,
            display: format!(
                "negative implementation is not allowed here because \
                 `{adt_qualified_name}` is not a trait",
            ),
        })?;

        write!(f, "\n{}", SourceCodeDisplay {
            span: &self.negative_implementation_span,
            help_display: Option::<i32>::None,
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

impl<T: State> Display<T> for MisOrderedGenericParameter {
    fn fmt(&self, _: &Table<T>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
pub struct PrivateEntityLeakedToPublicInterface<T> {
    /// The entity that was leaked.
    pub entity: T,

    /// The overall accessibility of the entity.
    pub entity_overall_accessibility: Accessibility,

    /// The span where the entity was leaked.
    pub leaked_span: Span,

    /// The ID of the public interface that contains the leaked entity.
    pub public_interface_id: GlobalID,
}

impl<S: State, T: Display<S>> Display<S>
    for PrivateEntityLeakedToPublicInterface<T>
{
    fn fmt(&self, table: &Table<S>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let entity_display = DisplayObject { display: &self.entity, table };

        let public_interface_qualified_name = table
            .get_qualified_name(self.public_interface_id)
            .ok_or(fmt::Error)?;
        let public_interface_accessibility = table
            .get_accessibility(self.public_interface_id)
            .ok_or(fmt::Error)?;

        let entity_accessibility_description = table
            .accessibility_description(self.entity_overall_accessibility)?;
        let parent_accessibility_description =
            table.accessibility_description(public_interface_accessibility)?;

        write!(f, "{}", Message {
            severity: Severity::Error,
            display: format!(
                "`{entity_display}` is {entity_accessibility_description} but \
                 it was declared in a `{public_interface_qualified_name}` \
                 interface, which is {parent_accessibility_description}",
            ),
        })?;

        write!(f, "\n{}", SourceCodeDisplay {
            span: &self.leaked_span,
            help_display: Option::<i32>::None,
        })?;

        Ok(())
    }
}

/// Generic arguments count mismatch.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MisMatchedGenericArgumentCount {
    /// The kind of the generic parameter.
    pub generic_kind: GenericKind,

    /// Span where mismatch occurred.
    pub generic_identifier_span: Span,

    /// Expected count of generic arguments.
    pub expected_count: usize,

    /// Supplied count of generic arguments.
    pub supplied_count: usize,
}

impl<T: State> Display<T> for MisMatchedGenericArgumentCount {
    fn fmt(&self, _: &Table<T>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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

impl<T: State> Display<T> for LifetimeParameterNotFound {
    fn fmt(&self, table: &Table<T>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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

impl<T: State> Display<T> for ExpectLifetime {
    fn fmt(&self, _: &Table<T>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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

/// The satisfiability of the predicate was undecidable (takes too long to
/// solve).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UndecidablePredicate<M: Model> {
    /// The span where the predicate satisfiability was undecidable.
    pub instantiation_span: Span,

    /// The predicate that was undecidable.
    pub predicate: Predicate<M>,

    /// The span where the predicate is defined.
    pub predicate_declaration_span: Option<Span>,
}

impl<T: State, M: Model> Display<T> for UndecidablePredicate<M> {
    fn fmt(&self, table: &Table<T>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", Message {
            severity: Severity::Error,
            display: format!(
                "overflow calculating the satisfiability of `{}` predicate \
                 when instantiating",
                DisplayObject { display: &self.predicate, table }
            ),
        })?;

        write!(f, "\n{}", SourceCodeDisplay {
            span: &self.instantiation_span,
            help_display: None::<i32>
        })?;

        if let Some(predicate_span) = self.predicate_declaration_span.as_ref() {
            write!(f, "\n{}", SourceCodeDisplay {
                span: predicate_span,
                help_display: Some("predicate defined here"),
            })?;
        }

        Ok(())
    }
}

/// The tuple type contains more than one unpacked type.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MoreThanOneUnpackedInTupleType {
    /// The span where the illegal tuple type was found.
    pub illegal_tuple_type_span: Span,
}

impl<T: State> Display<T> for MoreThanOneUnpackedInTupleType {
    fn fmt(&self, _: &Table<T>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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

impl<T: State> Display<T> for ExpectType {
    fn fmt(&self, table: &Table<T>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
pub struct DuplicatedGenericParameter<T> {
    /// The ID of the existing generic parameter.
    pub existing_generic_parameter_id: MemberID<ID<T>, GenericID>,

    /// The ID of the new generic parameter.
    pub duplicating_generic_parameter_span: Span,
}

impl<T: GenericParameter, S: State> Display<S>
    for DuplicatedGenericParameter<T>
{
    fn fmt(&self, table: &Table<S>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Some(generic_symbol) =
            table.get_generic(self.existing_generic_parameter_id.parent)
        else {
            return Err(fmt::Error);
        };

        let Some(generic_parameter) = T::get_generic_parameters_arena(
            &generic_symbol.generic_declaration().parameters,
        )
        .get(self.existing_generic_parameter_id.id) else {
            return Err(fmt::Error);
        };

        write!(f, "{}", Message {
            severity: Severity::Error,
            display: format!(
                "the generic parameter named `{}` is already defined",
                generic_parameter.name().unwrap_or("?")
            ),
        })?;

        if let Some(existing_span) = generic_parameter.span() {
            write!(f, "\n{}", SourceCodeDisplay {
                span: existing_span,
                help_display: Some("previously defined here"),
            })?;
        }

        write!(f, "\n{}", SourceCodeDisplay {
            span: &self.duplicating_generic_parameter_span,
            help_display: Some("redefined here"),
        })?;

        Ok(())
    }
}

/// The default generic parameter must be trailing.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DefaultGenericParameterMustBeTrailing {
    /// The span of the generic parameter.
    pub invalid_generic_default_parameter_spans: Vec<Span>,
}

impl<T: State> Display<T> for DefaultGenericParameterMustBeTrailing {
    fn fmt(&self, _: &Table<T>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", Message {
            severity: Severity::Error,
            display: "the default generic parameter must be trailing",
        })?;

        for span in &self.invalid_generic_default_parameter_spans {
            write!(f, "\n{}", SourceCodeDisplay {
                span,
                help_display: Option::<i32>::None,
            })?;
        }

        Ok(())
    }
}

/// The trait member was expected but the non-trait member was found.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitMemberExpected {
    /// The span where the non-trait member was found.
    pub non_trait_member_span: Span,
}

impl<T: State> Display<T> for TraitMemberExpected {
    fn fmt(&self, _: &Table<T>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", Message {
            severity: Severity::Error,
            display: "the trait member was expected but the non-trait member \
                      was found",
        })?;

        write!(f, "\n{}", SourceCodeDisplay {
            span: &self.non_trait_member_span,
            help_display: Option::<i32>::None,
        })?;

        Ok(())
    }
}

/// Trait member bound argument mismatched.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MisMatchedTraitMemberBoundArgument {
    /// The span where the trait member bound argument was found.
    pub trait_member_bound_argument_span: Span,
}

impl<T: State> Display<T> for MisMatchedTraitMemberBoundArgument {
    fn fmt(&self, _: &Table<T>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", Message {
            severity: Severity::Error,
            display: "the trait member bound argument mismatched",
        })?;

        write!(f, "\n{}", SourceCodeDisplay {
            span: &self.trait_member_bound_argument_span,
            help_display: Option::<i32>::None,
        })?;

        Ok(())
    }
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

impl<T: State> Display<T> for MismatchedTraitMemberAndImplementationMember {
    fn fmt(&self, table: &Table<T>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
pub struct UnusedGenericParameterInImplementation<ID> {
    /// The ID of the unused generic parameter.
    pub generic_parameter_id: LocalGenericParameterID,

    /// The ID of the implementation in which the generic parameter is unused.
    pub implementation_id: ID,
}

impl<T: State, ID: Copy + Into<GenericID>> Display<T>
    for UnusedGenericParameterInImplementation<ID>
{
    fn fmt(&self, table: &Table<T>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let generic_id: GenericID = self.implementation_id.into();
        let generic_symbol = table.get_generic(generic_id).ok_or(fmt::Error)?;

        let (span, name, kind) = {
            match self.generic_parameter_id {
                LocalGenericParameterID::Lifetime(lifetime_parameter_id) => {
                    let lifetime_param = generic_symbol
                        .generic_declaration()
                        .parameters
                        .lifetimes()
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
                        .types()
                        .get(type_parameter_id)
                        .ok_or(fmt::Error)?;

                    (
                        type_param.span.as_ref(),
                        type_param.name.as_deref().unwrap_or("{unknown}"),
                        "type parameter",
                    )
                }
                LocalGenericParameterID::Constant(constant_parameter_id) => {
                    let constant_param = generic_symbol
                        .generic_declaration()
                        .parameters
                        .constants()
                        .get(constant_parameter_id)
                        .ok_or(fmt::Error)?;

                    (
                        constant_param.span.as_ref(),
                        constant_param.name.as_deref().unwrap_or("{unknown}"),
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
    pub first_implementation_id: TraitImplementationID,

    /// The ID of the second implementation.
    pub second_implementation_id: TraitImplementationID,
}

impl<T: State> Display<T> for AmbiguousImplementation {
    fn fmt(&self, table: &Table<T>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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

impl<T: State> Display<T> for RedefinedHigherRankedLifetime {
    fn fmt(&self, _: &Table<T>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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

impl<T: State> Display<T> for MismatchedGenericParameterCountInImplementation {
    fn fmt(&self, table: &Table<T>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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

impl<T: State> Display<T> for UnknownTraitImplementationMember {
    fn fmt(&self, table: &Table<T>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let trait_qualified_name =
            table.get_qualified_name(self.trait_id.into()).ok_or(fmt::Error)?;
        let trait_sym =
            table.get_global(self.trait_id.into()).ok_or(fmt::Error)?;
        let trait_span = trait_sym.span();

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

impl<T: State> Display<T> for AlreadyImplementedTraitMember {
    fn fmt(&self, table: &Table<T>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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

/// The field is not accessible in the given scope.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FieldIsNotAccessible {
    /// The ID of the field that is not accessible.
    pub field_id: ID<Field>,

    /// The struct in which the field is not accessible.
    pub struct_id: ID<Struct>,

    /// The ID to the scope where the field is not accessible.
    pub referring_site: GlobalID,

    /// The span where the field is referred from.
    pub referring_identifier_span: Span,
}

impl<T: State> Display<T> for FieldIsNotAccessible {
    fn fmt(&self, table: &Table<T>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let struct_sym = table.get(self.struct_id).ok_or(fmt::Error)?;
        let field_sym =
            struct_sym.fields.get(self.field_id).ok_or(fmt::Error)?;
        let referring_site_qualified_name =
            table.get_qualified_name(self.referring_site).ok_or(fmt::Error)?;
        let struct_qualified_name = table
            .get_qualified_name(self.struct_id.into())
            .ok_or(fmt::Error)?;

        write!(f, "{}", Message {
            severity: Severity::Error,
            display: format!(
                "the field `{}` of `{struct_qualified_name}` is not \
                 accessible in the scope `{referring_site_qualified_name}`",
                field_sym.name
            ),
        })?;

        write!(f, "\n{}", SourceCodeDisplay {
            span: &self.referring_identifier_span,
            help_display: Option::<i32>::None,
        })?;

        Ok(())
    }
}

/// The type of the constant parameter in the implementation doesn't match the
/// type of the constant parameter in the trait.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MismatchedImplementationConstantTypeParameter {
    /// The constant parameter ID of the implementation member
    pub implementation_member_constant_parameter_id: ConstantParameterID,

    /// The constant parameter ID of the trait member
    pub trait_member_constant_parameter_id: ConstantParameterID,
}

impl<T: State> Display<T> for MismatchedImplementationConstantTypeParameter {
    fn fmt(&self, table: &Table<T>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let trait_member_span = table
            .get_generic(
                self.implementation_member_constant_parameter_id.parent,
            )
            .and_then(|x| {
                x.generic_declaration()
                    .parameters
                    .constants()
                    .get(self.implementation_member_constant_parameter_id.id)
                    .map(|x| x.span().cloned())
            })
            .ok_or(fmt::Error)?;

        let implementation_member_span = table
            .get_generic(self.trait_member_constant_parameter_id.parent)
            .and_then(|x| {
                x.generic_declaration()
                    .parameters
                    .constants()
                    .get(self.trait_member_constant_parameter_id.id)
                    .map(|x| x.span().cloned())
            })
            .ok_or(fmt::Error)?;

        write!(f, "{}", Message {
            severity: Severity::Error,
            display: "the type of the constant parameter in the \
                      implementation doesn't match the type of the constant \
                      parameter in the trait",
        })?;

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
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnimplementedTraitMembers {
    /// The list of trait members that are not implemented.
    pub unimplemented_trait_member_ids: HashSet<TraitMemberID>,

    /// The ID of the implementation in which the trait members are not
    pub implementation_id: ID<PositiveTraitImplementation>,
}

impl<T: State> Display<T> for UnimplementedTraitMembers {
    fn fmt(&self, table: &Table<T>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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

/// The bound is not satisfied upon instantiation.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UnsatisifedPredicate<M: Model> {
    /// The unsatisfied bound.
    pub predicate: Predicate<M>,

    /// The span of the instantiation that causes the bound check.
    pub instantiation_span: Span,

    /// The span of the trait member bound declaration.
    pub predicate_declaration_span: Option<Span>,
}

impl<S: State, M: Model> Display<S> for UnsatisifedPredicate<M> {
    fn fmt(&self, table: &Table<S>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", Message {
            severity: Severity::Error,
            display: format!(
                "the predicate `{}` is not satisfied",
                DisplayObject { display: &self.predicate, table }
            ),
        })?;

        write!(f, "\n{}", SourceCodeDisplay {
            span: &self.instantiation_span,
            help_display: Option::<i32>::None,
        })?;

        if let Some(span) = self.predicate_declaration_span.as_ref() {
            write!(f, "\n{}", SourceCodeDisplay {
                span,
                help_display: Some("predicate declared here"),
            })?;
        }

        Ok(())
    }
}

/// The field with the same name already exists in the struct.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DuplicatedField {
    /// The struct ID where the field is duplicated.
    pub struct_id: ID<Struct>,

    /// The ID of the existing field.
    pub field_id: ID<Field>,

    /// The span of the redeclaration.
    pub redeclaration_span: Span,
}

impl<T: State> Display<T> for DuplicatedField {
    fn fmt(&self, table: &Table<T>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let struct_qualified_name = table
            .get_qualified_name(self.struct_id.into())
            .ok_or(fmt::Error)?;

        let struct_sym = table.get(self.struct_id).ok_or(fmt::Error)?;
        let field_sym =
            struct_sym.fields.get(self.field_id).ok_or(fmt::Error)?;

        write!(f, "{}", Message {
            severity: Severity::Error,
            display: format!(
                "the field `{}` is already defined in the struct \
                 `{struct_qualified_name}`",
                field_sym.name
            ),
        })?;

        write!(f, "\n{}", SourceCodeDisplay {
            span: &self.redeclaration_span,
            help_display: Option::<i32>::None,
        })?;

        if let Some(span) = &field_sym.span {
            write!(f, "\n{}", SourceCodeDisplay {
                span,
                help_display: Some("is first defined here"),
            })?;
        }

        Ok(())
    }
}

/// The trait implementation member does not include the predicate defined in
/// the trait member.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UnsatisfiedTraitMemberPredicate<M: Model> {
    /// The ID of the trait implementation member.
    pub trait_implementation_id: TraitImplementationMemberID,

    /// The predicate that is missing in the trait implementation member.
    pub predicate: Predicate<M>,

    /// The span of the predicate declaration.
    pub predicate_span: Option<Span>,
}

impl<T: State, M: Model> Display<T> for UnsatisfiedTraitMemberPredicate<M> {
    fn fmt(&self, table: &Table<T>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Some(trait_implementation_symbol) =
            table.get_global(self.trait_implementation_id.into())
        else {
            return Err(fmt::Error);
        };

        let Some(trait_implementation_symbol_span) =
            trait_implementation_symbol.span()
        else {
            return Err(fmt::Error);
        };

        let Some(qualified_name) =
            table.get_qualified_name(self.trait_implementation_id.into())
        else {
            return Err(fmt::Error);
        };

        write!(f, "{}", Message {
            severity: Severity::Error,
            display: format!(
                "the trait implementation member `{}` does not \
                 include/satisfy the predicate `{}`",
                qualified_name,
                DisplayObject { display: &self.predicate, table }
            ),
        })?;

        write!(f, "\n{}", SourceCodeDisplay {
            span: trait_implementation_symbol_span,
            help_display: Option::<i32>::None,
        })?;

        if let Some(span) = self.predicate_span.as_ref() {
            write!(f, "\n{}", SourceCodeDisplay {
                span,
                help_display: Some("predicate declared here"),
            })?;
        }

        Ok(())
    }
}

/// The trait implementation member contains extraneous predicate--a predicate
/// that is not defined in the trait member.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExtraneousTraitMemberPredicate<M: Model> {
    /// The ID of the trait implementation member containing the extraneous
    /// predicate.
    pub trait_implementation_member_id: TraitImplementationMemberID,

    /// The extraneous predicate.
    pub predicate: Predicate<M>,

    /// The declaration span of the extraneous predicate.
    pub predicate_span: Option<Span>,
}

impl<T: State, M: Model> Display<T> for ExtraneousTraitMemberPredicate<M> {
    fn fmt(&self, table: &Table<T>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Some(trait_implementation_symbol) =
            table.get_global(self.trait_implementation_member_id.into())
        else {
            return Err(fmt::Error);
        };

        let Some(trait_implementation_symbol_span) =
            trait_implementation_symbol.span()
        else {
            return Err(fmt::Error);
        };

        let Some(qualified_name) = table
            .get_qualified_name(self.trait_implementation_member_id.into())
        else {
            return Err(fmt::Error);
        };

        write!(f, "{}", Message {
            severity: Severity::Error,
            display: format!(
                "the trait implementation member `{}` contains an extraneous \
                 predicate `{}` -- a predicate that is not defined in the \
                 trait member",
                qualified_name,
                DisplayObject { display: &self.predicate, table }
            ),
        })?;

        write!(f, "\n{}", SourceCodeDisplay {
            span: trait_implementation_symbol_span,
            help_display: Option::<i32>::None,
        })?;

        if let Some(span) = self.predicate_span.as_ref() {
            write!(f, "\n{}", SourceCodeDisplay {
                span,
                help_display: Some("extraneous predicate declared here"),
            })?;
        }

        Ok(())
    }
}

/// The kind of the pattern binding.
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
pub enum PatternBindingType {
    #[display(fmt = "struct")]
    Struct,
    #[display(fmt = "tuple")]
    Tuple,
}

/// Pattern expects a different kind of binding.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MismatchedPatternBindingType<M: Model> {
    /// The expected binding type of the pattern.
    pub expected_bindnig_type: PatternBindingType,

    /// The found type of the pattern.
    pub found_type: Type<M>,

    /// The span of the pattern.
    pub pattern_span: Span,
}

impl<T: State, M: Model> Display<T> for MismatchedPatternBindingType<M> {
    fn fmt(&self, table: &Table<T>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", Message {
            severity: Severity::Error,
            display: format!(
                "the pattern expects a {} type but found {} ",
                self.expected_bindnig_type,
                DisplayObject { table, display: &self.found_type }
            ),
        })?;

        write!(f, "\n{}", SourceCodeDisplay {
            span: &self.pattern_span,
            help_display: Option::<i32>::None,
        })?;

        Ok(())
    }
}

/// Field with given name was not found in the struct.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FieldNotFound {
    /// The span to the identifier that refers to non-existent field.
    pub identifier_span: Span,

    /// The struct ID where the field is not found.
    pub struct_id: ID<Struct>,
}

impl<T: State> Display<T> for FieldNotFound {
    fn fmt(&self, table: &Table<T>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let struct_qualified_name = table
            .get_qualified_name(self.struct_id.into())
            .ok_or(fmt::Error)?;

        write!(f, "{}", Message {
            severity: Severity::Error,
            display: format!(
                "the field `{}` is not found in the struct \
                 `{struct_qualified_name}`",
                self.identifier_span.str(),
            ),
        })?;

        write!(f, "\n{}", SourceCodeDisplay {
            span: &self.identifier_span,
            help_display: Option::<i32>::None,
        })?;

        Ok(())
    }
}

/// The field is already bound in the pattern.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AlreadyBoundFieldPattern {
    /// The span of the pattern.
    pub pattern_span: Span,

    /// The ID of the struct where the field is already bound.
    pub struct_id: ID<Struct>,

    /// The ID of the field that is already bound.
    pub field_id: ID<Field>,
}

impl<T: State> Display<T> for AlreadyBoundFieldPattern {
    fn fmt(&self, table: &Table<T>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let struct_sym = table.get(self.struct_id).ok_or(fmt::Error)?;
        let field_sym =
            struct_sym.fields.get(self.field_id).ok_or(fmt::Error)?;

        write!(f, "{}", Message {
            severity: Severity::Error,
            display: format!(
                "the field `{}` is already bound in the pattern",
                field_sym.name
            ),
        })?;

        write!(f, "\n{}", SourceCodeDisplay {
            span: &self.pattern_span,
            help_display: Option::<i32>::None,
        })?;

        Ok(())
    }
}

/// The pattern contains mismatched element length.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MismatchedTuplePatternLength {
    /// The span of the pattern.
    pub pattern_span: Span,

    /// The number of elements in the pattern.
    pub pattern_element_count: usize,

    /// The number of elements in the type.
    pub type_element_count: usize,
}

impl<T: State> Display<T> for MismatchedTuplePatternLength {
    fn fmt(&self, _: &Table<T>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", Message {
            severity: Severity::Error,
            display: format!(
                "the pattern contains mismatched element length: expected {} \
                 elements but found {}",
                self.type_element_count, self.pattern_element_count
            ),
        })?;

        write!(f, "\n{}", SourceCodeDisplay {
            span: &self.pattern_span,
            help_display: Option::<i32>::None,
        })?;

        Ok(())
    }
}

/// Can't bind a tuple pattern to a reference bound tuple type with unpacked
/// element.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FoundUnpackedElementInReferenceBoundTupleType {
    /// The span of the pattern.
    pub pattern_span: Span,
}

impl<T: State> Display<T> for FoundUnpackedElementInReferenceBoundTupleType {
    fn fmt(&self, _: &Table<T>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", Message {
            severity: Severity::Error,
            display: "can't bind a tuple pattern to a reference bound tuple \
                      type with unpacked element",
        })?;

        write!(f, "\n{}", SourceCodeDisplay {
            span: &self.pattern_span,
            help_display: Option::<i32>::None,
        })?;

        Ok(())
    }
}

/// A particular name has already been bound in the given scope.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AlreadyBoundName {
    /// The span of the already bound identifier.
    pub already_bound_identifier_span: Span,

    /// The span of the rebinding.
    pub new_binding_span: Span,
}

impl<T: State> Display<T> for AlreadyBoundName {
    fn fmt(&self, _: &Table<T>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = self.already_bound_identifier_span.str();

        write!(f, "{}", Message {
            severity: Severity::Error,
            display: format!(
                "the name `{name}` has already been bound in the scope"
            ),
        })?;

        write!(f, "\n{}", SourceCodeDisplay {
            span: &self.new_binding_span,
            help_display: Option::<i32>::None,
        })?;

        write!(f, "\n{}", SourceCodeDisplay {
            span: &self.already_bound_identifier_span,
            help_display: Some("the name is already bound here"),
        })?;

        Ok(())
    }
}

/// Implemented by all semantic errors.
pub trait Error: Debug + Display<Suboptimal> + Send + Sync + 'static {
    #[allow(missing_docs, clippy::missing_errors_doc)]
    fn as_display_with_table(&self) -> &dyn Display<Suboptimal>;
}

impl<U: Debug + Display<Suboptimal> + Send + Sync + 'static> Error for U
where
    for<'a> DisplayObject<'a, U, Suboptimal>: fmt::Display,
{
    fn as_display_with_table(&self) -> &dyn Display<Suboptimal> { self }
}
