//! Contains all the definition of errors that can be emitted by the semantic
//! analyzer.

use std::{
    any::Any,
    collections::{BTreeSet, HashSet},
    fmt::{self, Debug},
};

use pernixc_base::{
    diagnostic::{Diagnostic, Related, Report},
    log::{Message, Severity, SourceCodeDisplay},
    source_file::Span,
};

use crate::{
    arena::ID,
    ir::representation::binding::infer::ConstraintModel,
    symbol::{
        table::{
            self, representation::Index, Display, DisplayObject, State,
            Suboptimal, Table,
        },
        Accessibility, AdtImplementation, AdtImplementationFunction,
        CallableID, ConstantParameterID, Field, GenericID, GenericKind,
        GenericParameter, GlobalID, LocalGenericParameterID, MemberID, Module,
        PositiveTraitImplementation, ResolvableImplementationID, Struct, Trait,
        TraitImplementationMemberID, TraitMemberID, Variant,
    },
    type_system::{
        equality,
        model::Model,
        predicate::Predicate,
        term::{
            constant,
            r#type::{self, Qualifier, Type},
            GenericArguments,
        },
    },
};

/// An error type used for [`Report::Error`] associated type.
///
/// This typically caused by giving an invalid table (not the same table where
/// the error originated from) to the parameter [`Report::report`]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(clippy::module_name_repetitions)]
pub struct ReportError;

/// The global symbol with the same name already exists in the given scope.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GlobalRedifinition {
    /// The ID of the existing symbol.
    pub existing_global_id: GlobalID,

    /// The ID of the new symbol.
    pub new_global_id: GlobalID,

    /// The scope in which the duplication occurred.
    pub in_global_id: GlobalID,
}

impl Report<&Table<Suboptimal>> for GlobalRedifinition {
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<pernixc_base::diagnostic::Diagnostic, Self::Error> {
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
            return Err(ReportError);
        };

        let Some(new_symbol_span) = new_symbol.span() else {
            return Err(ReportError);
        };

        Ok(Diagnostic {
            span: new_symbol_span.clone(),
            message: format!(
                "the symbol `{}` is already defined in `{}`",
                existing_symbol.name(),
                scope_qualified_name
            ),
            severity: Severity::Error,
            help_message: None,
            related: existing_symbol
                .span()
                .map(|x| Related {
                    span: x.clone(),
                    message: "previously defined here".to_string(),
                })
                .into_iter()
                .collect(),
        })
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
    ) -> Result<String, ReportError> {
        match accessibility {
            Accessibility::Public => Ok("publicly accessible".to_owned()),
            Accessibility::Scoped(module_id) => {
                let module_qualified_name = self
                    .get_qualified_name(module_id.into())
                    .ok_or(ReportError)?;

                Ok(format!("accessible in `{module_qualified_name}`"))
            }
        }
    }
}

impl Report<&Table<Suboptimal>> for SymbolIsMoreAccessibleThanParent {
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        let (Some(symbol_name), Some(parent_qualified_name)) = (
            table.get_global(self.symbol_id).map(|x| x.name().to_owned()),
            table.get_qualified_name(self.parent_id),
        ) else {
            return Err(ReportError);
        };

        let accessibility_description =
            |accessibility: Accessibility| -> Result<String, ReportError> {
                match accessibility {
                    Accessibility::Public => {
                        Ok("publicly accessibility".to_owned())
                    }
                    Accessibility::Scoped(module_id) => {
                        let module_qualified_name = table
                            .get_qualified_name(module_id.into())
                            .ok_or(ReportError)?;

                        Ok(format!("accessible in `{module_qualified_name}`"))
                    }
                }
            };

        let (Some(symbol_accessibility), Some(parent_accessibility)) = (
            table.get_accessibility(self.symbol_id),
            table.get_accessibility(self.parent_id),
        ) else {
            return Err(ReportError);
        };

        let (Some(symbol_span), Some(parent_span)) = (
            table.get_global(self.symbol_id).and_then(|x| x.span().cloned()),
            table.get_global(self.parent_id).map(|x| x.span().cloned()),
        ) else {
            return Err(ReportError);
        };

        let symbol_accessibility_description =
            accessibility_description(symbol_accessibility)?;

        let parent_accessibility_description =
            accessibility_description(parent_accessibility)?;

        Ok(Diagnostic {
            span: symbol_span,
            message: format!(
                "the symbol `{symbol_name}` in `{parent_qualified_name}` is \
                 more accessible than the parent symbol"
            ),
            severity: Severity::Error,
            help_message: Some(format!(
                "the symbol `{symbol_name}` is \
                 {symbol_accessibility_description}"
            )),
            related: parent_span
                .map(|x| Related {
                    span: x,
                    message: format!(
                        "the parent symbol is \
                         {parent_accessibility_description}",
                    ),
                })
                .into_iter()
                .collect(),
        })
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

impl Report<&Table<Suboptimal>> for InvalidSymbolInImplementation {
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        let qualified_name = table
            .get_qualified_name(self.invalid_global_id)
            .ok_or(ReportError)?;

        Ok(Diagnostic {
            span: self.qualified_identifier_span.clone(),
            message: format!(
                "the symbol `{qualified_name}` is not a trait, marker, \
                 struct, or enum"
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
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
            Self::AdtImplementationFunction(_) => "implementation function",
            Self::Marker(_) => "marker",
            Self::PositiveMarkerImplementation(_) => {
                "positive marker implementation"
            }
            Self::NegativeMarkerImplementation(_) => {
                "negative marker implementation"
            }
        }
    }
}

impl Report<&Table<Suboptimal>> for ExpectModule {
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        let found_symbol_qualified_name =
            table.get_qualified_name(self.found_id).ok_or(ReportError)?;

        Ok(Diagnostic {
            span: self.module_path.clone(),
            message: format!(
                "expected a module in the module path, but found `{} {}`",
                self.found_id.kind_str(),
                found_symbol_qualified_name
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
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

impl Report<&Table<Suboptimal>> for SelfModuleUsing {
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        let module_qualified_name = table
            .get_qualified_name(self.module_id.into())
            .ok_or(ReportError)?;

        Ok(Diagnostic {
            span: self.using_span.clone(),
            message: format!(
                "the module `{module_qualified_name}` was found to have a \
                 `using` statement that uses itself",
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// The marker implementation must be final.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MarkerImplementationMustBeFinal {
    /// The span where the marker implementation was found.
    pub marker_signature_span: Span,
}

impl Report<&Table<Suboptimal>> for MarkerImplementationMustBeFinal {
    type Error = ReportError;

    fn report(&self, _: &Table<Suboptimal>) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.marker_signature_span.clone(),
            message: "the marker implementation must be final".to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// The module was found having a duplicate `using` statement.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UsingDuplication {
    /// The module name that was used more than once in the `using` statement.
    pub already_used_module_id: ID<Module>,

    /// The span where the `using` statement was found.
    pub using_span: Span,
}

impl Report<&Table<Suboptimal>> for UsingDuplication {
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        let already_used_module_qualified_name = table
            .get_qualified_name(self.already_used_module_id.into())
            .ok_or(ReportError)?;

        Ok(Diagnostic {
            span: self.using_span.clone(),
            message: format!(
                "the module `{already_used_module_qualified_name}` was used \
                 more than once"
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
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

impl Report<&Table<Suboptimal>> for SymbolIsNotAccessible {
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        let referring_site_qualified_name =
            table.get_qualified_name(self.referring_site).ok_or(ReportError)?;

        let referred_qualified_name =
            table.get_qualified_name(self.referred).ok_or(ReportError)?;

        Ok(Diagnostic {
            span: self.referred_span.clone(),
            message: format!(
                "the symbol `{referred_qualified_name}` is not accessible \
                 from `{referring_site_qualified_name}`",
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
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

impl Report<&Table<Suboptimal>> for ResolutionAmbiguity {
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        let candidates = self
            .candidates
            .iter()
            .copied()
            .map(|x| table.get_qualified_name(x).ok_or(ReportError))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Diagnostic {
            span: self.resolution_span.clone(),
            message: format!(
                "the resolution resulted into multiple candidates: {}",
                candidates.join(", ")
            ),
            severity: Severity::Error,
            help_message: Some(
                "try using fully qualified name to solve the amguity"
                    .to_string(),
            ),
            related: Vec::new(),
        })
    }
}

/// No method found for the given method call expression.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MethodNotFound {
    /// The span where the method call was made.
    pub method_call_span: Span,
}

impl Report<&Table<Suboptimal>> for MethodNotFound {
    type Error = ReportError;

    fn report(&self, _: &Table<Suboptimal>) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.method_call_span.clone(),
            message: "no method found for the given method call expression"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// The trait method call resolves into multiple candidates.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AmbiguousMethodCall {
    /// The span where the method call was made.
    pub method_call_span: Span,

    /// The candidates that were found.
    pub callable_candidates: Vec<CallableID>,
}

impl Report<&Table<Suboptimal>> for AmbiguousMethodCall {
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        let candidates = self
            .callable_candidates
            .iter()
            .copied()
            .map(|x| table.get_qualified_name(x.into()).ok_or(ReportError))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Diagnostic {
            span: self.method_call_span.clone(),
            message: format!(
                "the method call resulted into multiple candidates: {}",
                candidates.join(", ")
            ),
            severity: Severity::Error,
            help_message: Some(
                "try using fully qualified name to solve the amguity"
                    .to_string(),
            ),
            related: Vec::new(),
        })
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

impl Report<&Table<Suboptimal>> for SymbolNotFound {
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        if let Some(searched_in_module_id) = self.searched_global_id {
            let qualified_name = table
                .get_qualified_name(searched_in_module_id)
                .ok_or(ReportError)?;

            Ok(Diagnostic {
                span: self.resolution_span.clone(),
                message: format!(
                    "the symbol named `{}` does not exist in `{}`",
                    self.resolution_span.str(),
                    qualified_name
                ),
                severity: Severity::Error,
                help_message: None,
                related: Vec::new(),
            })
        } else {
            Ok(Diagnostic {
                span: self.resolution_span.clone(),
                message: format!(
                    "the symbol named `{}` does not exist",
                    self.resolution_span.str()
                ),
                severity: Severity::Error,
                help_message: None,
                related: Vec::new(),
            })
        }
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

impl Report<&Table<Suboptimal>> for NoGenericArgumentsRequired {
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        let qualified_name =
            table.get_qualified_name(self.global_id).ok_or(ReportError)?;

        Ok(Diagnostic {
            span: self.generic_argument_span.clone(),
            message: format!(
                "the symbol `{qualified_name}` doesn't require any generic \
                 arguments"
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// Expects a marker symbol but found other kind of symbol.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExpectedMarker {
    /// The ID of the symbol that was not a marker.
    pub found_id: GlobalID,

    /// The span of the marker path.
    pub marker_path: Span,
}

impl Report<&Table<Suboptimal>> for ExpectedMarker {
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        let found_symbol_qualified_name =
            table.get_qualified_name(self.found_id).ok_or(ReportError)?;

        Ok(Diagnostic {
            span: self.marker_path.clone(),
            message: format!(
                "expect a marker in the path, but found `{} {}`",
                self.found_id.kind_str(),
                found_symbol_qualified_name
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
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

impl Report<&Table<Suboptimal>> for ExpectTrait {
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        let found_symbol_qualified_name =
            table.get_qualified_name(self.found_id).ok_or(ReportError)?;

        Ok(Diagnostic {
            span: self.trait_path.clone(),
            message: format!(
                "expect a trait in the path, but found `{} {}`",
                self.found_id.kind_str(),
                found_symbol_qualified_name
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// The cyclic dependency was found in the given set of symbols.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CyclicDependency {
    /// List of symbols that are involved in the cycle.
    pub participants: BTreeSet<GlobalID>,
}

impl Report<&Table<Suboptimal>> for CyclicDependency {
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        let first_symbol =
            self.participants.iter().next().ok_or(ReportError)?;
        let symbol_span = table
            .get_global(*first_symbol)
            .ok_or(ReportError)?
            .span()
            .ok_or(ReportError)?;
        let symbol_list = self
            .participants
            .iter()
            .map(|&symbol| {
                let qualified_name =
                    table.get_qualified_name(symbol).ok_or(ReportError)?;

                Ok(format!("`{qualified_name}`"))
            })
            .collect::<Result<Vec<_>, _>>()?
            .join(", ");

        Ok(Diagnostic {
            span: symbol_span.clone(),
            message: format!(
                "the cyclic dependency was found in the given set of \
                 symbol(s): {symbol_list}"
            ),
            severity: Severity::Error,
            help_message: None,
            related: self
                .participants
                .iter()
                .filter_map(|&participant| {
                    table.get_global(participant).and_then(|x| x.span())
                })
                .map(|x| Related {
                    span: x.clone(),
                    message: "participated in cyclic dependency".to_string(),
                })
                .collect(),
        })
    }
}

/// The generic arguments were supplied in the wrong order.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MisorderedGenericArgument {
    /// The kind of the mis-ordered generic argument.
    pub generic_kind: GenericKind,

    /// The span of the generic argument.
    pub generic_argument: Span,
}

impl Report<&Table<Suboptimal>> for MisorderedGenericArgument {
    type Error = ReportError;

    fn report(&self, _: &Table<Suboptimal>) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.generic_argument.clone(),
            message: "the generic argument was supplied in the wrong order"
                .to_string(),
            severity: Severity::Error,
            help_message: match self.generic_kind {
                GenericKind::Type => Some(
                    "can't be supplied after constant arguments".to_string(),
                ),
                GenericKind::Lifetime => Some(
                    "can't be supplied after type or constant arguments"
                        .to_string(),
                ),
                GenericKind::Constant => None,
            },
            related: Vec::new(),
        })
    }
}

/// Implementation with body found on a marker.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FoundImplementationWithBodyOnMarker {
    /// The span where the implementation was found.
    pub implementation_span: Span,
}

impl Report<&Table<Suboptimal>> for FoundImplementationWithBodyOnMarker {
    type Error = ReportError;

    fn report(&self, _: &Table<Suboptimal>) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.implementation_span.clone(),
            message: "implementation with body found on a marker; expected an \
                      implementation with `delete` keyword or empty \
                      (delimited with semicolon)"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// Empty implementation found on a trait implementation.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FoundEmptyImplementationOnTrait {
    /// The span where the empty implementation was found.
    pub empty_implementation_signature_span: Span,
}

impl Report<&Table<Suboptimal>> for FoundEmptyImplementationOnTrait {
    type Error = ReportError;

    fn report(&self, _: &Table<Suboptimal>) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.empty_implementation_signature_span.clone(),
            message: "empty implementation found on a trait; expected an \
                      implementation with body or `delete` keyword"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// The adt implementation expects an implementation with a body.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExpectedImplementationWithBodyForAdt {
    /// The implementation that was found to be invalid.
    pub invalid_implementation_span: Span,
}

impl Report<&Table<Suboptimal>> for ExpectedImplementationWithBodyForAdt {
    type Error = ReportError;

    fn report(&self, _: &Table<Suboptimal>) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.invalid_implementation_span.clone(),
            message: "implementation on struct or enum expects an \
                      implementation with a body"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
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

impl Report<&Table<Suboptimal>> for MisOrderedGenericParameter {
    type Error = ReportError;

    fn report(&self, _: &Table<Suboptimal>) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.generic_parameter_span.clone(),
            message: "the generic parameter was declared in the wrong order"
                .to_string(),
            severity: Severity::Error,
            help_message: match self.generic_kind {
                GenericKind::Type => Some(
                    "can't be declared after constant parameters".to_string(),
                ),
                GenericKind::Lifetime => Some(
                    "can't be declared after type or constant parameters"
                        .to_string(),
                ),
                GenericKind::Constant => None,
            },
            related: Vec::new(),
        })
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

impl<T: Display<Suboptimal>> Report<&Table<Suboptimal>>
    for PrivateEntityLeakedToPublicInterface<T>
{
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.leaked_span.clone(),
            message: format!(
                "`{}` is {} but it was declared in a `{}` interface, which is \
                 {}",
                DisplayObject { display: &self.entity, table },
                table.accessibility_description(
                    self.entity_overall_accessibility
                )?,
                table
                    .get_qualified_name(self.public_interface_id)
                    .ok_or(ReportError)?,
                table.accessibility_description(
                    table
                        .get_accessibility(self.public_interface_id)
                        .ok_or(ReportError)?
                )?,
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// Generic arguments count mismatch.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MismatchedGenericArgumentCount {
    /// The kind of the generic parameter.
    pub generic_kind: GenericKind,

    /// Span where mismatch occurred.
    pub generic_identifier_span: Span,

    /// Expected count of generic arguments.
    pub expected_count: usize,

    /// Supplied count of generic arguments.
    pub supplied_count: usize,
}

impl Report<&Table<Suboptimal>> for MismatchedGenericArgumentCount {
    type Error = ReportError;

    fn report(&self, _: &Table<Suboptimal>) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.generic_identifier_span.clone(),
            message: format!(
                "expected {} {} arguments, but {} were supplied",
                self.expected_count,
                match self.generic_kind {
                    GenericKind::Type => "type",
                    GenericKind::Lifetime => "lifetime",
                    GenericKind::Constant => "constant",
                },
                self.supplied_count,
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
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

impl Report<&Table<Suboptimal>> for LifetimeParameterNotFound {
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        let referring_site_qualified_name =
            table.get_qualified_name(self.referring_site).ok_or(ReportError)?;

        Ok(Diagnostic {
            span: self.referred_span.clone(),
            message: format!(
                "the lifetime parameter `{}` was not found in \
                 `{referring_site_qualified_name}`",
                self.referred_span.str()
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// The inference term isn't allowed in the given context.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UnexpectedInference {
    /// Span where the inference term was found.
    pub unexpected_span: Span,

    /// The kind of the inference term.
    pub generic_kind: GenericKind,
}

impl Report<&Table<Suboptimal>> for UnexpectedInference {
    type Error = ReportError;

    fn report(&self, _: &Table<Suboptimal>) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.unexpected_span.clone(),
            message: format!("{} inference is not allowed here", match self
                .generic_kind
            {
                GenericKind::Type => "type",
                GenericKind::Lifetime => "lifetime",
                GenericKind::Constant => "constant",
            }),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
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

impl<M: Model> Report<&Table<Suboptimal>> for UndecidablePredicate<M>
where
    Predicate<M>: Display<Suboptimal>,
{
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.instantiation_span.clone(),
            message: format!(
                "overflow calculating the satisfiability of `{}` predicate \
                 when instantiating",
                DisplayObject { display: &self.predicate, table }
            ),
            severity: Severity::Error,
            help_message: None,
            related: self
                .predicate_declaration_span
                .as_ref()
                .map(|predicate_span| Related {
                    span: predicate_span.clone(),
                    message: "predicate defined here".to_string(),
                })
                .into_iter()
                .collect(),
        })
    }
}

/// Adt implementation member can only be a function.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UnexpectedAdtImplementationMember {
    /// The span where the unexpected member was found.
    pub unexpected_member_span: Span,
}

impl Report<&Table<Suboptimal>> for UnexpectedAdtImplementationMember {
    type Error = ReportError;

    fn report(&self, _: &Table<Suboptimal>) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.unexpected_member_span.clone(),
            message: "adt (struct and enum) implementation can only contain \
                      functions"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// The tuple type contains more than one unpacked type.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MoreThanOneUnpackedInTupleType {
    /// The span where the illegal tuple type was found.
    pub illegal_tuple_type_span: Span,
}

impl Report<&Table<Suboptimal>> for MoreThanOneUnpackedInTupleType {
    type Error = ReportError;

    fn report(&self, _: &Table<Suboptimal>) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.illegal_tuple_type_span.clone(),
            message: "the tuple type contains more than one unpacked type"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
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

impl Report<&Table<Suboptimal>> for ExpectType {
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        let qualified_name = table
            .get_qualified_name(self.resolved_global_id)
            .ok_or(ReportError)?;

        Ok(Diagnostic {
            span: self.non_type_symbol_span.clone(),
            message: format!(
                "the type was expected but found {} `{qualified_name}`",
                self.resolved_global_id.kind_str(),
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
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

impl<T: GenericParameter> Report<&Table<Suboptimal>>
    for DuplicatedGenericParameter<T>
{
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        let Some(generic_symbol) =
            table.get_generic(self.existing_generic_parameter_id.parent)
        else {
            return Err(ReportError);
        };

        let Some(generic_parameter) = T::get_generic_parameters_arena(
            &generic_symbol.generic_declaration().parameters,
        )
        .get(self.existing_generic_parameter_id.id) else {
            return Err(ReportError);
        };

        Ok(Diagnostic {
            span: self.duplicating_generic_parameter_span.clone(),
            message: format!(
                "the generic parameter named `{}` is already defined",
                generic_parameter.name().unwrap_or("?")
            ),
            severity: Severity::Error,
            help_message: None,
            related: generic_parameter
                .span()
                .map(|x| Related {
                    span: x.clone(),
                    message: "previously defined here".to_string(),
                })
                .into_iter()
                .collect(),
        })
    }
}

/// The default generic parameter must be trailing.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DefaultGenericParameterMustBeTrailing {
    /// The span of the generic parameter.
    pub invalid_generic_default_parameter_span: Span,
}

impl Report<&Table<Suboptimal>> for DefaultGenericParameterMustBeTrailing {
    type Error = ReportError;

    fn report(&self, _: &Table<Suboptimal>) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.invalid_generic_default_parameter_span.clone(),
            message: "the default generic parameter must be trailing"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// The trait member was expected but the non-trait member was found.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExpectTraitMember {
    /// The span where the non-trait member was found.
    pub non_trait_member_span: Span,
}

impl Report<&Table<Suboptimal>> for ExpectTraitMember {
    type Error = ReportError;

    fn report(&self, _: &Table<Suboptimal>) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.non_trait_member_span.clone(),
            message: "the trait member was expected but the non-trait member \
                      was found"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
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

impl Report<&Table<Suboptimal>>
    for MismatchedTraitMemberAndImplementationMember
{
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        let trait_member_qualified_identifier = table
            .get_qualified_name(self.trait_member_id.into())
            .ok_or(ReportError)?;
        let trait_member_sym =
            table.get_global(self.trait_member_id.into()).ok_or(ReportError)?;

        let trait_member_kind = match self.trait_member_id {
            TraitMemberID::Type(_) => TraitMemberKind::Type,
            TraitMemberID::Function(_) => TraitMemberKind::Function,
            TraitMemberID::Constant(_) => TraitMemberKind::Constant,
        };

        Ok(Diagnostic {
            span: self.implementation_member_identifer_span.clone(),
            message: format!(
                "the trait member `{trait_member_qualified_identifier}` is of \
                 kind `{trait_member_kind}` but the implementation member is \
                 of kind `{}`",
                self.found_kind
            ),
            severity: Severity::Error,
            help_message: None,
            related: trait_member_sym
                .span()
                .map(|x| Related {
                    span: x.clone(),
                    message: "the trait member is defined here".to_string(),
                })
                .into_iter()
                .collect(),
        })
    }
}

/// Generic parameter is unused in the implementation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UnusedGenericParameterInImplementation {
    /// The ID of the unused generic parameter.
    pub generic_parameter_id: LocalGenericParameterID,

    /// The ID of the implementation in which the generic parameter is unused.
    pub implementation_id: GenericID,
}

impl Report<&Table<Suboptimal>> for UnusedGenericParameterInImplementation {
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        let generic_id = self.implementation_id;
        let generic_symbol =
            table.get_generic(generic_id).ok_or(ReportError)?;

        let (span, name, kind) = {
            match self.generic_parameter_id {
                LocalGenericParameterID::Lifetime(lifetime_parameter_id) => {
                    let lifetime_param = generic_symbol
                        .generic_declaration()
                        .parameters
                        .lifetimes()
                        .get(lifetime_parameter_id)
                        .ok_or(ReportError)?;

                    (
                        lifetime_param.span(),
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
                        .ok_or(ReportError)?;

                    (
                        type_param.span(),
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
                        .ok_or(ReportError)?;

                    (
                        constant_param.span(),
                        constant_param.name.as_deref().unwrap_or("{unknown}"),
                        "constant parameter",
                    )
                }
            }
        };

        let span = span.ok_or(ReportError)?.clone();

        Ok(Diagnostic {
            span,
            message: format!(
                "the {kind} `{name}` is unused in the implementation",
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// Two implementations have the same specialty order.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AmbiguousImplementation {
    /// The ID of the first implementation.
    pub first_implementation_id: ResolvableImplementationID,

    /// The ID of the second implementation.
    pub second_implementation_id: ResolvableImplementationID,
}

impl Report<&Table<Suboptimal>> for AmbiguousImplementation {
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        let name = table
            .get_qualified_name(self.first_implementation_id.into())
            .ok_or(ReportError)?;

        Ok(Diagnostic {
            span: table
                .get_global(self.first_implementation_id.into())
                .ok_or(ReportError)?
                .span()
                .ok_or(ReportError)?
                .clone(),
            message: format!(
                "the implementations of the `{name}` are ambiguous",
            ),
            severity: Severity::Error,
            help_message: None,
            related: vec![
                Related {
                    span: table
                        .get_global(self.first_implementation_id.into())
                        .ok_or(ReportError)?
                        .span()
                        .ok_or(ReportError)?
                        .clone(),
                    message: format!(
                        "the first implementation of the trait `{name}`",
                    ),
                },
                Related {
                    span: table
                        .get_global(self.second_implementation_id.into())
                        .ok_or(ReportError)?
                        .span()
                        .ok_or(ReportError)?
                        .clone(),
                    message: format!(
                        "the second implementation of the trait `{name}`",
                    ),
                },
            ],
        })
    }
}

/// The higher-ranked lifetime with the same name already exists in the given
/// scope.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HigherRankedLifetimeRedefinition {
    /// The span of the redefinition.
    pub redefinition_span: Span,
}

impl Report<&Table<Suboptimal>> for HigherRankedLifetimeRedefinition {
    type Error = ReportError;

    fn report(&self, _: &Table<Suboptimal>) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.redefinition_span.clone(),
            message: "the higher-ranked lifetime with the same name already \
                      exists in the given scope"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
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

impl Report<&Table<Suboptimal>>
    for MismatchedGenericParameterCountInImplementation
{
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        let generic_kind = match self.generic_kind {
            GenericKind::Type => "type",
            GenericKind::Lifetime => "lifetime",
            GenericKind::Constant => "constant",
        };

        Ok(Diagnostic {
            span: table
                .get_global(self.implementation_member_id.into())
                .and_then(|x| x.span())
                .ok_or(ReportError)?
                .clone(),
            message: format!(
                "the implementation member has {} {generic_kind} parameters, \
                 but the trait member has {}",
                self.declared_count, self.expected_count
            ),
            severity: Severity::Error,
            help_message: None,
            related: table
                .get_global(self.trait_member_id.into())
                .and_then(|x| x.span())
                .map(|x| Related {
                    span: x.clone(),
                    message: "trait member declared here".to_string(),
                })
                .into_iter()
                .collect(),
        })
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

impl Report<&Table<Suboptimal>> for UnknownTraitImplementationMember {
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        let trait_qualified_name = table
            .get_qualified_name(self.trait_id.into())
            .ok_or(ReportError)?;
        let trait_sym =
            table.get_global(self.trait_id.into()).ok_or(ReportError)?;
        let trait_span = trait_sym.span();

        Ok(Diagnostic {
            span: self.identifier_span.clone(),
            message: format!(
                "the symbol named `{}` is not a member of the trait \
                 `{trait_qualified_name}`",
                self.identifier_span.str()
            ),
            severity: Severity::Error,
            help_message: None,
            related: trait_span
                .map(|x| Related {
                    span: x.clone(),
                    message: "trait declared here".to_string(),
                })
                .into_iter()
                .collect(),
        })
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

impl Report<&Table<Suboptimal>> for AlreadyImplementedTraitMember {
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        let trait_member_qualified_name = table
            .get_qualified_name(self.trait_member_id.into())
            .ok_or(ReportError)?;

        Ok(Diagnostic {
            span: self.new_implementation_span.clone(),
            message: format!(
                "the trait member `{trait_member_qualified_name}` is already \
                 implemented"
            ),
            severity: Severity::Error,
            help_message: None,
            related: vec![Related {
                span: table
                    .get_global(self.implemented_id.into())
                    .and_then(|x| x.span())
                    .ok_or(ReportError)?
                    .clone(),
                message: "the trait member is already implemented here"
                    .to_string(),
            }],
        })
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

impl Report<&Table<Suboptimal>> for FieldIsNotAccessible {
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        let struct_sym = table.get(self.struct_id).ok_or(ReportError)?;
        let field_sym =
            struct_sym.fields().get(self.field_id).ok_or(ReportError)?;
        let referring_site_qualified_name =
            table.get_qualified_name(self.referring_site).ok_or(ReportError)?;
        let struct_qualified_name = table
            .get_qualified_name(self.struct_id.into())
            .ok_or(ReportError)?;

        Ok(Diagnostic {
            span: self.referring_identifier_span.clone(),
            message: format!(
                "the field `{}` of `{struct_qualified_name}` is not \
                 accessible in the scope `{referring_site_qualified_name}`",
                field_sym.name
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
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

impl Report<&Table<Suboptimal>>
    for MismatchedImplementationConstantTypeParameter
{
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
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
            .ok_or(ReportError)?;

        let implementation_member_span = table
            .get_generic(self.trait_member_constant_parameter_id.parent)
            .and_then(|x| {
                x.generic_declaration()
                    .parameters
                    .constants()
                    .get(self.trait_member_constant_parameter_id.id)
                    .map(|x| x.span().cloned())
            })
            .ok_or(ReportError)?;

        Ok(Diagnostic {
            span: implementation_member_span.ok_or(ReportError)?,
            message: "the type of the constant parameter in the \
                      implementation doesn't match the type of the constant \
                      parameter in the trait"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: trait_member_span
                .map(|x| Related {
                    span: x,
                    message: "the constant parameter in the trait is declared \
                              here"
                        .to_string(),
                })
                .into_iter()
                .collect(),
        })
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

impl Report<&Table<Suboptimal>> for UnimplementedTraitMembers {
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        let trait_member_qualified_names = self
            .unimplemented_trait_member_ids
            .iter()
            .map(|&trait_member_id| {
                table
                    .get_qualified_name(trait_member_id.into())
                    .ok_or(ReportError)
            })
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Diagnostic {
            span: table
                .get_global(self.implementation_id.into())
                .and_then(|x| x.span())
                .ok_or(ReportError)?
                .clone(),
            message: format!(
                "not all trait member(s) are implemented in the \
                 implementation: {}",
                trait_member_qualified_names.join(", ")
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// The bound is not satisfied upon instantiation.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UnsatisifedPredicate<M: Model> {
    /// The unsatisfied bound.
    pub predicate: Predicate<M>,

    /// The span of the instantiation that causes the bound check.
    pub instantiation_span: Span,

    /// The span of the predicate declaration.
    pub predicate_declaration_span: Option<Span>,
}

impl<M: Model> Report<&Table<Suboptimal>> for UnsatisifedPredicate<M>
where
    Predicate<M>: Display<Suboptimal>,
{
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.instantiation_span.clone(),
            message: format!(
                "the predicate `{}` is not satisfied",
                DisplayObject { display: &self.predicate, table }
            ),
            severity: Severity::Error,
            help_message: None,
            related: self
                .predicate_declaration_span
                .as_ref()
                .map(|predicate_span| Related {
                    span: predicate_span.clone(),
                    message: "predicate declared here".to_string(),
                })
                .into_iter()
                .collect(),
        })
    }
}

/// Tuple pack pattern expected.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExpectTuplePackPattern {
    /// Where the tuple pack pattern was expected.
    pub illegal_tuple_span: Span,
}

impl Report<&Table<Suboptimal>> for ExpectTuplePackPattern {
    type Error = ReportError;

    fn report(&self, _: &Table<Suboptimal>) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.illegal_tuple_span.clone(),
            message: "tuple pack pattern expected".to_string(),
            severity: Severity::Error,
            help_message: Some(
                "unpacked tuple type needs to be matched with a tuple pack \
                 pattern"
                    .to_string(),
            ),
            related: Vec::new(),
        })
    }
}

/// Not all fields are bound in the pattern.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UnboundFields {
    /// The ID of the struct where the fields are unbound.
    pub field_ids: Vec<ID<Field>>,

    /// The ID of the struct where the fields are unbound.
    pub struct_id: ID<Struct>,

    /// The span of the pattern where the fields are unbound.
    pub pattern_span: Span,
}

impl Report<&Table<Suboptimal>> for UnboundFields {
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        let struct_sym = table.get(self.struct_id).ok_or(ReportError)?;
        let field_names = self
            .field_ids
            .iter()
            .map(|&field_id| {
                struct_sym
                    .fields()
                    .get(field_id)
                    .map(|field_sym| field_sym.name.clone())
            })
            .collect::<Option<Vec<_>>>()
            .ok_or(ReportError)?;

        Ok(Diagnostic {
            span: self.pattern_span.clone(),
            message: format!(
                "not all fields are bound in the pattern: {}",
                field_names.join(", ")
            ),
            severity: Severity::Error,
            help_message: Some(
                "try to handle all fields in the pattern or use the `..` \
                 syntax after the fields to ignore the rest"
                    .to_string(),
            ),
            related: Vec::new(),
        })
    }
}

/// The pattern contains more than one packed tuple pattern.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MoreThanOnePackedTuplePattern {
    /// The span where the illegal tuple pattern was found.
    pub illegal_tuple_pattern_span: Span,
}

impl Report<&Table<Suboptimal>> for MoreThanOnePackedTuplePattern {
    type Error = ReportError;

    fn report(&self, _: &Table<Suboptimal>) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.illegal_tuple_pattern_span.clone(),
            message: "the pattern contains more than one packed tuple pattern"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// The field with the same name already exists in the struct.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FieldDuplication {
    /// The struct ID where the field is duplicated.
    pub struct_id: ID<Struct>,

    /// The ID of the existing field.
    pub field_id: ID<Field>,

    /// The span of the redeclaration.
    pub redeclaration_span: Span,
}

impl Report<&Table<Suboptimal>> for FieldDuplication {
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        let struct_qualified_name = table
            .get_qualified_name(self.struct_id.into())
            .ok_or(ReportError)?;

        let struct_sym = table.get(self.struct_id).ok_or(ReportError)?;
        let field_sym =
            struct_sym.fields().get(self.field_id).ok_or(ReportError)?;

        Ok(Diagnostic {
            span: self.redeclaration_span.clone(),
            message: format!(
                "the field `{}` is already defined in the struct \
                 `{struct_qualified_name}`",
                field_sym.name
            ),
            severity: Severity::Error,
            help_message: None,
            related: vec![Related {
                span: field_sym.span.clone().ok_or(ReportError)?,
                message: "is first defined here".to_string(),
            }],
        })
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

impl<M: Model> Report<&Table<Suboptimal>> for UnsatisfiedTraitMemberPredicate<M>
where
    Predicate<M>: Display<Suboptimal>,
{
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        let trait_implementation_symbol = table
            .get_global(self.trait_implementation_id.into())
            .ok_or(ReportError)?;

        let trait_implementation_symbol_span =
            trait_implementation_symbol.span().ok_or(ReportError)?;

        let qualified_name = table
            .get_qualified_name(self.trait_implementation_id.into())
            .ok_or(ReportError)?;

        Ok(Diagnostic {
            span: trait_implementation_symbol_span.clone(),
            message: format!(
                "the trait implementation member `{}` does not \
                 include/satisfy the predicate `{}`",
                qualified_name,
                DisplayObject { display: &self.predicate, table }
            ),
            severity: Severity::Error,
            help_message: None,
            related: self
                .predicate_span
                .as_ref()
                .map(|predicate_span| Related {
                    span: predicate_span.clone(),
                    message: "predicate declared here".to_string(),
                })
                .into_iter()
                .collect(),
        })
    }
}

/// The name is already exists in the given module.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConflictingUsing {
    /// The span of the using statement.
    pub using_span: Span,

    /// The name that conflicts with the existing name in the module.
    pub name: String,

    /// The module where the name is already defined.
    pub module_id: ID<Module>,

    /// The span of the conflicting name.
    ///
    /// This can either be the span to the declared symbol or the previous
    /// using that uses the given name.
    pub conflicting_span: Span,
}

impl Report<&Table<Suboptimal>> for ConflictingUsing {
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        let module_qualified_name = table
            .get_qualified_name(self.module_id.into())
            .ok_or(ReportError)?;

        Ok(Diagnostic {
            span: self.using_span.clone(),
            message: format!(
                "the using `{name}` conflicts with the existing name in the \
                 module `{module_qualified_name}`",
                name = self.name,
            ),
            severity: Severity::Error,
            help_message: None,
            related: vec![Related {
                span: self.conflicting_span.clone(),
                message: "the conflicting name is declared here".to_string(),
            }],
        })
    }
}

/// `this` keyword is used outside the allowed scope.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ThisNotFound {
    /// The span where the `this` keyword was found.
    pub span: Span,
}

impl Report<&Table<Suboptimal>> for ThisNotFound {
    type Error = ReportError;

    fn report(&self, _: &Table<Suboptimal>) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.span.clone(),
            message: "`this` keyword cannot be used here".to_string(),
            severity: Severity::Error,
            help_message: Some(
                "`this` keyword can only be used in `trait` and `implements` \
                 to refer to that particular symbol"
                    .to_string(),
            ),
            related: Vec::new(),
        })
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

impl<M: Model> Report<&Table<Suboptimal>> for ExtraneousTraitMemberPredicate<M>
where
    Predicate<M>: Display<Suboptimal>,
{
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        let trait_implementation_symbol = table
            .get_global(self.trait_implementation_member_id.into())
            .ok_or(ReportError)?;

        let trait_implementation_symbol_span =
            trait_implementation_symbol.span().ok_or(ReportError)?;

        let qualified_name = table
            .get_qualified_name(self.trait_implementation_member_id.into())
            .ok_or(ReportError)?;

        Ok(Diagnostic {
            span: trait_implementation_symbol_span.clone(),
            message: format!(
                "the trait implementation member `{}` contains an extraneous \
                 predicate `{}` -- a predicate that is not defined in the \
                 trait member",
                qualified_name,
                DisplayObject { display: &self.predicate, table }
            ),
            severity: Severity::Error,
            help_message: None,
            related: self
                .predicate_span
                .as_ref()
                .map(|predicate_span| Related {
                    span: predicate_span.clone(),
                    message: "extraneous predicate declared here".to_string(),
                })
                .into_iter()
                .collect(),
        })
    }
}

/// Th numeric literal is too large.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TooLargetNumericLiteral {
    /// The span of the numeric literal.
    pub span: Span,
}

impl Report<&Table<Suboptimal>> for TooLargetNumericLiteral {
    type Error = ReportError;

    fn report(&self, _: &Table<Suboptimal>) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.span.clone(),
            message: "the numeric literal is too large".to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
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
    #[display(fmt = "enum")]
    Enum,
    #[display(fmt = "integer")]
    Integer,
    #[display(fmt = "boolean")]
    Boolean,
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

impl<M: Model> Report<&Table<Suboptimal>> for MismatchedPatternBindingType<M>
where
    Type<M>: Display<Suboptimal>,
{
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.pattern_span.clone(),
            message: format!(
                "the pattern expects a {} type but found {} ",
                self.expected_bindnig_type,
                DisplayObject { display: &self.found_type, table }
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// Indexing past the unpacked element in tuple is not allowed.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CannotIndexPastUnpackedTuple<M: Model> {
    /// The span of the index.
    pub index_span: Span,

    /// The type of the tuple.
    pub tuple_type: r#type::Tuple<M>,
}

impl<M: Model> Report<&Table<Suboptimal>> for CannotIndexPastUnpackedTuple<M>
where
    r#type::Tuple<M>: Display<Suboptimal>,
{
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        let unpacked_index = self
            .tuple_type
            .elements
            .iter()
            .position(|x| x.is_unpacked)
            .ok_or(ReportError)?;

        Ok(Diagnostic {
            span: self.index_span.clone(),
            message: "indexing past the unpacked element in tuple is not \
                      allowed"
                .to_string(),

            severity: Severity::Error,
            help_message: Some(format!(
                "the tuple type is `{}` having {} element(s) and the unpacked \
                 element is at index {}",
                DisplayObject { display: &self.tuple_type, table },
                self.tuple_type.elements.len(),
                unpacked_index,
            )),
            related: Vec::new(),
        })
    }
}

/// The tuple index is too large.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TooLargeTupleIndex {
    /// The span of the tuple index.
    pub access_span: Span,
}

impl Report<&Table<Suboptimal>> for TooLargeTupleIndex {
    type Error = ReportError;

    fn report(&self, _: &Table<Suboptimal>) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.access_span.clone(),
            message: "the tuple index is too large".to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// The tuple index is out of bounds.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TupleIndexOutOfBOunds<M: Model> {
    /// The span of the tuple index.
    pub access_span: Span,

    /// The index of the tuple.
    pub tuple_type: r#type::Tuple<M>,
}

impl<M: Model> Report<&Table<Suboptimal>> for TupleIndexOutOfBOunds<M>
where
    r#type::Tuple<M>: Display<Suboptimal>,
{
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        let count = self.tuple_type.elements.len();
        Ok(Diagnostic {
            span: self.access_span.clone(),
            message: "the tuple index is out of bounds".to_string(),
            severity: Severity::Error,
            help_message: Some(format!(
                "the tuple type is `{}` having {} element(s)",
                DisplayObject { display: &self.tuple_type, table },
                count,
            )),
            related: Vec::new(),
        })
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

impl Report<&Table<Suboptimal>> for FieldNotFound {
    type Error = ReportError;

    fn report(&self, _: &Table<Suboptimal>) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.identifier_span.clone(),
            message: "the field is not found in the struct".to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
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

impl Report<&Table<Suboptimal>> for AlreadyBoundFieldPattern {
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        let struct_sym = table.get(self.struct_id).ok_or(ReportError)?;
        let field_sym =
            struct_sym.fields().get(self.field_id).ok_or(ReportError)?;

        Ok(Diagnostic {
            span: self.pattern_span.clone(),
            message: format!(
                "the field `{}` is already bound in the pattern",
                field_sym.name
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
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

impl Report<&Table<Suboptimal>> for MismatchedTuplePatternLength {
    type Error = ReportError;

    fn report(&self, _: &Table<Suboptimal>) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.pattern_span.clone(),
            message: format!(
                "the pattern contains mismatched element length: expected {} \
                 element(s) but found {}",
                self.type_element_count, self.pattern_element_count
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// Can't bind a tuple pattern to a reference bound tuple type with unpacked
/// element.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FoundPackTuplePatternInReferenceBoundTupleType {
    /// The span of the pattern.
    pub pattern_span: Span,
}

impl Report<&Table<Suboptimal>>
    for FoundPackTuplePatternInReferenceBoundTupleType
{
    type Error = ReportError;

    fn report(&self, _: &Table<Suboptimal>) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.pattern_span.clone(),
            message: "can't bind a tuple pattern to a reference bound tuple \
                      type with pack element"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// The unpack operator can only be used once in a tuple expression.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MoreThanOneUnpackedInTupleExpression<M: Model> {
    /// The span of the tuple expression.
    pub span: Span,

    /// The type of the tuple expression.
    pub r#type: Type<M>,
}

impl<M: Model> Report<&Table<Suboptimal>>
    for MoreThanOneUnpackedInTupleExpression<M>
where
    Type<M>: Display<Suboptimal>,
{
    type Error = ReportError;

    fn report(&self, _: &Table<Suboptimal>) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.span.clone(),
            message: "the unpack operator can only be used once in a tuple \
                      expression"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// The numeric suffix is unknown.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InvalidNumericSuffix {
    /// The span of the numeric suffix.
    pub suffix_span: Span,
}

impl Report<&Table<Suboptimal>> for InvalidNumericSuffix {
    type Error = ReportError;

    fn report(&self, _: &Table<Suboptimal>) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.suffix_span.clone(),
            message: format!(
                "the numeric suffix `{}` is unknown",
                self.suffix_span.str()
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
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

/// The given l-value cannot be referenced as the given one.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MismatchedQualifierForReferenceOf {
    /// The span of the reference of expression
    pub reference_of_span: Span,

    /// The qualifier of the l-value.
    pub found_qualifier: Qualifier,

    /// The requested qualifier for the reference of expression.
    pub expected_qualifier: Qualifier,

    /// Whether or not the l-value is behind an another reference.
    pub is_behind_reference: bool,
}

impl Report<&Table<Suboptimal>> for MismatchedQualifierForReferenceOf {
    type Error = ReportError;

    fn report(&self, _: &Table<Suboptimal>) -> Result<Diagnostic, Self::Error> {
        let message = if self.is_behind_reference {
            format!(
                "the l-value is behind a reference with qualifier `{}` but \
                 the reference of expression expects a qualifier `{}`",
                self.found_qualifier, self.expected_qualifier
            )
        } else {
            format!(
                "the l-value has qualifier `{}` but the reference of \
                 expression expects a qualifier `{}`",
                self.found_qualifier, self.expected_qualifier
            )
        };

        Ok(Diagnostic {
            span: self.reference_of_span.clone(),
            message,
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

impl Report<&Table<Suboptimal>> for AlreadyBoundName {
    type Error = ReportError;

    fn report(&self, _: &Table<Suboptimal>) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.new_binding_span.clone(),
            message: format!(
                "the name `{}` has already been bound in the scope",
                self.already_bound_identifier_span.str()
            ),
            severity: Severity::Error,
            help_message: None,
            related: vec![Related {
                span: self.already_bound_identifier_span.clone(),
                message: "the name is already bound here".to_string(),
            }],
        })
    }
}

/// Thee numeric expression has a suffix of an integral type but the expression
/// has decimal point.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FloatingPointLiteralHasIntegralSuffix {
    /// The span of the numeric literal.
    pub numeric_literal_span: Span,
}

impl Report<&Table<Suboptimal>> for FloatingPointLiteralHasIntegralSuffix {
    type Error = ReportError;

    fn report(&self, _: &Table<Suboptimal>) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.numeric_literal_span.clone(),
            message: "the numeric expression has a suffix of an integral type \
                      but the expression has decimal point"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// Expected an l-value but found an r-value.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExpectLValue {
    /// The span of the r-value.
    pub expression_span: Span,
}

impl Report<&Table<Suboptimal>> for ExpectLValue {
    type Error = ReportError;

    fn report(&self, _: &Table<Suboptimal>) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.expression_span.clone(),
            message: "expected an l-value".to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// The variant doesn't have an associated value but pattern contains an
/// associated value.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UnexpectedAssociatedPattern {
    /// The variant ID that the pattern matches.
    pub variant_id: ID<Variant>,

    /// The span of the pattern with the associated pattern.
    pub associated_pattern_span: Span,
}

impl Report<&Table<Suboptimal>> for UnexpectedAssociatedPattern {
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        let variant_name = table
            .get_qualified_name(self.variant_id.into())
            .ok_or(ReportError)?;

        Ok(Diagnostic {
            span: self.associated_pattern_span.clone(),
            message: format!(
                "the variant `{variant_name}` doesn't have an associated \
                 value but the matched pattern contains one",
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// The value has been moved inside the loop, which could be used in the
/// subsequent iteration.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MoveInLoop {
    /// The span of the moved value.
    pub moved_value_span: Span,
}

impl Report<&Table<Suboptimal>> for MoveInLoop {
    type Error = ReportError;

    fn report(&self, _: &Table<Suboptimal>) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.moved_value_span.clone(),
            message: "the value has been moved inside the loop, which could \
                      be used in the subsequent iteration"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// The value behind the mutable reference has been moved out and needs to be
/// restored.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MovedOutValueFromMutableReference {
    /// The span of the moved out value.
    pub moved_out_value_span: Span,

    /// The span of the mutable reference.
    pub reassignment_span: Option<Span>,
}

impl Report<&Table<Suboptimal>> for MovedOutValueFromMutableReference {
    type Error = ReportError;

    fn report(&self, _: &Table<Suboptimal>) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.moved_out_value_span.clone(),
            message: "the value behind the mutable reference has been moved \
                      out and needs to be restored"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: self
                .reassignment_span
                .as_ref()
                .map(|reassignment_span| Related {
                    span: reassignment_span.clone(),
                    message: "this assignment makes the value behind mutable \
                              reference inaccessible from now on..."
                        .to_string(),
                })
                .into_iter()
                .collect(),
        })
    }
}

/// Adt implementation function cannot be used as a method.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AdtImplementationFunctionCannotBeUsedAsMethod {
    /// The ID of the ADT implementation function.
    pub adt_implementation_function_id: ID<AdtImplementationFunction>,

    /// The span of the function call.
    pub span: Span,
}

impl Report<&Table<Suboptimal>>
    for AdtImplementationFunctionCannotBeUsedAsMethod
{
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        let adt_implementation_function_symbol = table
            .get_qualified_name(self.adt_implementation_function_id.into())
            .ok_or(ReportError)?;

        Ok(Diagnostic {
            span: self.span.clone(),
            message: format!(
                "the ADT implementation function \
                 `{adt_implementation_function_symbol}` cannot be used as a \
                 method",
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// The field access is not allowed to have generic arguments.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UnexpectedGenericArgumentsInField {
    /// The span of the field access.
    pub field_access_span: Span,
}

impl Report<&Table<Suboptimal>> for UnexpectedGenericArgumentsInField {
    type Error = ReportError;

    fn report(&self, _: &Table<Suboptimal>) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.field_access_span.clone(),
            message: "the field access is not allowed to have generic \
                      arguments"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// The variant expects an associated pattern but none was provided.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExpectAssociatedPattern {
    /// The ID of the variant where the associated pattern is expected.
    pub variant_id: ID<Variant>,

    /// The span of the variant pattern where the associated pattern is
    /// expected.
    pub pattern_span: Span,
}

impl Report<&Table<Suboptimal>> for ExpectAssociatedPattern {
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        let variant_sym = table.get(self.variant_id).ok_or(ReportError)?;

        Ok(Diagnostic {
            span: self.pattern_span.clone(),
            message: format!(
                "the enum variant `{}` expects an associated pattern",
                variant_sym.name()
            ),
            severity: Severity::Error,
            help_message: Some(
                "if you want to discard the associated value, use the `case \
                 Name(..)` pattern"
                    .to_string(),
            ),
            related: Vec::new(),
        })
    }
}

/// The enum variant expects an associated value but none was provided.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExpectAssociatedValue {
    /// The ID of the variant where the associated value is expected.
    pub variant_id: ID<Variant>,

    /// The span of the variant.
    pub span: Span,
}

impl Report<&Table<Suboptimal>> for ExpectAssociatedValue {
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        let variant_sym = table.get(self.variant_id).ok_or(ReportError)?;

        Ok(Diagnostic {
            span: self.span.clone(),
            message: format!(
                "the enum variant `{}` expects an associated value",
                variant_sym.name()
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// The enum variant does not expect an associated value but was provided.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VariantDoesNotExpectAssociatedValue {
    /// The ID of the variant where the associated value is not expected.
    pub variant_id: ID<Variant>,

    /// The span of the variant.
    pub span: Span,
}

impl Report<&Table<Suboptimal>> for VariantDoesNotExpectAssociatedValue {
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        let variant_sym = table.get(self.variant_id).ok_or(ReportError)?;

        Ok(Diagnostic {
            span: self.span.clone(),
            message: format!(
                "the enum variant `{}` does not expect an associated value",
                variant_sym.name()
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// The expected argument count does not match the found argument count.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MismatchedArgumentCount {
    /// The ID of the symbol that was supplied with the wrong number of
    /// arguments.
    ///
    /// This can be an enum variant, function, trait function, trait
    /// implementation function, and ADT implementation function.
    pub called_id: GlobalID,

    /// The expected argument count.
    pub expected_count: usize,

    /// The found argument count.
    pub found_count: usize,

    /// The span of the function call.
    pub span: Span,
}

impl Report<&Table<Suboptimal>> for MismatchedArgumentCount {
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        let called_symbol =
            table.get_qualified_name(self.called_id).ok_or(ReportError)?;

        Ok(Diagnostic {
            span: self.span.clone(),
            message: format!(
                "the symbol `{}` expects {} argument(s) but found {}",
                called_symbol, self.expected_count, self.found_count
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// The given symbol cannot be called.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SymbolIsNotCallable {
    /// The ID of the symbol that cannot be called.
    pub called_id: GlobalID,

    /// The span of the call.
    pub span: Span,
}

impl Report<&Table<Suboptimal>> for SymbolIsNotCallable {
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        let called_symbol =
            table.get_qualified_name(self.called_id).ok_or(ReportError)?;

        Ok(Diagnostic {
            span: self.span.clone(),
            message: format!("the symbol `{called_symbol}` cannot be called"),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// Found a cyclic inference in the type checking.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CyclicInference<M: Model> {
    /// The type that is involved in the cyclic inference.
    pub first: Type<M>,

    /// The type that is involved in the cyclic inference.
    pub second: Type<M>,

    /// The sapn where the type check occurred.
    pub span: Span,
}

impl<M: Model> Report<&Table<Suboptimal>> for CyclicInference<M>
where
    Type<M>: Display<Suboptimal>,
{
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.span.clone(),
            message: format!(
                "found a cyclic inference: `{}` and `{}`",
                DisplayObject { display: &self.first, table },
                DisplayObject { display: &self.second, table }
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// Mismatched type error.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MismatchedType<M: Model> {
    /// The expected type.
    pub expected_type: Type<M>,

    /// The found type.
    pub found_type: Type<M>,

    /// The sapn where the type check occurred.
    pub span: Span,
}

impl<M: Model> Report<&Table<Suboptimal>> for MismatchedType<M>
where
    Type<M>: Display<Suboptimal>,
{
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.span.clone(),
            message: format!(
                "mismatched type: expected `{}` but found `{}`",
                DisplayObject { display: &self.expected_type, table },
                DisplayObject { display: &self.found_type, table }
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// The struct type was expected but the found type is different.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExpectStruct {
    /// The span of the qualified identifier.
    pub span: Span,
}

impl Report<&Table<Suboptimal>> for ExpectStruct {
    type Error = ReportError;

    fn report(&self, _: &Table<Suboptimal>) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.span.clone(),
            message: "struct type expected".to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// The struct expression contains uninitialized fields.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UninitializedFields {
    /// The ID of the struct where the fields are uninitialized.
    pub struct_id: ID<Struct>,

    /// The set of uninitialized fields.
    pub uninitialized_fields: HashSet<ID<Field>>,

    /// The span of the struct expression.
    pub struct_expression_span: Span,
}

impl Report<&Table<Suboptimal>> for UninitializedFields {
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        let struct_sym = table.get(self.struct_id).ok_or(ReportError)?;

        Ok(Diagnostic {
            span: self.struct_expression_span.clone(),
            message: "the struct expression contains uninitialized fields"
                .to_string(),
            severity: Severity::Error,
            help_message: Some(format!(
                "the following fields are uninitialized: {}",
                self.uninitialized_fields
                    .iter()
                    .map(|&field_id| {
                        struct_sym
                            .fields()
                            .get(field_id)
                            .map(|x| x.name.clone())
                            .ok_or(ReportError)
                    })
                    .collect::<Result<Vec<_>, ReportError>>()?
                    .join(", ")
            )),
            related: Vec::new(),
        })
    }
}

/// The field is initialized more than once.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DuplicatedFieldInitialization {
    /// The ID of the field that is initialized more than once.
    pub field_id: ID<Field>,

    /// The ID of the struct where the field is initialized.
    pub struct_id: ID<Struct>,

    /// The span of the first initialization.
    pub prior_initialization_span: Span,

    /// The span of the duplicate initialization.
    pub duplicate_initialization_span: Span,
}

impl Report<&Table<Suboptimal>> for DuplicatedFieldInitialization {
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        let struct_sym = table.get(self.struct_id).ok_or(ReportError)?;
        let field_sym =
            struct_sym.fields().get(self.field_id).ok_or(ReportError)?;

        Ok(Diagnostic {
            span: self.duplicate_initialization_span.clone(),
            message: format!(
                "the field `{}` is initialized more than once",
                field_sym.name
            ),
            severity: Severity::Error,
            help_message: None,
            related: vec![Related {
                span: self.prior_initialization_span.clone(),
                message: "the field is first initialized here".to_string(),
            }],
        })
    }
}

/// Expected an arrary type to index into elements but found a different type.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExpectArray<M: Model> {
    /// The span of the expression where the elements are indexed.
    pub span: Span,

    /// The type that is not an array type.
    pub r#type: Type<M>,
}

impl<M: Model> Report<&Table<Suboptimal>> for ExpectArray<M>
where
    Type<M>: Display<Suboptimal>,
{
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.span.clone(),
            message: format!(
                "expected an array type to index into elements but found `{}`",
                DisplayObject { display: &self.r#type, table }
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// Expected a tuple type to access the field but found a different type.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TupleExpected<M: Model> {
    /// The span of the expression where the field is accessed.
    pub span: Span,

    /// The type that is not a tuple type.
    pub r#type: Type<M>,
}

impl<M: Model> Report<&Table<Suboptimal>> for TupleExpected<M>
where
    Type<M>: Display<Suboptimal>,
{
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.span.clone(),
            message: format!(
                "expected a tuple type to access the field but found `{}`",
                DisplayObject { display: &self.r#type, table }
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// Expected a struct type to access the field but found a different type.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExpectStructType<M: Model> {
    /// The span of the expression where the field is accessed.
    pub span: Span,

    /// The type that is not a struct type.
    pub r#type: Type<M>,
}

impl<M: Model> Report<&Table<Suboptimal>> for ExpectStructType<M>
where
    Type<M>: Display<Suboptimal>,
{
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.span.clone(),
            message: format!(
                "expected a struct type to access the field but found `{}`",
                DisplayObject { display: &self.r#type, table }
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// The expression of the given type cannot be dereferenced.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CannotDereference<M: Model> {
    /// The type of the expression that cannot be dereferenced.
    pub found_type: Type<M>,

    /// The span of the expression with dereference operator.
    pub span: Span,
}

impl<M: Model> Report<&Table<Suboptimal>> for CannotDereference<M>
where
    Type<M>: Display<Suboptimal>,
{
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.span.clone(),
            message: format!(
                "the expression of type `{}` cannot be dereferenced",
                DisplayObject { display: &self.found_type, table }
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// The generic arguments are not compatible with the generic arguments defined
/// in the implementation.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MismatchedImplementationArguments<M: Model> {
    /// The ID of the ADT implementation where the generic arguments are
    /// mismatched.
    pub adt_implementation_id: ID<AdtImplementation>,

    /// The generic arguments found in the implementation.
    pub found_generic_arguments: GenericArguments<M>,

    /// The span of the instantiation that causes the mismatch.
    pub instantiation_span: Span,
}

impl<M: Model> Report<&Table<Suboptimal>>
    for MismatchedImplementationArguments<M>
where
    GenericArguments<M>: Display<Suboptimal>,
{
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        let adt_implementation_symbol =
            table.get(self.adt_implementation_id).ok_or(ReportError)?;

        Ok(Diagnostic {
            span: self.instantiation_span.clone(),
            message: "the generic arguments are not compatible with the \
                      generic arguments defined in the implementation"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: adt_implementation_symbol
                .span()
                .as_ref()
                .map(|span| Related {
                    span: span.clone(),
                    message: "the implementation is defined here".to_string(),
                })
                .into_iter()
                .collect(),
        })
    }
}

/// The implementation is not general enough to satisfy the required
/// predicate's forall lifetimes.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ImplementationIsNotGeneralEnough<M: Model> {
    /// The ID of the implementation where the predicate is not satisfied.
    pub resolvable_implementation_id: ResolvableImplementationID,

    /// The generic arguments required by the trait predicate.
    pub generic_arguments: GenericArguments<M>,

    /// The span where the trait predicate was declared.
    pub predicate_declaration_span: Option<Span>,

    /// The span of the instantiation that cuases the error
    pub instantiation_span: Span,
}

impl<M: Model> Report<&Table<Suboptimal>>
    for ImplementationIsNotGeneralEnough<M>
where
    GenericArguments<M>: table::Display<Suboptimal>,
{
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        let resolvable_implementation_symbol = table
            .get_global(self.resolvable_implementation_id.into())
            .ok_or(ReportError)?;

        Ok(Diagnostic {
            span: self.instantiation_span.clone(),
            message: format!(
                "the implementation is not general enough to satisfy the \
                 required forall lifetimes in the generic arguments: {}",
                DisplayObject { table, display: &self.generic_arguments }
            ),
            severity: Severity::Error,
            help_message: None,
            related: self
                .predicate_declaration_span
                .as_ref()
                .map(|span| Related {
                    span: span.clone(),
                    message: "the predicate is declared here".to_string(),
                })
                .into_iter()
                .chain(resolvable_implementation_symbol.span().map(|span| {
                    Related {
                        span: span.clone(),
                        message: "the implementation is defined here"
                            .to_string(),
                    }
                }))
                .collect(),
        })
    }
}

/// The trait type equality is declared in such a way that it can be expanded
/// infinitely.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RecursiveTraitTypeEquality<M: Model> {
    /// The trait type equality that is recursive.
    pub trait_type_equality:
        equality::Equality<r#type::TraitMember<M>, r#type::Type<M>>,

    /// The span where the recursive trait type equalities are declared.
    pub predicate_declaration_spans: Vec<Span>,
}

impl<M: Model> Report<&Table<Suboptimal>> for RecursiveTraitTypeEquality<M>
where
    equality::Equality<r#type::TraitMember<M>, r#type::Type<M>>:
        table::Display<Suboptimal>,
{
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        let mut span_iter = self.predicate_declaration_spans.iter();
        let first_span = span_iter.next().ok_or(ReportError)?;

        Ok(Diagnostic {
            span: first_span.clone(),
            message: format!(
                "the predicate `{}` is recursive, it can be expanded \
                 infinitely",
                DisplayObject { table, display: &self.trait_type_equality }
            ),
            severity: Severity::Error,
            help_message: None,
            related: span_iter
                .map(|span| Related {
                    span: span.clone(),
                    message: "the recursive trait type equality is declared \
                              here"
                        .to_string(),
                })
                .collect(),
        })
    }
}

/// The value is used after it has been moved.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UseAfterMove {
    /// The span where the value is used.
    pub use_span: Span,

    /// The span where the value is moved.
    pub move_span: Span,
}

impl Report<&Table<Suboptimal>> for UseAfterMove {
    type Error = ReportError;

    fn report(&self, _: &Table<Suboptimal>) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.use_span.clone(),
            message: "the value is used after it has been moved".to_string(),
            severity: Severity::Error,
            help_message: None,
            related: vec![Related {
                span: self.move_span.clone(),
                message: "the value is moved here".to_string(),
            }],
        })
    }
}

/// The value is used before it has been initialized.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UseBeforeInitialization {
    /// The span where the value is used.
    pub use_span: Span,
}

impl Report<&Table<Suboptimal>> for UseBeforeInitialization {
    type Error = ReportError;

    fn report(&self, _: &Table<Suboptimal>) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.use_span.clone(),
            message: "the value is used before it has been initialized"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// The predicates are ambiguous to each other.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AmbiguousPredicates<M: Model> {
    /// The list of predicates that are ambiguous to each other.
    pub predicates: Vec<Predicate<M>>,

    /// The span where the ambiguous predicates are declared.
    pub predicate_declaration_spans: Vec<Span>,
}

impl<M: Model> Report<&Table<Suboptimal>> for AmbiguousPredicates<M>
where
    Predicate<M>: table::Display<Suboptimal>,
{
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        let mut span_iter = self.predicate_declaration_spans.iter();
        let first_span = span_iter.next().ok_or(ReportError)?;

        Ok(Diagnostic {
            span: first_span.clone(),
            message: format!(
                "the predicates are ambiguous to each other: {}",
                self.predicates
                    .iter()
                    .map(|predicate| {
                        DisplayObject { table, display: predicate }.to_string()
                    })
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            severity: Severity::Error,
            help_message: None,
            related: span_iter
                .map(|span| Related {
                    span: span.clone(),
                    message: "the ambiguous predicate is declared here"
                        .to_string(),
                })
                .collect(),
        })
    }
}

/// The ADT implementation is not general enough to satisfy the required forall
/// lifetimes in the generic arguments
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AdtImplementationIsNotGeneralEnough<M: Model> {
    /// The ADT implementation ID where the generic arguments are not general
    /// enough.
    pub adt_implementation_id: ID<AdtImplementation>,

    /// The generic arguments supplied to the ADT.
    pub generic_arguments: GenericArguments<M>,

    /// The span location of where the ADT is instantiated.
    pub instantiation_span: Span,
}

impl<M: Model> Report<&Table<Suboptimal>>
    for AdtImplementationIsNotGeneralEnough<M>
where
    GenericArguments<M>: table::Display<Suboptimal>,
{
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        let adt_implementation_symbol =
            table.get(self.adt_implementation_id).ok_or(ReportError)?;

        let implemented_qualified_name = table
            .get_qualified_name(
                adt_implementation_symbol.implemented_id().into(),
            )
            .ok_or(ReportError)?;

        Ok(Diagnostic {
            span: self.instantiation_span.clone(),
            message: format!(
                "the implementation is not general enough for {}{}",
                implemented_qualified_name,
                DisplayObject { table, display: &self.generic_arguments }
            ),
            severity: Severity::Error,
            help_message: None,
            related: adt_implementation_symbol
                .span()
                .as_ref()
                .map(|span| Related {
                    span: span.clone(),
                    message: "the implementation is defined here".to_string(),
                })
                .into_iter()
                .collect(),
        })
    }
}

/// Not all flow paths in this block expression express a value.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NotAllFlowPathsExpressValue {
    /// The span of the block expression.
    pub span: Span,
}

impl Report<&Table<Suboptimal>> for NotAllFlowPathsExpressValue {
    type Error = ReportError;

    fn report(&self, _: &Table<Suboptimal>) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.span.clone(),
            message: "not all flow paths in this block expression express a \
                      value"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// The implementation is final but it is overriden by another
/// implementation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FinalImplementationCannotBeOverriden {
    /// The ID of the final implementation that is overriden.
    pub final_implementation_id: ResolvableImplementationID,

    /// The ID of the implementation that overrides the final implementation.
    pub overriden_implementation_id: ResolvableImplementationID,
}

impl Report<&Table<Suboptimal>> for FinalImplementationCannotBeOverriden {
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        let final_impl_symbol = table
            .get_implementation(self.final_implementation_id.into())
            .ok_or(ReportError)?;

        let overriden_impl_symbol = table
            .get_implementation(self.overriden_implementation_id.into())
            .ok_or(ReportError)?;

        let final_qual_name = table
            .get_qualified_name(self.final_implementation_id.into())
            .ok_or(ReportError)?;

        let impl_qual_name = table
            .get_qualified_name(self.overriden_implementation_id.into())
            .ok_or(ReportError)?;

        Ok(Diagnostic {
            span: overriden_impl_symbol
                .span()
                .or_else(|| final_impl_symbol.span())
                .ok_or(ReportError)?
                .clone(),
            message: format!(
                "the implementation `{}{}` overrides the implementation \
                 `{}{}`, which is final",
                impl_qual_name,
                DisplayObject {
                    table,
                    display: overriden_impl_symbol.arguments()
                },
                final_qual_name,
                DisplayObject { table, display: final_impl_symbol.arguments() },
            ),
            severity: Severity::Error,
            help_message: None,
            related: final_impl_symbol
                .span()
                .map(|span| Related {
                    span: span.clone(),
                    message: "the final implementation is defined here"
                        .to_string(),
                })
                .into_iter()
                .chain(overriden_impl_symbol.span().map(|span| {
                    Related {
                        span: span.clone(),
                        message: "the overriden implementation is defined here"
                            .to_string(),
                    }
                }))
                .collect::<Vec<_>>(),
        })
    }
}

/// Overflow calculating the requirement for instantiation of a particular
/// symbol.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct OverflowCalculatingRequirementForInstantiation {
    /// The span where the instantiation occurs.
    pub instantiation_span: Span,
}

impl Report<&Table<Suboptimal>>
    for OverflowCalculatingRequirementForInstantiation
{
    type Error = ReportError;

    fn report(&self, _: &Table<Suboptimal>) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.instantiation_span.clone(),
            message: "overflow calculating the requirement for instantiation"
                .to_string(),
            severity: Severity::Error,
            help_message: Some(
                "try reducing the complexity of the given symbol or this \
                 instantiation"
                    .to_string(),
            ),
            related: Vec::new(),
        })
    }
}

/// The constant argument has a mismatched type.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConstantArgumentTypeMismatched<M: Model> {
    /// The span of the constant argument.
    pub span: Span,

    /// The expected type of the constant argument.
    pub expected_type: Type<M>,

    /// The constant argument that has a mismatched type.
    pub constant_argument: constant::Constant<M>,
}

impl<M: Model> Report<&Table<Suboptimal>> for ConstantArgumentTypeMismatched<M>
where
    Type<M>: Display<Suboptimal>,
    constant::Constant<M>: Display<Suboptimal>,
{
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.span.clone(),
            message: format!(
                "the constant argument `{}` has a mismatched type: expected \
                 `{}`",
                DisplayObject { display: &self.constant_argument, table },
                DisplayObject { display: &self.expected_type, table }
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// The predicate declared is definite (its satisfiability is already known).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DefinitePremisePredicate<M: Model> {
    /// The span of the definite premise.
    pub span: Span,

    /// The definite premise.
    pub predicate: Predicate<M>,
}

impl<M: Model> Report<&Table<Suboptimal>> for DefinitePremisePredicate<M>
where
    Predicate<M>: Display<Suboptimal>,
{
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.span.clone(),
            message: format!(
                "the predicate `{}` is definite, the satisfiability of the \
                 predicate is already known when the it's declared",
                DisplayObject { display: &self.predicate, table }
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// Cannot dereference a particular reference as the given qualifier as it's
/// behind a reference which has more restrictive qualifier.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BehindReferenceQualifierMismathced {
    /// The qualifier of the reference behind the expression.
    pub behind_reference_qualifier: Qualifier,

    /// The expected qualifier of the reference.
    pub expected_qualifier: Qualifier,

    /// The span to be dereferenced.
    pub expression_span: Span,
}

impl Report<&Table<Suboptimal>> for BehindReferenceQualifierMismathced {
    type Error = ReportError;

    fn report(&self, _: &Table<Suboptimal>) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.expression_span.clone(),
            message: format!(
                "cannot dereference the expression `{}` as `{}` since it's \
                 behind a reference with `{}` qualifier",
                self.expression_span.str(),
                self.expected_qualifier,
                self.behind_reference_qualifier
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// Attempts to assign to a non-mutable l-value.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AssignToNonMutable {
    /// The span of the assignment.
    pub span: Span,

    /// The qualifier of the l-value.
    pub qualifier: Qualifier,
}

impl Report<&Table<Suboptimal>> for AssignToNonMutable {
    type Error = ReportError;

    fn report(&self, _: &Table<Suboptimal>) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.span.clone(),
            message: format!(
                "cannot assign to `{}` since it's {}",
                self.span.str(),
                self.qualifier
            ),
            severity: Severity::Error,
            help_message: Some(
                "the value must be `mutable` to assign".to_string(),
            ),
            related: Vec::new(),
        })
    }
}

/// Can't perform bitwise operation on the given type.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InvalidBitwiseOperation<M: Model> {
    /// The type where the bitwise operation is not allowed.
    pub found_type: Type<M>,

    /// The span of the bitwise operation.
    pub span: Span,
}

impl<M: Model> Report<&Table<Suboptimal>> for InvalidBitwiseOperation<M>
where
    Type<M>: Display<Suboptimal>,
{
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.span.clone(),
            message: format!(
                "can't perform bitwise operation on expression with type `{}`",
                DisplayObject { display: &self.found_type, table }
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// Can't perform relational operation on the given type.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InvalidRelationalOperation<M: Model> {
    /// The type where the relational operation is not allowed.
    pub found_type: Type<M>,

    /// The span of the relational operation.
    pub span: Span,
}

impl<M: Model> Report<&Table<Suboptimal>> for InvalidRelationalOperation<M>
where
    Type<M>: Display<Suboptimal>,
{
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.span.clone(),
            message: format!(
                "can't perform relational operation on expression with type \
                 `{}`",
                DisplayObject { display: &self.found_type, table }
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// An enumeration of either a `break` or `continue` control flow.
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
pub enum LoopControlFlow {
    #[display(fmt = "break")]
    Break,

    #[display(fmt = "continue")]
    Continue,
}

/// The loop control flow expression can't be used outside of a loop.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LoopControlFlowOutsideLoop {
    /// The span of the break expression.
    pub span: Span,

    /// The kind of control flow.
    pub control_flow: LoopControlFlow,
}

impl Report<&Table<Suboptimal>> for LoopControlFlowOutsideLoop {
    type Error = ReportError;

    fn report(&self, _: &Table<Suboptimal>) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.span.clone(),
            message: format!(
                "`{}` expression can't be used outside of a loop",
                self.control_flow
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// The `epxress` expression can't be used outside of a block expression.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExpressOutsideBlock {
    /// The span of the expression.
    pub span: Span,
}

impl Report<&Table<Suboptimal>> for ExpressOutsideBlock {
    type Error = ReportError;

    fn report(&self, _: &Table<Suboptimal>) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.span.clone(),
            message: "`express` expression can't be used outside of a block \
                      expression"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// The loop with the given label name was not found.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LoopWithGivenLabelNameNotFound {
    /// The span of the label identifier.
    pub span: Span,
}

impl Report<&Table<Suboptimal>> for LoopWithGivenLabelNameNotFound {
    type Error = ReportError;

    fn report(&self, _: &Table<Suboptimal>) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.span.clone(),
            message: format!(
                "loop with label named `{}` was not found",
                self.span.str()
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// The block with the given label name was not found.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BlockWithGivenLableNameNotFound {
    /// The span of the label identifier.
    pub span: Span,
}

impl Report<&Table<Suboptimal>> for BlockWithGivenLableNameNotFound {
    type Error = ReportError;

    fn report(&self, _: &Table<Suboptimal>) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.span.clone(),
            message: format!(
                "block with label named `{}` was not found",
                self.span.str()
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// The `return` expression is not allowed in this context.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ReturnIsNotAllowed {
    /// The span of the return expression.
    pub span: Span,
}

impl Report<&Table<Suboptimal>> for ReturnIsNotAllowed {
    type Error = ReportError;

    fn report(&self, _: &Table<Suboptimal>) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.span.clone(),
            message: "`return` expression is not allowed in this context"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// The given expression is not callable.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExpressionIsNotCallable {
    /// The span of the expression without the call operator.
    pub span: Span,
}

impl Report<&Table<Suboptimal>> for ExpressionIsNotCallable {
    type Error = ReportError;

    fn report(&self, _: &Table<Suboptimal>) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.span.clone(),
            message: format!(
                "the expression `{}` is not callable",
                self.span.str()
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// The symbol cannot be used as an expression.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SymbolCannotBeUsedAsAnExpression {
    /// The span of the symbol reference.
    pub span: Span,

    /// The ID of the symbol that cannot be used as an expression.
    pub symbol: GlobalID,
}

impl Report<&Table<Suboptimal>> for SymbolCannotBeUsedAsAnExpression {
    type Error = ReportError;

    fn report(&self, _: &Table<Suboptimal>) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.span.clone(),
            message: format!(
                "the symbol `{}` cannot be used as an expression",
                self.span.str()
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// The given type cannot be used in cast expression.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InvalidCastType<M: Model> {
    /// The span of the type reference.
    pub span: Span,

    /// The type that cannot be used in cast expression.
    pub r#type: Type<M>,
}

impl<M: Model> Report<&Table<Suboptimal>> for InvalidCastType<M>
where
    Type<M>: Display<Suboptimal>,
{
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.span.clone(),
            message: format!(
                "cannot castt an expression to type type `{}`",
                DisplayObject { display: &self.r#type, table }
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// The reftuable pattern is found in a packed tuple.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ReftuablePatternFoundInPackedTuple {
    /// The span of the reftuable pattern.
    pub reftuable_pattern_span: Span,
}

impl Report<&Table<Suboptimal>> for ReftuablePatternFoundInPackedTuple {
    type Error = ReportError;

    fn report(&self, _: &Table<Suboptimal>) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.reftuable_pattern_span.clone(),
            message: "reftuable pattern found in a packed tuple".to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// The match arm is unreachable.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UnreachableMatchArm {
    /// The span of the match arm.
    pub match_arm_span: Span,
}

impl Report<&Table<Suboptimal>> for UnreachableMatchArm {
    type Error = ReportError;

    fn report(&self, _: &Table<Suboptimal>) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.match_arm_span.clone(),
            message: "unreachable match arm".to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// Couldn't further infer the type, explicit type annotation is required.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeAnnotationRequired {
    /// The span of the expression/declration where the type annotation is
    /// required.
    pub span: Span,

    /// The type that couldn't be further inferred.
    pub r#type: Type<ConstraintModel>,
}

impl Report<&Table<Suboptimal>> for TypeAnnotationRequired {
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.span.clone(),
            message: format!(
                "couldn't further infer the type `{}`, explicit type \
                 annotation is required",
                DisplayObject { display: &self.r#type, table }
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// The match expression is non-exhaustive.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NonExhaustiveMatch {
    /// The span of the match expression.
    pub match_expression_span: Span,
}

impl Report<&Table<Suboptimal>> for NonExhaustiveMatch {
    type Error = ReportError;

    fn report(&self, _: &Table<Suboptimal>) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.match_expression_span.clone(),
            message: "non-exhaustive match expression".to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// The calling convention is an `extern` block is invalid.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UnknownExternCallingConvention {
    /// The span of the extern calling convention.
    pub span: Span,
}

impl Report<&Table<Suboptimal>> for UnknownExternCallingConvention {
    type Error = ReportError;

    fn report(&self, _: &Table<Suboptimal>) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.span.clone(),
            message: format!(
                "unknown calling convention `{}` in `extern`",
                self.span.str()
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

impl<T: State> Display<T> for UnknownExternCallingConvention {
    fn fmt(&self, _: &Table<T>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", Message {
            severity: Severity::Error,
            display: format!(
                "unknown calling convention `{}` in `extern`",
                self.span.str()
            )
        })?;

        write!(f, "\n{}", SourceCodeDisplay {
            span: &self.span,
            help_display: Option::<i32>::None
        })?;

        Ok(())
    }
}

/// Not all flow paths in the function return a value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NotAllFlowPathsReturnAValue {
    /// The function which contains an error.
    pub callable_id: CallableID,
}

impl Report<&Table<Suboptimal>> for NotAllFlowPathsReturnAValue {
    type Error = ReportError;

    fn report(
        &self,
        table: &Table<Suboptimal>,
    ) -> Result<Diagnostic, Self::Error> {
        let callable_symbol =
            table.get_callable(self.callable_id).ok_or(ReportError)?;
        let span = callable_symbol.span().cloned().ok_or(ReportError)?;

        Ok(Diagnostic {
            span,
            message: "not all flow paths in the function return a value"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// Implemented by all semantic errors.
pub trait Error:
    for<'a> Report<&'a Table<Suboptimal>, Error = ReportError>
    + Debug
    + Any
    + Send
    + Sync
    + 'static
{
    #[allow(missing_docs)]
    fn as_any(&self) -> &dyn Any;

    #[allow(missing_docs)]
    fn as_any_mut(&mut self) -> &mut dyn Any;
}

impl<
        U: for<'a> Report<&'a Table<Suboptimal>, Error = ReportError>
            + Debug
            + Any
            + Send
            + Sync
            + 'static,
    > Error for U
{
    fn as_any(&self) -> &dyn Any { self }

    fn as_any_mut(&mut self) -> &mut dyn Any { self }
}
