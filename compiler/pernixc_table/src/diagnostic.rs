//! Contains the diagnostic related to the table.

use std::{any::Any, collections::HashSet, fmt::Debug};

use pernixc_diagnostic::{Diagnostic as DiagnosticReport, Related, Report};
use pernixc_log::Severity;
use pernixc_source_file::Span;

use super::{GlobalID, Representation, Table, TargetID};
use crate::component::{Accessibility, LocationSpan, Name, SymbolKind};

/// Implemented by all diagnostic objects.
pub trait Diagnostic:
    for<'a> Report<&'a Table> + Debug + Any + Send + Sync + 'static
{
    #[allow(missing_docs)]
    fn as_any(&self) -> &dyn Any;

    #[allow(missing_docs)]
    fn as_any_mut(&mut self) -> &mut dyn Any;
}

impl<U: for<'a> Report<&'a Table> + Debug + Any + Send + Sync + 'static>
    Diagnostic for U
{
    fn as_any(&self) -> &dyn Any { self }

    fn as_any_mut(&mut self) -> &mut dyn Any { self }
}

impl<U: for<'a> Report<&'a Table> + Debug + Any + Send + Sync + 'static> From<U>
    for Box<dyn Diagnostic>
{
    fn from(value: U) -> Self { Box::new(value) }
}

/// The item symbol with the same name already exists in the given scope.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ItemRedifinition {
    /// The ID of the existing symbol.
    pub existing_id: GlobalID,

    /// The ID of the new symbol.
    pub new_id: GlobalID,

    /// The scope in which the duplication occurred.
    pub in_id: GlobalID,
}

impl Report<&Table> for ItemRedifinition {
    fn report(&self, table: &Table) -> pernixc_diagnostic::Diagnostic {
        let existing_symbol_span = table.get::<LocationSpan>(self.existing_id);
        let new_symbol_span = table.get::<LocationSpan>(self.new_id);
        let existing_symbol_name = table.get::<Name>(self.existing_id);
        let in_name = table.get_qualified_name(self.in_id);

        DiagnosticReport {
            span: new_symbol_span.span.clone().unwrap(),
            message: format!(
                "the symbol `{}` is already defined in `{in_name}`",
                existing_symbol_name.as_str(),
            ),
            severity: Severity::Error,
            help_message: None,
            related: vec![Related {
                span: existing_symbol_span.span.clone().unwrap(),
                message: "previously defined here".to_string(),
            }],
        }
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

impl Representation {
    fn accessibility_description(
        &self,
        target_id: TargetID,
        accessibility: Accessibility,
    ) -> String {
        match accessibility {
            Accessibility::Public => "publicly accessible".to_owned(),
            Accessibility::Scoped(module_id) => {
                let module_qualified_name = self
                    .get_qualified_name(GlobalID::new(target_id, module_id));

                format!("accessible in `{module_qualified_name}`")
            }
        }
    }
}

impl Report<&Table> for SymbolIsMoreAccessibleThanParent {
    fn report(&self, table: &Table) -> pernixc_diagnostic::Diagnostic {
        let symbol_name = table.get::<Name>(self.symbol_id);
        let parent_qualified_name = table.get_qualified_name(self.parent_id);

        let symbol_accessibility = table.get_accessibility(self.symbol_id);
        let parent_accessibility = table.get_accessibility(self.parent_id);

        let symbol_span = table.get::<LocationSpan>(self.symbol_id);
        let parent_span = table.get::<LocationSpan>(self.parent_id);

        let symbol_accessibility_description = table.accessibility_description(
            self.symbol_id.target_id,
            symbol_accessibility,
        );

        let parent_accessibility_description = table.accessibility_description(
            self.parent_id.target_id,
            parent_accessibility,
        );

        DiagnosticReport {
            span: symbol_span.span.clone().unwrap(),
            message: format!(
                "the symbol `{}` in `{parent_qualified_name}` is more \
                 accessible than the parent symbol",
                symbol_name.as_str()
            ),
            severity: Severity::Error,
            help_message: Some(format!(
                "the symbol `{}` is {symbol_accessibility_description}",
                symbol_name.as_str()
            )),
            related: vec![Related {
                span: parent_span.span.clone().unwrap(),
                message: format!(
                    "the parent symbol is {parent_accessibility_description}",
                ),
            }],
        }
    }
}

/// The calling convention is an `extern` block is invalid.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UnknownExternCallingConvention {
    /// The span of the extern calling convention.
    pub span: Span,
}

impl Report<&Table> for UnknownExternCallingConvention {
    fn report(&self, _: &Table) -> DiagnosticReport {
        DiagnosticReport {
            span: self.span.clone(),
            message: format!(
                "unknown calling convention `{}` in `extern`",
                self.span.str()
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
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

impl Report<&Table> for ExpectModule {
    fn report(&self, table: &Table) -> DiagnosticReport {
        let found_symbol_qualified_name =
            table.get_qualified_name(self.found_id);
        let kind = table.get::<SymbolKind>(self.found_id);

        DiagnosticReport {
            span: self.module_path.clone(),
            message: format!(
                "expected a module in the module path, but found `{} {}`",
                kind.kind_str(),
                found_symbol_qualified_name
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
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
    pub module_id: GlobalID,

    /// The span of the conflicting name.
    ///
    /// This can either be the span to the declared symbol or the previous
    /// using that uses the given name.
    pub conflicting_span: Option<Span>,
}

impl Report<&Table> for ConflictingUsing {
    fn report(&self, table: &Table) -> DiagnosticReport {
        let module_qualified_name = table.get_qualified_name(self.module_id);

        DiagnosticReport {
            span: self.using_span.clone(),
            message: format!(
                "the using `{name}` conflicts with the existing name in the \
                 module `{module_qualified_name}`",
                name = self.name,
            ),
            severity: Severity::Error,
            help_message: None,
            related: self
                .conflicting_span
                .as_ref()
                .map(|span| {
                    vec![Related {
                        span: span.clone(),
                        message: "conflicting symbol defined here".to_string(),
                    }]
                })
                .unwrap_or_default(),
        }
    }
}

/// The symbol in the implementation is not trait, struct, or enum.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InvalidSymbolInImplementation {
    /// The ID of the symbol that was found in the implementation.
    pub invalid_item_id: GlobalID,

    /// The span where invalid symbol was found.
    pub qualified_identifier_span: Span,
}

impl Report<&Table> for InvalidSymbolInImplementation {
    fn report(&self, table: &Table) -> DiagnosticReport {
        let qualified_name = table.get_qualified_name(self.invalid_item_id);

        DiagnosticReport {
            span: self.qualified_identifier_span.clone(),
            message: format!(
                "the symbol `{qualified_name}` is not a trait, marker, \
                 struct, or enum"
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// Empty implementation found on a trait implementation.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FoundEmptyImplementationOnTrait {
    /// The span where the empty implementation was found.
    pub empty_implementation_signature_span: Span,
}

impl Report<&Table> for FoundEmptyImplementationOnTrait {
    fn report(&self, _: &Table) -> DiagnosticReport {
        DiagnosticReport {
            span: self.empty_implementation_signature_span.clone(),
            message: "empty implementation found on a trait; expected an \
                      implementation with body or `delete` keyword"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// The `const` implementation is invalid in this context.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InvalidConstImplementation {
    /// The span where the invalid `const` implementation was found.
    pub span: Span,
}

impl Report<&Table> for InvalidConstImplementation {
    fn report(&self, _: &Table) -> DiagnosticReport {
        DiagnosticReport {
            span: self.span.clone(),
            message: "invalid `const` implementation in this context"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// The `final` implementation is invalid in this context.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InvalidFinalImplementation {
    /// The span where the invalid final implementation was found.
    pub span: Span,
}

impl Report<&Table> for InvalidFinalImplementation {
    fn report(&self, _: &Table) -> DiagnosticReport {
        DiagnosticReport {
            span: self.span.clone(),
            message: "invalid `final` implementation in this context"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// Implementation on marker must always be `final`
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NonFinalMarkerImplementation {
    /// The span of the implementation.
    pub span: Span,
}

impl Report<&Table> for NonFinalMarkerImplementation {
    fn report(&self, _: &Table) -> DiagnosticReport {
        DiagnosticReport {
            span: self.span.clone(),
            message: "implementation on marker must always be `final`"
                .to_string(),
            severity: Severity::Error,
            help_message: Some(
                "prefix the keyword `final` before the `implements` keyword"
                    .to_string(),
            ),
            related: Vec::new(),
        }
    }
}

/// The implementation contains a member that is not a member of the trait.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UnknownTraitImplementationMember {
    /// The span to the identifier of the unknown member.
    pub identifier_span: Span,

    /// The ID of the trait implementation.
    pub trait_id: GlobalID,
}

impl Report<&Table> for UnknownTraitImplementationMember {
    fn report(&self, table: &Table) -> DiagnosticReport {
        let trait_qualified_name = table.get_qualified_name(self.trait_id);
        let trait_span = table.get::<LocationSpan>(self.trait_id).span.clone();

        DiagnosticReport {
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
                    span: x,
                    message: "trait declared here".to_string(),
                })
                .into_iter()
                .collect(),
        }
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
    pub trait_member_id: GlobalID,

    /// The implementation member kind
    pub found_kind: TraitMemberKind,

    /// The span of the implementation member's identifier.
    pub implementation_member_identifer_span: Span,
}

impl Report<&Table> for MismatchedTraitMemberAndImplementationMember {
    fn report(&self, table: &Table) -> DiagnosticReport {
        let trait_member_qualified_identifier =
            table.get_qualified_name(self.trait_member_id);
        let trait_member_sym_kind =
            *table.get::<SymbolKind>(self.trait_member_id);
        let trait_member_span =
            table.get::<LocationSpan>(self.trait_member_id).span.clone();

        let trait_member_kind = match trait_member_sym_kind {
            SymbolKind::TraitType => TraitMemberKind::Type,
            SymbolKind::TraitFunction => TraitMemberKind::Function,
            SymbolKind::TraitConstant => TraitMemberKind::Constant,
            _ => panic!(
                "must be some kind of trait member, but found \
                 {trait_member_sym_kind:?}",
            ),
        };

        DiagnosticReport {
            span: self.implementation_member_identifer_span.clone(),
            message: format!(
                "the trait member `{trait_member_qualified_identifier}` is of \
                 kind `{trait_member_kind}` but the implementation member is \
                 of kind `{}`",
                self.found_kind
            ),
            severity: Severity::Error,
            help_message: None,
            related: trait_member_span
                .map(|span| Related {
                    span,
                    message: "the trait member is defined here".to_string(),
                })
                .into_iter()
                .collect(),
        }
    }
}

/// Not all trait members are implemented in the implementation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnimplementedTraitMembers {
    /// The list of trait members that are not implemented.
    pub unimplemented_trait_member_ids: HashSet<GlobalID>,

    /// The ID of the implementation in which the trait members are not
    pub implementation_id: GlobalID,
}

impl Report<&Table> for UnimplementedTraitMembers {
    fn report(&self, table: &Table) -> DiagnosticReport {
        let trait_member_qualified_names = self
            .unimplemented_trait_member_ids
            .iter()
            .map(|&trait_member_id| table.get_qualified_name(trait_member_id))
            .collect::<Vec<_>>();

        let implementation_span = table
            .get::<LocationSpan>(self.implementation_id)
            .span
            .clone()
            .unwrap();

        DiagnosticReport {
            span: implementation_span,
            message: format!(
                "not all trait member(s) are implemented in the \
                 implementation: {}",
                trait_member_qualified_names.join(", ")
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// Implementation with body found on a marker.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FoundImplementationWithBodyOnMarker {
    /// The span where the implementation was found.
    pub implementation_span: Span,
}

impl Report<&Table> for FoundImplementationWithBodyOnMarker {
    fn report(&self, _: &Table) -> DiagnosticReport {
        DiagnosticReport {
            span: self.implementation_span.clone(),
            message: "implementation with body found on a marker; expected an \
                      implementation with `delete` keyword or empty \
                      (delimited with semicolon)"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// The adt implementation expects an implementation with a body.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExpectedImplementationWithBodyForAdt {
    /// The implementation that was found to be invalid.
    pub invalid_implementation_span: Span,
}

impl Report<&Table> for ExpectedImplementationWithBodyForAdt {
    fn report(&self, _: &Table) -> DiagnosticReport {
        DiagnosticReport {
            span: self.invalid_implementation_span.clone(),
            message: "implementation on struct or enum expects an \
                      implementation with a body"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// Adt implementation member can only be a function.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UnexpectedAdtImplementationMember {
    /// The span where the unexpected member was found.
    pub unexpected_member_span: Span,
}

impl Report<&Table> for UnexpectedAdtImplementationMember {
    fn report(&self, _: &Table) -> DiagnosticReport {
        DiagnosticReport {
            span: self.unexpected_member_span.clone(),
            message: "adt (struct and enum) implementation can only contain \
                      functions"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}
