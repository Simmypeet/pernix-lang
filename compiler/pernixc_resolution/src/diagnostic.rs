//! Contains the diagnostic types related to the resolution process.

use pernixc_diagnostic::{Diagnostic, Report};
use pernixc_log::Severity;
use pernixc_semantic::{component::SymbolKind, GlobalID, Table};
use pernixc_source_file::Span;
use pernixc_term::generic_parameter::GenericKind;

/// The lifetime parameter was not found in the given scope.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LifetimeParameterNotFound {
    /// The span where the lifetime parameter was referred from.
    pub referred_span: Span,

    /// The [`GlobalID`] where the referenced occurred from.
    pub referring_site: GlobalID,
}

impl Report<&Table> for LifetimeParameterNotFound {
    fn report(&self, table: &Table) -> Diagnostic {
        let referring_site_qualified_name =
            table.get_qualified_name(self.referring_site);

        Diagnostic {
            span: self.referred_span.clone(),
            message: format!(
                "the lifetime parameter `{}` was not found in \
                 `{referring_site_qualified_name}`",
                self.referred_span.str()
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
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

impl Report<&Table> for UnexpectedInference {
    fn report(&self, _: &Table) -> Diagnostic {
        Diagnostic {
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
        }
    }
}

/// The tuple type contains more than one unpacked type.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MoreThanOneUnpackedInTupleType {
    /// The span where the illegal tuple type was found.
    pub illegal_tuple_type_span: Span,
}

impl Report<&Table> for MoreThanOneUnpackedInTupleType {
    fn report(&self, _: &Table) -> Diagnostic {
        Diagnostic {
            span: self.illegal_tuple_type_span.clone(),
            message: "the tuple type contains more than one unpacked type"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
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

impl Report<&Table> for ExpectType {
    fn report(&self, table: &Table) -> Diagnostic {
        let qualified_name = table.get_qualified_name(self.resolved_global_id);
        let kind = *table.get::<SymbolKind>(self.resolved_global_id);

        Diagnostic {
            span: self.non_type_symbol_span.clone(),
            message: format!(
                "the type was expected but found {} `{qualified_name}`",
                kind.kind_str()
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
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

impl Report<&Table> for MismatchedGenericArgumentCount {
    fn report(&self, _: &Table) -> Diagnostic {
        Diagnostic {
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
        }
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

impl Report<&Table> for MisorderedGenericArgument {
    fn report(&self, _: &Table) -> Diagnostic {
        Diagnostic {
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
        }
    }
}
