//! Defines all kinds of diagnostics occurred during resolution.

use flexstr::SharedStr;
use pernixc_diagnostic::{Highlight, Report, Severity};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::TrackedEngine;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::ByteIndex;
use pernixc_stable_hash::StableHash;
// re-exports
pub use pernixc_symbol::name::diagnostic::{
    SymbolIsNotAccessible, SymbolNotFound,
};
use pernixc_symbol::{
    kind::get_kind, name::get_qualified_name, source_map::to_absolute_span,
};
use pernixc_target::Global;
use pernixc_term::generic_parameters::GenericKind;

/// Enumeration of all kinds of diagnostic generated during resolution.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
    derive_more::From,
)]
#[allow(missing_docs)]
pub enum Diagnostic {
    MoreThanOneUnpackedInTupleType(MoreThanOneUnpackedInTupleType),
    MisorderedGenericArgument(MisorderedGenericArgument),
    ThisNotFound(ThisNotFound),
    SymbolNotFound(SymbolNotFound),
    SymbolIsNotAccessible(SymbolIsNotAccessible),
    LifetimeParameterNotFound(LifetimeParameterNotFound),
    UnexpectedInference(UnexpectedInference),
    MismatchedGenericArgumentCount(MismatchedGenericArgumentCount),
    NoGenericArgumentsRequired(NoGenericArgumentsRequired),
    ExpectType(ExpectType),
}

impl Report<&TrackedEngine> for Diagnostic {
    type Location = ByteIndex;

    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        match self {
            Self::MoreThanOneUnpackedInTupleType(diagnostic) => {
                diagnostic.report(engine).await
            }
            Self::MisorderedGenericArgument(diagnostic) => {
                diagnostic.report(engine).await
            }
            Self::ThisNotFound(diagnostic) => diagnostic.report(engine).await,
            Self::SymbolNotFound(diagnostic) => diagnostic.report(engine).await,
            Self::SymbolIsNotAccessible(diagnostic) => {
                diagnostic.report(engine).await
            }
            Self::LifetimeParameterNotFound(diagnostic) => {
                diagnostic.report(engine).await
            }
            Self::UnexpectedInference(diagnostic) => {
                diagnostic.report(engine).await
            }
            Self::MismatchedGenericArgumentCount(diagnostic) => {
                diagnostic.report(engine).await
            }
            Self::NoGenericArgumentsRequired(diagnostic) => {
                diagnostic.report(engine).await
            }
            Self::ExpectType(diagnostic) => diagnostic.report(engine).await,
        }
    }
}

/// The `this` keyword was used in the wrong context.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
pub struct ThisNotFound {
    /// The span where the `this` keyword was used.
    pub this_span: RelativeSpan,
}

impl Report<&TrackedEngine> for ThisNotFound {
    type Location = ByteIndex;

    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        pernixc_diagnostic::Diagnostic {
            primary_highlight: Some(Highlight::new(
                engine.to_absolute_span(&self.this_span).await,
                Some(
                    "`this` keyword is only available in `implements`, \
                     `struct`, `enum`, or `trait`"
                        .to_string(),
                ),
            )),
            message: "the `this` keyword is not allowed here".to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// The lifetime parameter was not found in the given scope.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
pub struct LifetimeParameterNotFound {
    /// The span where the lifetime parameter was referred from.
    pub referred_span: RelativeSpan,

    /// The name of the lifetime that was searched.
    pub name: SharedStr,

    /// The site where the search occurred.
    pub referring_site: Global<pernixc_symbol::ID>,
}

impl Report<&TrackedEngine> for LifetimeParameterNotFound {
    type Location = ByteIndex;

    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        let referring_site_qualified_name =
            engine.get_qualified_name(self.referring_site).await;

        pernixc_diagnostic::Diagnostic {
            primary_highlight: Some(Highlight::new(
                engine.to_absolute_span(&self.referred_span).await,
                Some(format!(
                    "the lifetime parameter `{}` was not found in \
                     `{referring_site_qualified_name}`",
                    self.name.as_str()
                )),
            )),
            message: "cannot find the lifetime parameter".to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// The inference term isn't allowed in the given context.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
pub struct UnexpectedInference {
    /// Span where the inference term was found.
    pub unexpected_span: RelativeSpan,

    /// The kind of the inference term.
    pub generic_kind: GenericKind,
}

impl Report<&TrackedEngine> for UnexpectedInference {
    type Location = ByteIndex;

    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        pernixc_diagnostic::Diagnostic {
            primary_highlight: Some(Highlight::new(
                engine.to_absolute_span(&self.unexpected_span).await,
                None,
            )),
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

/// The type was expected but the non-type symbol was found.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
pub struct ExpectType {
    /// The span where the non-type symbol was found.
    pub non_type_symbol_span: RelativeSpan,

    /// The resolved symbol ID where the non-type symbol was found.
    pub resolved_global_id: Global<pernixc_symbol::ID>,
}

impl Report<&TrackedEngine> for ExpectType {
    type Location = ByteIndex;

    async fn report(
        &self,
        table: &TrackedEngine,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        let qualified_name =
            table.get_qualified_name(self.resolved_global_id).await;
        let kind = table.get_kind(self.resolved_global_id).await;

        pernixc_diagnostic::Diagnostic {
            primary_highlight: Some(Highlight::new(
                table.to_absolute_span(&self.non_type_symbol_span).await,
                Some(format!(
                    "the type was expected but found `{} {qualified_name}`",
                    kind.kind_str()
                )),
            )),
            message: "type expected".to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// The tuple type contains more than one unpacked type.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
pub struct MoreThanOneUnpackedInTupleType {
    /// The span where the illegal tuple type was found.
    pub illegal_tuple_type_span: RelativeSpan,
}

impl Report<&TrackedEngine> for MoreThanOneUnpackedInTupleType {
    type Location = ByteIndex;

    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        pernixc_diagnostic::Diagnostic {
            primary_highlight: Some(Highlight::new(
                engine.to_absolute_span(&self.illegal_tuple_type_span).await,
                None,
            )),
            message: "the tuple type cannot contain more than one unpacked \
                      type"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// The generic arguments were supplied in the wrong order.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
pub struct MisorderedGenericArgument {
    /// The kind of the mis-ordered generic argument.
    pub generic_kind: GenericKind,

    /// The span of the generic argument.
    pub generic_argument: RelativeSpan,
}

impl Report<&TrackedEngine> for MisorderedGenericArgument {
    type Location = ByteIndex;

    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        pernixc_diagnostic::Diagnostic {
            primary_highlight: Some(Highlight::new(
                engine.to_absolute_span(&self.generic_argument).await,
                match self.generic_kind {
                    GenericKind::Type => Some(
                        "can't be supplied after constant arguments"
                            .to_string(),
                    ),
                    GenericKind::Lifetime => Some(
                        "can't be supplied after type or constant arguments"
                            .to_string(),
                    ),
                    GenericKind::Constant => None,
                },
            )),
            message: "the generic argument was supplied in the wrong order"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// Generic arguments count mismatch.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
pub struct MismatchedGenericArgumentCount {
    /// The kind of the generic parameter.
    pub generic_kind: GenericKind,

    /// Span where mismatch occurred.
    pub generic_identifier_span: RelativeSpan,

    /// Expected count of generic arguments.
    pub expected_count: usize,

    /// Supplied count of generic arguments.
    pub supplied_count: usize,
}

impl Report<&TrackedEngine> for MismatchedGenericArgumentCount {
    type Location = ByteIndex;

    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        pernixc_diagnostic::Diagnostic {
            primary_highlight: Some(Highlight::new(
                engine.to_absolute_span(&self.generic_identifier_span).await,
                Some(format!(
                    "expected {} {} arguments, but {} were supplied",
                    self.expected_count,
                    match self.generic_kind {
                        GenericKind::Type => "type",
                        GenericKind::Lifetime => "lifetime",
                        GenericKind::Constant => "constant",
                    },
                    self.supplied_count,
                )),
            )),
            message: "mismatched generic argument count".to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// The symbol doesn't require any generic arguments but some were supplied.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
pub struct NoGenericArgumentsRequired {
    /// The symbol that  was supplied with generic arguments.
    pub global_id: Global<pernixc_symbol::ID>,

    /// The span where the generic arguments were supplied.
    pub generic_argument_span: RelativeSpan,
}

impl Report<&TrackedEngine> for NoGenericArgumentsRequired {
    type Location = ByteIndex;

    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        let qualified_name = engine.get_qualified_name(self.global_id).await;

        pernixc_diagnostic::Diagnostic {
            primary_highlight: Some(Highlight::new(
                engine.to_absolute_span(&self.generic_argument_span).await,
                Some(format!(
                    "the symbol `{qualified_name}` doesn't require any \
                     generic arguments"
                )),
            )),
            message: "generic arguments was supplied to the symbol that \
                      doesn't have a generic parameter"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}
