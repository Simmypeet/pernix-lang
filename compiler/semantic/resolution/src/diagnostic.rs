//! Defines all kinds of diagnostics occurred during resolution.

use flexstr::SharedStr;
use pernixc_diagnostic::{Highlight, Report, Severity};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::{runtime::executor, TrackedEngine};
use pernixc_semantic_element::import::get_import_map;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::ByteIndex;
use pernixc_stable_hash::StableHash;
use pernixc_symbol::{
    kind::{get_kind, Kind},
    member::try_get_members,
    name::get_qualified_name,
    source_map::to_absolute_span,
};
use pernixc_target::{get_target_map, Global};
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
    ExpectModule(ExpectModule),
}

impl Report for Diagnostic {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Rendered<ByteIndex>, executor::CyclicError>
    {
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
            Self::ExpectModule(diagnostic) => diagnostic.report(engine).await,
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

impl Report for ThisNotFound {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Rendered<ByteIndex>, executor::CyclicError>
    {
        Ok(pernixc_diagnostic::Rendered {
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
        })
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

impl Report for LifetimeParameterNotFound {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Rendered<ByteIndex>, executor::CyclicError>
    {
        let referring_site_qualified_name =
            engine.get_qualified_name(self.referring_site).await;

        Ok(pernixc_diagnostic::Rendered {
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
        })
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

impl Report for UnexpectedInference {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Rendered<ByteIndex>, executor::CyclicError>
    {
        Ok(pernixc_diagnostic::Rendered {
            primary_highlight: Some(Highlight::new(
                engine.to_absolute_span(&self.unexpected_span).await,
                Some(format!(
                    "expected an explicit {} to be inserted here",
                    match self.generic_kind {
                        GenericKind::Type => "type",
                        GenericKind::Lifetime => "lifetime",
                        GenericKind::Constant => "constant",
                    }
                )),
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
        })
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

impl Report for ExpectType {
    async fn report(
        &self,
        table: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Rendered<ByteIndex>, executor::CyclicError>
    {
        let qualified_name =
            table.get_qualified_name(self.resolved_global_id).await;
        let kind = table.get_kind(self.resolved_global_id).await;

        Ok(pernixc_diagnostic::Rendered {
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
        })
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

impl Report for MoreThanOneUnpackedInTupleType {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Rendered<ByteIndex>, executor::CyclicError>
    {
        Ok(pernixc_diagnostic::Rendered {
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
        })
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

impl Report for MisorderedGenericArgument {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Rendered<ByteIndex>, executor::CyclicError>
    {
        Ok(pernixc_diagnostic::Rendered {
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
        })
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

impl Report for MismatchedGenericArgumentCount {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Rendered<ByteIndex>, executor::CyclicError>
    {
        Ok(pernixc_diagnostic::Rendered {
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
        })
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

impl Report for NoGenericArgumentsRequired {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Rendered<ByteIndex>, executor::CyclicError>
    {
        let qualified_name = engine.get_qualified_name(self.global_id).await;

        Ok(pernixc_diagnostic::Rendered {
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
        })
    }
}

/// The symbol was not found in the given scope.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
)]
pub struct SymbolNotFound {
    /// The [`GlobalID`] where the symbol was searched in. If `None`, the root
    /// module was searched.
    pub searched_item_id: Option<Global<pernixc_symbol::ID>>,

    /// The span where the symbol was searched from.
    pub resolution_span: RelativeSpan,

    /// The name that failed to resolved.
    pub name: SharedStr,
}

#[allow(clippy::cast_precision_loss)]
fn suggest<'a>(
    not_found_name: &str,
    available_names: impl IntoIterator<Item = &'a str>,
) -> Option<&'a str> {
    // Calculate appropriate maximum distance based on input length
    let max_distance = match not_found_name.len() {
        0..=3 => 1,   // Very short: allow only 1 char difference
        4..=6 => 2,   // Short: allow 2 char difference
        7..=10 => 3,  // Medium: allow 3 char difference
        11..=15 => 4, // Long: allow 4 char difference
        _ => not_found_name.len() / 4, // Very long: allow 25% difference
    };

    let mut best_candidate = None;
    let mut best_score = f64::NEG_INFINITY;

    for candidate in available_names {
        let distance = strsim::levenshtein(not_found_name, candidate);

        // Skip if distance is too large
        if distance > max_distance {
            continue;
        }

        // Skip exact matches (shouldn't happen in real usage)
        if distance == 0 {
            continue;
        }

        // Calculate confidence score
        let max_len = not_found_name.len().max(candidate.len()) as f64;
        let base_score = 1.0 - (distance as f64 / max_len);

        // Apply bonus for common typo patterns

        if base_score > best_score {
            best_score = base_score;
            best_candidate = Some(candidate);
        }
    }

    // Only return suggestion if confidence is reasonable
    if best_score > 0.4 {
        best_candidate
    } else {
        None
    }
}

impl Report for SymbolNotFound {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Rendered<ByteIndex>, executor::CyclicError>
    {
        let searched_item_id_qualified_name = match self.searched_item_id {
            Some(id) => Some(engine.get_qualified_name(id).await),
            None => None,
        };

        let did_you_mean = 'dym: {
            if let Some(item) = self.searched_item_id {
                let Some(members) = engine.try_get_members(item).await else {
                    break 'dym None;
                };

                let kind = engine.get_kind(item).await;

                match kind {
                    Kind::Module => {
                        let imports = engine.get_import_map(item).await;

                        suggest(
                            &self.name,
                            members
                                .member_ids_by_name
                                .keys()
                                .map(flexstr::FlexStr::as_str)
                                .chain(
                                    imports
                                        .keys()
                                        .map(flexstr::FlexStr::as_str),
                                ),
                        )
                        .map(ToString::to_string)
                    }

                    _ => suggest(
                        &self.name,
                        members
                            .member_ids_by_name
                            .keys()
                            .map(flexstr::FlexStr::as_str),
                    )
                    .map(ToString::to_string),
                }
            } else {
                let target_map = engine.get_target_map().await;
                suggest(
                    &self.name,
                    target_map.keys().map(flexstr::FlexStr::as_str),
                )
                .map(ToString::to_string)
            }
        };

        let span_message = searched_item_id_qualified_name.map_or_else(
            || {
                format!(
                    "the target named `{}` is not found",
                    self.name.as_str()
                )
            },
            |x| {
                format!(
                    "the symbol named `{}` does not exist in `{x}`",
                    self.name.as_str(),
                )
            },
        );

        Ok(pernixc_diagnostic::Rendered {
            primary_highlight: Some(Highlight::new(
                engine.to_absolute_span(&self.resolution_span).await,
                Some(span_message),
            )),
            message: "the symbol could not be found".to_string(),
            severity: Severity::Error,
            help_message: did_you_mean
                .as_ref()
                .map(|suggestion| format!("did you mean `{suggestion}`?")),
            related: Vec::new(),
        })
    }
}

/// The symbol is not accessible from the referring site.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
)]
pub struct SymbolIsNotAccessible {
    /// [`Global`] ID where the [`Self::referred`] is referred.
    pub referring_site: Global<pernixc_symbol::ID>,

    /// The symbol that was referred and is not accessible.
    pub referred: Global<pernixc_symbol::ID>,

    /// The span where the [`Self::referred`] is referred from.
    pub referred_span: RelativeSpan,
}

impl Report for SymbolIsNotAccessible {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Rendered<ByteIndex>, executor::CyclicError>
    {
        let referring_site_qualified_name =
            engine.get_qualified_name(self.referring_site).await;
        let referred_qualified_name =
            engine.get_qualified_name(self.referred).await;

        Ok(pernixc_diagnostic::Rendered {
            primary_highlight: Some(Highlight::new(
                engine.to_absolute_span(&self.referred_span).await,
                Some(format!(
                    "the symbol `{referred_qualified_name}` is not accessible \
                     from `{referring_site_qualified_name}`",
                )),
            )),
            message: "the symbol is not accessible".to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// Expected a module in the module path, but found other kind of symbol.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
)]
pub struct ExpectModule {
    /// The module path that was expected to be a module.
    pub module_path: RelativeSpan,

    /// The ID of the symbol that was found instead of a module.
    pub found_id: Global<pernixc_symbol::ID>,
}

impl Report for ExpectModule {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Rendered<ByteIndex>, executor::CyclicError>
    {
        let found_symbol_qualified_name =
            engine.get_qualified_name(self.found_id).await;

        let kind = engine.get_kind(self.found_id).await;

        Ok(pernixc_diagnostic::Rendered {
            primary_highlight: Some(Highlight::new(
                engine.to_absolute_span(&self.module_path).await,
                None,
            )),
            message: format!(
                "expected a module in the module path, but found `{} {}`",
                kind.kind_str(),
                found_symbol_qualified_name
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}
