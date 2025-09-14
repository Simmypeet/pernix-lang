//! Contains the diagnostics related to naming and symbol resolution.

use enum_as_inner::EnumAsInner;
use flexstr::SharedStr;
use pernixc_diagnostic::{Highlight, Report, Severity};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::{runtime::executor, TrackedEngine};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::ByteIndex;
use pernixc_stable_hash::StableHash;
use pernixc_target::{get_target_map, Global};

use crate::{
    import::get_imports,
    kind::{get_kind, Kind},
    member::try_get_members,
    name::get_qualified_name,
    source_map::to_absolute_span,
    ID,
};

/// Enumeration of all diagnostics related to naming and symbol resolution.
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
    EnumAsInner,
)]
#[allow(missing_docs)]
pub enum Diagnostic {
    SymbolNotFound(SymbolNotFound),
    SymbolIsNotAccessible(SymbolIsNotAccessible),
    ExpectModule(ExpectModule),
}

impl Report for Diagnostic {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Diagnostic<ByteIndex>, executor::CyclicError>
    {
        match self {
            Self::SymbolNotFound(err) => err.report(engine).await,
            Self::SymbolIsNotAccessible(err) => err.report(engine).await,
            Self::ExpectModule(err) => err.report(engine).await,
        }
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
    pub searched_item_id: Option<Global<ID>>,

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
    ) -> Result<pernixc_diagnostic::Diagnostic<ByteIndex>, executor::CyclicError>
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
                        let imports = engine.get_imports(item).await;

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

        Ok(pernixc_diagnostic::Diagnostic {
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
    pub referring_site: Global<ID>,

    /// The symbol that was referred and is not accessible.
    pub referred: Global<ID>,

    /// The span where the [`Self::referred`] is referred from.
    pub referred_span: RelativeSpan,
}

impl Report for SymbolIsNotAccessible {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Diagnostic<ByteIndex>, executor::CyclicError>
    {
        let referring_site_qualified_name =
            engine.get_qualified_name(self.referring_site).await;
        let referred_qualified_name =
            engine.get_qualified_name(self.referred).await;

        Ok(pernixc_diagnostic::Diagnostic {
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
    pub found_id: Global<ID>,
}

impl Report for ExpectModule {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Diagnostic<ByteIndex>, executor::CyclicError>
    {
        let found_symbol_qualified_name =
            engine.get_qualified_name(self.found_id).await;

        let kind = engine.get_kind(self.found_id).await;

        Ok(pernixc_diagnostic::Diagnostic {
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
