//! Contains all the diagnostics related creating a new compilation target.

use std::{any::Any, fmt::Debug};

use flexstr::SharedStr;
use pernixc_diagnostic::{Related, Report, Severity};
use pernixc_lexical::tree::{RelativeLocation, RelativeSpan};
use pernixc_query::{Engine, TrackedEngine};
use pernixc_target::{Global, TargetID};

use crate::{
    accessibility::{Accessibility, Ext as _},
    import::Ext as _,
    kind::{Ext as _, Kind},
    member::Ext as _,
    name::Ext as _,
    span::Ext as _,
    symbol,
    target::MapExt,
};

/// Implemented all the diagnostic objects and provides type erasure.
pub trait Diagnostic:
    for<'a> Report<&'a Engine, Location = RelativeLocation>
    + Debug
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
        U: for<'a> Report<&'a Engine, Location = RelativeLocation>
            + Debug
            + Any
            + Send
            + Sync
            + 'static,
    > Diagnostic for U
{
    fn as_any(&self) -> &dyn Any { self }

    fn as_any_mut(&mut self) -> &mut dyn Any { self }
}

impl<
        U: for<'a> Report<&'a Engine, Location = RelativeLocation>
            + Debug
            + Any
            + Send
            + Sync
            + 'static,
    > From<U> for Box<dyn Diagnostic>
{
    fn from(value: U) -> Self { Box::new(value) }
}

/// The item symbol with the same name already exists in the given scope.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ItemRedifinition {
    /// The ID of the existing symbol.
    pub existing_id: Global<symbol::ID>,

    /// The ID of the new symbol.
    pub new_id: Global<symbol::ID>,

    /// The scope in which the duplication occurred.
    pub in_id: Global<symbol::ID>,
}

impl Report<&Engine> for ItemRedifinition {
    type Location = RelativeLocation;

    fn report(
        &self,
        engine: &Engine,
    ) -> pernixc_diagnostic::Diagnostic<RelativeLocation> {
        let engine = engine.tracked();

        let existing_symbol_span = engine.get_span(self.existing_id);
        let new_symbol_span = engine.get_span(self.new_id);
        let existing_symbol_name = engine.get_name(self.existing_id);
        let in_name = engine.get_qualified_name(self.in_id);

        pernixc_diagnostic::Diagnostic {
            span: new_symbol_span
                .map(|x| (x, Some("redefinition here".to_string()))),

            message: format!(
                "symbol `{}` is already defined in the scope `{}`",
                *existing_symbol_name, in_name
            ),
            severity: pernixc_diagnostic::Severity::Error,
            help_message: None,
            related: existing_symbol_span
                .map(|span| pernixc_diagnostic::Related {
                    span,
                    message: format!(
                        "symbol `{}` is already defined here",
                        *existing_symbol_name
                    ),
                })
                .into_iter()
                .collect(),
        }
    }
}

/// The symbol was not found in the given scope.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SymbolNotFound {
    /// The [`GlobalID`] where the symbol was searched in. If `None`, the root
    /// module was searched.
    pub searched_item_id: Option<Global<symbol::ID>>,

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

impl Report<&Engine> for SymbolNotFound {
    type Location = RelativeLocation;

    fn report(
        &self,
        engine: &Engine,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        let engine = engine.tracked();

        let searched_item_id_qualified_name =
            self.searched_item_id.map(|x| engine.get_qualified_name(x));

        let did_you_mean = self.searched_item_id.map_or_else(
            || {
                let target_map = engine.get_target_map();
                suggest(
                    &self.name,
                    target_map.keys().map(flexstr::FlexStr::as_str),
                )
                .map(ToString::to_string)
            },
            |x| {
                let members = engine.try_get_members(x)?;

                let kind = engine.get_kind(x);

                match kind {
                    Kind::Module => suggest(
                        &self.name,
                        members
                            .member_ids_by_name
                            .keys()
                            .map(flexstr::FlexStr::as_str)
                            .chain(
                                engine
                                    .get_imports(x)
                                    .0
                                    .keys()
                                    .map(flexstr::FlexStr::as_str),
                            ),
                    )
                    .map(ToString::to_string),

                    _ => suggest(
                        &self.name,
                        members
                            .member_ids_by_name
                            .keys()
                            .map(flexstr::FlexStr::as_str),
                    )
                    .map(ToString::to_string),
                }
            },
        );

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

        pernixc_diagnostic::Diagnostic {
            span: Some((self.resolution_span, Some(span_message))),
            message: "the symbol could not be found".to_string(),
            severity: Severity::Error,
            help_message: did_you_mean
                .as_ref()
                .map(|suggestion| format!("did you mean `{suggestion}`?")),
            related: Vec::new(),
        }
    }
}

/// The symbol is not accessible from the referring site.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SymbolIsNotAccessible {
    /// [`Global`] ID where the [`Self::referred`] is referred.
    pub referring_site: Global<symbol::ID>,

    /// The symbol that was referred and is not accessible.
    pub referred: Global<symbol::ID>,

    /// The span where the [`Self::referred`] is referred from.
    pub referred_span: RelativeSpan,
}

impl Report<&Engine> for SymbolIsNotAccessible {
    type Location = RelativeLocation;

    fn report(
        &self,
        engine: &Engine,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        let engine = engine.tracked();

        let referring_site_qualified_name =
            engine.get_qualified_name(self.referring_site);
        let referred_qualified_name = engine.get_qualified_name(self.referred);

        pernixc_diagnostic::Diagnostic {
            span: Some((
                self.referred_span,
                Some(format!(
                    "the symbol `{referred_qualified_name}` is not accessible \
                     from `{referring_site_qualified_name}`",
                )),
            )),
            message: "the symbol is not accessible".to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// Expected a module in the module path, but found other kind of symbol.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExpectModule {
    /// The module path that was expected to be a module.
    pub module_path: RelativeSpan,

    /// The ID of the symbol that was found instead of a module.
    pub found_id: Global<symbol::ID>,
}

impl Report<&Engine> for ExpectModule {
    type Location = RelativeLocation;

    fn report(
        &self,
        engine: &Engine,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        let engine = engine.tracked();

        let found_symbol_qualified_name =
            engine.get_qualified_name(self.found_id);

        let kind = engine.get_kind(self.found_id);

        pernixc_diagnostic::Diagnostic {
            span: Some((self.module_path, None)),
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

/// The import items that have a `from` clause cannot have the `target` as the
/// root path.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TargetRootInImportIsNotAllowedwithFrom {
    /// The span where the target root is found.
    pub target_root_span: RelativeSpan,
}

impl Report<&Engine> for TargetRootInImportIsNotAllowedwithFrom {
    type Location = RelativeLocation;

    fn report(
        &self,
        _: &Engine,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        pernixc_diagnostic::Diagnostic {
            span: Some((
                self.target_root_span,
                Some(
                    "the `target` root path is not allowed with `from` clause"
                        .to_string(),
                ),
            )),
            message: "import items that have a `from` clause cannot have the \
                      `target` as the root path"
                .to_string(),
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
    pub using_span: RelativeSpan,

    /// The name that conflicts with the existing name in the module.
    pub name: SharedStr,

    /// The module where the name is already defined.
    pub module_id: Global<symbol::ID>,

    /// The span of the conflicting name.
    ///
    /// This can either be the span to the declared symbol or the previous
    /// using that uses the given name.
    pub conflicting_span: Option<RelativeSpan>,
}

impl Report<&Engine> for ConflictingUsing {
    type Location = RelativeLocation;

    fn report(
        &self,
        engine: &Engine,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        let engine = engine.tracked();

        let module_qualified_name = engine.get_qualified_name(self.module_id);

        pernixc_diagnostic::Diagnostic {
            span: Some((
                self.using_span,
                Some(format!(
                    "the using `{name}` conflicts with the existing name in \
                     the module `{module_qualified_name}`",
                    name = self.name
                )),
            )),
            message: "the using statement conflicts with an existing name in \
                      the module"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: self
                .conflicting_span
                .as_ref()
                .map(|span| {
                    vec![Related {
                        span: *span,
                        message: format!(
                            "this symbol already defined the name `{}`",
                            self.name
                        ),
                    }]
                })
                .unwrap_or_default(),
        }
    }
}

/// The symbol is more accessible than the parent symbol.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SymbolIsMoreAccessibleThanParent {
    /// The ID of the symbol that is more accessible than the parent symbol.
    pub symbol_id: Global<symbol::ID>,

    /// The ID of the parent symbol.
    pub parent_id: Global<symbol::ID>,
}

fn accessibility_description(
    engine: &TrackedEngine,
    target_id: TargetID,
    accessibility: Accessibility<symbol::ID>,
) -> String {
    match accessibility {
        Accessibility::Public => "publicly accessible".to_owned(),
        Accessibility::Scoped(module_id) => {
            let module_qualified_name =
                engine.get_qualified_name(Global::new(target_id, module_id));

            format!("accessible in `{module_qualified_name}`")
        }
    }
}

impl Report<&Engine> for SymbolIsMoreAccessibleThanParent {
    type Location = RelativeLocation;

    fn report(
        &self,
        engine: &Engine,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        let engine = engine.tracked();

        let symbol_name = engine.get_name(self.symbol_id);
        let parent_qualified_name = engine.get_qualified_name(self.parent_id);

        let symbol_accessibility = engine.get_accessibility(self.symbol_id);
        let parent_accessibility = engine.get_accessibility(self.parent_id);

        let symbol_span = engine.get_span(self.symbol_id);
        let parent_span = engine.get_span(self.parent_id);

        let symbol_accessibility_description = accessibility_description(
            &engine,
            self.symbol_id.target_id,
            symbol_accessibility,
        );

        let parent_accessibility_description = accessibility_description(
            &engine,
            self.parent_id.target_id,
            parent_accessibility,
        );

        pernixc_diagnostic::Diagnostic {
            span: symbol_span.map(|span| {
                (
                    span,
                    Some(format!(
                        "the symbol `{}` is \
                         {symbol_accessibility_description}, which is more \
                         accessible than its parent symbol \
                         `{parent_qualified_name}`",
                        symbol_name.as_str(),
                    )),
                )
            }),
            message: "the symbol is more accessible than its parent symbol"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: parent_span
                .map(|span| {
                    vec![Related {
                        span,
                        message: format!(
                            "the parent symbol `{parent_qualified_name}` is \
                             {parent_accessibility_description}",
                        ),
                    }]
                })
                .unwrap_or_default(),
        }
    }
}
