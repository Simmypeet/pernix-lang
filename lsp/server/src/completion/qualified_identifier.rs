//! Offers completion suggestions when user completing qualified identifier

use std::sync::Arc;

use fuzzy_matcher::FuzzyMatcher;
use log::info;
use pernixc_diagnostic::ByteIndex;
use pernixc_extend::extend;
use pernixc_hash::HashSet;
use pernixc_query::TrackedEngine;
use pernixc_semantic_element::import::get_import_map;
use pernixc_source_file::{
    GlobalSourceID, SourceElement, Span, get_source_file_by_id,
};
use pernixc_symbol::{
    get_target_root_module_id,
    kind::get_kind,
    member::{get_members, try_get_members},
    name::{get_name, get_qualified_name},
    parent::{get_closest_module_id, get_parent_global},
    source_file_module::get_source_file_module,
    source_map::to_absolute_span,
};
use pernixc_syntax::QualifiedIdentifier;
use pernixc_target::{Global, TargetID};
use tower_lsp::lsp_types::CompletionItemKind;

use crate::pointing::{
    self, Resolution, get_symbol_scope_at_byte_index,
    resolve_qualified_identifier_path,
};

#[extend]
fn allow_completion(self: pernixc_symbol::kind::Kind) -> bool {
    match self {
        pernixc_symbol::kind::Kind::Module
        | pernixc_symbol::kind::Kind::Struct
        | pernixc_symbol::kind::Kind::Trait
        | pernixc_symbol::kind::Kind::Enum
        | pernixc_symbol::kind::Kind::Type
        | pernixc_symbol::kind::Kind::Constant
        | pernixc_symbol::kind::Kind::Function
        | pernixc_symbol::kind::Kind::ExternFunction
        | pernixc_symbol::kind::Kind::Variant
        | pernixc_symbol::kind::Kind::TraitType
        | pernixc_symbol::kind::Kind::TraitFunction
        | pernixc_symbol::kind::Kind::TraitConstant
        | pernixc_symbol::kind::Kind::Effect
        | pernixc_symbol::kind::Kind::EffectOperation
        | pernixc_symbol::kind::Kind::Marker
        | pernixc_symbol::kind::Kind::ImplementationType
        | pernixc_symbol::kind::Kind::ImplementationFunction
        | pernixc_symbol::kind::Kind::ImplementationConstant => true,

        pernixc_symbol::kind::Kind::PositiveImplementation
        | pernixc_symbol::kind::Kind::NegativeImplementation => false,
    }
}

#[extend]
fn kind_to_completion_item_kind(
    self: pernixc_symbol::kind::Kind,
) -> CompletionItemKind {
    match self {
        pernixc_symbol::kind::Kind::Module => CompletionItemKind::MODULE,
        pernixc_symbol::kind::Kind::Struct => CompletionItemKind::STRUCT,

        pernixc_symbol::kind::Kind::Trait
        | pernixc_symbol::kind::Kind::Effect
        | pernixc_symbol::kind::Kind::Marker => CompletionItemKind::INTERFACE,

        pernixc_symbol::kind::Kind::Enum => CompletionItemKind::ENUM,
        pernixc_symbol::kind::Kind::Type => CompletionItemKind::TYPE_PARAMETER,

        pernixc_symbol::kind::Kind::Constant => CompletionItemKind::CONSTANT,

        pernixc_symbol::kind::Kind::Function
        | pernixc_symbol::kind::Kind::ExternFunction => {
            CompletionItemKind::FUNCTION
        }

        pernixc_symbol::kind::Kind::Variant => CompletionItemKind::ENUM_MEMBER,

        pernixc_symbol::kind::Kind::TraitType
        | pernixc_symbol::kind::Kind::ImplementationType => {
            CompletionItemKind::TYPE_PARAMETER
        }

        pernixc_symbol::kind::Kind::TraitConstant
        | pernixc_symbol::kind::Kind::ImplementationConstant => {
            CompletionItemKind::CONSTANT
        }

        pernixc_symbol::kind::Kind::TraitFunction
        | pernixc_symbol::kind::Kind::EffectOperation
        | pernixc_symbol::kind::Kind::ImplementationFunction => {
            CompletionItemKind::METHOD
        }

        pernixc_symbol::kind::Kind::PositiveImplementation
        | pernixc_symbol::kind::Kind::NegativeImplementation => {
            CompletionItemKind::CLASS
        }
    }
}

/// Priority of searching matching qualified identifier completions
///
/// We'll retrieve the matching qualified identifier and two places:
/// 1. The qualified identifier directly at the cursor
/// 2. The qualified identifier one byte before the cursor (to handle the case
///    where the cursor is at the end of the identifier)
///
/// If both exists, we'll prefer the one that is more specified (i.e is covered
/// by another).
///
/// For example, `Outer[Inner<cursor>]`, this would both match `Outer[...]` and
/// `Inner`, but we should prefer `Inner` as it is more specific because it is
/// covered by `Outer`.
#[extend]
pub async fn retrieve_qulaified_identifier_matching(
    self: &TrackedEngine,
    byte_index: ByteIndex,
    token_tree: &pernixc_lexical::tree::Tree,
    syntax_tree: &pernixc_syntax::item::module::Content,
) -> Option<QualifiedIdentifierMatching> {
    let node = pernixc_parser::concrete_tree::Node::Branch(
        syntax_tree.inner_tree().clone(),
    );

    let at_cursor = byte_index;
    let before_cursor = byte_index.checked_sub(1);

    let matching_at_cursor = node
        .get_deepest_ast::<pernixc_syntax::QualifiedIdentifier>(
            token_tree, at_cursor,
        );

    let matching_before_cursor = before_cursor.and_then(|byte_index| {
        node.get_deepest_ast::<pernixc_syntax::QualifiedIdentifier>(
            token_tree, byte_index,
        )
    });

    let result = match (matching_at_cursor, matching_before_cursor) {
        (Some(qual_at), Some(qual_before)) => {
            let span_at = qual_at.span();
            let span_before = qual_before.span();

            if span_at.start >= span_before.start
                && span_at.end <= span_before.end
            {
                // at_cursor is covered by before_cursor
                let token_span = node
                    .get_pointing_token(token_tree, at_cursor)
                    .map(|token| token.span);

                Some((qual_at, token_span))
            } else {
                // before_cursor is covered by at_cursor
                let token_span = node
                    .get_pointing_token(token_tree, before_cursor.unwrap())
                    .map(|token| token.span);

                Some((qual_before, token_span))
            }
        }
        (Some(qual_at), None) => {
            let token_span = node
                .get_pointing_token(token_tree, at_cursor)
                .map(|token| token.span);

            Some((qual_at, token_span))
        }
        (None, Some(qual_before)) => {
            let token_span = node
                .get_pointing_token(token_tree, before_cursor.unwrap())
                .map(|token| token.span);

            Some((qual_before, token_span))
        }
        (None, None) => None,
    };

    if let Some((qualified_identifier, token)) = result {
        let abs_token_span = if let Some(token_span) = token {
            Some(self.to_absolute_span(&token_span).await)
        } else {
            None
        };

        Some(QualifiedIdentifierMatching {
            qualified_identifier,
            hovering_token_span: abs_token_span,
        })
    } else {
        None
    }
}

/// The result of matching a qualified identifier at a given byte index.
#[derive(Debug, Clone)]
pub struct QualifiedIdentifierMatching {
    /// The matched qualified identifier.
    pub qualified_identifier: QualifiedIdentifier,

    /// The span of the token that is being hovered. This could be `None` if
    /// the cursor is exactly at the end of the qualified identifier or
    /// is in between insignificant tokens (whitespaces, etc.).
    pub hovering_token_span: Option<Span<ByteIndex>>,
}

/// Provides completion suggestions for qualified identifiers at the given
/// byte index.
#[extend]
#[allow(clippy::too_many_lines)]
pub async fn qualified_identifier_completion(
    self: &pernixc_query::TrackedEngine,
    byte_index: ByteIndex,
    source_id: GlobalSourceID,
    syntax_tree: pernixc_syntax::item::module::Content,
    token_tree: &pernixc_lexical::tree::Tree,
    target_id: TargetID,
    completions: &mut Vec<tower_lsp::lsp_types::CompletionItem>,
) -> Result<(), pernixc_query::runtime::executor::CyclicError> {
    let module =
        target_id.make_global(self.get_source_file_module(source_id).await);

    // get the current symbol scope at the byte index for symbol resolution
    let current_site = self
        .get_symbol_scope_at_byte_index(module, source_id, byte_index)
        .await;

    let node = pernixc_parser::concrete_tree::Node::Branch(
        syntax_tree.inner_tree().clone(),
    );

    let Some(QualifiedIdentifierMatching {
        qualified_identifier,
        hovering_token_span: token_abs_span,
    }) = self
        .retrieve_qulaified_identifier_matching(
            byte_index,
            token_tree,
            &syntax_tree,
        )
        .await
    else {
        info!(" No qualified identifier found at byte index {byte_index} ");
        return Ok(());
    };

    let Some(resolved_path) = self
        .resolve_qualified_identifier_path(
            current_site,
            &qualified_identifier,
            token_abs_span,
        )
        .await?
    else {
        info!(
            " Qualified identifier at byte index {byte_index} could not be \
             resolved "
        );
        return Ok(());
    };

    info!(
        " Qualified identifier at byte index {byte_index} resolved to \
         {resolved_path:?} "
    );

    // determine the nearest module id for import suggestions
    let nearest_module =
        target_id.make_global(self.get_closest_module_id(current_site).await);

    let prior_scope = match resolved_path {
        pointing::Resolution::Success(success_resolution) => {
            success_resolution.parent_scope
        }
        pointing::Resolution::FailAtCursor(fail_at_cursor) => fail_at_cursor,
        pointing::Resolution::FailedAtLastSegment(_)
        | pointing::Resolution::NoMatchFound(_) => {
            // check if the cursor is at the end of the qualified identifier,
            // only then suggest members of the symbol
            let qual_span = qualified_identifier.span();
            let qual_abs_span = token_tree.absolute_span_of(&qual_span);

            if byte_index == qual_abs_span.end {
                match resolved_path {
                    pointing::Resolution::FailedAtLastSegment(global) => global,
                    pointing::Resolution::NoMatchFound(global) => Some(global),

                    pointing::Resolution::Success(_)
                    | pointing::Resolution::FailAtCursor(_) => {
                        unreachable!()
                    }
                }
            } else {
                // cursor is not at the end, no suggestions
                return Ok(());
            }
        }
    };

    if let Some(symbol) = prior_scope {
        // suggest members of the resolved symbol
        let Some(members) = self.try_get_members(symbol).await else {
            return Ok(());
        };

        for (member_name, id) in &members.member_ids_by_name {
            let kind = self.get_kind(target_id.make_global(*id)).await;

            if !kind.allow_completion() {
                continue;
            }

            completions.push(tower_lsp::lsp_types::CompletionItem {
                label: member_name.to_string(),
                kind: Some(kind.kind_to_completion_item_kind()),
                sort_text: Some(format!("10_member_{member_name}")),
                ..Default::default()
            });
        }
    } else {
        // suggest module members, imports, this keyword, target keyword,
        // etc.

        let module_members = self.get_members(nearest_module).await;
        let module_imports = self.get_import_map(nearest_module).await;

        let mut inserted_ids = HashSet::default();

        for (member_name, id) in &module_members.member_ids_by_name {
            let kind =
                self.get_kind(nearest_module.target_id.make_global(*id)).await;

            if !kind.allow_completion() {
                continue;
            }

            completions.push(tower_lsp::lsp_types::CompletionItem {
                label: member_name.to_string(),
                kind: Some(kind.kind_to_completion_item_kind()),
                sort_text: Some(format!("10_member_{member_name}")),
                ..Default::default()
            });

            inserted_ids.insert(target_id.make_global(*id));
        }

        for (import_name, import) in module_imports.iter() {
            let kind = self.get_kind(import.id).await;
            if !kind.allow_completion() {
                continue;
            }

            completions.push(tower_lsp::lsp_types::CompletionItem {
                label: import_name.to_string(),
                kind: Some(kind.kind_to_completion_item_kind()),
                sort_text: Some(format!("20_import_{import_name}")),
                ..Default::default()
            });

            inserted_ids.insert(import.id);
        }

        completions.push(tower_lsp::lsp_types::CompletionItem {
            label: "this".to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            sort_text: Some("30_qual_this".to_string()),
            ..Default::default()
        });

        completions.push(tower_lsp::lsp_types::CompletionItem {
            label: "target".to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            sort_text: Some("30_qual_target".to_string()),
            ..Default::default()
        });

        let root_module_id = self.get_target_root_module_id(target_id).await;
        inserted_ids.insert(target_id.make_global(root_module_id));

        // collects all other available symbols that might not be included
        // in the current module
        self.get_other_available_symbols(
            &resolved_path,
            qualified_identifier,
            target_id,
            async |x| !inserted_ids.contains(&x),
            completions,
        )
        .await;
    }

    Ok(())
}

/// Gets all the other available symbols that might not be readily in the scope.
/// Usually this requires auto `import` insertions.
#[extend]
async fn get_other_available_symbols(
    self: &TrackedEngine,
    resolution: &Resolution,
    qualified_identifier: QualifiedIdentifier,
    target_id: TargetID,
    check: impl AsyncFn(Global<pernixc_symbol::ID>) -> bool,
    completions: &mut Vec<tower_lsp::lsp_types::CompletionItem>,
) {
    let string = match resolution {
        Resolution::FailAtCursor(Some(_))
        | Resolution::FailedAtLastSegment(Some(_))
        | Resolution::Success(_)
        | Resolution::NoMatchFound(_) => return,

        Resolution::FailAtCursor(None)
        | Resolution::FailedAtLastSegment(None) => {
            let Some(qual) = qualified_identifier.root() else {
                return;
            };

            let span = self.to_absolute_span(&qual.span()).await;
            let source_file = self.get_source_file_by_id(span.source_id).await;

            source_file.content()[span.range()].to_string()
        }
    };

    self.collect_all_symbols_in_target(target_id, &string, check, completions)
        .await;
}

async fn collect_module_id(
    engine: &TrackedEngine,
    current_module_id: Global<pernixc_symbol::ID>,
    results: &mut Vec<pernixc_symbol::ID>,
) {
    let members = engine.get_members(current_module_id).await;

    for id in members.member_ids_by_name.values().copied() {
        results.push(id);

        // recursively collect from child modules
        let kind =
            engine.get_kind(current_module_id.target_id.make_global(id)).await;

        if kind == pernixc_symbol::kind::Kind::Module {
            Box::pin(collect_module_id(
                engine,
                current_module_id.target_id.make_global(id),
                results,
            ))
            .await;
        }
    }
}

#[pernixc_query::query(
    key(GlobalImportSuggestionKey),
    value(Arc<[pernixc_symbol::ID]>),
    id(TargetID),
    executor(GlobalImportSuggestionExecutor),
    extend(method(get_global_import_suggestion_ids), no_cyclic)
)]
pub async fn get_global_import_suggestions(
    target_id: TargetID,
    engine: &TrackedEngine,
) -> Result<
    Arc<[pernixc_symbol::ID]>,
    pernixc_query::runtime::executor::CyclicError,
> {
    let root_module_id = engine.get_target_root_module_id(target_id).await;
    let mut results = vec![root_module_id];

    collect_module_id(
        engine,
        target_id.make_global(root_module_id),
        &mut results,
    )
    .await;

    Ok(results.into())
}

pernixc_register::register!(
    GlobalImportSuggestionKey,
    GlobalImportSuggestionExecutor
);

/// Collects all symbols in the target that match the given criteria.
#[extend]
pub async fn collect_all_symbols_in_target(
    self: &TrackedEngine,
    target_id: TargetID,
    current: &str,
    check: impl AsyncFn(Global<pernixc_symbol::ID>) -> bool,
    completions: &mut Vec<tower_lsp::lsp_types::CompletionItem>,
) {
    // currently, we'll gather modules, structs, enums, functions, constants,
    // traits, effects, and markers.
    let imports = self.get_global_import_suggestion_ids(target_id).await;
    let fuzzy_matcher = fuzzy_matcher::skim::SkimMatcherV2::default();

    for id in imports.iter().copied().map(|x| target_id.make_global(x)) {
        if !check(id).await {
            continue;
        }

        let Some(parent_module) = self.get_parent_global(id).await else {
            continue;
        };

        // add a completion item with import information
        let kind = self.get_kind(id).await;

        if !kind.allow_completion() {
            continue;
        }

        let name = self.get_name(id).await;

        // fuzzy match the current input
        if fuzzy_matcher.fuzzy_match(&name.to_std_string(), current).is_none() {
            continue;
        }

        let parent_module_qual_name =
            self.get_qualified_name(parent_module).await;

        completions.push(tower_lsp::lsp_types::CompletionItem {
            label: name.to_std_string(),
            kind: Some(kind.kind_to_completion_item_kind()),
            sort_text: Some(format!("50_global_{current}")),
            detail: Some(format!("from {parent_module_qual_name}")),
            ..Default::default()
        });
    }
}
