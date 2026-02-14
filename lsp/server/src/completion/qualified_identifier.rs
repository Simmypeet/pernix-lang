//! Offers completion suggestions when user completing qualified identifier

use std::sync::Arc;

use fuzzy_matcher::{FuzzyMatcher, skim::SkimMatcherV2};
use linkme::distributed_slice;
use log::info;
use pernixc_diagnostic::ByteIndex;
use pernixc_extend::extend;
use pernixc_hash::HashSet;
use pernixc_qbice::{Config, PERNIX_PROGRAM, TrackedEngine};
use pernixc_semantic_element::import::get_import_map;
use pernixc_source_file::{
    GlobalSourceID, SourceElement, Span, get_source_file_by_id,
};
use pernixc_symbol::{
    ID,
    accessibility::symbol_accessible,
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
use pernixc_tokio::{chunk::chunk_for_tasks, scoped};
use qbice::{
    Decode, Encode, Query, StableHash, executor, program::Registration,
    storage::intern::Interned,
};
use tokio::sync::RwLock;
use tower_lsp::lsp_types::{
    CompletionItem, CompletionItemKind, CompletionItemLabelDetails,
};

use crate::pointing::{
    self, get_symbol_scope_at_byte_index, resolve_qualified_identifier_path,
};

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

            if span_at.start <= span_before.start
                && span_at.end >= span_before.end
            {
                // at_cursor is covered by before_cursor
                let token_span = node
                    .get_pointing_token(token_tree, at_cursor)
                    .and_then(|token| {
                        token.kind.is_identifier().then_some(token.span)
                    });

                Some((qual_at, token_span))
            } else {
                // before_cursor is covered by at_cursor
                let token_span = node
                    .get_pointing_token(token_tree, before_cursor.unwrap())
                    .and_then(|token| {
                        token.kind.is_identifier().then_some(token.span)
                    });

                Some((qual_before, token_span))
            }
        }
        (Some(qual_at), None) => {
            let token_span =
                node.get_pointing_token(token_tree, at_cursor).and_then(
                    |token| token.kind.is_identifier().then_some(token.span),
                );

            Some((qual_at, token_span))
        }
        (None, Some(qual_before)) => {
            let token_span = node
                .get_pointing_token(token_tree, before_cursor.unwrap())
                .and_then(|token| {
                    token.kind.is_identifier().then_some(token.span)
                });

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
    self: &TrackedEngine,
    byte_index: ByteIndex,
    source_id: GlobalSourceID,
    syntax_tree: pernixc_syntax::item::module::Content,
    token_tree: &pernixc_lexical::tree::Tree,
    target_id: TargetID,
) -> Vec<Completion> {
    let module =
        target_id.make_global(self.get_source_file_module(source_id).await);

    // get the current symbol scope at the byte index for symbol resolution
    let current_site = self
        .get_symbol_scope_at_byte_index(module, source_id, byte_index)
        .await;

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
        return Vec::new();
    };

    let Some(resolved_path) = self
        .resolve_qualified_identifier_path(
            current_site,
            &qualified_identifier,
            token_abs_span,
        )
        .await
    else {
        info!(
            " Qualified identifier at byte index {byte_index} could not be \
             resolved "
        );
        return Vec::new();
    };

    info!(
        " Qualified identifier at byte index {byte_index} resolved to \
         {resolved_path:?} "
    );

    // determine the nearest module id for import suggestions
    let nearest_module_id =
        target_id.make_global(self.get_closest_module_id(current_site).await);

    let prior_scope = match resolved_path {
        pointing::Resolution::Success(success_resolution) => {
            success_resolution.parent_scope
        }

        pointing::Resolution::Failed(fail_at_cursor) => {
            if fail_at_cursor.fail_at_cursor {
                // only suggest members if the failure happened at the cursor
                fail_at_cursor.prior_scope
            } else {
                // no suggestions
                return Vec::new();
            }
        }

        pointing::Resolution::NoMatchFound(no_match_found) => {
            // check if the cursor is at the end of the qualified identifier,
            // only then suggest members of the symbol
            let qual_span = qualified_identifier.span();
            let qual_abs_span = token_tree.absolute_span_of(&qual_span);

            if byte_index == qual_abs_span.end {
                Some(no_match_found)
            } else {
                // cursor is not at the end, no suggestions
                return Vec::new();
            }
        }
    };

    let pointing_string: Option<Interned<str>> =
        if let Some(token_span) = token_abs_span {
            let source_file =
                self.get_source_file_by_id(token_span.source_id).await;

            Some(self.intern_unsized(
                source_file.content()[token_span.range()].to_owned(),
            ))
        } else {
            None
        };

    create_completions(
        self,
        nearest_module_id,
        prior_scope,
        pointing_string.as_ref(),
    )
    .await
}

/// A completion candidate when there're already some prior scope resolved.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MemberCompletion {
    /// The completed symbol ID.
    pub completion: Global<ID>,
}

/// A completion candidate when completing the root-level qualified identifier
/// and the candidates are members of the current module.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModuleCompletion {
    /// The completed symbol ID.
    pub completion: Global<ID>,
}

/// A completion candidate when completing the root-level qualified identifier
/// and the candidates are imports of the current module.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ImportCompletion {
    /// The completion string to be shown.
    pub completion_string: Interned<str>,

    /// The symbol ID of the import.
    pub symbol: Global<ID>,
}

/// A completion candidate when completing the root-level qualified identifier
/// and the candidates are global symbols.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GlobalCompletion {
    /// The completed symbol ID.
    pub completion: Global<ID>,
}

/// The completion suggestion.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Completion {
    Member(MemberCompletion),
    Module(ModuleCompletion),
    Import(ImportCompletion),
    Global(GlobalCompletion),
    ThisKeyword,
    TargetKeyword,
}

impl Completion {
    /// Converts the completion suggestion to an LSP `CompletionItem`.
    pub async fn to_lsp_completion(
        self,
        engine: &TrackedEngine,
    ) -> CompletionItem {
        match self {
            Self::Member(member_completion) => {
                let name = engine.get_name(member_completion.completion).await;

                let kind = engine
                    .get_kind(member_completion.completion)
                    .await
                    .kind_to_completion_item_kind();

                CompletionItem {
                    label: name.to_string(),
                    kind: Some(kind),
                    sort_text: Some("100".to_string()),

                    ..Default::default()
                }
            }

            Self::Module(module_completion) => {
                let name = engine.get_name(module_completion.completion).await;

                let kind = engine
                    .get_kind(module_completion.completion)
                    .await
                    .kind_to_completion_item_kind();

                CompletionItem {
                    label: name.to_string(),
                    kind: Some(kind),
                    sort_text: Some("100".to_string()),

                    ..Default::default()
                }
            }

            Self::Import(import_completion) => {
                let kind = engine
                    .get_kind(import_completion.symbol)
                    .await
                    .kind_to_completion_item_kind();

                CompletionItem {
                    label: import_completion.completion_string.to_string(),
                    kind: Some(kind),
                    sort_text: Some("100".to_string()),
                    ..Default::default()
                }
            }

            Self::Global(global_completion) => {
                let name = engine.get_name(global_completion.completion).await;

                let kind = engine
                    .get_kind(global_completion.completion)
                    .await
                    .kind_to_completion_item_kind();

                let detail = if let Some(parent_module_id) =
                    engine.get_parent_global(global_completion.completion).await
                {
                    let qualified_name =
                        engine.get_qualified_name(parent_module_id).await;

                    Some(format!("(from {qualified_name})"))
                } else {
                    None
                };

                CompletionItem {
                    label: name.to_string(),
                    kind: Some(kind),
                    sort_text: Some("200".to_string()),

                    // include a text specifying that this will import a new
                    // symbol at the top of the module
                    label_details: detail.map(|detail| {
                        CompletionItemLabelDetails {
                            detail: Some(detail),
                            description: None,
                        }
                    }),

                    ..Default::default()
                }
            }

            Self::ThisKeyword => CompletionItem {
                label: "this".to_string(),
                kind: Some(CompletionItemKind::KEYWORD),
                sort_text: Some("300".to_string()),
                ..Default::default()
            },

            Self::TargetKeyword => CompletionItem {
                label: "target".to_string(),
                kind: Some(CompletionItemKind::KEYWORD),
                sort_text: Some("300".to_string()),
                ..Default::default()
            },
        }
    }
}

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

#[allow(clippy::cognitive_complexity)]
async fn create_completions(
    engine: &TrackedEngine,
    nearest_module_id: Global<ID>,
    prior_scope: Option<Global<ID>>,
    str: Option<&Interned<str>>,
) -> Vec<Completion> {
    let mut completions = Vec::new();
    let target_id = nearest_module_id.target_id;
    let fuzzy_matcher = SkimMatcherV2::default();

    if let Some(symbol) = prior_scope {
        // suggest members of the resolved symbol
        let Some(members) = engine.try_get_members(symbol).await else {
            return Vec::default();
        };

        for id in members.member_ids_by_name.values().copied() {
            let kind = engine.get_kind(target_id.make_global(id)).await;

            if !kind.allow_completion() {
                continue;
            }

            // check accessibility
            if !engine
                .symbol_accessible(nearest_module_id, target_id.make_global(id))
                .await
            {
                continue;
            }

            if let Some(str) = str {
                let name = engine.get_name(target_id.make_global(id)).await;

                if fuzzy_matcher.fuzzy_match(&name, str).is_none() {
                    continue;
                }
            }

            completions.push(Completion::Member(MemberCompletion {
                completion: target_id.make_global(id),
            }));
        }
    } else {
        // suggest module members, imports, this keyword, target keyword,
        // etc.

        let module_members = engine.get_members(nearest_module_id).await;
        let module_imports = engine.get_import_map(nearest_module_id).await;

        // avoid duplicate completions from the later global symbol search
        let mut inserted_ids = HashSet::default();

        for id in module_members.member_ids_by_name.values().copied() {
            let kind = engine
                .get_kind(nearest_module_id.target_id.make_global(id))
                .await;

            if !kind.allow_completion() {
                continue;
            }

            if let Some(str) = str {
                let name = engine
                    .get_name(nearest_module_id.target_id.make_global(id))
                    .await;

                if fuzzy_matcher.fuzzy_match(&name, str).is_none() {
                    continue;
                }
            }

            completions.push(Completion::Module(ModuleCompletion {
                completion: target_id.make_global(id),
            }));

            inserted_ids.insert(target_id.make_global(id));
        }

        for (import_name, import) in module_imports.iter() {
            let kind = engine.get_kind(import.id).await;
            if !kind.allow_completion() {
                continue;
            }

            if let Some(str) = str
                && fuzzy_matcher.fuzzy_match(import_name, str).is_none()
            {
                continue;
            }

            completions.push(Completion::Import(ImportCompletion {
                completion_string: import_name.clone(),
                symbol: import.id,
            }));

            inserted_ids.insert(import.id);
        }

        // this and target keywords
        if let Some(str) = str {
            if fuzzy_matcher.fuzzy_match("this", str).is_none() {
                // skip adding this keyword
            } else {
                completions.push(Completion::ThisKeyword);
            }
        } else {
            completions.push(Completion::ThisKeyword);
        }

        if let Some(str) = str {
            if fuzzy_matcher.fuzzy_match("target", str).is_none() {
                // skip adding target keyword
            } else {
                completions.push(Completion::TargetKeyword);
            }
        } else {
            completions.push(Completion::TargetKeyword);
        }

        // collects all other available symbols that might not be included
        // in the current module
        if let Some(str) = str {
            completions = create_global_import_candidate(
                engine,
                target_id,
                inserted_ids,
                str,
                nearest_module_id,
                completions,
            )
            .await;
        }
    }

    completions
}

async fn create_global_import_candidate(
    engine: &TrackedEngine,
    target_id: TargetID,
    inserted: HashSet<Global<ID>>,
    str: &Interned<str>,
    current_module_id: Global<pernixc_symbol::ID>,
    completion: Vec<Completion>,
) -> Vec<Completion> {
    let candidates = Arc::new(RwLock::new(completion));
    let inserted = Arc::new(inserted);

    let global_import_ids =
        engine.get_global_import_suggestion_ids(target_id).await;

    scoped!(|handles| async {
        for chunk_id in global_import_ids
            .chunk_for_tasks()
            .map(<[pernixc_symbol::ID]>::to_vec)
        {
            let engine = engine.clone();
            let str = str.clone();
            let candidates = candidates.clone();
            let inserted = inserted.clone();

            handles.spawn(async move {
                let fuzzy_matcher = SkimMatcherV2::default();

                for id in chunk_id {
                    if inserted.contains(&target_id.make_global(id)) {
                        continue;
                    }

                    if !engine
                        .symbol_accessible(
                            current_module_id,
                            target_id.make_global(id),
                        )
                        .await
                    {
                        continue;
                    }

                    let name = engine.get_name(target_id.make_global(id)).await;

                    if fuzzy_matcher.fuzzy_match(&name, &str).is_none() {
                        continue;
                    }

                    candidates.write().await.push(Completion::Global(
                        GlobalCompletion {
                            completion: target_id.make_global(id),
                        },
                    ));
                }
            });
        }
    });

    Arc::into_inner(candidates).unwrap().into_inner()
}

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
    Encode,
    Decode,
    Query,
)]
#[value(Interned<[pernixc_symbol::ID]>)]
#[extend(name = get_global_import_suggestion_ids, by_val)]
struct GlobalImportSuggestionKey {
    target_id: TargetID,
}

#[executor(config = Config)]
async fn global_import_suggestion_executor(
    &GlobalImportSuggestionKey { target_id }: &GlobalImportSuggestionKey,
    engine: &TrackedEngine,
) -> Interned<[pernixc_symbol::ID]> {
    let root_module_id = engine.get_target_root_module_id(target_id).await;

    engine
        .query(&ModuleImportSuggestionKey {
            module_id: target_id.make_global(root_module_id),
        })
        .await
}

#[distributed_slice(PERNIX_PROGRAM)]
static GLOBAL_IMPORT_SUGGESTION_EXECUTOR: Registration<Config> =
    Registration::new::<
        GlobalImportSuggestionKey,
        GlobalImportSuggestionExecutor,
    >();

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
    Encode,
    Decode,
    Query,
)]
#[value(Interned<[pernixc_symbol::ID]>)]
struct ModuleImportSuggestionKey {
    module_id: Global<ID>,
}

#[executor(config = Config)]
async fn module_import_suggestion_executor(
    &ModuleImportSuggestionKey { module_id }: &ModuleImportSuggestionKey,
    engine: &TrackedEngine,
) -> Interned<[pernixc_symbol::ID]> {
    let members = engine.get_members(module_id).await;
    let mut results = vec![module_id.id];

    scoped!(|handles| async {
        for id in members.member_ids_by_name.values().copied() {
            // recursively collect from child modules
            let kind =
                engine.get_kind(module_id.target_id.make_global(id)).await;

            if kind == pernixc_symbol::kind::Kind::Module {
                let engine = engine.clone();

                handles.spawn(async move {
                    engine
                        .query(&ModuleImportSuggestionKey {
                            module_id: module_id.target_id.make_global(id),
                        })
                        .await
                });
            } else {
                results.push(id);
            }
        }

        while let Some(child_ids) = handles.next().await {
            results.extend(child_ids.iter().copied());
        }
    });

    engine.intern_unsized(results)
}

#[distributed_slice(PERNIX_PROGRAM)]
static MODULE_IMPORT_SUGGESTION_EXECUTOR: Registration<Config> =
    Registration::new::<
        ModuleImportSuggestionKey,
        ModuleImportSuggestionExecutor,
    >();
