use std::{path::Path, sync::Arc};

use linkme::distributed_slice;
use pernixc_extend::extend;
use pernixc_handler::{Handler, Storage};
use pernixc_hash::{DashMap, HashMap, HashSet};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_qbice::{Config, PERNIX_PROGRAM, TrackedEngine};
use pernixc_source_file::{LocalSourceID, get_stable_path_id};
use pernixc_symbol::{
    AllSymbolIDKey, ID, accessibility::Accessibility, kind::Kind, linkage,
    member::Member,
};
use pernixc_syntax::QualifiedIdentifier;
use pernixc_target::{Global, TargetID, get_invocation_arguments};
use qbice::{
    Decode, Encode, Identifiable, Query, StableHash, executor,
    program::Registration, storage::intern::Interned,
};

use crate::{
    diagnostic::{Diagnostic, SourceFileLoadFail},
    table::builder::Builder,
};

mod builder;

mod effect;
mod r#enum;
mod r#extern;
mod implements;
mod module;
mod r#trait;

/// The key to query each table node.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
)]
pub enum Key {
    /// Build the symbol table at the target root file.
    Root(TargetID),

    /// Build the symbol table for a submodule defined in another file.
    Submodule {
        /// Describes where the file is located and how it is defined.
        external_submodule: Interned<ExternalSubmodule>,

        /// The target ID for which the submodule is defined.
        target_id: TargetID,
    },
}

/// Represents a module that is defined via `public module subModule`
/// declaration syntax and its content is defined in an another file.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
    Identifiable,
)]
pub struct ExternalSubmodule {
    /// The ID assigned to the submodule.
    pub submodule_id: ID,

    /// The qualified name of the submodule.
    pub submodule_qualified_name: Arc<[Interned<str>]>,

    /// The path to the source file containing the submodule.
    pub path: Interned<Path>,

    /// The accessibility in the `public module subModule` declaration.
    pub accessibility: Accessibility<ID>,

    /// The span of the identifier in the `public module subModule`
    /// declaration.
    pub span: RelativeSpan,
}

/// A query for retrieving the [`Table`] node.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
    Query,
)]
#[value(Interned<Table>)]
pub struct TableKey(pub Key);

/// A query for retrieving only the diagnostics from a [`Table`] node.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
    Query,
)]
#[value(Interned<[Diagnostic]>)]
pub struct DiagnosticKey(pub Key);

/// A type-alias for a read-only map from symbol ID to value.
pub type ReadOnlyMap<V> = Interned<HashMap<ID, V>>;

/// A symbol table from parsing a single file.
///
/// This table contains various maps from symbol IDs to their properties,
/// such as their kinds, names, spans, members, accessibilities, and various
/// syntax extractions. This table stores only symbols defined in a single
/// source file.
#[derive(Debug, Clone, Encode, Decode, StableHash, Identifiable)]
#[allow(clippy::type_complexity)]
pub struct Table {
    /// Maps the ID of the symbol to its kind.
    ///
    /// Every symbol must have a kind, therefore, this map is guaranteed to
    /// contain every symbol ID that is present
    pub kinds: ReadOnlyMap<Kind>,

    /// Maps the ID of the symbol to its name.
    ///
    /// This is not a qualified name, but rather a simple name of the symbol.
    pub names: ReadOnlyMap<Interned<str>>,

    /// Maps the ID of the symbol to its span in the source code.
    ///
    /// The span shall point to the identifier of the symbol in the source
    /// code, not the whole declaration.
    pub spans: ReadOnlyMap<Option<RelativeSpan>>,

    /// Maps the ID of the symbol to its list of members if that particular
    /// kind of symbol can have members.
    pub members: ReadOnlyMap<Interned<Member>>,

    /// Maps the ID of the symbol to its accessibility.
    pub accessibilities: ReadOnlyMap<Accessibility<ID>>,

    /// Maps the ID of the implements member (type, function, constant) to its
    /// extracted access modifier syntax.
    pub implements_access_modifier_syntaxes:
        ReadOnlyMap<Option<pernixc_syntax::AccessModifier>>,

    /// Maps the ID of the symbol to its generic parameters declaration syntax.
    ///
    /// This represents the generic parameters `['a, T, const N: usize]`
    /// declaration of the symbol.
    pub generic_parameter_syntaxes: ReadOnlyMap<
        Option<pernixc_syntax::item::generic_parameters::GenericParameters>,
    >,

    /// Maps the ID of the symbol to its where clause declaration syntax.
    ///
    /// This represents the `where` clause of the symbol, such as
    /// `where: T: Trait, U: AnotherTrait`.
    pub where_clause_syntaxes:
        ReadOnlyMap<Option<pernixc_syntax::item::where_clause::Predicates>>,

    /// Maps the ID of the symbol to its type alias declaration syntax.
    ///
    /// This represents the `= Type` part that presents only in type alias
    /// declarations.
    pub type_alias_syntaxes: ReadOnlyMap<Option<pernixc_syntax::r#type::Type>>,

    /// Maps the ID of the symbol to its constant type annotation syntax.
    ///
    /// This represents the `const T: Type` part that presents only in
    /// constant declarations.
    pub constant_type_annotation_syntaxes:
        ReadOnlyMap<Option<pernixc_syntax::r#type::Type>>,

    /// Maps the ID of the symbol to its constant expression syntax.
    ///
    /// This represents the `= Expression` part that presents only in
    /// constant declarations.
    pub constant_expression_syntaxes:
        ReadOnlyMap<Option<pernixc_syntax::expression::Expression>>,

    /// Maps the ID of the symbol to its function signature syntax.
    ///
    /// This represents the `fn function_name(...) -> T` part that presents
    /// only in function declarations.
    pub function_signature_syntaxes: ReadOnlyMap<(
        Option<pernixc_syntax::item::function::Parameters>,
        Option<pernixc_syntax::item::function::ReturnType>,
    )>,

    /// Maps the struct ID to its body syntax, which contains a list of fields.
    pub fields_syntaxes: ReadOnlyMap<
        Option<
            pernixc_syntax::item::Body<pernixc_syntax::item::r#struct::Field>,
        >,
    >,

    /// Maps the variant ID to its associated type syntax, which is represented
    /// by the `Variant(Type)` syntax.
    pub variant_associated_type_syntaxes:
        ReadOnlyMap<Option<pernixc_syntax::r#type::Type>>,

    /// Maps the enum variant ID to the order of its declaration in the enum.
    pub variant_declaration_orders: ReadOnlyMap<usize>,

    /// Maps the module symbol ID to the list of import syntaxes that
    /// are defined in the module.
    pub import_syntaxes:
        ReadOnlyMap<Interned<[pernixc_syntax::item::module::Import]>>,

    /// Maps the implements ID to its qualified identifier syntax.
    pub implements_qualified_identifier_syntaxes:
        ReadOnlyMap<QualifiedIdentifier>,

    /// Maps the implements ID to its `final` keyword.
    pub final_keywords: ReadOnlyMap<Option<pernixc_syntax::Keyword>>,

    /// Maps the function ID to its body syntax, which contains a list of
    /// statements.
    pub function_body_syntaxes: ReadOnlyMap<
        Option<
            pernixc_syntax::item::Members<pernixc_syntax::statement::Statement>,
        >,
    >,

    /// Maps the function ID to its `do` effect syntax if it has one.
    pub function_effect_annotation_syntaxes:
        ReadOnlyMap<Option<pernixc_syntax::item::function::EffectAnnotation>>,

    /// Maps the function ID to its `unsafe` keyword if it has one.
    pub function_unsafe_keywords: ReadOnlyMap<Option<pernixc_syntax::Keyword>>,

    /// Maps the function ID to its linkage.
    pub function_linkages: ReadOnlyMap<linkage::Linkage>,

    /// Maps the symbol ID to its scope span in the source code.
    pub scope_spans: ReadOnlyMap<Option<RelativeSpan>>,

    /// Maps the module ID to the external submodules where its content is
    /// defined in. This added to the table via `public module subModule`
    /// declaration syntax.
    ///
    /// The content of the submodule is not defined in the current [`Table`],
    /// this is simply a reference to where the submodule is defined in
    pub external_submodules: ReadOnlyMap<Interned<ExternalSubmodule>>,

    /// The ID of the module that this table represents.
    pub module_id: pernixc_symbol::ID,

    /// The diagnostics that were collected while building the table.
    pub diagnostics: Interned<[Diagnostic]>,
}

/// A query for retrieving only the kinds map from a [`Table`].
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
    Query,
)]
#[value(ReadOnlyMap<Kind>)]
pub struct KindMapKey(pub Key);

#[executor(config = Config)]
async fn kind_map_executor(
    key: &KindMapKey,
    engine: &TrackedEngine,
) -> ReadOnlyMap<Kind> {
    let KindMapKey(inner_key) = key;
    let table = engine.query(&TableKey(inner_key.clone())).await;
    table.kinds.clone()
}

#[distributed_slice(PERNIX_PROGRAM)]
static KIND_MAP_EXECUTOR: Registration<Config> =
    Registration::new::<KindMapKey, KindMapExecutor>();

/// A query for retrieving only the external submodules map from a [`Table`].
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
    Query,
)]
#[value(ReadOnlyMap<Interned<ExternalSubmodule>>)]
pub struct ExternalSubmoduleMapKey(pub Key);

#[executor(config = Config)]
async fn external_submodule_map_executor(
    key: &ExternalSubmoduleMapKey,
    engine: &TrackedEngine,
) -> ReadOnlyMap<Interned<ExternalSubmodule>> {
    let ExternalSubmoduleMapKey(inner_key) = key;
    let table = engine.query(&TableKey(inner_key.clone())).await;
    table.external_submodules.clone()
}

#[distributed_slice(PERNIX_PROGRAM)]
static EXTERNAL_SUBMODULE_MAP_EXECUTOR: Registration<Config> =
    Registration::new::<ExternalSubmoduleMapKey, ExternalSubmoduleMapExecutor>(
    );

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum ModuleKind {
    Root,
    Submodule {
        submodule_id: ID,
        submodule_qualified_name: Arc<[Interned<str>]>,
        accessibility: Accessibility<ID>,
        span: RelativeSpan,
    },
}

#[executor(config = Config)]
#[allow(clippy::too_many_lines)]
async fn table_executor(
    key: &TableKey,
    engine: &TrackedEngine,
) -> Interned<Table> {
    let TableKey(inner_key) = key;
    let (
        syntax_tree_result,
        token_tree_result,
        source_file,
        target_id,
        module_kind,
        is_root,
    ) = match inner_key {
        Key::Root(target_id) => {
            let invocation_arguments =
                engine.get_invocation_arguments(*target_id).await;

            (
                engine
                    .query(&pernixc_syntax::Key {
                        path: engine.intern_unsized(
                            invocation_arguments.command.input().file.clone(),
                        ),
                        target_id: *target_id,
                    })
                    .await,
                engine
                    .query(&pernixc_lexical::Key {
                        path: engine.intern_unsized(
                            invocation_arguments.command.input().file.clone(),
                        ),
                        target_id: *target_id,
                    })
                    .await,
                engine
                    .query(&pernixc_source_file::Key {
                        path: engine.intern_unsized(
                            invocation_arguments.command.input().file.clone(),
                        ),
                        target_id: *target_id,
                    })
                    .await,
                *target_id,
                ModuleKind::Root,
                true,
            )
        }

        Key::Submodule { external_submodule, target_id } => (
            engine
                .query(&pernixc_syntax::Key {
                    path: external_submodule.path.clone(),
                    target_id: *target_id,
                })
                .await,
            engine
                .query(&pernixc_lexical::Key {
                    path: external_submodule.path.clone(),
                    target_id: *target_id,
                })
                .await,
            engine
                .query(&pernixc_source_file::Key {
                    path: external_submodule.path.clone(),
                    target_id: *target_id,
                })
                .await,
            *target_id,
            ModuleKind::Submodule {
                submodule_id: external_submodule.submodule_id,
                submodule_qualified_name: external_submodule
                    .submodule_qualified_name
                    .clone(),
                accessibility: external_submodule.accessibility,
                span: external_submodule.span,
            },
            false,
        ),
    };

    let storage = Storage::default();

    // Handle load errors by creating diagnostics and using empty content
    let tree = match syntax_tree_result {
        Ok(tree) => Some(tree),
        Err(e) => {
            // Create a diagnostic for the load failure
            let (path, submodule_span) = match &key.0 {
                Key::Root(target_id) => {
                    let invocation_arguments =
                        engine.get_invocation_arguments(*target_id).await;
                    (
                        engine.intern_unsized(
                            invocation_arguments.command.input().file.clone(),
                        ),
                        None,
                    )
                }
                Key::Submodule { external_submodule, .. } => (
                    external_submodule.path.clone(),
                    Some(external_submodule.span),
                ),
            };

            storage.receive(Diagnostic::SourceFileLoadFail(
                SourceFileLoadFail {
                    error_message: e.to_string(),
                    path,
                    submodule_span,
                },
            ));

            // Continue with empty content
            None
        }
    };

    let builder = Arc::new(Builder::new(
        engine.clone(),
        storage,
        source_file.ok(),
        token_tree_result.ok().map(|x| x.0),
        target_id,
        is_root,
    ));

    let module_content = tree.and_then(|t| t.0);
    let scope_span =
        module_content.as_ref().map(pernixc_source_file::SourceElement::span);

    let (join_handler, module_id) =
        builder.create_module(module_content, scope_span, module_kind).await;

    join_handler.await.expect("failed to join handler");

    let Ok(builder) = Arc::try_unwrap(builder) else {
        panic!("some threads are not joined")
    };

    builder.into_table(module_id)
}

#[distributed_slice(PERNIX_PROGRAM)]
static TABLE_EXECUTOR: Registration<Config> =
    Registration::new::<TableKey, TableExecutor>();

#[executor(config = Config)]
async fn diagnostic_executor(
    key: &DiagnosticKey,
    engine: &TrackedEngine,
) -> Interned<[Diagnostic]> {
    // Query the table and return only its diagnostics field
    let DiagnosticKey(inner_key) = key;
    let table = engine.query(&TableKey(inner_key.clone())).await;

    table.diagnostics.clone()
}

#[distributed_slice(PERNIX_PROGRAM)]
static DIAGNOSTIC_EXECUTOR: Registration<Config> =
    Registration::new::<DiagnosticKey, DiagnosticExecutor>();

/// Used to identify in which table node the symbol is defined.
#[derive(Debug, Clone, Encode, Decode, StableHash)]
pub struct Map {
    /// Maps the symbol ID to the key where the symbol's information is stored.
    ///
    /// The value is stored as [`Option<Arc<ExternalSubmodule>>`] where the
    /// `None` value indicates that the symbol is defined in the root file
    /// whereas the `Some` value indicates that the symbol is defined in an
    /// external submodule.
    pub keys_by_symbol_id:
        Interned<HashMap<ID, Option<Interned<ExternalSubmodule>>>>,

    /// Maps the source file ID to its path and the external submodule (if it
    /// is).
    #[allow(clippy::type_complexity)]
    pub paths_by_source_id: Interned<
        HashMap<
            LocalSourceID,
            (Interned<Path>, Option<Interned<ExternalSubmodule>>),
        >,
    >,

    /// Paths that failed to resolve to a source file.
    #[allow(clippy::type_complexity)]
    pub failed_paths: Interned<
        HashSet<(Interned<Path>, Option<Interned<ExternalSubmodule>>)>,
    >,
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
    Encode,
    Decode,
    StableHash,
    Query,
)]
#[value(Map)]
pub struct MapKey(pub TargetID);

#[executor(config = Config)]
#[allow(clippy::too_many_lines)]
async fn map_executor(key: &MapKey, engine: &TrackedEngine) -> Map {
    #[allow(
        clippy::needless_pass_by_value,
        clippy::type_complexity,
        clippy::manual_async_fn
    )]
    fn traverse_table<'x>(
        engine: &'x TrackedEngine,
        table_key: &'x Key,
        keys_by_symbol_id: Arc<
            DashMap<ID, Option<Interned<ExternalSubmodule>>>,
        >,
        paths_by_source_id: Arc<
            DashMap<
                LocalSourceID,
                (Interned<Path>, Option<Interned<ExternalSubmodule>>),
            >,
        >,
        failed_paths: Arc<
            DashMap<(Interned<Path>, Option<Interned<ExternalSubmodule>>), ()>,
        >,
        current_external_submodule: Option<Interned<ExternalSubmodule>>,
    ) -> impl std::future::Future<Output = ()> + Send + 'x {
        async move {
            // Query the table for this key
            let kinds = engine.query(&KindMapKey(table_key.clone())).await;

            let external_submodules =
                engine.query(&ExternalSubmoduleMapKey(table_key.clone())).await;

            // Get the path and target_id for this table
            let (path, current_target_id) = match &table_key {
                Key::Root(target_id) => {
                    let invocation_arguments =
                        engine.get_invocation_arguments(*target_id).await;
                    (
                        engine.intern_unsized::<Path, _>(
                            invocation_arguments.command.input().file.clone(),
                        ),
                        *target_id,
                    )
                }
                Key::Submodule { external_submodule, target_id } => {
                    (external_submodule.path.clone(), *target_id)
                }
            };

            // Calculate the source file ID for this path
            if let Ok(source_file_id) =
                engine.get_stable_path_id(path.clone()).await
            {
                // Add the path to source file mapping
                paths_by_source_id.insert(
                    source_file_id,
                    (path, current_external_submodule.clone()),
                );
            } else {
                failed_paths
                    .insert((path, current_external_submodule.clone()), ());
            }

            // Add all symbols from this table to the symbol map
            for (symbol_id, _kind) in kinds.iter() {
                keys_by_symbol_id
                    .insert(*symbol_id, current_external_submodule.clone());
            }

            // Recursively traverse external submodules

            let mut handles = Vec::new();

            for i in external_submodules.iter() {
                let external_submodule = i.1.clone();
                let engine = engine.clone();

                let submodule_key = Key::Submodule {
                    external_submodule: external_submodule.clone(),
                    target_id: current_target_id,
                };

                let keys_by_symbol_id = keys_by_symbol_id.clone();
                let paths_by_source_id = paths_by_source_id.clone();
                let failed_paths = failed_paths.clone();

                handles.push(tokio::spawn(async move {
                    traverse_table(
                        &engine,
                        &submodule_key,
                        keys_by_symbol_id,
                        paths_by_source_id,
                        failed_paths,
                        Some(external_submodule),
                    )
                    .await;
                }));
            }

            // Wait for all traversals to complete
            for handle in handles {
                handle.await.unwrap();
            }
        }
    }

    let MapKey(target_id) = key;

    let keys_by_symbol_id = Arc::new(DashMap::default());
    let paths_by_source_id = Arc::new(DashMap::default());
    let failed_paths = Arc::new(DashMap::default());

    // Start traversal from the root
    traverse_table(
        engine,
        &Key::Root(*target_id),
        keys_by_symbol_id.clone(),
        paths_by_source_id.clone(),
        failed_paths.clone(),
        None,
    )
    .await;

    Map {
        keys_by_symbol_id: engine.intern(
            Arc::try_unwrap(keys_by_symbol_id)
                .expect("Failed to convert DashMap to ReadOnlyView")
                .into_iter()
                .collect(),
        ),
        paths_by_source_id: engine.intern(
            Arc::try_unwrap(paths_by_source_id)
                .expect("Failed to convert DashMap to ReadOnlyView")
                .into_iter()
                .collect(),
        ),
        failed_paths: engine.intern(
            Arc::try_unwrap(failed_paths)
                .expect("Failed to convert DashMap to ReadOnlyView")
                .into_iter()
                .map(|x| x.0)
                .collect(),
        ),
    }
}

#[distributed_slice(PERNIX_PROGRAM)]
static MAP_EXECUTOR: Registration<Config> =
    Registration::new::<MapKey, MapExecutor>();

/// Retrieves all symbol IDs for the given target ID.
#[executor(config = Config)]
async fn all_symbol_ids_executor(
    key: &AllSymbolIDKey,
    engine: &TrackedEngine,
) -> Arc<[ID]> {
    let map = engine.query(&MapKey(key.target_id)).await;
    map.keys_by_symbol_id.keys().copied().collect()
}

#[distributed_slice(PERNIX_PROGRAM)]
static ALL_SYMBOL_ID_EXECUTOR: Registration<Config> =
    Registration::new::<AllSymbolIDKey, AllSymbolIdsExecutor>();

/// Gets the table node where the information of the given symbol ID is stored.
#[extend]
pub async fn get_table_of_symbol(
    self: &TrackedEngine,
    id: Global<ID>,
) -> Interned<Table> {
    let map = self.query(&MapKey(id.target_id)).await;

    let node_key = map
        .keys_by_symbol_id
        .get(&id.id)
        .unwrap_or_else(|| panic!("invalid symbol ID: {:?}", id.id))
        .as_ref()
        .map_or_else(
            || Key::Root(id.target_id),
            |x| Key::Submodule {
                external_submodule: x.clone(),
                target_id: id.target_id,
            },
        );

    self.query(&TableKey(node_key)).await
}
