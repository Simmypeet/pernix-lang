//! Contains the logic for building the symbol table from the syntax tree.

use std::{
    collections::hash_map,
    hash::{Hash, Hasher},
    path::Path,
    sync::Arc,
};

use flexstr::{FlexStr, SharedStr};
use pernixc_extend::extend;
use pernixc_handler::{Handler, Storage};
use pernixc_hash::{DashMap, HashMap, HashSet, ReadOnlyView};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::{
    runtime::{
        executor::{CyclicError, Future},
        persistence::serde::DynamicRegistry,
    },
    TrackedEngine,
};
use pernixc_serialize::{
    de::Deserializer, ser::Serializer, Deserialize, Serialize,
};
use pernixc_source_file::{calculate_path_id, SourceFile};
use pernixc_stable_hash::StableHash;
use pernixc_syntax::item::{
    module::Member as ModuleMemberSyn, r#trait::Member as TraitMemberSyn,
};
use pernixc_target::{
    get_invocation_arguments, get_target_seed, Global, TargetID,
};
use pernixc_tokio::scoped;
use tokio::task::JoinHandle;

use crate::{
    accessibility::Accessibility,
    diagnostic::{
        Diagnostic, ItemRedefinition, RecursiveFileRequest, SourceFileLoadFail,
    },
    kind::Kind,
    member::Member,
};

pub mod accessibility;
pub mod diagnostic;
pub mod import;
pub mod kind;
pub mod member;
pub mod name;
pub mod parent;
pub mod source_map;
pub mod span;
pub mod syntax;

#[cfg(any(test, feature = "arbitrary"))]
pub mod arbitrary;

/// Registers all the required executors to run the queries.
pub fn register_executors(
    executor: &mut pernixc_query::runtime::executor::Registry,
) {
    executor.register(Arc::new(accessibility::Executor));
    executor.register(Arc::new(accessibility::MemberIsMoreAccessibleExecutor));

    executor.register(Arc::new(kind::Executor));
    executor.register(Arc::new(kind::AllSymbolOfKindExecutor));

    executor.register(Arc::new(member::Executor));

    executor.register(Arc::new(name::Executor));

    executor.register(Arc::new(parent::IntermediateExecutor));
    executor.register(Arc::new(parent::Executor));

    executor.register(Arc::new(span::Executor));

    executor.register(Arc::new(source_map::FilePathExecutor));

    executor.register(Arc::new(syntax::ImportExecutor));

    executor.register(Arc::new(import::WithDiagnosticExecutor));
    executor.register(Arc::new(import::Executor));
    executor.register(Arc::new(import::DiagnosticExecutor));

    executor.register(Arc::new(diagnostic::RenderedExecutor));

    executor.register(Arc::new(TableExecutor));
    executor.register(Arc::new(DiagnosticExecutor));
    executor.register(Arc::new(KindMapExecutor));
    executor.register(Arc::new(ExternalSubmoduleMapExecutor));
    executor.register(Arc::new(MapExecutor));
}

/// Registers all the necessary runtime information for the query engine.
pub fn register_serde<
    S: Serializer<Registry>,
    D: Deserializer<Registry>,
    Registry: DynamicRegistry<S, D> + Send + Sync,
>(
    serde_registry: &mut Registry,
) where
    S::Error: Send + Sync,
{
    serde_registry.register::<accessibility::Key>();
    serde_registry.register::<accessibility::MemberIsMoreAaccessibleKey>();

    serde_registry.register::<kind::Key>();
    serde_registry.register::<kind::AllSymbolOfKindKey>();

    serde_registry.register::<member::Key>();

    serde_registry.register::<name::Key>();

    serde_registry.register::<parent::IntermediateKey>();
    serde_registry.register::<parent::Key>();

    serde_registry.register::<span::Key>();

    serde_registry.register::<source_map::FilePathKey>();

    serde_registry.register::<syntax::ImportKey>();

    serde_registry.register::<import::WithDiagnosticKey>();
    serde_registry.register::<import::Key>();
    serde_registry.register::<import::DiagnosticKey>();

    serde_registry.register::<diagnostic::RenderedKey>();

    serde_registry.register::<TableKey>();
    serde_registry.register::<DiagnosticKey>();
    serde_registry.register::<KindMapKey>();
    serde_registry.register::<ExternalSubmoduleMapKey>();
    serde_registry.register::<MapKey>();
}

/// Registers the keys that should be skipped during serialization and
/// deserialization in the query engine's persistence layer
pub const fn skip_persistence(
    _persistence: &mut pernixc_query::runtime::persistence::Persistence,
) {
}

/// Represents a unique identifier for the symbols in the compilation target.
/// This ID is only unique within the context of a single target. If wants to
/// use identifier across multiple targets, it should be combined with the
/// [`Global`]
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Default,
    Serialize,
    Deserialize,
    StableHash,
)]
pub struct ID(pub u64);

/// A kind of ID used to unique identify a symbol inside a particular global
/// symbol.
///
/// For example, we can use this struct to uniquely identify a generic parameter
/// inside a particular function symbol. Where the [`Self::parent_id`] is the ID
/// of the function symbol and the [`Self::id`] is the ID of the generic
/// parameter.
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
    derive_new::new,
)]
pub struct MemberID<InnerID> {
    /// The parent ID of the member, which is the ID of the symbol that
    /// contains this member.
    pub parent_id: Global<ID>,

    /// The ID of the member.
    pub id: InnerID,
}

/// The key to query each table node.
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
pub enum Key {
    /// Build the symbol table at the target root file.
    Root(TargetID),

    /// Build the symbol table for a submodule defined in another file.
    Submodule {
        /// Describes where the file is located and how it is defined.
        external_submodule: Arc<ExternalSubmodule>,

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
    Serialize,
    Deserialize,
    StableHash,
)]
pub struct ExternalSubmodule {
    /// The ID assigned to the submodule.
    pub submodule_id: ID,

    /// The qualified name of the submodule.
    pub submodule_qualified_name: Arc<[SharedStr]>,

    /// The path to the source file containing the submodule.
    pub path: Arc<Path>,

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
    Serialize,
    Deserialize,
    StableHash,
    pernixc_query::Key,
)]
#[value(Arc<Table>)]
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
    Serialize,
    Deserialize,
    StableHash,
    pernixc_query::Key,
)]
#[value(Arc<HashSet<Diagnostic>>)]
pub struct DiagnosticKey(pub Key);

/// A symbol table from parsing a single file.
#[derive(Debug, Clone, Serialize, Deserialize, StableHash)]
#[allow(clippy::type_complexity)]
pub struct Table {
    /// Maps the ID of the symbol to its kind.
    ///
    /// Every symbol must have a kind, therefore, this map is guaranteed to
    /// contain every symbol ID that is present
    pub kinds: Arc<ReadOnlyView<ID, Kind>>,

    /// Maps the ID of the symbol to its name.
    ///
    /// This is not a qualified name, but rather a simple name of the symbol.
    pub names: Arc<ReadOnlyView<ID, SharedStr>>,

    /// Maps the ID of the symbol to its span in the source code.
    ///
    /// The span shall point to the identifier of the symbol in the source
    /// code, not the whole declaration.
    pub spans: Arc<ReadOnlyView<ID, Option<RelativeSpan>>>,

    /// Maps the ID of the symbol to its list of members if that particular
    /// kind of symbol can have members.
    pub members: Arc<ReadOnlyView<ID, Arc<Member>>>,

    /// Maps the ID of the symbol to its accessibility.
    pub accessibilities: Arc<ReadOnlyView<ID, Accessibility<ID>>>,

    /// Maps the ID of the symbol to its generic parameters declaration syntax.
    ///
    /// This represents the generic parameters `['a, T, const N: usize]`
    /// declaration of the symbol.
    pub generic_parameter_syntaxes: Arc<
        ReadOnlyView<
            ID,
            Option<pernixc_syntax::item::generic_parameters::GenericParameters>,
        >,
    >,

    /// Maps the ID of the symbol to its where clause declaration syntax.
    ///
    /// This represents the `where` clause of the symbol, such as
    /// `where: T: Trait, U: AnotherTrait`.
    pub where_clause_syntaxes: Arc<
        ReadOnlyView<
            ID,
            Option<pernixc_syntax::item::where_clause::Predicates>,
        >,
    >,

    /// Maps the ID of the symbol to its type alias declaration syntax.
    ///
    /// This represents the `= Type` part that presents only in type alias
    /// declarations.
    pub type_alias_syntaxes:
        Arc<ReadOnlyView<ID, Option<pernixc_syntax::r#type::Type>>>,

    /// Maps the ID of the symbol to its constant type annotation syntax.
    ///
    /// This represents the `const T: Type` part that presents only in
    /// constant declarations.
    pub constant_type_annotation_syntaxes:
        Arc<ReadOnlyView<ID, Option<pernixc_syntax::r#type::Type>>>,

    /// Maps the ID of the symbol to its constant expression syntax.
    ///
    /// This represents the `= Expression` part that presents only in
    /// constant declarations.
    pub constant_expression_syntaxes:
        Arc<ReadOnlyView<ID, Option<pernixc_syntax::expression::Expression>>>,

    /// Maps the ID of the symbol to its function signature syntax.
    ///
    /// This represents the `fn function_name(...) -> T` part that presents
    /// only in function declarations.
    pub function_signature_syntaxes: Arc<
        ReadOnlyView<
            ID,
            (
                Option<pernixc_syntax::item::function::Parameters>,
                Option<pernixc_syntax::item::function::ReturnType>,
            ),
        >,
    >,

    /// Maps the struct ID to its body syntax, which contains a list of fields.
    pub fields_syntaxes: Arc<
        ReadOnlyView<
            ID,
            Option<
                pernixc_syntax::item::Body<
                    pernixc_syntax::item::r#struct::Field,
                >,
            >,
        >,
    >,

    /// Maps the variant ID to its associated type syntax, which is represented
    /// by the `Variant(Type)` syntax.
    pub variant_associated_type_syntaxes:
        Arc<ReadOnlyView<ID, Option<pernixc_syntax::r#type::Type>>>,

    /// Maps the module symbol ID to the list of import syntaxes that
    /// are defined in the module.
    pub import_syntaxes:
        Arc<ReadOnlyView<ID, Arc<[pernixc_syntax::item::module::Import]>>>,

    /// Maps the module ID to the external submodules where its content is
    /// defined in. This added to the table via `public module subModule`
    /// declaration syntax.
    ///
    /// The content of the submodule is not defined in the current [`Table`],
    /// this is simply a reference to where the submodule is defined in
    pub external_submodules: Arc<ReadOnlyView<ID, Arc<ExternalSubmodule>>>,

    /// The diagnostics that were collected while building the table.
    pub diagnostics: Arc<HashSet<Diagnostic>>,
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
    Serialize,
    Deserialize,
    StableHash,
    pernixc_query::Key,
)]
#[value(Arc<ReadOnlyView<ID, Kind>>)]
pub struct KindMapKey(pub Key);

#[pernixc_query::executor(key(KindMapKey), name(KindMapExecutor))]
pub async fn kind_map_executor(
    KindMapKey(key): &KindMapKey,
    engine: &TrackedEngine,
) -> Result<Arc<ReadOnlyView<ID, Kind>>, CyclicError> {
    let table = engine.query(&TableKey(key.clone())).await?;
    Ok(table.kinds.clone())
}

/// A query for retrieving only the external submodules map from a [`Table`].
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
    pernixc_query::Key,
)]
#[value(Arc<ReadOnlyView<ID, Arc<ExternalSubmodule>>>)]
pub struct ExternalSubmoduleMapKey(pub Key);

#[pernixc_query::executor(
    key(ExternalSubmoduleMapKey),
    name(ExternalSubmoduleMapExecutor)
)]
pub async fn external_submodule_map_executor(
    ExternalSubmoduleMapKey(key): &ExternalSubmoduleMapKey,
    engine: &TrackedEngine,
) -> Result<Arc<ReadOnlyView<ID, Arc<ExternalSubmodule>>>, CyclicError> {
    let table = engine.query(&TableKey(key.clone())).await?;
    Ok(table.external_submodules.clone())
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum ModuleKind {
    Root,
    Submodule {
        submodule_id: ID,
        submodule_qualified_name: Arc<[SharedStr]>,
        accessibility: Accessibility<ID>,
        span: RelativeSpan,
    },
}

struct TableContext {
    engine: TrackedEngine,
    storage: Storage<Diagnostic>,
    target_id: TargetID,

    kinds: DashMap<ID, Kind>,
    names: DashMap<ID, SharedStr>,
    spans: DashMap<ID, Option<RelativeSpan>>,
    members: DashMap<ID, Arc<Member>>,
    accessibilities: DashMap<ID, Accessibility<ID>>,

    external_submodules: DashMap<ID, Arc<ExternalSubmodule>>,

    generic_parameter_syntaxes: DashMap<
        ID,
        Option<pernixc_syntax::item::generic_parameters::GenericParameters>,
    >,
    where_clause_syntaxes:
        DashMap<ID, Option<pernixc_syntax::item::where_clause::Predicates>>,
    type_alias_syntaxes: DashMap<ID, Option<pernixc_syntax::r#type::Type>>,
    constant_type_annotation_syntaxes:
        DashMap<ID, Option<pernixc_syntax::r#type::Type>>,
    constant_expression_syntaxes:
        DashMap<ID, Option<pernixc_syntax::expression::Expression>>,
    function_signature_syntaxes: DashMap<
        ID,
        (
            Option<pernixc_syntax::item::function::Parameters>,
            Option<pernixc_syntax::item::function::ReturnType>,
        ),
    >,
    fields_syntaxes: DashMap<
        ID,
        Option<
            pernixc_syntax::item::Body<pernixc_syntax::item::r#struct::Field>,
        >,
    >,
    variant_associated_type_syntaxes:
        DashMap<ID, Option<pernixc_syntax::r#type::Type>>,
    import_syntaxes: DashMap<ID, Arc<[pernixc_syntax::item::module::Import]>>,

    is_root: bool,
}

#[pernixc_query::executor(key(TableKey), name(TableExecutor))]
#[allow(clippy::too_many_lines)]
pub async fn table_executor(
    TableKey(key): &TableKey,
    engine: &TrackedEngine,
) -> Result<Arc<Table>, pernixc_query::runtime::executor::CyclicError> {
    let (tree_result, target_id, module_kind, is_root) = match key {
        Key::Root(target_id) => {
            let invocation_arguments =
                engine.get_invocation_arguments(*target_id).await;

            (
                engine
                    .query(&pernixc_syntax::Key {
                        path: invocation_arguments
                            .command
                            .input()
                            .file
                            .clone()
                            .into(),
                        target_id: *target_id,
                    })
                    .await?,
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
                .await?,
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
    let tree = match tree_result {
        Ok(tree) => Some(tree),
        Err(e) => {
            // Create a diagnostic for the load failure
            let (path, submodule_span) = match key {
                Key::Root(target_id) => {
                    let invocation_arguments =
                        engine.get_invocation_arguments(*target_id).await;
                    (
                        Arc::from(
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

    let context = Arc::new(TableContext {
        engine: engine.clone(),
        storage,
        target_id,

        kinds: DashMap::default(),
        names: DashMap::default(),
        spans: DashMap::default(),
        members: DashMap::default(),
        accessibilities: DashMap::default(),

        external_submodules: DashMap::default(),

        generic_parameter_syntaxes: DashMap::default(),
        where_clause_syntaxes: DashMap::default(),
        type_alias_syntaxes: DashMap::default(),
        constant_type_annotation_syntaxes: DashMap::default(),
        constant_expression_syntaxes: DashMap::default(),
        function_signature_syntaxes: DashMap::default(),
        fields_syntaxes: DashMap::default(),
        variant_associated_type_syntaxes: DashMap::default(),
        import_syntaxes: DashMap::default(),

        is_root,
    });

    context
        .create_module(tree.and_then(|t| t.0), module_kind)
        .await
        .await
        .expect("failed to join task");

    let Ok(context) = Arc::try_unwrap(context) else {
        panic!("some threads are not joined")
    };

    Ok(Arc::new(Table {
        kinds: Arc::new(context.kinds.into_read_only()),
        names: Arc::new(context.names.into_read_only()),
        spans: Arc::new(context.spans.into_read_only()),
        members: Arc::new(context.members.into_read_only()),
        accessibilities: Arc::new(context.accessibilities.into_read_only()),

        // syntax extractions
        generic_parameter_syntaxes: Arc::new(
            context.generic_parameter_syntaxes.into_read_only(),
        ),
        where_clause_syntaxes: Arc::new(
            context.where_clause_syntaxes.into_read_only(),
        ),
        type_alias_syntaxes: Arc::new(
            context.type_alias_syntaxes.into_read_only(),
        ),
        constant_type_annotation_syntaxes: Arc::new(
            context.constant_type_annotation_syntaxes.into_read_only(),
        ),
        constant_expression_syntaxes: Arc::new(
            context.constant_expression_syntaxes.into_read_only(),
        ),
        function_signature_syntaxes: Arc::new(
            context.function_signature_syntaxes.into_read_only(),
        ),
        fields_syntaxes: Arc::new(context.fields_syntaxes.into_read_only()),
        variant_associated_type_syntaxes: Arc::new(
            context.variant_associated_type_syntaxes.into_read_only(),
        ),
        import_syntaxes: Arc::new(context.import_syntaxes.into_read_only()),

        external_submodules: Arc::new(
            context.external_submodules.into_read_only(),
        ),
        diagnostics: Arc::new(context.storage.into_vec().into_iter().collect()),
    }))
}

#[pernixc_query::executor(key(DiagnosticKey), name(DiagnosticExecutor))]
pub async fn diagnostic_executor(
    DiagnosticKey(key): &DiagnosticKey,
    engine: &TrackedEngine,
) -> Result<
    Arc<HashSet<Diagnostic>>,
    pernixc_query::runtime::executor::CyclicError,
> {
    // Query the table and return only its diagnostics field
    let table = engine.query(&TableKey(key.clone())).await?;

    Ok(table.diagnostics.clone())
}

#[derive(Debug, Clone, PartialEq, Eq, typed_builder::TypedBuilder)]
#[allow(clippy::option_option)]
struct Entry {
    pub identifier: pernixc_syntax::Identifier,
    pub kind: kind::Kind,

    #[builder(default, setter(strip_option))]
    pub member: Option<Arc<member::Member>>,

    #[builder(default, setter(strip_option))]
    pub accessibility: Option<Option<pernixc_syntax::AccessModifier>>,

    #[builder(default, setter(strip_option))]
    pub generic_parameters_syntax: Option<
        Option<pernixc_syntax::item::generic_parameters::GenericParameters>,
    >,

    #[builder(default, setter(strip_option))]
    pub where_clause_syntax:
        Option<Option<pernixc_syntax::item::where_clause::Predicates>>,

    #[builder(default, setter(strip_option))]
    pub type_alias_syntax: Option<Option<pernixc_syntax::r#type::Type>>,

    #[builder(default, setter(strip_option))]
    pub constant_type_annotation_syntax:
        Option<Option<pernixc_syntax::r#type::Type>>,

    #[builder(default, setter(strip_option))]
    pub constant_expression_syntax:
        Option<Option<pernixc_syntax::expression::Expression>>,

    #[builder(default, setter(strip_option))]
    pub function_signature_syntax: Option<(
        Option<pernixc_syntax::item::function::Parameters>,
        Option<pernixc_syntax::item::function::ReturnType>,
    )>,

    #[builder(default, setter(strip_option))]
    pub fields_syntax: Option<
        Option<
            pernixc_syntax::item::Body<pernixc_syntax::item::r#struct::Field>,
        >,
    >,

    #[builder(default, setter(strip_option))]
    pub variant_associated_type_syntax:
        Option<Option<pernixc_syntax::r#type::Type>>,
}

impl TableContext {
    async fn add_symbol_entry(&self, id: ID, parent_id: ID, entry: Entry) {
        Self::insert_to_table(&self.names, id, entry.identifier.kind.0);
        Self::insert_to_table(&self.spans, id, Some(entry.identifier.span));
        Self::insert_to_table(&self.kinds, id, entry.kind);

        if let Some(accessibility) = entry.accessibility {
            let accessibility = match accessibility {
                Some(pernixc_syntax::AccessModifier::Private(_)) => {
                    Accessibility::Scoped(parent_id)
                }
                Some(pernixc_syntax::AccessModifier::Internal(_)) => {
                    Accessibility::Scoped(
                        self.engine
                            .get_target_root_module_id(self.target_id)
                            .await,
                    )
                }
                Some(pernixc_syntax::AccessModifier::Public(_)) | None => {
                    Accessibility::Public
                }
            };
            Self::insert_to_table(&self.accessibilities, id, accessibility);
        }

        if let Some(member) = entry.member {
            Self::insert_to_table(&self.members, id, member);
        }

        if let Some(generic_parameters_syntax) = entry.generic_parameters_syntax
        {
            Self::insert_to_table(
                &self.generic_parameter_syntaxes,
                id,
                generic_parameters_syntax,
            );
        }

        if let Some(where_clause_syntax) = entry.where_clause_syntax {
            Self::insert_to_table(
                &self.where_clause_syntaxes,
                id,
                where_clause_syntax,
            );
        }

        if let Some(type_alias_syntax) = entry.type_alias_syntax {
            Self::insert_to_table(
                &self.type_alias_syntaxes,
                id,
                type_alias_syntax,
            );
        }

        if let Some(constant_type_annotation_syntax) =
            entry.constant_type_annotation_syntax
        {
            Self::insert_to_table(
                &self.constant_type_annotation_syntaxes,
                id,
                constant_type_annotation_syntax,
            );
        }

        if let Some(constant_expression_syntax) =
            entry.constant_expression_syntax
        {
            Self::insert_to_table(
                &self.constant_expression_syntaxes,
                id,
                constant_expression_syntax,
            );
        }

        if let Some(function_signature_syntax) = entry.function_signature_syntax
        {
            Self::insert_to_table(
                &self.function_signature_syntaxes,
                id,
                function_signature_syntax,
            );
        }

        if let Some(fields_syntax) = entry.fields_syntax {
            Self::insert_to_table(&self.fields_syntaxes, id, fields_syntax);
        }

        if let Some(variant_associated_type_syntax) =
            entry.variant_associated_type_syntax
        {
            Self::insert_to_table(
                &self.variant_associated_type_syntaxes,
                id,
                variant_associated_type_syntax,
            );
        }
    }

    fn insert_to_table<V>(map: &DashMap<ID, V>, id: ID, value: V) {
        assert!(
            map.insert(id, value).is_none(),
            "Possible hash collision detected, please try re-building the \
             project and delete the incremental directory if the problem \
             persists."
        );
    }

    async fn create_module(
        self: &Arc<Self>,
        module_content: Option<pernixc_syntax::item::module::Content>,
        module_kind: ModuleKind,
    ) -> JoinHandle<()> {
        // extract the information about the module
        let (accessibility, current_module_id, module_qualified_name, span) =
            match module_kind {
                ModuleKind::Root => {
                    let invocation_arguments = self
                        .engine
                        .get_invocation_arguments(self.target_id)
                        .await;

                    let target_name =
                        invocation_arguments.command.input().target_name();

                    let current_module_id = self
                        .engine
                        .get_target_root_module_id(self.target_id)
                        .await;

                    let module_qualified_name = Arc::from([target_name]);

                    (
                        Accessibility::Public,
                        current_module_id,
                        module_qualified_name,
                        None,
                    )
                }
                ModuleKind::Submodule {
                    submodule_id,
                    submodule_qualified_name,
                    accessibility,
                    span,
                } => (
                    accessibility,
                    submodule_id,
                    submodule_qualified_name,
                    Some(span),
                ),
            };

        let context = self.clone();

        tokio::spawn(async move {
            let mut member_builder = MemberBuilder::new(
                current_module_id,
                module_qualified_name,
                context.target_id,
            );
            let mut imports = Vec::new();

            if let Some(module_content) = module_content {
                context
                    .handle_module_content(
                        module_content,
                        &mut member_builder,
                        &mut imports,
                    )
                    .await;
            }

            Self::insert_to_table(
                &context.names,
                current_module_id,
                member_builder.symbol_qualified_name.last().cloned().unwrap(),
            );
            Self::insert_to_table(
                &context.accessibilities,
                current_module_id,
                accessibility,
            );
            Self::insert_to_table(
                &context.kinds,
                current_module_id,
                Kind::Module,
            );
            Self::insert_to_table(&context.spans, current_module_id, span);
            Self::insert_to_table(
                &context.members,
                current_module_id,
                Arc::new(Member {
                    member_ids_by_name: member_builder.member_ids_by_name,
                    redefinitions: member_builder.redefinitions,
                }),
            );
            Self::insert_to_table(
                &context.import_syntaxes,
                current_module_id,
                imports.into(),
            );

            context.storage.as_vec_mut().extend(
                member_builder
                    .redefinition_errors
                    .into_iter()
                    .map(Diagnostic::ItemRedefinition),
            );
        })
    }

    #[allow(clippy::manual_async_fn)]
    fn handle_module_member<'x>(
        self: &'x Arc<Self>,
        module_syntax: &'x pernixc_syntax::item::module::Module,
        member_builder: &'x mut MemberBuilder,
    ) -> impl std::future::Future<Output = Option<JoinHandle<()>>> + Send + 'x
    {
        async move {
            self.handle_module_member_internal(module_syntax, member_builder)
                .await
        }
    }

    #[allow(clippy::too_many_lines)]
    async fn handle_module_member_internal(
        self: &Arc<Self>,
        module_syntax: &pernixc_syntax::item::module::Module,
        member_builder: &mut MemberBuilder,
    ) -> Option<JoinHandle<()>> {
        let signature = module_syntax.signature()?;
        let identifier = signature.identifier()?;

        let access_modifier = module_syntax.access_modifier();
        let next_submodule_qualified_name = member_builder
            .symbol_qualified_name
            .iter()
            .cloned()
            .chain(std::iter::once(identifier.kind.0.clone()))
            .collect::<Arc<[_]>>();

        let accessibility = match access_modifier {
            Some(pernixc_syntax::AccessModifier::Private(_)) => {
                Accessibility::Scoped(member_builder.symbol_id)
            }
            Some(pernixc_syntax::AccessModifier::Internal(_)) => {
                Accessibility::Scoped(
                    self.engine.get_target_root_module_id(self.target_id).await,
                )
            }
            Some(pernixc_syntax::AccessModifier::Public(_)) | None => {
                Accessibility::Public
            }
        };

        let span = identifier.span;

        if let Some(member) = module_syntax.inline_body() {
            let next_submodule_id =
                member_builder.add_member(identifier, &self.engine).await;

            Some(
                self.create_module(member.content(), ModuleKind::Submodule {
                    submodule_id: next_submodule_id,
                    submodule_qualified_name: next_submodule_qualified_name,
                    accessibility,
                    span,
                })
                .await,
            )
        } else {
            let invocation_arguments =
                self.engine.get_invocation_arguments(self.target_id).await;

            let mut load_path = invocation_arguments
                .command
                .input()
                .file
                .parent()
                .map(ToOwned::to_owned)
                .unwrap_or_default();

            load_path.extend(
                next_submodule_qualified_name
                    .iter()
                    .skip(1)
                    .map(FlexStr::as_str),
            );

            load_path.set_extension("pnx");

            if self.is_root
                && invocation_arguments
                    .command
                    .input()
                    .file
                    .file_stem()
                    .unwrap_or_default()
                    .to_string_lossy()
                    .as_ref()
                    == identifier.kind.0.as_str()
            {
                // this is the recursive file loading, so we skip it
                self.storage.receive(Diagnostic::RecursiveFileRequest(
                    RecursiveFileRequest {
                        submodule_span: identifier.span,
                        path: load_path,
                    },
                ));
                return None;
            }

            let existing_member_id = member_builder
                .member_ids_by_name
                .get(&identifier.kind.0)
                .copied();

            if let Some(existing_member_id) = existing_member_id {
                member_builder.redefinition_errors.insert(ItemRedefinition {
                    existing_id: self.target_id.make_global(existing_member_id),
                    redefinition_span: identifier.span,
                    in_id: self.target_id.make_global(member_builder.symbol_id),
                });
            } else {
                let id =
                    member_builder.add_member(identifier, &self.engine).await;

                self.external_submodules.insert(
                    id,
                    Arc::new(ExternalSubmodule {
                        path: Arc::from(load_path),
                        submodule_id: id,
                        submodule_qualified_name: next_submodule_qualified_name,
                        accessibility,
                        span,
                    }),
                );
            }

            None
        }
    }

    #[allow(clippy::too_many_lines)]
    async fn handle_trait_member(
        self: &Arc<Self>,
        trait_syntax: &pernixc_syntax::item::r#trait::Trait,
        module_member_builder: &mut MemberBuilder,
    ) -> Option<JoinHandle<()>> {
        let signature = trait_syntax.signature()?;
        let identifier = signature.identifier()?;

        let next_submodule_qualified_name = module_member_builder
            .symbol_qualified_name
            .iter()
            .cloned()
            .chain(std::iter::once(identifier.kind.0.clone()))
            .collect::<Arc<[_]>>();

        let trait_id = module_member_builder
            .add_member(identifier.clone(), &self.engine)
            .await;

        let trait_body = trait_syntax.body();
        let members =
            trait_body.as_ref().and_then(pernixc_syntax::item::Body::members);

        let access_modifier = trait_syntax.access_modifier();
        let generic_parameters = signature.generic_parameters();
        let where_clause = trait_body
            .and_then(|x| x.where_clause())
            .and_then(|x| x.predicates());

        let parent_module_id = module_member_builder.symbol_id;

        let context = self.clone();

        Some(tokio::spawn(async move {
            let mut trait_member_builder = MemberBuilder::new(
                trait_id,
                next_submodule_qualified_name,
                context.target_id,
            );

            // add each of the member to the trait member
            for member in members
                .as_ref()
                .iter()
                .flat_map(|x| x.members())
                .filter_map(|x| x.into_line().ok())
            {
                let entry = match member {
                    TraitMemberSyn::Type(member) => {
                        let Some(signature) =
                            member.signature().and_then(|x| x.identifier())
                        else {
                            continue;
                        };

                        Entry::builder()
                            .kind(Kind::TraitType)
                            .identifier(signature)
                            .accessibility(member.access_modifier())
                            .generic_parameters_syntax(
                                member
                                    .signature()
                                    .and_then(|x| x.generic_parameters()),
                            )
                            .where_clause_syntax(
                                member
                                    .trailing_where_clause()
                                    .and_then(|x| x.where_clause())
                                    .and_then(|x| x.predicates()),
                            )
                            .build()
                    }

                    TraitMemberSyn::Function(member) => {
                        let Some(signature) =
                            member.signature().and_then(|x| x.identifier())
                        else {
                            continue;
                        };

                        Entry::builder()
                            .kind(Kind::TraitFunction)
                            .identifier(signature)
                            .accessibility(member.access_modifier())
                            .generic_parameters_syntax(
                                member
                                    .signature()
                                    .and_then(|x| x.generic_parameters()),
                            )
                            .where_clause_syntax(
                                member
                                    .trailing_where_clause()
                                    .and_then(|x| x.where_clause())
                                    .and_then(|x| x.predicates()),
                            )
                            .function_signature_syntax((
                                member.signature().and_then(|x| x.parameters()),
                                member
                                    .signature()
                                    .and_then(|x| x.return_type()),
                            ))
                            .build()
                    }

                    TraitMemberSyn::Constant(member) => {
                        let Some(signature) =
                            member.signature().and_then(|x| x.identifier())
                        else {
                            continue;
                        };

                        Entry::builder()
                            .kind(Kind::TraitConstant)
                            .identifier(signature)
                            .accessibility(member.access_modifier())
                            .generic_parameters_syntax(
                                member
                                    .signature()
                                    .and_then(|x| x.generic_parameters()),
                            )
                            .where_clause_syntax(
                                member
                                    .trailing_where_clause()
                                    .and_then(|x| x.where_clause())
                                    .and_then(|x| x.predicates()),
                            )
                            .constant_type_annotation_syntax(
                                member.signature().and_then(|x| x.r#type()),
                            )
                            .build()
                    }
                };

                let member_id = trait_member_builder
                    .add_member(entry.identifier.clone(), &context.engine)
                    .await;

                context.add_symbol_entry(member_id, trait_id, entry).await;
            }

            context
                .add_symbol_entry(
                    trait_id,
                    parent_module_id,
                    Entry::builder()
                        .kind(Kind::Trait)
                        .identifier(identifier)
                        .accessibility(access_modifier)
                        .generic_parameters_syntax(generic_parameters)
                        .where_clause_syntax(where_clause)
                        .member(Arc::new(Member {
                            member_ids_by_name: trait_member_builder
                                .member_ids_by_name,
                            redefinitions: trait_member_builder.redefinitions,
                        }))
                        .build(),
                )
                .await;

            context.storage.as_vec_mut().extend(
                trait_member_builder
                    .redefinition_errors
                    .into_iter()
                    .map(Diagnostic::ItemRedefinition),
            );
        }))
    }

    #[allow(clippy::too_many_lines)]
    async fn handle_enum_member(
        self: &Arc<Self>,
        enum_syntax: &pernixc_syntax::item::r#enum::Enum,
        module_member_builder: &mut MemberBuilder,
    ) -> Option<JoinHandle<()>> {
        let signature = enum_syntax.signature()?;
        let identifier = signature.identifier()?;

        let next_submodule_qualified_name = module_member_builder
            .symbol_qualified_name
            .iter()
            .cloned()
            .chain(std::iter::once(identifier.kind.0.clone()))
            .collect::<Arc<[_]>>();

        let enum_id = module_member_builder
            .add_member(identifier.clone(), &self.engine)
            .await;

        let enum_body = enum_syntax.body();
        let members =
            enum_body.as_ref().and_then(pernixc_syntax::item::Body::members);

        let access_modifier = enum_syntax.access_modifier();
        let generic_parameters = signature.generic_parameters();
        let where_clause = enum_body
            .and_then(|x| x.where_clause())
            .and_then(|x| x.predicates());

        let parent_module_id = module_member_builder.symbol_id;

        let context = self.clone();

        Some(tokio::spawn(async move {
            let mut enum_member_builder = MemberBuilder::new(
                enum_id,
                next_submodule_qualified_name,
                context.target_id,
            );

            // add each of the member to the trait member
            for variant in members
                .as_ref()
                .iter()
                .flat_map(|x| x.members())
                .filter_map(|x| x.into_line().ok())
            {
                let Some(identifier) = variant.identifier() else {
                    continue;
                };

                let variant_id = enum_member_builder
                    .add_member(identifier.clone(), &context.engine)
                    .await;

                let entry = Entry::builder()
                    .kind(Kind::Variant)
                    .identifier(identifier)
                    .variant_associated_type_syntax(
                        variant.association().and_then(|x| x.r#type()),
                    )
                    .build();

                context.add_symbol_entry(variant_id, enum_id, entry).await;
            }

            context
                .add_symbol_entry(
                    enum_id,
                    parent_module_id,
                    Entry::builder()
                        .kind(Kind::Enum)
                        .identifier(identifier)
                        .accessibility(access_modifier)
                        .generic_parameters_syntax(generic_parameters)
                        .where_clause_syntax(where_clause)
                        .member(Arc::new(Member {
                            member_ids_by_name: enum_member_builder
                                .member_ids_by_name,
                            redefinitions: enum_member_builder.redefinitions,
                        }))
                        .build(),
                )
                .await;

            context.storage.as_vec_mut().extend(
                enum_member_builder
                    .redefinition_errors
                    .into_iter()
                    .map(Diagnostic::ItemRedefinition),
            );
        }))
    }

    #[allow(clippy::needless_pass_by_value, clippy::too_many_lines)]
    async fn handle_module_content(
        self: &Arc<Self>,
        module_content: pernixc_syntax::item::module::Content,
        module_member_builder: &mut MemberBuilder,
        imports: &mut Vec<pernixc_syntax::item::module::Import>,
    ) {
        // will be joined later
        scoped!(|tasks| async move {
            for item in
                module_content.members().filter_map(|x| x.into_line().ok())
            {
                let entry = match item {
                    ModuleMemberSyn::Module(module) => {
                        // custom handling for the module member
                        if let Some(handle) = self
                            .handle_module_member(
                                &module,
                                module_member_builder,
                            )
                            .await
                        {
                            tasks.push(handle);
                        }

                        continue;
                    }

                    ModuleMemberSyn::Trait(trait_syntax) => {
                        // custom handling for the trait member
                        if let Some(handle) = self
                            .handle_trait_member(
                                &trait_syntax,
                                module_member_builder,
                            )
                            .await
                        {
                            tasks.push(handle);
                        }

                        continue;
                    }

                    ModuleMemberSyn::Function(function_syntax) => {
                        let Some(identifier) = function_syntax
                            .signature()
                            .and_then(|x| x.identifier())
                        else {
                            continue;
                        };

                        Entry::builder()
                            .kind(Kind::Function)
                            .identifier(identifier.clone())
                            .accessibility(function_syntax.access_modifier())
                            .generic_parameters_syntax(
                                function_syntax
                                    .signature()
                                    .and_then(|x| x.generic_parameters()),
                            )
                            .where_clause_syntax(
                                function_syntax
                                    .body()
                                    .and_then(|x| x.where_clause())
                                    .and_then(|x| x.predicates()),
                            )
                            .function_signature_syntax((
                                function_syntax
                                    .signature()
                                    .and_then(|x| x.parameters()),
                                function_syntax
                                    .signature()
                                    .and_then(|x| x.return_type()),
                            ))
                            .build()
                    }

                    ModuleMemberSyn::Type(type_syntax) => {
                        let Some(identifier) = type_syntax
                            .signature()
                            .and_then(|x| x.identifier())
                        else {
                            continue;
                        };

                        Entry::builder()
                            .kind(Kind::Type)
                            .identifier(identifier.clone())
                            .accessibility(type_syntax.access_modifier())
                            .generic_parameters_syntax(
                                type_syntax
                                    .signature()
                                    .and_then(|x| x.generic_parameters()),
                            )
                            .where_clause_syntax(
                                type_syntax
                                    .body()
                                    .and_then(|x| x.trailing_where_clause())
                                    .and_then(|x| x.where_clause())
                                    .and_then(|x| x.predicates()),
                            )
                            .type_alias_syntax(
                                type_syntax.body().and_then(|x| x.r#type()),
                            )
                            .build()
                    }

                    ModuleMemberSyn::Constant(constant_syntax) => {
                        let Some(identifier) = constant_syntax
                            .signature()
                            .and_then(|x| x.identifier())
                        else {
                            continue;
                        };

                        Entry::builder()
                            .kind(Kind::Constant)
                            .identifier(identifier.clone())
                            .accessibility(constant_syntax.access_modifier())
                            .generic_parameters_syntax(
                                constant_syntax
                                    .signature()
                                    .and_then(|x| x.generic_parameters()),
                            )
                            .where_clause_syntax(
                                constant_syntax
                                    .body()
                                    .and_then(|x| x.trailing_where_clause())
                                    .and_then(|x| x.where_clause())
                                    .and_then(|x| x.predicates()),
                            )
                            .constant_type_annotation_syntax(
                                constant_syntax
                                    .signature()
                                    .and_then(|x| x.r#type()),
                            )
                            .constant_expression_syntax(
                                constant_syntax
                                    .body()
                                    .and_then(|x| x.expression()),
                            )
                            .build()
                    }

                    ModuleMemberSyn::Struct(struct_syntax) => {
                        let Some(identifier) = struct_syntax
                            .signature()
                            .and_then(|x| x.identifier())
                        else {
                            continue;
                        };

                        Entry::builder()
                            .kind(Kind::Struct)
                            .identifier(identifier.clone())
                            .accessibility(struct_syntax.access_modifier())
                            .generic_parameters_syntax(
                                struct_syntax
                                    .signature()
                                    .and_then(|x| x.generic_parameters()),
                            )
                            .where_clause_syntax(
                                struct_syntax
                                    .body()
                                    .and_then(|x| x.where_clause())
                                    .and_then(|x| x.predicates()),
                            )
                            .fields_syntax(struct_syntax.body())
                            .build()
                    }

                    ModuleMemberSyn::Enum(enum_syntax) => {
                        // custom handling for the enum member
                        if let Some(handle) = self
                            .handle_enum_member(
                                &enum_syntax,
                                module_member_builder,
                            )
                            .await
                        {
                            tasks.push(handle);
                        }

                        continue;
                    }

                    ModuleMemberSyn::Marker(marker_syntax) => {
                        let Some(identifier) = marker_syntax
                            .signature()
                            .and_then(|x| x.identifier())
                        else {
                            continue;
                        };

                        Entry::builder()
                            .kind(Kind::Marker)
                            .identifier(identifier.clone())
                            .accessibility(marker_syntax.access_modifier())
                            .generic_parameters_syntax(
                                marker_syntax
                                    .signature()
                                    .and_then(|x| x.generic_parameters()),
                            )
                            .where_clause_syntax(
                                marker_syntax
                                    .trailing_where_clause()
                                    .and_then(|x| x.where_clause())
                                    .and_then(|x| x.predicates()),
                            )
                            .build()
                    }

                    ModuleMemberSyn::Import(import) => {
                        // collect the import syntax for later processing
                        imports.push(import);
                        continue;
                    }

                    ModuleMemberSyn::Implements(_)
                    | ModuleMemberSyn::Extern(_) => continue,
                };

                let member_id = module_member_builder
                    .add_member(entry.identifier.clone(), &self.engine)
                    .await;

                self.add_symbol_entry(
                    member_id,
                    module_member_builder.symbol_id,
                    entry,
                )
                .await;
            }
        });
    }
}

struct MemberBuilder {
    symbol_id: ID,
    symbol_qualified_name: Arc<[SharedStr]>,
    target_id: TargetID,

    member_ids_by_name: HashMap<SharedStr, ID>,
    name_occurrences: HashMap<SharedStr, usize>,
    redefinitions: HashSet<ID>,

    redefinition_errors: HashSet<ItemRedefinition>,
}

impl MemberBuilder {
    fn new(
        symbol_id: ID,
        symbol_qualified_name: Arc<[SharedStr]>,
        target_id: TargetID,
    ) -> Self {
        Self {
            symbol_id,
            symbol_qualified_name,
            target_id,

            member_ids_by_name: HashMap::default(),
            name_occurrences: HashMap::default(),
            redefinitions: HashSet::default(),

            redefinition_errors: HashSet::default(),
        }
    }

    async fn add_member(
        &mut self,
        identifier: pernixc_syntax::Identifier,
        engine: &TrackedEngine,
    ) -> ID {
        let occurrences =
            self.name_occurrences.entry(identifier.kind.0.clone()).or_default();

        let current_count = *occurrences;
        *occurrences += 1;

        let new_member_id = engine
            .calculate_qualified_name_id(
                self.symbol_qualified_name
                    .iter()
                    .map(flexstr::FlexStr::as_str)
                    .chain(std::iter::once(identifier.kind.0.as_str())),
                self.target_id,
                Some(self.symbol_id),
                current_count,
            )
            .await;

        match self.member_ids_by_name.entry(identifier.kind.0) {
            hash_map::Entry::Occupied(occupied_entry) => {
                self.redefinition_errors.insert(ItemRedefinition {
                    existing_id: self
                        .target_id
                        .make_global(*occupied_entry.get()),
                    redefinition_span: identifier.span,
                    in_id: self.target_id.make_global(self.symbol_id),
                });

                self.redefinitions.insert(new_member_id);
            }
            hash_map::Entry::Vacant(vacant_entry) => {
                vacant_entry.insert(new_member_id);
            }
        }

        new_member_id
    }
}

/// Calculates the ID of the symbol with the given sequence of qualified names
/// and the target ID.
///
/// The ID is calculated by hashing the sequence of names, the target ID, and
/// the declaration order.
///
/// The declaration order is used to handle cases of the redefinition of symbols
/// with the same name in the same scope. In case of the symbol with no
/// redefinition, passing `0` as the declaration order is sufficient.
#[extend]
#[allow(clippy::collection_is_never_read)]
pub async fn calculate_qualified_name_id<'a>(
    self: &TrackedEngine,
    qualified_name_sequence: impl IntoIterator<Item = &'a str>,
    target_id: TargetID,
    parent_id: Option<ID>,
    declaration_order: usize,
) -> ID {
    let mut hasher = siphasher::sip::SipHasher24::default();
    let target_seed = self.get_target_seed(target_id).await;

    target_seed.hash(&mut hasher);
    parent_id.hash(&mut hasher);

    for name in qualified_name_sequence {
        // hash the name of the symbol
        name.hash(&mut hasher);
    }

    declaration_order.hash(&mut hasher);

    ID(hasher.finish())
}

/// Returns the root module ID for the given target ID.
#[extend]
pub async fn get_target_root_module_id(
    self: &TrackedEngine,
    target_id: TargetID,
) -> ID {
    let invocation_arguments = self.get_invocation_arguments(target_id).await;
    let target_name = invocation_arguments.command.input().target_name();

    self.calculate_qualified_name_id(
        std::iter::once(target_name.as_str()),
        target_id,
        None,
        0,
    )
    .await
}

/// Used to identify in which table node the symbol is defined.
#[derive(
    Debug, Clone, Serialize, Deserialize, StableHash, pernixc_query::Value,
)]
#[id(TargetID)]
#[key(MapKey)]
pub struct Map {
    /// Maps the symbol ID to the key where the symbol's information is stored.
    ///
    /// The value is stored as [`Option<Arc<ExternalSubmodule>>`] where the
    /// `None` value indicates that the symbol is defined in the root file
    /// whereas the `Some` value indicates that the symbol is defined in an
    /// external submodule.
    pub keys_by_symbol_id:
        Arc<ReadOnlyView<ID, Option<Arc<ExternalSubmodule>>>>,

    /// Maps the source file ID to its path and the external submodule (if it
    /// is).
    #[allow(clippy::type_complexity)]
    pub paths_by_source_id: Arc<
        ReadOnlyView<
            pernixc_arena::ID<SourceFile>,
            (Arc<Path>, Option<Arc<ExternalSubmodule>>),
        >,
    >,
}

#[pernixc_query::executor(key(MapKey), name(MapExecutor))]
#[allow(clippy::too_many_lines)]
pub async fn map_executor(
    &MapKey(target_id): &MapKey,
    engine: &TrackedEngine,
) -> Result<Map, pernixc_query::runtime::executor::CyclicError> {
    #[allow(clippy::needless_pass_by_value, clippy::type_complexity)]
    fn traverse_table<'x>(
        engine: &'x TrackedEngine,
        table_key: &'x Key,
        keys_by_symbol_id: Arc<DashMap<ID, Option<Arc<ExternalSubmodule>>>>,
        paths_by_source_id: Arc<
            DashMap<
                pernixc_arena::ID<SourceFile>,
                (Arc<Path>, Option<Arc<ExternalSubmodule>>),
            >,
        >,
        current_external_submodule: Option<Arc<ExternalSubmodule>>,
    ) -> impl Future<'x, ()> {
        async move {
            // Query the table for this key
            let kinds = engine.query(&KindMapKey(table_key.clone())).await?;

            let external_submodules: Arc<
                ReadOnlyView<ID, Arc<ExternalSubmodule>>,
            > = engine
                .query(&ExternalSubmoduleMapKey(table_key.clone()))
                .await?;

            // Get the path and target_id for this table
            let (path, current_target_id) = match &table_key {
                Key::Root(target_id) => {
                    let invocation_arguments =
                        engine.get_invocation_arguments(*target_id).await;
                    (
                        Arc::from(
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
            let source_file_id =
                engine.calculate_path_id(&path, current_target_id).await;

            // Add the path to source file mapping
            paths_by_source_id.insert(
                source_file_id,
                (path, current_external_submodule.clone()),
            );

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

                handles.push(tokio::spawn(async move {
                    traverse_table(
                        &engine,
                        &submodule_key,
                        keys_by_symbol_id,
                        paths_by_source_id,
                        Some(external_submodule),
                    )
                    .await
                }));
            }

            // Wait for all traversals to complete
            let mut error = None;
            for handle in handles {
                if let Err(e) = handle.await.unwrap() {
                    error = Some(e);
                }
            }

            if let Some(e) = error {
                return Err(e);
            }

            Ok::<(), CyclicError>(())
        }
    }

    let keys_by_symbol_id = Arc::new(DashMap::default());
    let paths_by_source_id = Arc::new(DashMap::default());

    // Start traversal from the root
    traverse_table(
        engine,
        &Key::Root(target_id),
        keys_by_symbol_id.clone(),
        paths_by_source_id.clone(),
        None,
    )
    .await?;

    Ok(Map {
        keys_by_symbol_id: Arc::new(
            Arc::try_unwrap(keys_by_symbol_id)
                .expect("Failed to convert DashMap to ReadOnlyView")
                .into_read_only(),
        ),
        paths_by_source_id: Arc::new(
            Arc::try_unwrap(paths_by_source_id)
                .expect("Failed to convert DashMap to ReadOnlyView")
                .into_read_only(),
        ),
    })
}

/// Gets the table node where the information of the given symbol ID is stored.
#[extend]
async fn get_table_of_symbol(
    self: &TrackedEngine,
    id: Global<ID>,
) -> Arc<Table> {
    let map = self.query(&MapKey(id.target_id)).await.unwrap();

    let node_key = map
        .keys_by_symbol_id
        .get(&id.id)
        .unwrap_or_else(|| panic!("invalid symbol ID: {:?}", id.id))
        .as_ref()
        .map_or_else(
            || crate::Key::Root(id.target_id),
            |x| crate::Key::Submodule {
                external_submodule: x.clone(),
                target_id: id.target_id,
            },
        );

    self.query(&crate::TableKey(node_key)).await.unwrap()
}
