use std::{collections::hash_map, path::Path, sync::Arc};

use bon::Builder;
use enum_as_inner::EnumAsInner;
use flexstr::SharedStr;
use pernixc_extend::extend;
use pernixc_handler::{Handler, Storage};
use pernixc_hash::{DashMap, HashMap, HashSet, ReadOnlyView};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::{
    runtime::executor::{self, CyclicError, Future},
    TrackedEngine,
};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::{calculate_path_id, SourceElement, SourceFile};
use pernixc_stable_hash::StableHash;
use pernixc_symbol::{
    accessibility::Accessibility,
    calculate_implements_id, calculate_qualified_name_id,
    get_target_root_module_id,
    kind::{self, Kind},
    linkage::{self, C},
    member::{self, Member},
    AllSymbolIDKey, ID,
};
use pernixc_syntax::{
    item::r#trait::Member as TraitMemberSyn, QualifiedIdentifier,
};
use pernixc_target::{get_invocation_arguments, Global, TargetID};
use tokio::task::JoinHandle;

use crate::diagnostic::{Diagnostic, ItemRedefinition, SourceFileLoadFail};

mod effect;
mod module;

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

pernixc_register::register!(TableKey, TableExecutor);

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

pernixc_register::register!(DiagnosticKey, DiagnosticExecutor);

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

    /// Maps the ID of the implements member (type, function, constant) to its
    /// extracted access modifier syntax.
    pub implements_access_modifier_syntaxes:
        Arc<ReadOnlyView<ID, Option<pernixc_syntax::AccessModifier>>>,

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

    /// Maps the enum variant ID to the order of its declaration in the enum.
    pub variant_declaration_orders: Arc<ReadOnlyView<ID, usize>>,

    /// Maps the module symbol ID to the list of import syntaxes that
    /// are defined in the module.
    pub import_syntaxes:
        Arc<ReadOnlyView<ID, Arc<[pernixc_syntax::item::module::Import]>>>,

    /// Maps the implements ID to its qualified identifier syntax.
    pub implements_qualified_identifier_syntaxes:
        Arc<ReadOnlyView<ID, QualifiedIdentifier>>,

    /// Maps the implements ID to its `final` keyword.
    pub final_keywords: Arc<ReadOnlyView<ID, Option<pernixc_syntax::Keyword>>>,

    /// Maps the function ID to its body syntax, which contains a list of
    /// statements.
    pub function_body_syntaxes: Arc<
        ReadOnlyView<
            ID,
            Option<
                pernixc_syntax::item::Members<
                    pernixc_syntax::statement::Statement,
                >,
            >,
        >,
    >,

    /// Maps the function ID to its `do` effect syntax if it has one.
    pub function_do_effect_syntaxes:
        Arc<ReadOnlyView<ID, Option<pernixc_syntax::item::function::DoEffect>>>,

    /// Maps the function ID to its linkage.
    pub function_linkages: Arc<ReadOnlyView<ID, linkage::Linkage>>,

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

pernixc_register::register!(KindMapKey, KindMapExecutor);

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

pernixc_register::register!(
    ExternalSubmoduleMapKey,
    ExternalSubmoduleMapExecutor
);

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

    implements_access_modifier_syntaxes:
        DashMap<ID, Option<pernixc_syntax::AccessModifier>>,

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
    function_linkages: DashMap<ID, linkage::Linkage>,
    fields_syntaxes: DashMap<
        ID,
        Option<
            pernixc_syntax::item::Body<pernixc_syntax::item::r#struct::Field>,
        >,
    >,
    variant_associated_type_syntaxes:
        DashMap<ID, Option<pernixc_syntax::r#type::Type>>,
    variant_declaration_orders: DashMap<ID, usize>,
    import_syntaxes: DashMap<ID, Arc<[pernixc_syntax::item::module::Import]>>,
    implements_qualified_identifier_syntaxes:
        DashMap<ID, pernixc_syntax::QualifiedIdentifier>,
    final_keywords: DashMap<ID, Option<pernixc_syntax::Keyword>>,

    function_body_syntaxes: DashMap<
        ID,
        Option<
            pernixc_syntax::item::Members<pernixc_syntax::statement::Statement>,
        >,
    >,

    function_do_effect_syntaxes:
        DashMap<ID, Option<pernixc_syntax::item::function::DoEffect>>,

    token_tree: Option<Arc<pernixc_lexical::tree::Tree>>,
    source_file: Option<Arc<SourceFile>>,

    is_root: bool,
}

#[pernixc_query::executor(key(TableKey), name(TableExecutor))]
#[allow(clippy::too_many_lines)]
pub async fn table_executor(
    TableKey(key): &TableKey,
    engine: &TrackedEngine,
) -> Result<Arc<Table>, pernixc_query::runtime::executor::CyclicError> {
    let (
        syntax_tree_result,
        token_tree_result,
        source_file,
        target_id,
        module_kind,
        is_root,
    ) = match key {
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
                engine
                    .query(&pernixc_lexical::Key {
                        path: invocation_arguments
                            .command
                            .input()
                            .file
                            .clone()
                            .into(),
                        target_id: *target_id,
                    })
                    .await?,
                engine
                    .query(&pernixc_source_file::Key {
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
            engine
                .query(&pernixc_lexical::Key {
                    path: external_submodule.path.clone(),
                    target_id: *target_id,
                })
                .await?,
            engine
                .query(&pernixc_source_file::Key {
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
    let tree = match syntax_tree_result {
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
        implements_access_modifier_syntaxes: DashMap::default(),
        external_submodules: DashMap::default(),
        generic_parameter_syntaxes: DashMap::default(),
        where_clause_syntaxes: DashMap::default(),
        type_alias_syntaxes: DashMap::default(),
        constant_type_annotation_syntaxes: DashMap::default(),
        constant_expression_syntaxes: DashMap::default(),
        function_signature_syntaxes: DashMap::default(),
        fields_syntaxes: DashMap::default(),
        variant_associated_type_syntaxes: DashMap::default(),
        variant_declaration_orders: DashMap::default(),
        import_syntaxes: DashMap::default(),
        implements_qualified_identifier_syntaxes: DashMap::default(),
        function_linkages: DashMap::default(),
        final_keywords: DashMap::default(),
        function_body_syntaxes: DashMap::default(),
        function_do_effect_syntaxes: DashMap::default(),
        token_tree: token_tree_result.ok().map(|x| x.0),
        source_file: source_file.ok(),
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
        implements_access_modifier_syntaxes: Arc::new(
            context.implements_access_modifier_syntaxes.into_read_only(),
        ),

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
        variant_declaration_orders: Arc::new(
            context.variant_declaration_orders.into_read_only(),
        ),
        import_syntaxes: Arc::new(context.import_syntaxes.into_read_only()),
        implements_qualified_identifier_syntaxes: Arc::new(
            context.implements_qualified_identifier_syntaxes.into_read_only(),
        ),
        final_keywords: Arc::new(context.final_keywords.into_read_only()),
        function_body_syntaxes: Arc::new(
            context.function_body_syntaxes.into_read_only(),
        ),
        function_linkages: Arc::new(context.function_linkages.into_read_only()),
        function_do_effect_syntaxes: Arc::new(
            context.function_do_effect_syntaxes.into_read_only(),
        ),

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

#[derive(Debug, Clone, PartialEq, Eq, EnumAsInner)]
enum Naming {
    Identifier(pernixc_syntax::Identifier),
    Implements(pernixc_syntax::QualifiedIdentifier),
}

#[derive(Debug, Clone, PartialEq, Eq, Builder)]
#[allow(clippy::option_option)]
struct Entry {
    pub naming: Naming,
    pub kind: kind::Kind,

    pub member: Option<Arc<member::Member>>,

    pub accessibility: Option<Option<pernixc_syntax::AccessModifier>>,

    pub generic_parameters_syntax: Option<
        Option<pernixc_syntax::item::generic_parameters::GenericParameters>,
    >,

    pub where_clause_syntax:
        Option<Option<pernixc_syntax::item::where_clause::Predicates>>,

    pub type_alias_syntax: Option<Option<pernixc_syntax::r#type::Type>>,

    pub constant_type_annotation_syntax:
        Option<Option<pernixc_syntax::r#type::Type>>,

    pub constant_expression_syntax:
        Option<Option<pernixc_syntax::expression::Expression>>,

    pub function_signature_syntax: Option<(
        Option<pernixc_syntax::item::function::Parameters>,
        Option<pernixc_syntax::item::function::ReturnType>,
    )>,

    pub fields_syntax: Option<
        Option<
            pernixc_syntax::item::Body<pernixc_syntax::item::r#struct::Field>,
        >,
    >,

    pub variant_associated_type_syntax:
        Option<Option<pernixc_syntax::r#type::Type>>,

    pub variant_declaration_order: Option<usize>,

    pub function_body_syntax: Option<
        Option<
            pernixc_syntax::item::Members<pernixc_syntax::statement::Statement>,
        >,
    >,

    pub final_keyword: Option<Option<pernixc_syntax::Keyword>>,

    pub function_do_effect_syntax:
        Option<Option<pernixc_syntax::item::function::DoEffect>>,

    pub function_linkage: Option<linkage::Linkage>,
}

impl TableContext {
    fn implements_qualified_identifier_name(
        &self,
        qualified_identifier_span: &RelativeSpan,
    ) -> SharedStr {
        let source_file = self.source_file.as_ref().unwrap();
        let token_tree = self.token_tree.as_ref().unwrap();

        format!(
            "[implements {}]",
            &source_file.content()[qualified_identifier_span
                .to_absolute_span(source_file, token_tree)
                .range()]
        )
        .into()
    }

    #[allow(clippy::too_many_lines)]
    async fn add_symbol_entry(&self, id: ID, parent_id: ID, entry: Entry) {
        match entry.naming {
            Naming::Identifier(token) => {
                Self::insert_to_table(&self.names, id, token.kind.0);
                Self::insert_to_table(&self.spans, id, Some(token.span));
            }
            Naming::Implements(qualified_identifier) => {
                Self::insert_to_table(
                    &self.names,
                    id,
                    self.implements_qualified_identifier_name(
                        &qualified_identifier.span(),
                    ),
                );
                Self::insert_to_table(
                    &self.spans,
                    id,
                    Some(qualified_identifier.span()),
                );
                Self::insert_to_table(
                    &self.implements_qualified_identifier_syntaxes,
                    id,
                    qualified_identifier,
                );
            }
        }
        Self::insert_to_table(&self.kinds, id, entry.kind);

        // Handle accessibility based on symbol kind
        if let Some(accessibility) = entry.accessibility {
            // Implements members store raw access modifier syntax, others store
            // processed accessibility
            match entry.kind {
                Kind::ImplementationConstant
                | Kind::ImplementationFunction
                | Kind::ImplementationType => {
                    // Store raw access modifier syntax for implements members
                    Self::insert_to_table(
                        &self.implements_access_modifier_syntaxes,
                        id,
                        accessibility,
                    );
                }
                _ => {
                    // Store processed accessibility for all other symbols
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
                        Some(pernixc_syntax::AccessModifier::Public(_))
                        | None => Accessibility::Public,
                    };
                    Self::insert_to_table(
                        &self.accessibilities,
                        id,
                        accessibility,
                    );
                }
            }
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

        if let Some(final_keyword) = entry.final_keyword {
            Self::insert_to_table(&self.final_keywords, id, final_keyword);
        }

        if let Some(function_body_syntax) = entry.function_body_syntax {
            Self::insert_to_table(
                &self.function_body_syntaxes,
                id,
                function_body_syntax,
            );
        }

        if let Some(variant_declaration_order) = entry.variant_declaration_order
        {
            Self::insert_to_table(
                &self.variant_declaration_orders,
                id,
                variant_declaration_order,
            );
        }

        if let Some(function_linkage) = entry.function_linkage {
            Self::insert_to_table(
                &self.function_linkages,
                id,
                function_linkage,
            );
        }

        if let Some(function_do_effect_syntax) = entry.function_do_effect_syntax
        {
            Self::insert_to_table(
                &self.function_do_effect_syntaxes,
                id,
                function_do_effect_syntax,
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
                    unnameds: member_builder.unnameds,
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

    #[allow(clippy::too_many_lines)]
    async fn handle_implements(
        self: &Arc<Self>,
        implements_syntax: pernixc_syntax::item::implements::Implements,
        module_member_builder: &mut MemberBuilder,
    ) {
        let Some(signature) = implements_syntax.signature() else {
            return;
        };

        let Some(qualified_identifier) = signature.qualified_identifier()
        else {
            return;
        };

        let body = implements_syntax.body();
        let qualified_identifier_span = qualified_identifier.span();

        let generic_parameters = signature.generic_parameters();
        let where_clause = match &body {
            Some(pernixc_syntax::item::implements::Body::Negative(
                negative_body,
            )) => negative_body
                .trailing_where_clause()
                .and_then(|x| x.where_clause())
                .and_then(|x| x.predicates()),

            Some(pernixc_syntax::item::implements::Body::Positive(body)) => {
                body.where_clause().and_then(|x| x.predicates())
            }

            None => None,
        };

        let implements_id = self
            .engine
            .calculate_implements_id(&qualified_identifier_span, self.target_id)
            .await;

        assert!(module_member_builder.unnameds.insert(implements_id));

        match body {
            Some(pernixc_syntax::item::implements::Body::Negative(_)) => {
                self.add_symbol_entry(
                    implements_id,
                    module_member_builder.symbol_id,
                    Entry::builder()
                        .naming(Naming::Implements(
                            qualified_identifier.clone(),
                        ))
                        .kind(Kind::NegativeImplementation)
                        .generic_parameters_syntax(generic_parameters)
                        .where_clause_syntax(where_clause)
                        .final_keyword(
                            implements_syntax
                                .signature()
                                .and_then(|x| x.final_keyword()),
                        )
                        .build(),
                )
                .await;
            }

            None => {
                self.add_symbol_entry(
                    implements_id,
                    module_member_builder.symbol_id,
                    Entry::builder()
                        .naming(Naming::Implements(
                            qualified_identifier.clone(),
                        ))
                        .kind(Kind::PositiveImplementation)
                        .generic_parameters_syntax(generic_parameters)
                        .where_clause_syntax(where_clause)
                        .final_keyword(
                            implements_syntax
                                .signature()
                                .and_then(|x| x.final_keyword()),
                        )
                        .member(Arc::default())
                        .build(),
                )
                .await;
            }

            Some(pernixc_syntax::item::implements::Body::Positive(body)) => {
                let member_builder = self
                    .handle_positive_implementation(
                        implements_id,
                        module_member_builder,
                        &qualified_identifier_span,
                        &body,
                    )
                    .await;

                self.add_symbol_entry(
                    implements_id,
                    module_member_builder.symbol_id,
                    Entry::builder()
                        .naming(Naming::Implements(
                            qualified_identifier.clone(),
                        ))
                        .kind(Kind::PositiveImplementation)
                        .generic_parameters_syntax(generic_parameters)
                        .where_clause_syntax(where_clause)
                        .final_keyword(
                            implements_syntax
                                .signature()
                                .and_then(|x| x.final_keyword()),
                        )
                        .member(Arc::new(Member {
                            member_ids_by_name: member_builder
                                .member_ids_by_name,
                            unnameds: member_builder.unnameds,
                        }))
                        .build(),
                )
                .await;

                self.storage.as_vec_mut().extend(
                    member_builder
                        .redefinition_errors
                        .into_iter()
                        .map(Diagnostic::ItemRedefinition),
                );
            }
        }
    }

    #[allow(clippy::too_many_lines)]
    async fn handle_positive_implementation(
        &self,
        implements_id: ID,
        module_member_builder: &MemberBuilder,
        qualified_identifier_span: &RelativeSpan,
        body: &pernixc_syntax::item::Body<
            pernixc_syntax::item::implements::Member,
        >,
    ) -> MemberBuilder {
        let mut impl_member_builder = MemberBuilder::new(
            implements_id,
            module_member_builder
                .symbol_qualified_name
                .iter()
                .cloned()
                .chain(std::iter::once(
                    self.implements_qualified_identifier_name(
                        qualified_identifier_span,
                    ),
                ))
                .collect(),
            self.target_id,
        );

        if let Some(members) = body.members() {
            for member in members.members().filter_map(|x| x.into_line().ok()) {
                let entry = match member {
                    pernixc_syntax::item::implements::Member::Constant(con) => {
                        let Some(identifier) =
                            con.signature().and_then(|x| x.identifier())
                        else {
                            continue;
                        };

                        Entry::builder()
                            .kind(Kind::ImplementationConstant)
                            .naming(Naming::Identifier(identifier))
                            .accessibility(con.access_modifier())
                            .generic_parameters_syntax(
                                con.signature()
                                    .and_then(|x| x.generic_parameters()),
                            )
                            .where_clause_syntax(
                                con.body()
                                    .and_then(|x| x.trailing_where_clause())
                                    .and_then(|x| x.where_clause())
                                    .and_then(|x| x.predicates()),
                            )
                            .constant_type_annotation_syntax(
                                con.signature().and_then(|x| x.r#type()),
                            )
                            .constant_expression_syntax(
                                con.body().and_then(|x| x.expression()),
                            )
                            .build()
                    }
                    pernixc_syntax::item::implements::Member::Function(fun) => {
                        let Some(identifier) = fun
                            .signature()
                            .and_then(|x| x.signature())
                            .and_then(|x| x.identifier())
                        else {
                            continue;
                        };

                        Entry::builder()
                            .kind(Kind::ImplementationFunction)
                            .naming(Naming::Identifier(identifier))
                            .accessibility(fun.access_modifier())
                            .generic_parameters_syntax(
                                fun.signature()
                                    .and_then(|x| x.signature())
                                    .and_then(|x| x.generic_parameters()),
                            )
                            .where_clause_syntax(
                                fun.body()
                                    .and_then(|x| x.where_clause())
                                    .and_then(|x| x.predicates()),
                            )
                            .function_signature_syntax((
                                fun.signature()
                                    .and_then(|x| x.signature())
                                    .and_then(|x| x.parameters()),
                                fun.signature()
                                    .and_then(|x| x.signature())
                                    .and_then(|x| x.return_type()),
                            ))
                            .function_body_syntax(
                                fun.body().and_then(|x| x.members()),
                            )
                            .function_do_effect_syntax(
                                fun.signature()
                                    .and_then(|x| x.signature())
                                    .and_then(|x| x.do_effect()),
                            )
                            .build()
                    }
                    pernixc_syntax::item::implements::Member::Type(ty) => {
                        let Some(identifier) =
                            ty.signature().and_then(|x| x.identifier())
                        else {
                            continue;
                        };

                        Entry::builder()
                            .kind(Kind::ImplementationType)
                            .naming(Naming::Identifier(identifier))
                            .accessibility(ty.access_modifier())
                            .generic_parameters_syntax(
                                ty.signature()
                                    .and_then(|x| x.generic_parameters()),
                            )
                            .where_clause_syntax(
                                ty.body()
                                    .and_then(|x| x.trailing_where_clause())
                                    .and_then(|x| x.where_clause())
                                    .and_then(|x| x.predicates()),
                            )
                            .type_alias_syntax(
                                ty.body().and_then(|x| x.r#type()),
                            )
                            .build()
                    }
                };

                let member_id = impl_member_builder
                    .add_member(
                        entry.naming.as_identifier().unwrap().clone(),
                        &self.engine,
                    )
                    .await;

                self.add_symbol_entry(member_id, implements_id, entry).await;
            }
        }

        impl_member_builder
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
                        let Some(identifier) =
                            member.signature().and_then(|x| x.identifier())
                        else {
                            continue;
                        };

                        Entry::builder()
                            .kind(Kind::TraitType)
                            .naming(Naming::Identifier(identifier))
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
                        let Some(identifier) =
                            member.signature().and_then(|x| x.identifier())
                        else {
                            continue;
                        };

                        Entry::builder()
                            .kind(Kind::TraitFunction)
                            .naming(Naming::Identifier(identifier))
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
                            .function_do_effect_syntax(
                                member.signature().and_then(|x| x.do_effect()),
                            )
                            .build()
                    }

                    TraitMemberSyn::Constant(member) => {
                        let Some(identifier) =
                            member.signature().and_then(|x| x.identifier())
                        else {
                            continue;
                        };

                        Entry::builder()
                            .kind(Kind::TraitConstant)
                            .naming(Naming::Identifier(identifier))
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
                    .add_member(
                        entry.naming.as_identifier().unwrap().clone(),
                        &context.engine,
                    )
                    .await;

                context.add_symbol_entry(member_id, trait_id, entry).await;
            }

            context
                .add_symbol_entry(
                    trait_id,
                    parent_module_id,
                    Entry::builder()
                        .kind(Kind::Trait)
                        .naming(Naming::Identifier(identifier))
                        .accessibility(access_modifier)
                        .generic_parameters_syntax(generic_parameters)
                        .where_clause_syntax(where_clause)
                        .member(Arc::new(Member {
                            member_ids_by_name: trait_member_builder
                                .member_ids_by_name,
                            unnameds: trait_member_builder.unnameds,
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
            for (order, variant) in members
                .as_ref()
                .iter()
                .flat_map(|x| x.members())
                .filter_map(|x| x.into_line().ok())
                .enumerate()
            {
                let Some(identifier) = variant.identifier() else {
                    continue;
                };

                let variant_id = enum_member_builder
                    .add_member(identifier.clone(), &context.engine)
                    .await;

                let entry = Entry::builder()
                    .kind(Kind::Variant)
                    .naming(Naming::Identifier(identifier))
                    .variant_associated_type_syntax(
                        variant.association().and_then(|x| x.r#type()),
                    )
                    .variant_declaration_order(order)
                    .build();

                context.add_symbol_entry(variant_id, enum_id, entry).await;
            }

            context
                .add_symbol_entry(
                    enum_id,
                    parent_module_id,
                    Entry::builder()
                        .kind(Kind::Enum)
                        .naming(Naming::Identifier(identifier))
                        .accessibility(access_modifier)
                        .generic_parameters_syntax(generic_parameters)
                        .where_clause_syntax(where_clause)
                        .member(Arc::new(Member {
                            member_ids_by_name: enum_member_builder
                                .member_ids_by_name,
                            unnameds: enum_member_builder.unnameds,
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

    async fn handle_extern(
        self: &Arc<Self>,
        extern_syn: &pernixc_syntax::item::r#extern::Extern,
        module_member_builder: &mut MemberBuilder,
    ) {
        let Some(body) = extern_syn.body() else {
            return;
        };

        let Some(convention) = extern_syn.convention() else {
            return;
        };

        let linkage = match convention.kind.as_str() {
            "C" | "c" => linkage::Linkage::C(C { variadic: false }),
            _ => linkage::Linkage::Unknown,
        };

        for function_syntax in
            body.functions().filter_map(|x| x.into_line().ok())
        {
            let Some(identifier) =
                function_syntax.signature().and_then(|x| x.identifier())
            else {
                continue;
            };

            let linkage = match linkage {
                linkage::Linkage::C(mut c) => {
                    c.variadic = function_syntax
                        .signature()
                        .and_then(|x| x.parameters())
                        .is_some_and(|x| {
                            x.parameters().any(|x| x.is_variadic())
                        });

                    linkage::Linkage::C(c)
                }

                linkage::Linkage::Unknown => linkage::Linkage::Unknown,
            };

            let entry = Entry::builder()
                .kind(Kind::ExternFunction)
                .naming(Naming::Identifier(identifier.clone()))
                .accessibility(function_syntax.access_modifier())
                .generic_parameters_syntax(
                    function_syntax
                        .signature()
                        .and_then(|x| x.generic_parameters()),
                )
                .where_clause_syntax(
                    function_syntax
                        .trailing_where_clause()
                        .and_then(|x| x.where_clause())
                        .and_then(|x| x.predicates()),
                )
                .function_signature_syntax((
                    function_syntax.signature().and_then(|x| x.parameters()),
                    function_syntax.signature().and_then(|x| x.return_type()),
                ))
                .function_linkage(linkage)
                .build();

            let member_id = module_member_builder
                .add_member(identifier, &self.engine)
                .await;

            self.add_symbol_entry(
                member_id,
                module_member_builder.symbol_id,
                entry,
            )
            .await;
        }
    }
}

struct MemberBuilder {
    symbol_id: ID,
    symbol_qualified_name: Arc<[SharedStr]>,
    target_id: TargetID,

    member_ids_by_name: HashMap<SharedStr, ID>,
    name_occurrences: HashMap<SharedStr, usize>,
    unnameds: HashSet<ID>,

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
            unnameds: HashSet::default(),

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

                self.unnameds.insert(new_member_id);
            }
            hash_map::Entry::Vacant(vacant_entry) => {
                vacant_entry.insert(new_member_id);
            }
        }

        new_member_id
    }
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

pernixc_register::register!(MapKey, MapExecutor);

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

/// Retrieves all symbol IDs for the given target ID.
#[pernixc_query::executor(key(AllSymbolIDKey), name(AllSymbolIDExecutor))]
pub async fn all_symbol_ids_executor(
    &AllSymbolIDKey(id): &AllSymbolIDKey,
    engine: &TrackedEngine,
) -> Result<Arc<[ID]>, executor::CyclicError> {
    let map = engine.query(&MapKey(id)).await?;
    Ok(map.keys_by_symbol_id.keys().copied().collect())
}

pernixc_register::register!(AllSymbolIDKey, AllSymbolIDExecutor);

/// Gets the table node where the information of the given symbol ID is stored.
#[extend]
pub async fn get_table_of_symbol(
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
            || Key::Root(id.target_id),
            |x| Key::Submodule {
                external_submodule: x.clone(),
                target_id: id.target_id,
            },
        );

    self.query(&TableKey(node_key)).await.unwrap()
}
