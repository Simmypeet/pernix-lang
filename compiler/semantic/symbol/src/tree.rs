//! Contains the implementation of building the symbol table from the file tree.

use std::{
    collections::hash_map,
    hash::{Hash, Hasher},
    path::Path,
    sync::Arc,
};

use flexstr::{FlexStr, SharedStr};
use pernixc_extend::extend;
use pernixc_file_tree::calculate_path_id;
use pernixc_handler::{Handler, Storage};
use pernixc_hash::{DashMap, HashMap, HashSet, ReadOnlyView};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::TrackedEngine;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::SourceFile;
use pernixc_stable_hash::StableHash;
use pernixc_syntax::item::module::Member as ModuleMemberSyn;
use pernixc_target::{get_invocation_arguments, get_target_seed, TargetID};

use crate::{
    accessibility::Accessibility,
    kind::Kind,
    member::Member,
    tree::diagnostic::{
        Diagnostic, ItemRedefinition, RecursiveFileRequest, SourceFileLoadFail,
    },
    ID,
};

pub mod diagnostic;

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

/// A symbol table from parsing a single file.
#[derive(Debug, Clone, Serialize, Deserialize, StableHash)]
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

struct TableContext<'a> {
    engine: &'a TrackedEngine<'a>,
    storage: &'a Storage<Diagnostic>,
    target_id: TargetID,

    kinds: DashMap<ID, Kind>,
    names: DashMap<ID, SharedStr>,
    spans: DashMap<ID, Option<RelativeSpan>>,
    members: DashMap<ID, Arc<Member>>,
    accessibilities: DashMap<ID, Accessibility<ID>>,

    external_submodules: DashMap<ID, Arc<ExternalSubmodule>>,

    is_root: bool,
}

#[pernixc_query::executor(key(TableKey), name(TableExecutor))]
pub fn table_executor(
    TableKey(key): &TableKey,
    engine: &TrackedEngine,
) -> Result<Arc<Table>, pernixc_query::runtime::executor::CyclicError> {
    let (tree_result, target_id, module_kind, is_root) = match key {
        Key::Root(target_id) => {
            let invocation_arguments =
                engine.get_invocation_arguments(*target_id);

            (
                engine.query(&pernixc_file_tree::syntax_tree::Key {
                    path: invocation_arguments
                        .command
                        .input()
                        .file
                        .clone()
                        .into(),
                    target_id: *target_id,
                })?,
                *target_id,
                ModuleKind::Root,
                true,
            )
        }

        Key::Submodule { external_submodule, target_id } => (
            engine.query(&pernixc_file_tree::syntax_tree::Key {
                path: external_submodule.path.clone(),
                target_id: *target_id,
            })?,
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
                        engine.get_invocation_arguments(*target_id);
                    (invocation_arguments.command.input().file.clone(), None)
                }
                Key::Submodule { external_submodule, .. } => (
                    external_submodule.path.as_ref().to_path_buf(),
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

    let context = TableContext {
        engine,
        storage: &storage,
        target_id,

        kinds: DashMap::default(),
        names: DashMap::default(),
        spans: DashMap::default(),
        members: DashMap::default(),
        accessibilities: DashMap::default(),

        external_submodules: DashMap::default(),

        is_root,
    };

    {
        let context = &context;

        rayon::scope(move |scope| {
            context.create_module(
                tree.and_then(|t| t.syntax_tree.map(|x| x.0)),
                module_kind,
                scope,
            );
        });
    }

    Ok(Arc::new(Table {
        kinds: Arc::new(context.kinds.into_read_only()),
        names: Arc::new(context.names.into_read_only()),
        spans: Arc::new(context.spans.into_read_only()),
        members: Arc::new(context.members.into_read_only()),
        accessibilities: Arc::new(context.accessibilities.into_read_only()),

        external_submodules: Arc::new(
            context.external_submodules.into_read_only(),
        ),

        diagnostics: Arc::new(storage.into_vec().into_iter().collect()),
    }))
}

impl<'ctx> TableContext<'ctx> {
    fn insert_to_table<V>(map: &DashMap<ID, V>, id: ID, value: V) {
        assert!(
            map.insert(id, value).is_none(),
            "Possible hash collision dfetected, please try re-building the \
             project and delete the incremental directory if the problem \
             persists."
        );
    }

    fn create_module<'scope>(
        &'ctx self,
        module_content: Option<pernixc_syntax::item::module::Content>,
        module_kind: ModuleKind,
        scope: &rayon::Scope<'scope>,
    ) where
        'ctx: 'scope,
    {
        // extract the information about the module
        let (accessibility, current_module_id, module_qualified_name, span) =
            match module_kind {
                ModuleKind::Root => {
                    let invocation_arguments =
                        self.engine.get_invocation_arguments(self.target_id);

                    let target_name =
                        invocation_arguments.command.input().target_name();

                    let current_module_id =
                        self.engine.calculate_qualified_name_id(
                            std::iter::once(target_name.as_str()),
                            self.target_id,
                            0,
                        );

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

        scope.spawn(move |scope| {
            let mut member_builder = MemberBuilder::new(
                current_module_id,
                module_qualified_name,
                self.target_id,
            );

            if let Some(module_content) = module_content {
                self.handle_module_content(
                    module_content,
                    &mut member_builder,
                    scope,
                );
            }

            Self::insert_to_table(
                &self.names,
                current_module_id,
                member_builder.symbol_qualified_name.last().cloned().unwrap(),
            );
            Self::insert_to_table(
                &self.accessibilities,
                current_module_id,
                accessibility,
            );
            Self::insert_to_table(&self.kinds, current_module_id, Kind::Module);
            Self::insert_to_table(&self.spans, current_module_id, span);
            Self::insert_to_table(
                &self.members,
                current_module_id,
                Arc::new(Member {
                    member_ids_by_name: member_builder.member_ids_by_name,
                    redefinitions: member_builder.redefinitions,
                }),
            );
        });
    }

    #[allow(clippy::too_many_lines)]
    fn handle_module_member<'scope>(
        &'ctx self,
        module_syntax: &pernixc_syntax::item::module::Module,
        member_builder: &mut MemberBuilder,
        scope: &rayon::Scope<'scope>,
    ) where
        'ctx: 'scope,
    {
        let Some(signature) = module_syntax.signature() else {
            return;
        };

        let Some(identifier) = signature.identifier() else {
            return;
        };

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
                    self.engine.get_target_root_module_id(self.target_id),
                )
            }
            Some(pernixc_syntax::AccessModifier::Public(_)) | None => {
                Accessibility::Public
            }
        };

        let span = identifier.span;

        if let Some(member) = module_syntax.inline_body() {
            let next_submodule_id =
                member_builder.add_member(identifier, self.engine);

            self.create_module(
                member.content(),
                ModuleKind::Submodule {
                    submodule_id: next_submodule_id,
                    submodule_qualified_name: next_submodule_qualified_name,
                    accessibility,
                    span,
                },
                scope,
            );
        } else {
            let invocation_arguments =
                self.engine.get_invocation_arguments(self.target_id);

            let mut load_path = invocation_arguments
                .command
                .input()
                .file
                .parent()
                .map(ToOwned::to_owned)
                .unwrap_or_default();

            load_path.extend(
                member_builder
                    .symbol_qualified_name
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
                return;
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
                let id = member_builder.add_member(identifier, self.engine);

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
        }
    }

    #[allow(clippy::needless_pass_by_value)]
    fn handle_module_content<'scope>(
        &'ctx self,
        module_content: pernixc_syntax::item::module::Content,
        member_builder: &mut MemberBuilder,
        scope: &rayon::Scope<'scope>,
    ) where
        'ctx: 'scope,
    {
        for item in module_content.members().filter_map(|x| x.into_line().ok())
        {
            match item {
                ModuleMemberSyn::Module(module) => {
                    self.handle_module_member(&module, member_builder, scope);
                }
                ModuleMemberSyn::Import(import) => todo!(),
                ModuleMemberSyn::Trait(_) => todo!(),
                ModuleMemberSyn::Function(function) => {
                    todo!()
                }
                ModuleMemberSyn::Type(_) => todo!(),
                ModuleMemberSyn::Struct(_) => todo!(),
                ModuleMemberSyn::Implements(implements) => todo!(),
                ModuleMemberSyn::Enum(_) => todo!(),
                ModuleMemberSyn::Constant(constant) => {
                    todo!()
                }
                ModuleMemberSyn::Extern(_) => todo!(),
                ModuleMemberSyn::Marker(marker) => todo!(),
            }
        }
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

    fn add_member(
        &mut self,
        identifier: pernixc_syntax::Identifier,
        engine: &TrackedEngine<'_>,
    ) -> ID {
        let occurrences =
            self.name_occurrences.entry(identifier.kind.0.clone()).or_default();

        let current_count = *occurrences;
        *occurrences += 1;

        let new_member_id = engine.calculate_qualified_name_id(
            self.symbol_qualified_name
                .iter()
                .map(flexstr::FlexStr::as_str)
                .chain(std::iter::once(identifier.kind.0.as_str())),
            self.target_id,
            current_count,
        );

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
pub fn calculate_qualified_name_id<'a>(
    self: &TrackedEngine<'_>,
    qualified_name_sequence: impl IntoIterator<Item = &'a str>,
    target_id: TargetID,
    declaration_order: usize,
) -> ID {
    let mut hasher = siphasher::sip::SipHasher24::default();
    let target_seed = self.get_target_seed(target_id);

    target_seed.hash(&mut hasher);

    for name in qualified_name_sequence {
        // hash the name of the symbol
        name.hash(&mut hasher);
    }

    declaration_order.hash(&mut hasher);

    ID(hasher.finish())
}

/// Returns the root module ID for the given target ID.
#[extend]
pub fn get_target_root_module_id(
    self: &TrackedEngine<'_>,
    target_id: TargetID,
) -> ID {
    let invocation_arguments = self.get_invocation_arguments(target_id);
    let target_name = invocation_arguments.command.input().target_name();

    self.calculate_qualified_name_id(
        std::iter::once(target_name.as_str()),
        target_id,
        0,
    )
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
            (Arc<Path>, Option<ExternalSubmodule>),
        >,
    >,
}

#[pernixc_query::executor(key(MapKey), name(MapExecutor))]
pub fn map_executor(
    &MapKey(target_id): &MapKey,
    engine: &TrackedEngine,
) -> Result<Map, pernixc_query::runtime::executor::CyclicError> {
    #[allow(clippy::items_after_statements)]
    #[allow(clippy::needless_pass_by_value)]
    #[allow(clippy::manual_let_else)]
    #[allow(clippy::match_same_arms)]
    fn traverse_table(
        engine: &TrackedEngine,
        table_key: Key,
        keys_by_symbol_id: &DashMap<ID, Option<Arc<ExternalSubmodule>>>,
        paths_by_source_id: &DashMap<
            pernixc_arena::ID<SourceFile>,
            (Arc<Path>, Option<ExternalSubmodule>),
        >,
        current_external_submodule: Option<Arc<ExternalSubmodule>>,
    ) -> Result<(), pernixc_query::runtime::executor::CyclicError> {
        // Query the table for this key
        let table = engine.query(&TableKey(table_key.clone()))?;

        // Get the path and target_id for this table
        let (path, current_target_id) = match &table_key {
            Key::Root(target_id) => {
                let invocation_arguments =
                    engine.get_invocation_arguments(*target_id);
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
        let source_file_id = engine.calculate_path_id(&path, current_target_id);

        // Add the path to source file mapping
        paths_by_source_id.insert(
            source_file_id,
            (path, current_external_submodule.as_ref().map(|x| (**x).clone())),
        );

        // Add all symbols from this table to the symbol map
        for (symbol_id, _kind) in table.kinds.iter() {
            keys_by_symbol_id
                .insert(*symbol_id, current_external_submodule.clone());
        }

        // Recursively traverse external submodules
        for (_module_id, external_submodule) in table.external_submodules.iter()
        {
            let submodule_key = Key::Submodule {
                external_submodule: external_submodule.clone(),
                target_id: current_target_id,
            };

            traverse_table(
                engine,
                submodule_key,
                keys_by_symbol_id,
                paths_by_source_id,
                Some(external_submodule.clone()),
            )?;
        }

        Ok(())
    }

    let keys_by_symbol_id = DashMap::default();
    let paths_by_source_id = DashMap::default();

    // Start traversal from the root
    traverse_table(
        engine,
        Key::Root(target_id),
        &keys_by_symbol_id,
        &paths_by_source_id,
        None,
    )?;

    Ok(Map {
        keys_by_symbol_id: Arc::new(keys_by_symbol_id.into_read_only()),
        paths_by_source_id: Arc::new(paths_by_source_id.into_read_only()),
    })
}

#[cfg(test)]
mod test;
