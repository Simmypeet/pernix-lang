//! Crate responsible for declaring symbols from the syntax tree.

use std::{
    hash::{Hash as _, Hasher as _},
    sync::Arc,
};

use flexstr::SharedStr;
use pernixc_handler::{Handler, Storage};
use pernixc_hash::{DashMap, HashMap, HashSet, ReadOnlyView};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_module_tree::{get_module_tree, ModuleTree};
use pernixc_query::{
    runtime::{
        executor,
        persistence::{serde::DynamicRegistry, Persistence},
    },
    Value,
};
use pernixc_serialize::{
    de::Deserializer, ser::Serializer, Deserialize, Serialize,
};
use pernixc_stable_hash::StableHash;
use pernixc_syntax::AccessModifier;
use pernixc_target::TargetID;

use crate::{
    accessibility::Accessibility, diagnostic::ItemRedifinition, kind::Kind,
};

pub mod accessibility;
pub mod diagnostic;
pub mod kind;
pub mod member;
pub mod name;
pub mod parent;
pub mod span;

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

impl ID {
    /// The constant symbol ID that is fixed to zero for every target. It
    /// represents the root module of the target.
    pub const ROOT_MODULE: Self = Self(0);
}

/// Represents a single symbol declaration.
#[derive(Debug, Clone, PartialEq, Eq, StableHash, Serialize, Deserialize)]
pub struct Entry {
    /// The kind of the symbol, such as module, function, type, etc.
    pub kind: kind::Kind,

    /// The name of the symbol, which is used to identify it in the symbol
    /// table.
    pub name: SharedStr,

    /// The members of the symbol, if the kind of symbol has members.
    ///
    /// For example, a module can has a member but a function cannot.
    pub members: Option<Arc<member::Member>>,

    /// The accessibility of the symbol, if the symbol kind has an
    /// accessibility
    pub accessibility: Option<accessibility::Accessibility<ID>>,

    /// The span of the symbol in the source code, if available. This is used
    /// for error reporting and code navigation.
    pub span: Option<RelativeSpan>,
}

/// The final result of building the symbol table. It contains all the
/// symbols that were declared in the compilation target. The symbols are
/// stored in the form of [`Entry`]s, which are indexed by their unique
/// [`ID`].
#[derive(Debug, Clone, StableHash, Serialize, Deserialize, Value)]
#[value(Arc<SymbolTable>)]
#[id(TargetID)]
pub struct SymbolTable {
    /// A map of all the symbol IDs to their corresponding entries.
    pub entries_by_id: ReadOnlyView<ID, Entry>,

    /// A list of errors that occurred during the symbol table generation.
    pub redefinition_errors: Arc<[ItemRedifinition]>,
}

/// An executor that builds the symbol table from the module tree.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Executor;

impl pernixc_query::runtime::executor::Executor<Key> for Executor {
    fn execute(
        &self,
        engine: &pernixc_query::TrackedEngine,
        key: &Key,
    ) -> Result<Arc<SymbolTable>, pernixc_query::runtime::executor::CyclicError>
    {
        let Ok(module_tree) = engine.get_module_tree(key.0) else {
            // creates an empty symbol table
            return Ok(Arc::new(SymbolTable {
                entries_by_id: DashMap::default().into_read_only(),
                redefinition_errors: Arc::new([]),
            }));
        };

        let storage = Storage::<ItemRedifinition>::default();
        let entries = DashMap::<ID, Entry>::default();
        let imports_by_global_id =
            DashMap::<ID, Vec<pernixc_syntax::item::module::Import>>::default();

        let context = Context {
            entries: &entries,
            imports_by_global_id: &imports_by_global_id,
            handler: &storage,
        };

        context.create_module(
            &module_tree.target_name,
            &module_tree.root_module_tree,
            None,
            &[],
        );

        Ok(Arc::new(SymbolTable {
            entries_by_id: entries.into_read_only(),
            redefinition_errors: Arc::from(storage.into_vec()),
        }))
    }
}

struct Context<'a> {
    entries: &'a DashMap<ID, Entry>,
    imports_by_global_id:
        &'a DashMap<ID, Vec<pernixc_syntax::item::module::Import>>,
    handler: &'a dyn Handler<ItemRedifinition>,
}

struct MemberBuilder<'a, 'm, 'r> {
    current_names: &'a [SharedStr],
    current_symbol_id: ID,

    member_ids_by_name: &'m mut HashMap<SharedStr, ID>,
    redefinitions: &'r mut HashSet<ID>,
}

#[derive(typed_builder::TypedBuilder)]
#[allow(clippy::option_option, unused)]
struct NewSymbol {
    name: pernixc_syntax::Identifier,
    symbol_kind: Kind,

    #[builder(default, setter(strip_option))]
    generic_parameters: Option<
        Option<pernixc_syntax::item::generic_parameters::GenericParameters>,
    >,

    #[builder(default, setter(strip_option))]
    where_clause:
        Option<Option<pernixc_syntax::item::where_clause::Predicates>>,

    #[builder(default, setter(strip_option))]
    access_modifier: Option<Option<pernixc_syntax::AccessModifier>>,

    #[builder(default, setter(strip_option))]
    members: Option<Arc<member::Member>>,
}

impl MemberBuilder<'_, '_, '_> {
    fn add_symbol(
        &mut self,
        build_context: &Context,
        new_symbol: NewSymbol,
    ) -> ID {
        let entry = Entry {
            kind: new_symbol.symbol_kind,
            name: new_symbol.name.kind.0.clone(),
            members: new_symbol.members,
            accessibility: new_symbol.access_modifier.map(|x| match x {
                Some(pernixc_syntax::AccessModifier::Private(_)) => {
                    Accessibility::Scoped(self.current_symbol_id)
                }
                Some(pernixc_syntax::AccessModifier::Internal(_)) => {
                    Accessibility::Scoped(ID::ROOT_MODULE)
                }
                Some(pernixc_syntax::AccessModifier::Public(_)) | None => {
                    Accessibility::Public
                }
            }),
            span: Some(new_symbol.name.span),
        };

        let id = generate_id(
            self.current_names.iter().map(flexstr::FlexStr::as_str),
            new_symbol.name.kind.0.as_str(),
            build_context.entries,
            entry,
        );

        match self.member_ids_by_name.entry(new_symbol.name.kind.0) {
            std::collections::hash_map::Entry::Vacant(vacant_entry) => {
                vacant_entry.insert(id);
            }
            std::collections::hash_map::Entry::Occupied(occupied_entry) => {
                build_context.handler.receive(ItemRedifinition {
                    existing_id: TargetID::Local
                        .make_global(*occupied_entry.get()),
                    new_id: TargetID::Local.make_global(id),
                    in_id: TargetID::Local.make_global(self.current_symbol_id),
                });
                self.redefinitions.insert(id);
            }
        }

        id
    }
}

impl Context<'_> {
    #[allow(clippy::too_many_lines)]
    fn create_trait<'s: 'scope, 'scope, 'env>(
        &'s self,
        module_member: &mut MemberBuilder,
        tr: pernixc_syntax::item::r#trait::Trait,
        scope: &'scope std::thread::Scope<'scope, 'env>,
    ) {
        let Some(identifier) = tr.signature().and_then(|x| x.identifier())
        else {
            return;
        };

        let trait_id = module_member.add_symbol(
            self,
            NewSymbol::builder()
                .name(identifier.clone())
                .symbol_kind(Kind::Trait)
                .generic_parameters(
                    tr.signature().and_then(|x| x.generic_parameters()),
                )
                .where_clause(tr.body().and_then(|x| {
                    x.where_clause().and_then(|x| x.predicates())
                }))
                .access_modifier(tr.access_modifier())
                .build(),
        );

        let mut trait_names = module_member.current_names.to_vec();
        trait_names.push(identifier.kind.0);

        scope.spawn(move || {
            // spawn a parallel task to handle trait members
            let mut member_ids_by_name = HashMap::<SharedStr, ID>::default();
            let mut redefinitions = HashSet::default();

            let mut trait_member_builder = MemberBuilder {
                current_names: &trait_names,
                current_symbol_id: trait_id,
                member_ids_by_name: &mut member_ids_by_name,
                redefinitions: &mut redefinitions,
            };

            for trait_member in tr
                .body()
                .and_then(|x| x.members())
                .iter()
                .flat_map(pernixc_syntax::item::Members::members)
                .filter_map(|x| x.into_line().ok())
            {
                match trait_member {
                    pernixc_syntax::item::r#trait::Member::Type(ty) => {
                        let Some(identifier) =
                            ty.signature().and_then(|x| x.identifier())
                        else {
                            continue;
                        };

                        trait_member_builder.add_symbol(
                            self,
                            NewSymbol::builder()
                                .name(identifier.clone())
                                .symbol_kind(Kind::TraitType)
                                .generic_parameters(
                                    ty.signature()
                                        .and_then(|x| x.generic_parameters()),
                                )
                                .where_clause(
                                    ty.trailing_where_clause()
                                        .and_then(|x| x.where_clause())
                                        .and_then(|x| x.predicates()),
                                )
                                .access_modifier(ty.access_modifier())
                                .build(),
                        );
                    }

                    pernixc_syntax::item::r#trait::Member::Function(f) => {
                        let Some(identifier) =
                            f.signature().and_then(|x| x.identifier())
                        else {
                            continue;
                        };

                        trait_member_builder.add_symbol(
                            self,
                            NewSymbol::builder()
                                .name(identifier.clone())
                                .symbol_kind(Kind::TraitFunction)
                                .generic_parameters(
                                    f.signature()
                                        .and_then(|x| x.generic_parameters()),
                                )
                                .where_clause(
                                    f.trailing_where_clause()
                                        .and_then(|x| x.where_clause())
                                        .and_then(|x| x.predicates()),
                                )
                                .access_modifier(f.access_modifier())
                                .build(),
                        );

                        // let global_id = TargetID::Local.make_global(id);

                        // self.engine.set_input(
                        //     &syntax::FunctionSignatureKey(global_id),
                        //     syntax::FunctionSignature {
                        //         parameters: f
                        //             .signature()
                        //             .and_then(|x| x.parameters()),
                        //         return_type: f
                        //             .signature()
                        //             .and_then(|x| x.return_type()),
                        //     },
                        // );
                    }

                    pernixc_syntax::item::r#trait::Member::Constant(cn) => {
                        let Some(identifier) =
                            cn.signature().and_then(|x| x.identifier())
                        else {
                            continue;
                        };

                        trait_member_builder.add_symbol(
                            self,
                            NewSymbol::builder()
                                .name(identifier.clone())
                                .symbol_kind(Kind::TraitConstant)
                                .generic_parameters(
                                    cn.signature()
                                        .and_then(|x| x.generic_parameters()),
                                )
                                .where_clause(
                                    cn.trailing_where_clause()
                                        .and_then(|x| x.where_clause())
                                        .and_then(|x| x.predicates()),
                                )
                                .access_modifier(cn.access_modifier())
                                .build(),
                        );
                    }
                }
            }

            // add the members to the module
            let mut entry = self.entries.get_mut(&trait_id).unwrap();
            entry.members = Some(Arc::new(member::Member {
                member_ids_by_name,
                redefinitions,
            }));
        });
    }

    fn create_enum<'s: 'scope, 'scope, 'env>(
        &'s self,
        module_member: &mut MemberBuilder,
        en: pernixc_syntax::item::r#enum::Enum,
        scope: &'scope std::thread::Scope<'scope, 'env>,
    ) {
        let Some(identifier) = en.signature().and_then(|x| x.identifier())
        else {
            return;
        };

        let enum_id = module_member.add_symbol(
            self,
            NewSymbol::builder()
                .name(identifier.clone())
                .symbol_kind(Kind::Enum)
                .generic_parameters(
                    en.signature().and_then(|x| x.generic_parameters()),
                )
                .where_clause(en.body().and_then(|x| {
                    x.where_clause().and_then(|x| x.predicates())
                }))
                .access_modifier(en.access_modifier())
                .build(),
        );

        let mut enum_names = module_member.current_names.to_vec();
        enum_names.push(identifier.kind.0);

        scope.spawn(move || {
            // spawn a parallel task to handle trait members
            let mut members = HashMap::<SharedStr, ID>::default();
            let mut redefinitions = HashSet::default();

            let mut member_builder = MemberBuilder {
                current_names: &enum_names,
                current_symbol_id: enum_id,
                member_ids_by_name: &mut members,
                redefinitions: &mut redefinitions,
            };

            for variant_syn in en
                .body()
                .and_then(|x| x.members())
                .iter()
                .flat_map(pernixc_syntax::item::Members::members)
                .filter_map(|x| x.into_line().ok())
            {
                let Some(identifier) = variant_syn.identifier() else {
                    continue;
                };

                member_builder.add_symbol(
                    self,
                    NewSymbol::builder()
                        .name(identifier.clone())
                        .symbol_kind(Kind::Variant)
                        .build(),
                );

                // let global_id = TargetID::Local.make_global(id);

                // self.engine.set_input(
                //     &syntax::VariantKey(global_id),
                //     syntax::Variant(variant_syn.association()),
                // );
            }

            // add the members to the module
            let mut entry = self.entries.get_mut(&enum_id).unwrap();
            entry.members = Some(Arc::new(member::Member {
                member_ids_by_name: members,
                redefinitions,
            }));
        });
    }

    #[allow(clippy::too_many_lines)]
    fn create_module(
        &self,
        name: &SharedStr,
        syntax_tree: &ModuleTree,
        parent_module_id: Option<ID>,
        parent_names: &[SharedStr],
    ) -> ID {
        // the id that will be assigned to the module
        let mut current_module_names = parent_names.to_vec();
        current_module_names.push(name.clone());

        let entry = Entry {
            kind: Kind::Module,
            name: name.clone(),
            members: None,
            accessibility: Some(match syntax_tree.access_modifier {
                Some(AccessModifier::Internal(_)) => {
                    Accessibility::Scoped(ID::ROOT_MODULE)
                }

                Some(AccessModifier::Private(_)) => Accessibility::Scoped(
                    parent_module_id.unwrap_or(ID::ROOT_MODULE),
                ),

                None | Some(AccessModifier::Public(_)) => Accessibility::Public,
            }),
            span: syntax_tree
                .signature
                .as_ref()
                .and_then(pernixc_syntax::item::module::Signature::identifier)
                .map(|x| x.span),
        };

        let current_module_id = if parent_module_id.is_some() {
            let id = generate_id(
                parent_names.iter().map(flexstr::FlexStr::as_str),
                name,
                self.entries,
                entry,
            );

            id
        } else {
            let id = ID::ROOT_MODULE;
            assert!(self.entries.insert(id, entry).is_none());
            id
        };

        // make sure atleast has an empty import list
        self.imports_by_global_id.entry(current_module_id).or_default();

        let mut members = HashMap::default();
        let mut redefinitions = HashSet::default();

        std::thread::scope(|scope| {
            syntax_tree
                .submodules_by_name
                .iter()
                .map(|(name, module_tree)| {
                    scope.spawn(|| {
                        (
                            name.clone(),
                            self.create_module(
                                name,
                                module_tree,
                                Some(current_module_id),
                                &current_module_names,
                            ),
                        )
                    })
                })
                .collect::<Vec<_>>()
                .into_iter()
                .for_each(|handle| {
                    let (name, id) = handle.join().unwrap();

                    assert!(
                        members.insert(name, id).is_none(),
                        "should've handled the redefinition earlier"
                    );
                });
        });

        let mut member_builder = MemberBuilder {
            current_names: &current_module_names,
            current_symbol_id: current_module_id,
            member_ids_by_name: &mut members,
            redefinitions: &mut redefinitions,
        };

        std::thread::scope(|scope| {
            for item in syntax_tree.content.iter().flat_map(|content| {
                content.members().filter_map(|x| x.into_line().ok())
            }) {
                match item {
                    pernixc_syntax::item::module::Member::Trait(tr) => {
                        self.create_trait(&mut member_builder, tr, scope);
                    }

                    pernixc_syntax::item::module::Member::Function(f) => {
                        let Some(identifier) =
                            f.signature().and_then(|x| x.identifier())
                        else {
                            continue;
                        };

                        member_builder.add_symbol(
                            self,
                            NewSymbol::builder()
                                .name(identifier)
                                .symbol_kind(Kind::Function)
                                .generic_parameters(
                                    f.signature()
                                        .and_then(|x| x.generic_parameters()),
                                )
                                .where_clause(f.body().and_then(|x| {
                                    x.where_clause()
                                        .and_then(|x| x.predicates())
                                }))
                                .access_modifier(f.access_modifier())
                                .build(),
                        );

                        // self.engine.set_input(
                        //     &syntax::FunctionSignatureKey(global_id),
                        //     syntax::FunctionSignature {
                        //         parameters: f
                        //             .signature()
                        //             .and_then(|x| x.parameters()),
                        //         return_type: f
                        //             .signature()
                        //             .and_then(|x| x.return_type()),
                        //     },
                        // );
                        // self.engine.set_input(
                        //     &syntax::StatementsKey(global_id),
                        //     syntax::Statements(f.body().and_then(|x|
                        // x.members())), );
                    }

                    pernixc_syntax::item::module::Member::Type(ty) => {
                        let Some(identifier) =
                            ty.signature().and_then(|x| x.identifier())
                        else {
                            continue;
                        };

                        member_builder.add_symbol(
                            self,
                            NewSymbol::builder()
                                .name(identifier)
                                .symbol_kind(Kind::Type)
                                .generic_parameters(
                                    ty.signature()
                                        .and_then(|x| x.generic_parameters()),
                                )
                                .where_clause(ty.body().and_then(|x| {
                                    x.trailing_where_clause().and_then(|x| {
                                        x.where_clause()
                                            .and_then(|x| x.predicates())
                                    })
                                }))
                                .access_modifier(ty.access_modifier())
                                .build(),
                        );

                        // let global_id = TargetID::Local.make_global(id);

                        // self.engine.set_input(
                        //     &syntax::TypeAliasKey(global_id),
                        //     syntax::TypeAlias(ty.body().and_then(|x|
                        // x.r#type())), );
                    }

                    pernixc_syntax::item::module::Member::Struct(st) => {
                        let Some(identifier) =
                            st.signature().and_then(|x| x.identifier())
                        else {
                            continue;
                        };

                        member_builder.add_symbol(
                            self,
                            NewSymbol::builder()
                                .name(identifier)
                                .symbol_kind(Kind::Struct)
                                .generic_parameters(
                                    st.signature()
                                        .and_then(|x| x.generic_parameters()),
                                )
                                .where_clause(st.body().and_then(|x| {
                                    x.where_clause()
                                        .and_then(|x| x.predicates())
                                }))
                                .access_modifier(st.access_modifier())
                                .build(),
                        );

                        // let global_id = TargetID::Local.make_global(id);

                        // self.engine.set_input(
                        //     &syntax::FieldsKey(global_id),
                        //     syntax::Fields(st.body().and_then(|x|
                        // x.members())), );
                    }

                    pernixc_syntax::item::module::Member::Enum(en) => {
                        self.create_enum(&mut member_builder, en, scope);
                    }

                    pernixc_syntax::item::module::Member::Constant(cn) => {
                        let Some(identifier) =
                            cn.signature().and_then(|x| x.identifier())
                        else {
                            continue;
                        };

                        member_builder.add_symbol(
                            self,
                            NewSymbol::builder()
                                .name(identifier)
                                .symbol_kind(Kind::Constant)
                                .generic_parameters(
                                    cn.signature()
                                        .and_then(|x| x.generic_parameters()),
                                )
                                .where_clause(
                                    cn.body()
                                        .and_then(|x| x.trailing_where_clause())
                                        .and_then(|x| x.where_clause())
                                        .and_then(|x| x.predicates()),
                                )
                                .access_modifier(cn.access_modifier())
                                .build(),
                        );
                    }

                    pernixc_syntax::item::module::Member::Marker(ma) => {
                        let Some(identifier) =
                            ma.signature().and_then(|x| x.identifier())
                        else {
                            continue;
                        };

                        member_builder.add_symbol(
                            self,
                            NewSymbol::builder()
                                .name(identifier)
                                .symbol_kind(Kind::Marker)
                                .generic_parameters(
                                    ma.signature()
                                        .and_then(|x| x.generic_parameters()),
                                )
                                .where_clause(
                                    ma.trailing_where_clause()
                                        .and_then(|x| x.where_clause())
                                        .and_then(|x| x.predicates()),
                                )
                                .access_modifier(ma.access_modifier())
                                .build(),
                        );
                    }

                    pernixc_syntax::item::module::Member::Import(im) => {
                        self.imports_by_global_id
                            .entry(current_module_id)
                            .or_default()
                            .push(im);
                    }

                    pernixc_syntax::item::module::Member::Module(_)
                    | pernixc_syntax::item::module::Member::Implements(_)
                    | pernixc_syntax::item::module::Member::Extern(_) => {}
                }
            }
        });

        let mut entry = self.entries.get_mut(&current_module_id).unwrap();
        entry.members = Some(Arc::new(member::Member {
            member_ids_by_name: members,
            redefinitions,
        }));

        current_module_id
    }
}

fn generate_id<'a>(
    parent_names: impl IntoIterator<Item = &'a str>,
    this_name: &str,
    generated_ids: &DashMap<ID, Entry>,
    entry: Entry,
) -> ID {
    let mut hasher = siphasher::sip::SipHasher24::default();

    for name in parent_names {
        // hash the name of the symbol
        name.hash(&mut hasher);
    }
    this_name.hash(&mut hasher);

    // encode attempts to the hasher
    let mut attempt = 0;
    loop {
        let mut attempt_hasher = hasher;
        attempt.hash(&mut attempt_hasher);

        let id = ID(attempt_hasher.finish());

        match (generated_ids.entry(id), id == ID::ROOT_MODULE) {
            (dashmap::Entry::Vacant(_), true)
            | (dashmap::Entry::Occupied(_), _) => {
                attempt += 1;
            }

            (dashmap::Entry::Vacant(vacant_entry), false) => {
                // insert the entry into the map
                vacant_entry.insert(entry);

                return id;
            }
        }
    }
}

/// Registers all the required executors to run the queries.
pub fn register_executors(executor: &mut executor::Registry) {
    executor.register(Arc::new(accessibility::Executor));
    executor.register(Arc::new(kind::Executor));
    executor.register(Arc::new(member::Executor));
    executor.register(Arc::new(name::Executor));
    executor.register(Arc::new(parent::Executor));
    executor.register(Arc::new(parent::IntermediateExecutor));
    executor.register(Arc::new(span::Executor));
    executor.register(Arc::new(diagnostic::Executor));
    executor.register(Arc::new(Executor));
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
    serde_registry.register::<kind::Key>();
    serde_registry.register::<member::Key>();
    serde_registry.register::<name::Key>();
    serde_registry.register::<parent::Key>();
    serde_registry.register::<parent::IntermediateKey>();
    serde_registry.register::<span::Key>();
    serde_registry.register::<diagnostic::Key>();
    serde_registry.register::<Key>();
}

/// Registers the keys that should be skipped during serialization and
/// deserialization in the query engine's persistence layer
pub fn skip_persistence(persistence: &mut Persistence) {
    persistence.skip_cache_value::<Key>();
}
