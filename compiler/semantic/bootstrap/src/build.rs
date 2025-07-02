use std::{
    hash::{Hash as _, Hasher as _},
    sync::Arc,
};

use derive_new::new;
use flexstr::SharedStr;
use parking_lot::RwLock;
use pernixc_handler::Handler;
use pernixc_hash::{HashMap, HashSet};
use pernixc_query::Engine;
use pernixc_source_file::SourceElement;
use pernixc_syntax::{
    item::module::ImportItems, AccessModifier, SimplePathRoot,
};
use pernixc_target::{Global, TargetID};
use rayon::iter::{
    IntoParallelIterator, IntoParallelRefIterator, ParallelIterator,
};

use crate::{
    accessibility::{self, Accessibility, Ext},
    diagnostic::{
        ConflictingUsing, Diagnostic, ExpectModule, ItemRedifinition,
        SymbolIsMoreAccessibleThanParent, SymbolIsNotAccessible,
        SymbolNotFound, TargetRootInImportIsNotAllowedwithFrom,
    },
    import::{self, Using},
    kind::{self, Ext as _, Kind},
    member::{self, Ext as _, Member},
    name::{self, Ext as _, Name},
    span::{self, Ext as _, Span},
    symbol, syntax,
    target::{Ext as _, MapExt},
    tree, HierarchyRelationship,
};

#[allow(clippy::too_many_lines)]
fn process_import_items(
    engine_rw: &RwLock<Engine>,
    defined_in_module_id: Global<symbol::ID>,
    import: &pernixc_syntax::item::module::Import,
    import_item: &ImportItems,
    start_from: Option<Global<symbol::ID>>,
    import_component: &mut import::Import,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) {
    'item: for import_item in import_item.items() {
        // if start from , then all the import items can't have `target` as
        // it root path

        let (Some(root), Some(simple_path)) = (
            import_item.simple_path().and_then(|x| x.root()),
            import_item.simple_path(),
        ) else {
            continue;
        };

        if root.is_target() && import.from().is_some() {
            handler.receive(Box::new(TargetRootInImportIsNotAllowedwithFrom {
                target_root_span: root.span(),
            }));
            continue;
        }

        let mut start = match start_from {
            Some(current) => {
                let identifier_root = root.as_identifier().unwrap();

                let Some(id) = engine_rw
                    .read()
                    .tracked()
                    .get_members(current)
                    .member_ids_by_name
                    .get(identifier_root.kind.0.as_str())
                    .copied()
                else {
                    handler.receive(Box::new(SymbolNotFound {
                        searched_item_id: Some(current),
                        resolution_span: root.span(),
                        name: identifier_root.kind.0.clone(),
                    }));
                    continue;
                };

                let result = Global::new(current.target_id, id);
                if !engine_rw
                    .read()
                    .tracked()
                    .symbol_accessible(defined_in_module_id, result)
                {
                    handler.receive(Box::new(SymbolIsNotAccessible {
                        referring_site: defined_in_module_id,
                        referred: Global::new(current.target_id, id),
                        referred_span: root.span(),
                    }));
                }

                result
            }

            None => match root {
                SimplePathRoot::Target(_) => Global::new(
                    defined_in_module_id.target_id,
                    symbol::ID::ROOT_MODULE,
                ),
                SimplePathRoot::Identifier(identifier) => {
                    let target_map =
                        engine_rw.read().tracked().get_target_map();

                    let Some(id) = target_map
                        .get(identifier.kind.0.as_str())
                        .copied()
                        .filter(|x| {
                            x == &defined_in_module_id.target_id
                                || engine_rw
                                    .read()
                                    .tracked()
                                    .get_target(defined_in_module_id.target_id)
                                    .linked_targets
                                    .contains(x)
                        })
                    else {
                        handler.receive(Box::new(SymbolNotFound {
                            searched_item_id: None,
                            resolution_span: identifier.span,
                            name: identifier.kind.0.clone(),
                        }));
                        continue;
                    };

                    Global::new(id, symbol::ID::ROOT_MODULE)
                }
            },
        };

        for rest in simple_path.subsequences().filter_map(|x| x.identifier()) {
            let Some(next) = engine_rw
                .read()
                .tracked()
                .get_members(start)
                .member_ids_by_name
                .get(rest.kind.0.as_str())
                .copied()
            else {
                handler.receive(Box::new(SymbolNotFound {
                    searched_item_id: Some(start),
                    resolution_span: rest.span,
                    name: rest.kind.0.clone(),
                }));
                continue 'item;
            };

            let next_id = Global::new(start.target_id, next);

            if !engine_rw
                .read()
                .tracked()
                .symbol_accessible(defined_in_module_id, next_id)
            {
                handler.receive(Box::new(SymbolIsNotAccessible {
                    referring_site: defined_in_module_id,
                    referred: next_id,
                    referred_span: rest.span,
                }));
            }

            start = next_id;
        }

        let name = import_item
            .alias()
            .as_ref()
            .and_then(pernixc_syntax::item::module::Alias::identifier)
            .map_or_else(
                || engine_rw.read().tracked().get_name(start).0,
                |x| x.kind.0,
            );

        // check if there's existing symbol right now
        let engine = engine_rw.read();
        let existing = engine
            .tracked()
            .get_members(defined_in_module_id)
            .member_ids_by_name
            .get(&name)
            .map(|x| {
                engine
                    .tracked()
                    .get_span(Global::new(defined_in_module_id.target_id, *x))
                    .0
            })
            .or_else(|| {
                import_component.get(name.as_str()).map(|x| Some(x.span))
            });

        if let Some(existing) = existing {
            handler.receive(Box::new(ConflictingUsing {
                using_span: import_item
                    .alias()
                    .as_ref()
                    .map_or_else(|| import_item.span(), SourceElement::span),
                name: name.clone(),
                module_id: defined_in_module_id,
                conflicting_span: existing,
            }));
        } else {
            import_component.insert(name, Using {
                id: start,
                span: import_item
                    .alias()
                    .as_ref()
                    .map_or_else(|| import_item.span(), SourceElement::span),
            });
        }
    }
}

pub(super) fn symbol_is_more_accessible_than_its_parent_check(
    engine: &Engine,
    symbol_ids: &HashSet<symbol::ID>,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) {
    symbol_ids
        .par_iter()
        .copied()
        .map(|x| TargetID::Local.make_global(x))
        .for_each(|symbol_id| {
            let kind = engine.tracked().get_kind(symbol_id);

            if kind != Kind::Trait {
                return;
            }

            let members = engine.tracked().get_members(symbol_id);
            let parent_accessibility =
                engine.tracked().get_accessibility(symbol_id);

            members
                .member_ids_by_name
                .par_iter()
                .map(|x| *x.1)
                .chain(members.redefinitions.par_iter().copied())
                .for_each(|member_id| {
                    let member_accessibility =
                        engine.tracked().get_accessibility(Global::new(
                            symbol_id.target_id,
                            member_id,
                        ));

                    if engine.tracked().accessibility_hierarchy_relationship(
                        TargetID::Local,
                        member_accessibility,
                        parent_accessibility,
                    ) == HierarchyRelationship::Parent
                    {
                        handler.receive(Box::new(
                            SymbolIsMoreAccessibleThanParent {
                                symbol_id: TargetID::Local
                                    .make_global(member_id),
                                parent_id: symbol_id,
                            },
                        ));
                    }
                });
        });
}

#[allow(clippy::too_many_lines)]
pub(super) fn insert_imports(
    engine_rw: &RwLock<Engine>,
    defined_in_module_id: Global<symbol::ID>,
    imports: impl IntoIterator<Item = pernixc_syntax::item::module::Import>,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) {
    let mut import_component = import::Import::default();

    for import in imports {
        let start_from = if let Some(from_simple_path) =
            import.from().and_then(|x| x.simple_path())
        {
            let engine = engine_rw.read();
            let Some(from_id) = engine.tracked().resolve_simple_path(
                &from_simple_path,
                defined_in_module_id,
                true,
                false,
                handler,
            ) else {
                return;
            };

            // must be module
            if engine.tracked().get_kind(from_id) != Kind::Module {
                handler.receive(Box::new(ExpectModule {
                    module_path: from_simple_path.inner_tree().span(),
                    found_id: from_id,
                }));
                continue;
            }

            Some(from_id)
        } else {
            None
        };

        let import_items = import.items().into_iter().flat_map(|x| {
            let test: Option<ImportItems> = match x {
            pernixc_syntax::item::module::ImportItemsKind::Regular(
                import_items,
            ) => Some(import_items),

            pernixc_syntax::item::module::ImportItemsKind::Parenthesized(i) => {
                i.import_items()
            }
        };

            test.into_iter()
        });

        for import_item in import_items {
            process_import_items(
                engine_rw,
                defined_in_module_id,
                &import,
                &import_item,
                start_from,
                &mut import_component,
                handler,
            );
        }
    }

    let mut engine = engine_rw.write();
    engine.set_input(
        &import::Key(defined_in_module_id),
        Arc::new(import_component),
    );
}

pub(super) struct MemberBuilder<'a, 'm, 'r> {
    current_names: &'a [SharedStr],
    current_symbol_id: symbol::ID,

    member_ids_by_name: &'m mut HashMap<SharedStr, symbol::ID>,
    redefinitions: &'r mut HashSet<symbol::ID>,
}

#[derive(typed_builder::TypedBuilder)]
#[allow(clippy::option_option)]
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
}

impl MemberBuilder<'_, '_, '_> {
    fn add_symbol(
        &mut self,
        build_context: &Context,
        new_symbol: NewSymbol,
    ) -> symbol::ID {
        let mut generated_ids =
            build_context.generated_ids_rw.upgradable_read();

        let identifier_span = new_symbol.name.span;

        let id = generate_id(
            self.current_names.iter().map(flexstr::FlexStr::as_str),
            new_symbol.name.kind.0.as_str(),
            &generated_ids,
        );

        generated_ids.with_upgraded(|x| {
            x.insert(id);
        });

        drop(generated_ids);

        match self.member_ids_by_name.entry(new_symbol.name.kind.0.clone()) {
            std::collections::hash_map::Entry::Vacant(vacant_entry) => {
                vacant_entry.insert(id);
            }
            std::collections::hash_map::Entry::Occupied(occupied_entry) => {
                build_context.handler.receive(Box::new(ItemRedifinition {
                    existing_id: TargetID::Local
                        .make_global(*occupied_entry.get()),
                    new_id: TargetID::Local.make_global(id),
                    in_id: TargetID::Local.make_global(self.current_symbol_id),
                }));
                self.redefinitions.insert(id);
            }
        }

        {
            let mut engine = build_context.engine_rw.write();
            let global_id = TargetID::Local.make_global(id);
            engine
                .set_input(&name::Key(global_id), Name(new_symbol.name.kind.0));
            engine
                .set_input(&span::Key(global_id), Span(Some(identifier_span)));
            engine.set_input(&kind::Key(global_id), new_symbol.symbol_kind);

            if let Some(generic_parameters) = new_symbol.generic_parameters {
                engine.set_input(
                    &syntax::GenericParametersKey(global_id),
                    syntax::GenericParameters(generic_parameters),
                );
            }

            if let Some(where_clause) = new_symbol.where_clause {
                engine.set_input(
                    &syntax::WhereClauseKey(global_id),
                    syntax::WhereClause(where_clause),
                );
            }

            if let Some(access_modifier) = new_symbol.access_modifier {
                let accessibility = match access_modifier {
                    Some(pernixc_syntax::AccessModifier::Private(_)) => {
                        Accessibility::Scoped(self.current_symbol_id)
                    }
                    Some(pernixc_syntax::AccessModifier::Internal(_)) => {
                        Accessibility::Scoped(symbol::ID::ROOT_MODULE)
                    }
                    Some(pernixc_syntax::AccessModifier::Public(_)) | None => {
                        Accessibility::Public
                    }
                };

                engine.set_input(&accessibility::Key(global_id), accessibility);
            }
        }

        id
    }
}

#[derive(new)]
pub(super) struct Context<'a> {
    engine_rw: &'a RwLock<Engine>,
    generated_ids_rw: &'a RwLock<HashSet<symbol::ID>>,
    imports_by_global_id: &'a pernixc_hash::DashMap<
        symbol::ID,
        Vec<pernixc_syntax::item::module::Import>,
    >,
    handler: &'a dyn Handler<Box<dyn Diagnostic>>,
}

impl Context<'_> {
    fn create_enum<'s: 'scope, 'scope>(
        &'s self,
        module_member: &mut MemberBuilder,
        en: pernixc_syntax::item::r#enum::Enum,
        scope: &rayon::Scope<'scope>,
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

        scope.spawn(move |_| {
            // spawn a parallel task to handle trait members
            let mut members = HashMap::<SharedStr, symbol::ID>::default();
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

                let id = member_builder.add_symbol(
                    self,
                    NewSymbol::builder()
                        .name(identifier.clone())
                        .symbol_kind(Kind::Variant)
                        .build(),
                );

                let mut engine_write = self.engine_rw.write();
                let global_id = TargetID::Local.make_global(id);

                engine_write.set_input(
                    &syntax::VariantKey(global_id),
                    syntax::Variant(variant_syn.association()),
                );
            }

            // add the members to the module
            let mut engine = self.engine_rw.write();
            let global_id = TargetID::Local.make_global(enum_id);
            engine.set_input(
                &member::Key(global_id),
                Arc::new(Member { member_ids_by_name: members, redefinitions }),
            );
        });
    }

    #[allow(clippy::too_many_lines)]
    fn create_trait<'s: 'scope, 'scope>(
        &'s self,
        module_member: &mut MemberBuilder,
        tr: pernixc_syntax::item::r#trait::Trait,
        scope: &rayon::Scope<'scope>,
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

        scope.spawn(move |_| {
            // spawn a parallel task to handle trait members
            let mut members = HashMap::<SharedStr, symbol::ID>::default();
            let mut redefinitions = HashSet::default();

            let mut trait_member_builder = MemberBuilder {
                current_names: &trait_names,
                current_symbol_id: trait_id,
                member_ids_by_name: &mut members,
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

                        let id = trait_member_builder.add_symbol(
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

                        let mut engine_write = self.engine_rw.write();
                        let global_id = TargetID::Local.make_global(id);

                        engine_write.set_input(
                            &syntax::FunctionSignatureKey(global_id),
                            syntax::FunctionSignature {
                                parameters: f
                                    .signature()
                                    .and_then(|x| x.parameters()),
                                return_type: f
                                    .signature()
                                    .and_then(|x| x.return_type()),
                            },
                        );
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
            let mut engine = self.engine_rw.write();
            let global_id = TargetID::Local.make_global(trait_id);
            engine.set_input(
                &member::Key(global_id),
                Arc::new(Member { member_ids_by_name: members, redefinitions }),
            );
        });
    }

    #[allow(clippy::too_many_lines)]
    pub(super) fn create_module(
        &self,
        name: SharedStr,
        syntax_tree: tree::Tree,
        parent_module_id: Option<symbol::ID>,
        parent_names: &[SharedStr],
    ) -> symbol::ID {
        // the id that will be assigned to the module
        let mut current_module_names = parent_names.to_vec();
        current_module_names.push(name.clone());

        let current_module_id = parent_module_id.map_or_else(
            || {
                let id = symbol::ID::ROOT_MODULE;
                self.generated_ids_rw.write().insert(id);
                id
            },
            |_| {
                let mut generated_ids = self.generated_ids_rw.upgradable_read();
                let id = generate_id(
                    parent_names.iter().map(flexstr::FlexStr::as_str),
                    &name,
                    &generated_ids,
                );

                generated_ids.with_upgraded(|x| {
                    x.insert(id);
                });

                id
            },
        );

        {
            let mut engine = self.engine_rw.write();
            engine.set_input(
                &kind::Key(TargetID::Local.make_global(current_module_id)),
                Kind::Module,
            );
            engine.set_input(
                &name::Key(TargetID::Local.make_global(current_module_id)),
                Name(name),
            );
            engine.set_input(
                &accessibility::Key(
                    TargetID::Local.make_global(current_module_id),
                ),
                match syntax_tree.access_modifier {
                    Some(AccessModifier::Internal(_)) => {
                        Accessibility::Scoped(symbol::ID::ROOT_MODULE)
                    }
                    Some(AccessModifier::Private(_)) => Accessibility::Scoped(
                        parent_module_id.unwrap_or(symbol::ID::ROOT_MODULE),
                    ),
                    Some(AccessModifier::Public(_)) | None => {
                        Accessibility::Public
                    }
                },
            );
        }

        // make sure atleast has an empty import list
        self.imports_by_global_id.entry(current_module_id).or_default();

        let members = RwLock::new(HashMap::<SharedStr, symbol::ID>::default());
        let mut redefinitions = HashSet::default();

        syntax_tree.submodules_by_name.into_par_iter().for_each(
            |(name, tree)| {
                let id = self.create_module(
                    name.clone(),
                    tree,
                    Some(current_module_id),
                    &current_module_names,
                );

                assert!(
                    members.write().insert(name, id).is_none(),
                    "should've handled the redefinition earlier"
                );
            },
        );

        let mut members = members.into_inner();
        let mut member_builder = MemberBuilder {
            current_names: &current_module_names,
            current_symbol_id: current_module_id,
            member_ids_by_name: &mut members,
            redefinitions: &mut redefinitions,
        };

        rayon::scope(|s| {
            for item in
                syntax_tree.content.members().filter_map(|x| x.into_line().ok())
            {
                match item {
                    pernixc_syntax::item::module::Member::Trait(tr) => {
                        self.create_trait(&mut member_builder, tr, s);
                    }

                    pernixc_syntax::item::module::Member::Function(f) => {
                        let Some(identifier) =
                            f.signature().and_then(|x| x.identifier())
                        else {
                            continue;
                        };

                        let id = member_builder.add_symbol(
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

                        let mut engine_write = self.engine_rw.write();
                        let global_id = TargetID::Local.make_global(id);

                        engine_write.set_input(
                            &syntax::FunctionSignatureKey(global_id),
                            syntax::FunctionSignature {
                                parameters: f
                                    .signature()
                                    .and_then(|x| x.parameters()),
                                return_type: f
                                    .signature()
                                    .and_then(|x| x.return_type()),
                            },
                        );
                        engine_write.set_input(
                            &syntax::StatementsKey(global_id),
                            syntax::Statements(
                                f.body().and_then(|x| x.members()),
                            ),
                        );
                    }

                    pernixc_syntax::item::module::Member::Type(ty) => {
                        let Some(identifier) =
                            ty.signature().and_then(|x| x.identifier())
                        else {
                            continue;
                        };

                        let id = member_builder.add_symbol(
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

                        let mut engine_write = self.engine_rw.write();
                        let global_id = TargetID::Local.make_global(id);

                        engine_write.set_input(
                            &syntax::TypeAliasKey(global_id),
                            syntax::TypeAlias(
                                ty.body().and_then(|x| x.r#type()),
                            ),
                        );
                    }

                    pernixc_syntax::item::module::Member::Struct(st) => {
                        let Some(identifier) =
                            st.signature().and_then(|x| x.identifier())
                        else {
                            continue;
                        };

                        let id = member_builder.add_symbol(
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

                        let mut engine_write = self.engine_rw.write();
                        let global_id = TargetID::Local.make_global(id);

                        engine_write.set_input(
                            &syntax::FieldsKey(global_id),
                            syntax::Fields(st.body().and_then(|x| x.members())),
                        );
                    }

                    pernixc_syntax::item::module::Member::Enum(en) => {
                        self.create_enum(&mut member_builder, en, s);
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

        // add the members to the module
        {
            let mut engine = self.engine_rw.write();
            let global_id = TargetID::Local.make_global(current_module_id);
            engine.set_input(
                &member::Key(global_id),
                Arc::new(Member { member_ids_by_name: members, redefinitions }),
            );
        }

        current_module_id
    }
}

fn generate_id<'a>(
    parent_names: impl IntoIterator<Item = &'a str>,
    this_name: &str,
    generated_ids: &HashSet<symbol::ID>,
) -> symbol::ID {
    let mut hasher = fnv::FnvHasher::default();

    for name in parent_names {
        // hash the name of the symbol
        name.hash(&mut hasher);
    }
    this_name.hash(&mut hasher);

    // encode attempts to the hasher
    let mut attempt = 0;
    loop {
        let mut attempt_hasher = fnv::FnvHasher::with_key(hasher.finish());
        attempt.hash(&mut attempt_hasher);

        let id = symbol::ID(attempt_hasher.finish());

        if generated_ids.contains(&id) || id == symbol::ID::ROOT_MODULE {
            // try again with a new attempt
            attempt += 1;
        } else {
            return id;
        }
    }
}
