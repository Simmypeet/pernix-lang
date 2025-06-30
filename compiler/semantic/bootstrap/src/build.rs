use std::{
    hash::{Hash as _, Hasher as _},
    sync::Arc,
};

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
use rayon::iter::{IntoParallelIterator, ParallelIterator};

use crate::{
    accessibility::{self, Accessibility, Ext as _},
    diagnostic::{
        ConflictingUsing, Diagnostic, ExpectModule, ItemRedifinition,
        SymbolIsNotAccessible, SymbolNotFound,
        TargetRootInImportIsNotAllowedwithFrom,
    },
    import::{self, Using},
    kind::{self, Ext as _, Kind},
    member::{self, Ext as _, Member},
    name::{self, Ext as _, Name},
    span::{self, Ext as _, Span},
    symbol, syntax,
    target::{Ext as _, MapExt},
    tree,
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
                    let target_map = engine_rw.read().get_target_map();

                    let Some(id) = target_map
                        .get(identifier.kind.0.as_str())
                        .copied()
                        .filter(|x| {
                            x == &defined_in_module_id.target_id
                                || engine_rw
                                    .read()
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
            .map_or_else(|| engine_rw.read().get_name(start).0, |x| x.kind.0);

        // check if there's existing symbol right now
        let engine = engine_rw.read();
        let existing = engine
            .get_members(defined_in_module_id)
            .member_ids_by_name
            .get(&name)
            .map(|x| {
                engine
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
            let Some(from_id) = engine.resolve_simple_path(
                &from_simple_path,
                defined_in_module_id,
                true,
                false,
                handler,
            ) else {
                return;
            };

            // must be module
            if engine.get_kind(from_id) != Kind::Module {
                handler.receive(Box::new(ExpectModule {
                    module_path: from_simple_path.inner_tree().span(),
                    found_id: from_id,
                }));
                return;
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
    engine.database.set_input(
        &import::Key(defined_in_module_id),
        Arc::new(import_component),
    );
}

#[allow(clippy::too_many_arguments, clippy::too_many_lines)]
pub(super) fn create_trait<'s: 'scope, 'm, 'r, 'scope>(
    engine_rw: &'s RwLock<Engine>,
    generated_ids_rw: &'s RwLock<HashSet<symbol::ID>>,
    tr: pernixc_syntax::item::r#trait::Trait,
    current_module_id: symbol::ID,
    current_module_names: &'s [SharedStr],
    members: &mut HashMap<SharedStr, symbol::ID>,
    redefinitions: &mut HashSet<symbol::ID>,
    handler: &'s dyn Handler<Box<dyn Diagnostic>>,
    scope: &rayon::Scope<'scope>,
) {
    let Some(identifier) = tr.signature().and_then(|x| x.identifier()) else {
        return;
    };

    let trait_id = add_symbol(
        engine_rw,
        identifier.clone(),
        current_module_names.iter().map(flexstr::FlexStr::as_str),
        Kind::Trait,
        current_module_id,
        members,
        redefinitions,
        generated_ids_rw,
        Some(syntax::GenericParameters(
            tr.signature().and_then(|x| x.generic_parameters()),
        )),
        Some(syntax::WhereClause(
            tr.body()
                .and_then(|x| x.where_clause().and_then(|x| x.predicates())),
        )),
        Some(tr.access_modifier()),
        handler,
    );

    scope.spawn(move |_| {
        // spawn a parallel task to handle trait members
        let mut members = HashMap::<SharedStr, symbol::ID>::default();
        let mut redefinitions = HashSet::default();

        let mut trait_names = current_module_names.to_vec();
        trait_names.push(identifier.kind.0.clone());

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

                    add_symbol(
                        engine_rw,
                        identifier,
                        trait_names.iter().map(flexstr::FlexStr::as_str),
                        Kind::TraitType,
                        trait_id,
                        &mut members,
                        &mut redefinitions,
                        generated_ids_rw,
                        Some(syntax::GenericParameters(
                            ty.signature().and_then(|x| x.generic_parameters()),
                        )),
                        Some(syntax::WhereClause(
                            ty.trailing_where_clause().and_then(|x| {
                                x.where_clause().and_then(|x| x.predicates())
                            }),
                        )),
                        Some(ty.access_modifier()),
                        handler,
                    );
                }
                pernixc_syntax::item::r#trait::Member::Function(f) => {
                    let Some(identifier) =
                        f.signature().and_then(|x| x.identifier())
                    else {
                        continue;
                    };

                    let id = add_symbol(
                        engine_rw,
                        identifier,
                        trait_names.iter().map(flexstr::FlexStr::as_str),
                        Kind::TraitFunction,
                        trait_id,
                        &mut members,
                        &mut redefinitions,
                        generated_ids_rw,
                        Some(syntax::GenericParameters(
                            f.signature().and_then(|x| x.generic_parameters()),
                        )),
                        Some(syntax::WhereClause(
                            f.trailing_where_clause().and_then(|x| {
                                x.where_clause().and_then(|x| x.predicates())
                            }),
                        )),
                        Some(f.access_modifier()),
                        handler,
                    );

                    let mut engine_write = engine_rw.write();
                    let global_id = TargetID::Local.make_global(id);

                    engine_write.database.set_input(
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

                    add_symbol(
                        engine_rw,
                        identifier,
                        trait_names.iter().map(flexstr::FlexStr::as_str),
                        Kind::TraitConstant,
                        trait_id,
                        &mut members,
                        &mut redefinitions,
                        generated_ids_rw,
                        Some(syntax::GenericParameters(
                            cn.signature().and_then(|x| x.generic_parameters()),
                        )),
                        Some(syntax::WhereClause(
                            cn.trailing_where_clause().and_then(|x| {
                                x.where_clause().and_then(|x| x.predicates())
                            }),
                        )),
                        Some(cn.access_modifier()),
                        handler,
                    );
                }
            }
        }

        // add the members to the module
        let mut engine = engine_rw.write();
        let global_id = TargetID::Local.make_global(trait_id);
        engine.database.set_input(
            &member::Key(global_id),
            Arc::new(Member { member_ids_by_name: members, redefinitions }),
        );
    });
}

#[allow(clippy::too_many_arguments, clippy::too_many_lines)]
pub(super) fn create_enum<'s: 'scope, 'm, 'r, 'scope>(
    engine_rw: &'s RwLock<Engine>,
    generated_ids_rw: &'s RwLock<HashSet<symbol::ID>>,
    en: pernixc_syntax::item::r#enum::Enum,
    current_module_id: symbol::ID,
    current_module_names: &'s [SharedStr],
    members: &mut HashMap<SharedStr, symbol::ID>,
    redefinitions: &mut HashSet<symbol::ID>,
    handler: &'s dyn Handler<Box<dyn Diagnostic>>,
    scope: &rayon::Scope<'scope>,
) {
    let Some(identifier) = en.signature().and_then(|x| x.identifier()) else {
        return;
    };

    let enum_id = add_symbol(
        engine_rw,
        identifier.clone(),
        current_module_names.iter().map(flexstr::FlexStr::as_str),
        Kind::Enum,
        current_module_id,
        members,
        redefinitions,
        generated_ids_rw,
        Some(syntax::GenericParameters(
            en.signature().and_then(|x| x.generic_parameters()),
        )),
        Some(syntax::WhereClause(
            en.body()
                .and_then(|x| x.where_clause().and_then(|x| x.predicates())),
        )),
        Some(en.access_modifier()),
        handler,
    );

    scope.spawn(move |_| {
        // spawn a parallel task to handle trait members
        let mut members = HashMap::<SharedStr, symbol::ID>::default();
        let mut redefinitions = HashSet::default();

        let mut enum_names = current_module_names.to_vec();
        enum_names.push(identifier.kind.0.clone());

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

            let id = add_symbol(
                engine_rw,
                identifier.clone(),
                enum_names.iter().map(flexstr::FlexStr::as_str),
                Kind::Variant,
                enum_id,
                &mut members,
                &mut redefinitions,
                generated_ids_rw,
                None,
                None,
                None,
                handler,
            );

            let mut engine_write = engine_rw.write();
            let global_id = TargetID::Local.make_global(id);

            engine_write.database.set_input(
                &syntax::VariantKey(global_id),
                syntax::Variant(variant_syn.association()),
            );
        }

        // add the members to the module
        let mut engine = engine_rw.write();
        let global_id = TargetID::Local.make_global(enum_id);
        engine.database.set_input(
            &member::Key(global_id),
            Arc::new(Member { member_ids_by_name: members, redefinitions }),
        );
    });
}

#[allow(clippy::too_many_lines, clippy::too_many_arguments)]
pub(super) fn create_module(
    engine_rw: &RwLock<Engine>,
    generated_ids_rw: &RwLock<HashSet<symbol::ID>>,
    name: SharedStr,
    syntax_tree: tree::Tree,
    parent_module_id: Option<symbol::ID>,
    parent_names: &[SharedStr],
    imports_by_global_id: &pernixc_hash::DashMap<
        symbol::ID,
        Vec<pernixc_syntax::item::module::Import>,
    >,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) -> symbol::ID {
    // the id that will be assigned to the module
    let mut current_module_names = parent_names.to_vec();
    current_module_names.push(name.clone());

    let current_module_id = parent_module_id.map_or_else(
        || {
            let id = symbol::ID::ROOT_MODULE;
            generated_ids_rw.write().insert(id);
            id
        },
        |_| {
            let mut generated_ids = generated_ids_rw.upgradable_read();
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
        let mut engine = engine_rw.write();
        engine.database.set_input(
            &kind::Key(TargetID::Local.make_global(current_module_id)),
            Kind::Module,
        );
        engine.database.set_input(
            &name::Key(TargetID::Local.make_global(current_module_id)),
            Name(name),
        );
        engine.database.set_input(
            &accessibility::Key(TargetID::Local.make_global(current_module_id)),
            match syntax_tree.access_modifier {
                Some(AccessModifier::Internal(_)) => {
                    Accessibility::Scoped(symbol::ID::ROOT_MODULE)
                }
                Some(AccessModifier::Private(_)) => Accessibility::Scoped(
                    parent_module_id.unwrap_or(symbol::ID::ROOT_MODULE),
                ),
                Some(AccessModifier::Public(_)) | None => Accessibility::Public,
            },
        );
    }

    // make sure atleast has an empty import list
    imports_by_global_id.entry(current_module_id).or_default();

    let members = RwLock::new(HashMap::<SharedStr, symbol::ID>::default());
    let mut redefinitions = HashSet::default();

    syntax_tree.submodules_by_name.into_par_iter().for_each(|(name, tree)| {
        let id = create_module(
            engine_rw,
            generated_ids_rw,
            name.clone(),
            tree,
            Some(current_module_id),
            &current_module_names,
            imports_by_global_id,
            handler,
        );

        assert!(
            members.write().insert(name, id).is_none(),
            "should've handled the redefinition earlier"
        );
    });

    let mut members = members.into_inner();

    rayon::scope(|s| {
        for item in
            syntax_tree.content.members().filter_map(|x| x.into_line().ok())
        {
            match item {
                pernixc_syntax::item::module::Member::Trait(tr) => {
                    create_trait(
                        engine_rw,
                        generated_ids_rw,
                        tr,
                        current_module_id,
                        &current_module_names,
                        &mut members,
                        &mut redefinitions,
                        handler,
                        s,
                    );
                }

                pernixc_syntax::item::module::Member::Function(f) => {
                    let Some(identifier) =
                        f.signature().and_then(|x| x.identifier())
                    else {
                        continue;
                    };

                    let id = add_symbol(
                        engine_rw,
                        identifier,
                        current_module_names
                            .iter()
                            .map(flexstr::FlexStr::as_str),
                        Kind::Function,
                        current_module_id,
                        &mut members,
                        &mut redefinitions,
                        generated_ids_rw,
                        Some(syntax::GenericParameters(
                            f.signature().and_then(|x| x.generic_parameters()),
                        )),
                        Some(syntax::WhereClause(f.body().and_then(|x| {
                            x.where_clause().and_then(|x| x.predicates())
                        }))),
                        Some(f.access_modifier()),
                        handler,
                    );

                    let mut engine_write = engine_rw.write();
                    let global_id = TargetID::Local.make_global(id);

                    engine_write.database.set_input(
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
                    engine_write.database.set_input(
                        &syntax::StatementsKey(global_id),
                        syntax::Statements(f.body().and_then(|x| x.members())),
                    );
                }

                pernixc_syntax::item::module::Member::Type(ty) => {
                    let Some(identifier) =
                        ty.signature().and_then(|x| x.identifier())
                    else {
                        continue;
                    };

                    let id = add_symbol(
                        engine_rw,
                        identifier,
                        current_module_names
                            .iter()
                            .map(flexstr::FlexStr::as_str),
                        Kind::Type,
                        current_module_id,
                        &mut members,
                        &mut redefinitions,
                        generated_ids_rw,
                        Some(syntax::GenericParameters(
                            ty.signature().and_then(|x| x.generic_parameters()),
                        )),
                        Some(syntax::WhereClause(ty.body().and_then(|x| {
                            x.trailing_where_clause().and_then(|x| {
                                x.where_clause().and_then(|x| x.predicates())
                            })
                        }))),
                        Some(ty.access_modifier()),
                        handler,
                    );

                    let mut engine_write = engine_rw.write();
                    let global_id = TargetID::Local.make_global(id);

                    engine_write.database.set_input(
                        &syntax::TypeAliasKey(global_id),
                        syntax::TypeAlias(ty.body().and_then(|x| x.r#type())),
                    );
                }

                pernixc_syntax::item::module::Member::Struct(st) => {
                    let Some(identifier) =
                        st.signature().and_then(|x| x.identifier())
                    else {
                        continue;
                    };

                    let id = add_symbol(
                        engine_rw,
                        identifier,
                        current_module_names
                            .iter()
                            .map(flexstr::FlexStr::as_str),
                        Kind::Struct,
                        current_module_id,
                        &mut members,
                        &mut redefinitions,
                        generated_ids_rw,
                        Some(syntax::GenericParameters(
                            st.signature().and_then(|x| x.generic_parameters()),
                        )),
                        Some(syntax::WhereClause(st.body().and_then(|x| {
                            x.where_clause().and_then(|x| x.predicates())
                        }))),
                        Some(st.access_modifier()),
                        handler,
                    );

                    let mut engine_write = engine_rw.write();
                    let global_id = TargetID::Local.make_global(id);

                    engine_write.database.set_input(
                        &syntax::FieldsKey(global_id),
                        syntax::Fields(st.body().and_then(|x| x.members())),
                    );
                }

                pernixc_syntax::item::module::Member::Enum(en) => {
                    create_enum(
                        engine_rw,
                        generated_ids_rw,
                        en,
                        current_module_id,
                        &current_module_names,
                        &mut members,
                        &mut redefinitions,
                        handler,
                        s,
                    );
                }

                pernixc_syntax::item::module::Member::Constant(cn) => {
                    let Some(identifier) =
                        cn.signature().and_then(|x| x.identifier())
                    else {
                        continue;
                    };

                    add_symbol(
                        engine_rw,
                        identifier,
                        current_module_names
                            .iter()
                            .map(flexstr::FlexStr::as_str),
                        Kind::Constant,
                        current_module_id,
                        &mut members,
                        &mut redefinitions,
                        generated_ids_rw,
                        Some(syntax::GenericParameters(
                            cn.signature().and_then(|x| x.generic_parameters()),
                        )),
                        Some(syntax::WhereClause(cn.body().and_then(|x| {
                            x.trailing_where_clause().and_then(|x| {
                                x.where_clause().and_then(|x| x.predicates())
                            })
                        }))),
                        Some(cn.access_modifier()),
                        handler,
                    );
                }

                pernixc_syntax::item::module::Member::Marker(ma) => {
                    let Some(identifier) =
                        ma.signature().and_then(|x| x.identifier())
                    else {
                        continue;
                    };

                    add_symbol(
                        engine_rw,
                        identifier,
                        current_module_names
                            .iter()
                            .map(flexstr::FlexStr::as_str),
                        Kind::Marker,
                        current_module_id,
                        &mut members,
                        &mut redefinitions,
                        generated_ids_rw,
                        Some(syntax::GenericParameters(
                            ma.signature().and_then(|x| x.generic_parameters()),
                        )),
                        Some(syntax::WhereClause(
                            ma.trailing_where_clause().and_then(|x| {
                                x.where_clause().and_then(|x| x.predicates())
                            }),
                        )),
                        Some(ma.access_modifier()),
                        handler,
                    );
                }

                pernixc_syntax::item::module::Member::Import(im) => {
                    imports_by_global_id
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
        let mut engine = engine_rw.write();
        let global_id = TargetID::Local.make_global(current_module_id);
        engine.database.set_input(
            &member::Key(global_id),
            Arc::new(Member { member_ids_by_name: members, redefinitions }),
        );
    }

    current_module_id
}

#[allow(clippy::too_many_arguments, clippy::option_option)]
fn add_symbol<'a>(
    engine: &RwLock<Engine>,
    name: pernixc_syntax::Identifier,
    parent_names: impl IntoIterator<Item = &'a str>,
    symbol_kind: Kind,
    parent_id: symbol::ID,
    parent_members: &mut HashMap<SharedStr, symbol::ID>,
    parent_redefinitions: &mut HashSet<symbol::ID>,
    generated_ids_rw: &RwLock<HashSet<symbol::ID>>,
    extract_generic_parameters: Option<syntax::GenericParameters>,
    extract_where_clause: Option<syntax::WhereClause>,
    access_modifier: Option<Option<pernixc_syntax::AccessModifier>>,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) -> symbol::ID {
    let mut generated_ids = generated_ids_rw.upgradable_read();
    let identifier_span = name.span;

    let id = generate_id(parent_names, name.kind.0.as_str(), &generated_ids);

    generated_ids.with_upgraded(|x| {
        x.insert(id);
    });

    drop(generated_ids);

    match parent_members.entry(name.kind.0.clone()) {
        std::collections::hash_map::Entry::Vacant(vacant_entry) => {
            vacant_entry.insert(id);
        }
        std::collections::hash_map::Entry::Occupied(occupied_entry) => {
            handler.receive(Box::new(ItemRedifinition {
                existing_id: TargetID::Local.make_global(*occupied_entry.get()),
                new_id: TargetID::Local.make_global(id),
                in_id: TargetID::Local.make_global(parent_id),
            }));
            parent_redefinitions.insert(id);
        }
    }

    {
        let mut engine = engine.write();
        let global_id = TargetID::Local.make_global(id);
        engine.database.set_input(&name::Key(global_id), Name(name.kind.0));
        engine
            .database
            .set_input(&span::Key(global_id), Span(Some(identifier_span)));
        engine.database.set_input(&kind::Key(global_id), symbol_kind);

        if let Some(generic_parameters) = extract_generic_parameters {
            engine.database.set_input(
                &syntax::GenericParametersKey(global_id),
                generic_parameters,
            );
        }

        if let Some(where_clause) = extract_where_clause {
            engine
                .database
                .set_input(&syntax::WhereClauseKey(global_id), where_clause);
        }

        if let Some(access_modifier) = access_modifier {
            let accessibility = match access_modifier {
                Some(pernixc_syntax::AccessModifier::Private(_)) => {
                    Accessibility::Scoped(parent_id)
                }
                Some(pernixc_syntax::AccessModifier::Internal(_)) => {
                    Accessibility::Scoped(symbol::ID::ROOT_MODULE)
                }
                Some(pernixc_syntax::AccessModifier::Public(_)) | None => {
                    Accessibility::Public
                }
            };

            engine
                .database
                .set_input(&accessibility::Key(global_id), accessibility);
        }
    }

    id
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
