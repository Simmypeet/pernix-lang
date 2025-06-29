use std::{
    hash::{Hash as _, Hasher as _},
    sync::Arc,
};

use flexstr::SharedStr;
use parking_lot::RwLock;
use pernixc_handler::Handler;
use pernixc_hash::{HashMap, HashSet};
use pernixc_query::Engine;
use pernixc_target::TargetID;
use rayon::iter::{IntoParallelIterator, ParallelIterator};

use crate::{
    diagnostic::{Diagnostic, ItemRedifinition},
    kind::{self, Kind},
    member::{self, Member},
    name::{self, Name},
    span::{self, Span},
    symbol, syntax, tree,
};

#[allow(clippy::too_many_arguments, clippy::too_many_lines)]
pub(super) fn create_trait<'s: 'scope, 'm, 'r, 'scope>(
    engine_rw: &'s RwLock<Engine>,
    generated_ids_rw: &'s RwLock<HashSet<symbol::ID>>,
    tr: pernixc_syntax::item::r#trait::Trait,
    current_module_id: symbol::ID,
    current_module_names: &'s [SharedStr],
    members: &mut HashMap<SharedStr, symbol::ID>,
    redefinitions: &mut HashSet<symbol::ID>,
    handler: &'s dyn Handler<Diagnostic>,
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
    handler: &'s dyn Handler<Diagnostic>,
    scope: &rayon::Scope<'scope>,
) {
    let Some(identifier) = en.signature().and_then(|x| x.identifier()) else {
        return;
    };

    let trait_id = add_symbol(
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
                trait_id,
                &mut members,
                &mut redefinitions,
                generated_ids_rw,
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
        let global_id = TargetID::Local.make_global(trait_id);
        engine.database.set_input(
            &member::Key(global_id),
            Arc::new(Member { member_ids_by_name: members, redefinitions }),
        );
    });
}

#[allow(clippy::too_many_lines)]
pub(super) fn create_module(
    engine_rw: &RwLock<Engine>,
    generated_ids_rw: &RwLock<HashSet<symbol::ID>>,
    name: SharedStr,
    syntax_tree: tree::Tree,
    parent_module_id: Option<symbol::ID>,
    parent_names: &[SharedStr],
    handler: &dyn Handler<Diagnostic>,
) {
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
    }

    syntax_tree.submodules_by_name.into_par_iter().for_each(|(name, tree)| {
        create_module(
            engine_rw,
            generated_ids_rw,
            name,
            tree,
            parent_module_id,
            &current_module_names,
            handler,
        );
    });

    let mut members = HashMap::<SharedStr, symbol::ID>::default();
    let mut redefinitions = HashSet::default();

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
                        handler,
                    );
                }

                pernixc_syntax::item::module::Member::Import(_)
                | pernixc_syntax::item::module::Member::Module(_)
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
}

#[allow(clippy::too_many_arguments)]
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
    handler: &dyn Handler<Diagnostic>,
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
            handler.receive(Diagnostic::ItemRedifinition(ItemRedifinition {
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
        engine.database.set_input(&span::Key(global_id), Span(identifier_span));
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
