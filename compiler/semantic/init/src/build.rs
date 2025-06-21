use std::hash::{Hash as _, Hasher as _};

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
    name::{self, Ext, Name},
    parent::Ext as _,
    span::{self, Span},
    symbol, tree,
};

pub(super) fn create_module(
    engine_rw: &RwLock<Engine>,
    generated_ids_rw: &RwLock<HashSet<symbol::ID>>,
    name: SharedStr,
    syntax_tree: tree::Tree,
    parent_module_id: Option<symbol::ID>,
    handler: &dyn Handler<Diagnostic>,
) {
    // the id that will be assigned to the module
    let current_module_id = parent_module_id.map_or_else(
        || symbol::ID::ROOT_MODULE,
        |x| {
            let mut generated_ids = generated_ids_rw.upgradable_read();
            let id = generate_id(&engine_rw.read(), x, &name, &generated_ids);

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
            handler,
        );
    });

    let mut members = HashMap::<SharedStr, symbol::ID>::default();
    for item in syntax_tree.content.members().filter_map(|x| x.into_line().ok())
    {
        match item {
            pernixc_syntax::item::module::Member::Module(_) => {}

            pernixc_syntax::item::module::Member::Import(import) => todo!(),
            pernixc_syntax::item::module::Member::Trait(tr) => {
                let Some(identifier) =
                    tr.signature().and_then(|x| x.identifier())
                else {
                    continue;
                };

                add_symbol(
                    engine_rw,
                    identifier,
                    Kind::Trait,
                    current_module_id,
                    &mut members,
                    generated_ids_rw,
                    handler,
                );
            }
            pernixc_syntax::item::module::Member::Function(function) => todo!(),
            pernixc_syntax::item::module::Member::Type(_) => todo!(),
            pernixc_syntax::item::module::Member::Struct(_) => todo!(),
            pernixc_syntax::item::module::Member::Implements(implements) => {
                todo!()
            }
            pernixc_syntax::item::module::Member::Enum(_) => todo!(),
            pernixc_syntax::item::module::Member::Constant(constant) => todo!(),
            pernixc_syntax::item::module::Member::Extern(_) => todo!(),
            pernixc_syntax::item::module::Member::Marker(marker) => todo!(),
        }
    }
}

fn add_symbol(
    engine: &RwLock<Engine>,
    name: pernixc_syntax::Identifier,
    symbol_kind: Kind,
    parent_id: symbol::ID,
    parent_members: &mut HashMap<SharedStr, symbol::ID>,
    generated_ids_rw: &RwLock<HashSet<symbol::ID>>,
    handler: &dyn Handler<Diagnostic>,
) {
    let mut generated_ids = generated_ids_rw.upgradable_read();
    let identifier_span = name.span;
    let id =
        generate_id(&engine.read(), parent_id, &name.kind.0, &generated_ids);

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
        }
    }

    {
        let mut engine = engine.write();
        let global_id = TargetID::Local.make_global(id);
        engine.database.set_input(&name::Key(global_id), Name(name.kind.0));
        engine.database.set_input(&span::Key(global_id), Span(identifier_span));
        engine.database.set_input(&kind::Key(global_id), symbol_kind);
    }
}

fn generate_id(
    engine: &Engine,
    parent_id: symbol::ID,
    name: &str,
    generated_ids: &HashSet<symbol::ID>,
) -> symbol::ID {
    let mut hasher = fnv::FnvHasher::default();

    name.hash(&mut hasher);
    for parent in engine.scope_walker(TargetID::Local.make_global(parent_id)) {
        engine
            .get_name(TargetID::Local.make_global(parent))
            .0
            .as_str()
            .hash(&mut hasher);
    }

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
