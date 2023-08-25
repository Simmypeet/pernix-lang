use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
};

use pernixc_system::arena;
use proptest::{
    prelude::Arbitrary,
    proptest,
    strategy::{Just, Strategy},
};

use crate::{
    table::{self, Table},
    Accessibility, Module,
};

#[derive(Debug, Clone, PartialEq, Eq)]
struct Submodule {
    accessibility: Accessibility,
    tree: ModuleTree,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ModuleTree {
    submodules_by_name: HashMap<String, Submodule>,
}

impl Table {
    fn new_module_from_tree(
        &mut self,
        module_tree: ModuleTree,
        name: String,
        accessibility: Accessibility,
        parent_module_id: Option<arena::ID<Module>>,
    ) -> arena::ID<Module> {
        let module_id = self.modules.insert(Module {
            name,
            accessibility,
            parent_module_id,
            module_child_ids_by_name: HashMap::new(), // will be filled later
            usings: HashSet::new(),
        });

        for (name, submodule) in module_tree.submodules_by_name {
            let submodule_id = self.new_module_from_tree(
                submodule.tree,
                name.clone(),
                submodule.accessibility,
                Some(module_id),
            );

            self.modules[module_id]
                .module_child_ids_by_name
                .insert(name, submodule_id.into());
        }

        module_id
    }
}

/// Returns a strategy that generates `using` statements for the given [`Table`].
pub(in crate::table) fn table_with_usings_strategy() -> impl Strategy<Value = Table> {
    let table_with_usings = table_with_module_strategy().prop_flat_map(|table| {
        let module_ids: Cow<[arena::ID<Module>]> = table.modules.ids().collect();

        (
            proptest::collection::hash_map(
                proptest::sample::select(module_ids.clone()),
                proptest::collection::hash_set(proptest::sample::select(module_ids), 0..=4),
                0..=table.modules.len(),
            )
            .prop_map(|mut x| {
                for (module_id, module_usings) in &mut x {
                    module_usings.remove(module_id);
                }

                x
            }),
            Just(table),
        )
    });

    table_with_usings.prop_map(|(usings_by_module_id, mut table)| {
        for (module_id, usings) in usings_by_module_id {
            table.modules[module_id].usings = usings;
        }
        table
    })
}

/// Returns a strategy that produces [`Table`] with module trees.
pub(in crate::table) fn table_with_module_strategy() -> impl Strategy<Value = Table> {
    let leaf = Just(ModuleTree {
        submodules_by_name: HashMap::new(),
    });

    let module = leaf.prop_recursive(4, 24, 4, |inner| {
        proptest::collection::hash_map(
            crate::input::module_or_target_name(),
            (Accessibility::arbitrary(), inner).prop_map(|(accessibility, tree)| Submodule {
                accessibility,
                tree,
            }),
            0..=4,
        )
        .prop_map(|submodules_by_name| ModuleTree { submodules_by_name })
    });

    let targets =
        proptest::collection::hash_map(crate::input::module_or_target_name(), module, 1..=4);
    targets.prop_map(|targets| {
        let mut table = Table::new();
        for (name, module_tree) in targets {
            let module_id =
                table.new_module_from_tree(module_tree, name.clone(), Accessibility::Public, None);

            table.target_root_module_ids_by_name.insert(name, module_id);
        }
        table
    })
}

proptest! {
    #[test]
    fn create_module_test(
        input_table in table_with_module_strategy(),
    ) {
        table::tests::verify_table(&input_table, |targets, _| {
            let mut table = Table::new();
            table.create_modules(&targets);
            Ok(table)
        })?;
    }

    #[test]
    fn populate_usings_in_workspace_test(
        input_table in table_with_usings_strategy()
    ) {
        table::tests::verify_table(&input_table, |targets, handler| {
            let mut table = Table::new();
            table.create_modules(&targets);
            table.populate_usings_in_workspace(&targets, handler);
            Ok(table)
        })?;
    }
}
