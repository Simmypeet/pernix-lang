use std::collections::HashMap;

use derive_more::{Deref, DerefMut};
use proptest::{
    prop_oneof,
    strategy::{Just, Strategy},
};

use crate::Accessibility;

#[derive(Debug, Clone, Deref, DerefMut)]
struct Arc<T>(std::sync::Arc<T>);

impl<T> Arc<T> {
    fn new(value: T) -> Self { Self(std::sync::Arc::new(value)) }
}

impl<T> PartialEq for Arc<T> {
    fn eq(&self, other: &Self) -> bool { std::sync::Arc::ptr_eq(&self.0, &other.0) }
}

impl<T> Eq for Arc<T> {}

impl<T> std::hash::Hash for Arc<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::sync::Arc::as_ptr(&self.0).hash(state);
    }
}

#[derive(Debug, Clone)]
struct ModuleTree {
    name: String,
    accessibility: Accessibility,
    submodules: HashMap<String, Arc<ModuleTree>>,
}

fn get_modules(module: &Arc<ModuleTree>) -> Vec<Arc<ModuleTree>> {
    fn populate_modules(module: &Arc<ModuleTree>, modules: &mut Vec<Arc<ModuleTree>>) {
        modules.push(module.clone());

        for submodule in module.submodules.values() {
            populate_modules(submodule, modules);
        }
    }

    let mut modules = Vec::new();

    populate_modules(module, &mut modules);

    modules
}

#[derive(Debug, Clone)]
struct ModuleUsings {
    using_vecs_by_module: HashMap<Arc<ModuleTree>, Vec<Arc<ModuleTree>>>,
}

fn module_usings(root_module: &Arc<ModuleTree>) -> impl Strategy<Value = ModuleUsings> {
    let modules = get_modules(root_module);

    proptest::collection::vec(
        (
            proptest::sample::select(modules.clone()),
            proptest::sample::subsequence(modules.clone(), 1..=4),
        )
            .prop_filter(
                "using modules must not contain the same module itself",
                |(module, usings)| !usings.contains(module),
            ),
        0..=modules.len(),
    )
    .prop_map(move |using_vecs_by_module| ModuleUsings {
        using_vecs_by_module: using_vecs_by_module.into_iter().collect(),
    })
}

fn accessibility() -> impl Strategy<Value = Accessibility> {
    prop_oneof![
        Just(Accessibility::Public),
        Just(Accessibility::Private),
        Just(Accessibility::Internal),
    ]
}

fn module_tree() -> impl Strategy<Value = Arc<ModuleTree>> {
    let leaf = (
        pernixc_lexical::token::strategy::identifier(),
        accessibility(),
    )
        .prop_map(|(name, accessibility)| {
            Arc::new(ModuleTree {
                name,
                accessibility,
                submodules: HashMap::new(),
            })
        });

    leaf.prop_recursive(8, 64, 8, |inner| {
        (
            pernixc_lexical::token::strategy::identifier(),
            accessibility(),
            proptest::collection::vec(inner, 0..=8),
        )
            .prop_map(|(name, accessibility, submodules)| {
                Arc::new(ModuleTree {
                    name,
                    accessibility,
                    submodules: submodules
                        .into_iter()
                        .map(|submodule| (submodule.name.clone(), submodule))
                        .collect(),
                })
            })
    })
}
