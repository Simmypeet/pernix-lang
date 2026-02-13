use std::sync::Arc;

use pernixc_qbice::{Engine, InMemoryFactory};
use pernixc_term::{
    constant::Constant,
    generic_arguments::GenericArguments,
    lifetime::Lifetime,
    sub_term::TermLocation,
    r#type::Type,
    visitor::{self, MutableRecursive},
};
use qbice::{serialize::Plugin, stable_hash::SeededStableHasherBuilder};
use tempfile::tempdir;

use crate::{order, term::Term};

struct Purge;

impl MutableRecursive<Lifetime> for Purge {
    fn visit(
        &mut self,
        _: &mut Lifetime,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        true
    }
}

impl MutableRecursive<Type> for Purge {
    fn visit(
        &mut self,
        ty: &mut Type,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        if ty.is_trait_member() {
            *ty = Type::Error(pernixc_term::error::Error);
        }

        true
    }
}

impl MutableRecursive<Constant> for Purge {
    fn visit(
        &mut self,
        _: &mut Constant,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        true
    }
}

pub(super) fn purge_trait_associated_type<T: Term>(mut term: T) -> T {
    let mut purge = Purge;
    visitor::accept_recursive_mut(&mut term, &mut purge);

    term
}

pub(super) fn purge_trait_associated_type_in_generic_arguments(
    generic_arguments: GenericArguments,
) -> GenericArguments {
    GenericArguments {
        lifetimes: generic_arguments
            .lifetimes
            .into_iter()
            .map(purge_trait_associated_type)
            .collect(),
        types: generic_arguments
            .types
            .into_iter()
            .map(purge_trait_associated_type)
            .collect(),
        constants: generic_arguments
            .constants
            .into_iter()
            .map(purge_trait_associated_type)
            .collect(),
    }
}

pub async fn create_test_engine() -> (Arc<Engine>, tempfile::TempDir) {
    let tempdir = tempdir().unwrap();
    let mut engine = Engine::new_with(
        Plugin::default(),
        InMemoryFactory,
        SeededStableHasherBuilder::new(0),
    )
    .await
    .unwrap();

    engine.register_executor(Arc::new(order::ImplementsOrderExecutor));

    (Arc::new(engine), tempdir)
}
