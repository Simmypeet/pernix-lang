use std::sync::Arc;

use pernixc_query::Engine;
use pernixc_term::{
    constant::Constant,
    generic_arguments::GenericArguments,
    lifetime::Lifetime,
    r#type::Type,
    sub_term::TermLocation,
    visitor::{self, MutableRecursive},
};

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

pub fn create_engine() -> Arc<Engine> {
    let mut engine = Engine::default();
    engine.runtime.executor.register(Arc::new(order::ImplementsOrderExecutor));
    Arc::new(engine)
}
