use std::sync::Arc;

use pernixc_extend::extend;
use pernixc_qbice::{Engine, InMemoryFactory};
use pernixc_term::{
    constant::Constant,
    generic_arguments::GenericArguments,
    instance::Instance,
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
        if ty.is_instance_associated() {
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

impl MutableRecursive<Instance> for Purge {
    fn visit(
        &mut self,
        inst: &mut Instance,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        if inst.is_instance_associated() {
            *inst = Instance::Error(pernixc_term::error::Error);
        }

        true
    }
}

pub(super) fn purge_instance_associated<T: Term>(mut term: T) -> T {
    let mut purge = Purge;
    visitor::accept_recursive_mut(&mut term, &mut purge);
    term
}

fn purge_instance_associated_mut<T: Term>(term: &mut T) {
    let mut purge = Purge;
    visitor::accept_recursive_mut(term, &mut purge);
}

#[extend]
pub(super) fn purge_instance_associated_in_generic_args(
    mut self: GenericArguments,
) -> GenericArguments {
    for arg in self.lifetimes_mut() {
        purge_instance_associated_mut(arg);
    }

    for arg in self.types_mut() {
        purge_instance_associated_mut(arg);
    }

    for arg in self.constants_mut() {
        purge_instance_associated_mut(arg);
    }

    for arg in self.instances_mut() {
        purge_instance_associated_mut(arg);
    }

    self
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
