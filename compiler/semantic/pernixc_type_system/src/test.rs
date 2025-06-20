use pernixc_semantic::term::{
    self,
    constant::Constant,
    generic_arguments::GenericArguments,
    lifetime::Lifetime,
    r#type::Type,
    sub_term::TermLocation,
    visitor::{self, MutableRecursive},
    Model,
};

use crate::term::Term;

struct Purge;

impl<M: Model> MutableRecursive<Lifetime<M>> for Purge {
    fn visit(
        &mut self,
        _: &mut Lifetime<M>,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        true
    }
}

impl<M: Model> MutableRecursive<Type<M>> for Purge {
    fn visit(
        &mut self,
        ty: &mut Type<M>,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        if ty.is_trait_member() {
            *ty = Type::Error(term::Error);
        }

        true
    }
}

impl<M: Model> MutableRecursive<Constant<M>> for Purge {
    fn visit(
        &mut self,
        _: &mut Constant<M>,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        true
    }
}

pub(super) fn purge<T: Term>(mut term: T) -> T {
    let mut purge = Purge;
    visitor::accept_recursive_mut(&mut term, &mut purge);

    term
}

pub(super) fn purge_generic_arguments<M: Model>(
    generic_arguments: GenericArguments<M>,
) -> GenericArguments<M> {
    GenericArguments {
        lifetimes: generic_arguments.lifetimes.into_iter().map(purge).collect(),
        types: generic_arguments.types.into_iter().map(purge).collect(),
        constants: generic_arguments.constants.into_iter().map(purge).collect(),
    }
}
