//! Contains the definition of [`Fresh`].

use super::{
    model::Model,
    sub_term::TermLocation,
    term::{constant::Constant, lifetime::Lifetime, r#type::Type, Term},
    visitor::{self, MutableRecursive},
};

/// Trait for generating a fresh inference variable.
pub trait Fresh {
    /// Generates a new inference variable.
    fn fresh() -> Self;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
struct FreshLifetimeInference;

impl<M: Model> MutableRecursive<Lifetime<M>> for FreshLifetimeInference
where
    M::LifetimeInference: Fresh,
{
    fn visit(
        &mut self,
        term: &mut Lifetime<M>,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        *term = Lifetime::Inference(M::LifetimeInference::fresh());
        true
    }
}

impl<M: Model> MutableRecursive<Type<M>> for FreshLifetimeInference
where
    M::LifetimeInference: Fresh,
{
    fn visit(
        &mut self,
        _: &mut Type<M>,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        // do nothing
        true
    }
}

impl<M: Model> MutableRecursive<Constant<M>> for FreshLifetimeInference
where
    M::LifetimeInference: Fresh,
{
    fn visit(
        &mut self,
        _: &mut Constant<M>,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        // do nothing
        true
    }
}

/// Replaces all the lifetime terms found in the given `term` in place with
/// fresh lifetime inference created by the [`Fresh`] trait.
pub fn replace_with_fresh_lifeteime_inference<T: Term>(term: &mut T)
where
    <T::Model as Model>::LifetimeInference: Fresh,
{
    let mut fresh_lifetime_inference = FreshLifetimeInference;
    visitor::accept_recursive_mut(term, &mut fresh_lifetime_inference);
}
