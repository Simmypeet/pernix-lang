//! Contains the definition of [`Normalizer`]

use super::{
    model::Model,
    term::{constant::Constant, lifetime::Lifetime, r#type::Type},
    Environment, ExceedLimitError,
};
use crate::symbol::table::State;

/// The object used to normalize the inference variables into the concrete term.
pub trait Normalizer<M: Model>: Sized {
    /// Normalizes the lifetime inference variable into the concrete lifetime
    /// term.
    fn normalize_lifetime(
        lifetime: &M::LifetimeInference,
        environment: &Environment<M, impl State, Self>,
    ) -> Result<Option<Lifetime<M>>, ExceedLimitError>;

    /// Normalizes the type inference variable into the concrete type
    /// term.
    fn normalize_type(
        ty: &M::TypeInference,
        environment: &Environment<M, impl State, Self>,
    ) -> Result<Option<Type<M>>, ExceedLimitError>;

    /// Normalizes the constant inference variable into the concrete constant
    /// term.
    fn normalize_constant(
        constant: &M::ConstantInference,
        environment: &Environment<M, impl State, Self>,
    ) -> Result<Option<Constant<M>>, ExceedLimitError>;
}

/// The default normalizer that does not normalize the inference variables.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct NoOp;

impl<M: Model> Normalizer<M> for NoOp {
    fn normalize_lifetime(
        _: &<M as Model>::LifetimeInference,
        _: &Environment<M, impl State, Self>,
    ) -> Result<Option<Lifetime<M>>, ExceedLimitError> {
        Ok(None)
    }

    fn normalize_type(
        _: &<M as Model>::TypeInference,
        _: &Environment<M, impl State, Self>,
    ) -> Result<Option<Type<M>>, ExceedLimitError> {
        Ok(None)
    }

    fn normalize_constant(
        _: &<M as Model>::ConstantInference,
        _: &Environment<M, impl State, Self>,
    ) -> Result<Option<Constant<M>>, ExceedLimitError> {
        Ok(None)
    }
}
