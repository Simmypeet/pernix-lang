//! Contains the definition of [`Normalizer`]

use super::{
    model::Model,
    observer::Observer,
    query::Context,
    term::{constant::Constant, r#type::Type},
    Environment, Output, OverflowError,
};
use crate::symbol::table::State;

/// The object used to normalize the inference variables into the concrete term.
pub trait Normalizer<M: Model, T: State>: Sized {
    /// Normalizes the type inference variable into the concrete type
    /// term.
    fn normalize_type(
        ty: &Type<M>,
        environment: &Environment<M, T, Self, impl Observer<M, T>>,
        context: &mut Context<M>,
    ) -> Result<Output<Type<M>, M>, OverflowError>;

    /// Normalizes the constant inference variable into the concrete constant
    /// term.
    fn normalize_constant(
        constant: &Constant<M>,
        environment: &Environment<M, T, Self, impl Observer<M, T>>,
        context: &mut Context<M>,
    ) -> Result<Output<Constant<M>, M>, OverflowError>;
}

/// The default normalizer that does not normalize the inference variables.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct NoOp;

/// The instance of [`NoOp`] normalizer.
pub const NO_OP: &'static NoOp = &NoOp;

impl<M: Model, T: State> Normalizer<M, T> for NoOp {
    fn normalize_type(
        _: &Type<M>,
        _: &Environment<M, T, Self, impl Observer<M, T>>,
        _: &mut Context<M>,
    ) -> Result<Output<Type<M>, M>, OverflowError> {
        Ok(None)
    }

    fn normalize_constant(
        _: &Constant<M>,
        _: &Environment<M, T, Self, impl Observer<M, T>>,
        _: &mut Context<M>,
    ) -> Result<Output<Constant<M>, M>, OverflowError> {
        Ok(None)
    }
}
