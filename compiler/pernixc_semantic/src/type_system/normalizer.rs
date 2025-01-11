//! Contains the definition of [`Normalizer`]

use super::{
    model::Model,
    query::Context,
    term::{constant::Constant, r#type::Type},
    Environment, Output, OverflowError,
};

/// The object used to normalize the inference variables into the concrete term.
pub trait Normalizer<M: Model>: Sized {
    /// Normalizes the type inference variable into the concrete type
    /// term.
    ///
    /// # Errors
    ///
    /// See [`OverflowError`] for more information.
    fn normalize_type(
        ty: &Type<M>,
        environment: &Environment<M, Self>,
        context: &mut Context<M>,
    ) -> Result<Output<Type<M>, M>, OverflowError>;

    /// Normalizes the constant inference variable into the concrete constant
    /// term.
    ///
    /// # Errors
    ///
    /// See [`OverflowError`] for more information.
    fn normalize_constant(
        constant: &Constant<M>,
        environment: &Environment<M, Self>,
        context: &mut Context<M>,
    ) -> Result<Output<Constant<M>, M>, OverflowError>;
}

/// The default normalizer that does not normalize the inference variables.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct NoOp;

/// The instance of [`NoOp`] normalizer.
pub const NO_OP: &NoOp = &NoOp;

impl<M: Model> Normalizer<M> for NoOp {
    fn normalize_type(
        _: &Type<M>,
        _: &Environment<M, Self>,
        _: &mut Context<M>,
    ) -> Result<Output<Type<M>, M>, OverflowError> {
        Ok(None)
    }

    fn normalize_constant(
        _: &Constant<M>,
        _: &Environment<M, Self>,
        _: &mut Context<M>,
    ) -> Result<Output<Constant<M>, M>, OverflowError> {
        Ok(None)
    }
}
