//! Contains the definition of [`Normalizer`]

use pernixc_term::{constant::Constant, r#type::Type, Model};

use crate::{environment::Environment, Error, Succeeded};

/// The object used to normalize the inference variables into the concrete term.
pub trait Normalizer<M: Model>: Sized {
    /// Normalizes the type inference variable into the concrete type
    /// term.
    ///
    /// # Errors
    ///
    /// See [`AbruptError`] for more information.
    fn normalize_type(
        ty: &Type<M>,
        environment: &Environment<M, Self>,
    ) -> Result<Option<Succeeded<Type<M>, M>>, Error>;

    /// Normalizes the constant inference variable into the concrete constant
    /// term.
    ///
    /// # Errors
    ///
    /// See [`AbruptError`] for more information.
    fn normalize_constant(
        constant: &Constant<M>,
        environment: &Environment<M, Self>,
    ) -> Result<Option<Succeeded<Constant<M>, M>>, Error>;
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
    ) -> Result<Option<Succeeded<Type<M>, M>>, Error> {
        Ok(None)
    }

    fn normalize_constant(
        _: &Constant<M>,
        _: &Environment<M, Self>,
    ) -> Result<Option<Succeeded<Constant<M>, M>>, Error> {
        Ok(None)
    }
}
