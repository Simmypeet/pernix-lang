//! Contains the definition of [`Normalizer`]

use pernixc_term::{constant::Constant, instance::Instance, r#type::Type};

use crate::{OverflowError, Succeeded, environment::Environment};

/// The object used to normalize the inference variables into the concrete term.
pub trait Normalizer: Sized + Send + Sync {
    /// Normalizes the type inference variable into the concrete type
    /// term.
    ///
    /// # Errors
    ///
    /// See [`OverflowError`] for more information.
    fn normalize_type(
        ty: &Type,
        environment: &Environment<Self>,
    ) -> impl std::future::Future<
        Output = Result<Option<Succeeded<Type>>, OverflowError>,
    > + Send;

    /// Normalizes the constant inference variable into the concrete constant
    /// term.
    ///
    /// # Errors
    ///
    /// See [`OverflowError`] for more information.
    fn normalize_constant(
        constant: &Constant,
        environment: &Environment<Self>,
    ) -> impl std::future::Future<
        Output = Result<Option<Succeeded<Constant>>, OverflowError>,
    > + Send;

    /// Normalizes the instance inference variable into the concrete instance
    /// term.
    ///
    /// # Errors
    ///
    /// See [`OverflowError`] for more information.
    fn normalize_instance(
        instance: &Instance,
        environment: &Environment<Self>,
    ) -> impl std::future::Future<
        Output = Result<Option<Succeeded<Instance>>, OverflowError>,
    > + Send;
}

/// The default normalizer that does not normalize the inference variables.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct NoOp;

/// The instance of [`NoOp`] normalizer.
pub const NO_OP: &NoOp = &NoOp;

impl Normalizer for NoOp {
    async fn normalize_type(
        _: &Type,
        _: &Environment<'_, Self>,
    ) -> Result<Option<Succeeded<Type>>, OverflowError> {
        Ok(None)
    }

    async fn normalize_constant(
        _: &Constant,
        _: &Environment<'_, Self>,
    ) -> Result<Option<Succeeded<Constant>>, OverflowError> {
        Ok(None)
    }

    async fn normalize_instance(
        _: &Instance,
        _: &Environment<'_, Self>,
    ) -> Result<Option<Succeeded<Instance>>, OverflowError> {
        Ok(None)
    }
}
