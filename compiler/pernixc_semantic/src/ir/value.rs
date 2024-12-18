//! Contains the definition of [`Value`].

use literal::Literal;
use register::Register;

use super::{representation::Values, Transform, TypeOfError};
use crate::{
    arena::{Key, ID},
    symbol::{table, GlobalID},
    type_system::{
        environment::Environment, model::Model, normalizer::Normalizer,
        observer::Observer, simplify, term::r#type::Type, Succeeded,
    },
};

pub mod literal;
pub mod register;

/// An enumeration representing a value in the IR.
///
/// The value can be either a [`Register`] or a [`Literal`].
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    enum_as_inner::EnumAsInner,
    derive_more::From,
)]
#[allow(missing_docs)]
pub enum Value<M: Model> {
    Register(ID<Register<M>>),
    Literal(Literal<M>),
}

impl<M: Model> Value<M> {
    /// Transforms the [`Value`] another model using the given transformer.
    #[allow(clippy::missing_errors_doc)]
    pub fn transform_model<T: Transform<Type<M>>>(
        self,
        transformer: &mut T,
    ) -> Result<Value<T::Target>, T::Error> {
        Ok(match self {
            Self::Register(register) => {
                Value::Register(ID::from_index(register.into_index()))
            }
            Self::Literal(literal) => {
                Value::Literal(literal.transform_model(transformer)?)
            }
        })
    }
}

impl<M: Model> Values<M> {
    /// Gets the type of the [`Value`]
    ///
    /// # Parameters
    ///
    /// - `value`: The value to get the type of.
    /// - `current_site`: The site where the IR binding is being taken place in.
    /// - `table`: The table to get the required information from.
    ///
    /// # Errors
    ///
    /// See [`TypeOfError`] for the possible errors that can occur.
    pub fn type_of_value<S: table::State>(
        &self,
        value: &Value<M>,
        current_site: GlobalID,
        environment: &Environment<
            M,
            S,
            impl Normalizer<M, S>,
            impl Observer<M, S>,
        >,
    ) -> Result<Succeeded<Type<M>, M>, TypeOfError> {
        match value {
            Value::Register(register) => {
                self.type_of_register(*register, current_site, environment)
            }
            Value::Literal(literal) => {
                Ok(simplify::simplify(&literal.r#type(), environment)?)
            }
        }
    }
}
