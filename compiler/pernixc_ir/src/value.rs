//! Contains the definition of [`Value`].

use std::ops::Deref;

use literal::Literal;
use pernixc_arena::{Key, ID};
use pernixc_table::GlobalID;
use pernixc_term::r#type::Type;
use pernixc_type_system::{
    environment::Environment, normalizer::Normalizer, simplify::Simplify,
    Error, Succeeded,
};
use register::Register;
use serde::{Deserialize, Serialize};

use super::Values;
use crate::model::Transform;

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
    Serialize,
    Deserialize,
    enum_as_inner::EnumAsInner,
    derive_more::From,
)]
#[allow(missing_docs)]
pub enum Value<M: pernixc_term::Model> {
    Register(ID<Register<M>>),
    Literal(Literal<M>),
}

impl<M: pernixc_term::Model> Value<M> {
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

impl<M: pernixc_term::Model> Values<M> {
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
    /// See [`AbruptError`] for more information.
    pub fn type_of_value(
        &self,
        value: &Value<M>,
        current_site: GlobalID,
        environment: &Environment<M, impl Normalizer<M>>,
    ) -> Result<Succeeded<Type<M>, M>, Error> {
        match value {
            Value::Register(register) => {
                self.type_of_register(*register, current_site, environment)
            }
            Value::Literal(literal) => Ok(environment
                .query(&Simplify(literal.r#type()))?
                .map(|x| x.deref().clone())
                .unwrap()),
        }
    }
}
