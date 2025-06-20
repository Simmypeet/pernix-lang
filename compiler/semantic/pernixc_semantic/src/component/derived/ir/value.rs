//! Contains the definition of [`Value`].

use literal::Literal;
use pernixc_arena::{Key, ID};
use register::Register;
use serde::{Deserialize, Serialize};

use crate::{
    component::derived::ir::model::Transform,
    term::{self, r#type::Type},
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
    Serialize,
    Deserialize,
    enum_as_inner::EnumAsInner,
    derive_more::From,
)]
#[allow(missing_docs)]
pub enum Value<M: term::Model> {
    Register(ID<Register<M>>),
    Literal(Literal<M>),
}

impl<M: term::Model> Value<M> {
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
