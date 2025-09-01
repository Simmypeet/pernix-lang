//! Contains the definition of [`Value`].

use literal::Literal;
use pernixc_arena::ID;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::Global;
use pernixc_term::r#type::Type;
use pernixc_type_system::{
    environment::Environment, normalizer::Normalizer, Error, Succeeded,
};

use crate::{value::register::Register, Values};
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
    StableHash,
    enum_as_inner::EnumAsInner,
    derive_more::From,
)]
#[allow(missing_docs, clippy::large_enum_variant)]
pub enum Value {
    Register(ID<Register>),
    Literal(Literal),
}

/// An extension trait on [`Values`] taht provides the ability to get the type
/// of the values in the IR.
pub trait TypeOf<V> {
    /// Gets the type of the value
    ///
    /// # Parameters
    ///
    /// - `value`: The value to get the type of.
    /// - `current_site`: The site where the IR binding is being taken place in.
    /// - `environment`: The environment to get the type from.
    ///
    /// # Errors
    ///
    /// See [`Error`] for the possible errors that can occur.
    fn type_of<'s, 'e, 'n, N: Normalizer>(
        &'s self,
        value: V,
        current_site: Global<pernixc_symbol::ID>,
        environment: &'e Environment<'n, N>,
    ) -> impl std::future::Future<Output = Result<Succeeded<Type>, Error>>
           + use<'s, 'e, 'n, Self, V, N>;
}

impl TypeOf<&Value> for Values {
    async fn type_of<N: Normalizer>(
        &self,
        value: &Value,
        current_site: Global<pernixc_symbol::ID>,
        environment: &Environment<'_, N>,
    ) -> Result<Succeeded<Type>, Error> {
        match value {
            Value::Register(id) => {
                self.type_of(*id, current_site, environment).await
            }
            Value::Literal(literal) => {
                self.type_of(literal, current_site, environment).await
            }
        }
    }
}
