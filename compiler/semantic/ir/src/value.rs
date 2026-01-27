//! Contains the definition of [`Value`].

use bon::Builder;
use literal::Literal;
use pernixc_arena::ID;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_qbice::TrackedEngine;
use qbice::{Decode, Encode, StableHash};
use pernixc_target::Global;
use pernixc_term::r#type::Type;
use pernixc_type_system::{
    Error, Succeeded, environment::Environment as TyEnvironment,
    normalizer::Normalizer,
};

use crate::{
    Values, capture::Captures, closure_parameters::ClosureParameters,
    value::register::Register,
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
    Encode,
    Decode,
    StableHash,
    enum_as_inner::EnumAsInner,
    derive_more::From,
)]
#[allow(missing_docs, clippy::large_enum_variant)]
pub enum Value {
    Register(ID<Register>),
    Literal(Literal),
}

/// Representing an environment that the [`Values`] is in. This is primarily
/// used for type checking and retrieving the type of a [`Value`].
#[derive(Debug, Clone, Copy, Builder)]
pub struct Environment<'e, N> {
    /// The environment for type system operations.
    pub type_environment: &'e TyEnvironment<'e, N>,

    /// If the IR was built with captures, the captures here is used for
    /// accessing the captured values.
    pub captures: Option<&'e Captures>,

    /// If the IR was built with closure parameters, the closure parameters
    /// here is used for accessing the closure parameters.
    pub closure_parameters: Option<&'e ClosureParameters>,

    /// The handling scopes in the current context.
    pub handling_scopes: &'e crate::handling_scope::HandlingScopes,

    /// The site where the IR binding is being taken place in.
    pub current_site: Global<pernixc_symbol::ID>,
}

impl<'e, N: Normalizer> Environment<'e, N> {
    /// Gets the tracked engine from the type environment.
    #[must_use]
    pub fn tracked_engine(&self) -> &'e TrackedEngine {
        self.type_environment.tracked_engine()
    }

    /// Gets the captures, unwrapping the option. Panics if there are no
    /// captures.
    #[must_use]
    pub const fn captures(&self) -> &'e Captures { self.captures.unwrap() }

    /// Gets the closure parameters, unwrapping the option. Panics if there
    /// are no closure parameters.
    #[must_use]
    pub const fn closure_parameters(&self) -> &'e ClosureParameters {
        self.closure_parameters.unwrap()
    }
}

/// An extension trait on [`Values`] that provides the ability to get the type
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
        environment: &'e Environment<'n, N>,
    ) -> impl std::future::Future<Output = Result<Succeeded<Type>, Error>>
    + use<'s, 'e, 'n, Self, V, N>;
}

impl TypeOf<&Value> for Values {
    async fn type_of<N: Normalizer>(
        &self,
        value: &Value,
        environment: &Environment<'_, N>,
    ) -> Result<Succeeded<Type>, Error> {
        match value {
            Value::Register(id) => self.type_of(*id, environment).await,
            Value::Literal(literal) => self.type_of(literal, environment).await,
        }
    }
}

impl Value {
    /// Creates a new error literal value with the given type and span.
    #[must_use]
    pub const fn error(r#type: Type, span: RelativeSpan) -> Self {
        Self::Literal(Literal::Error(literal::Error { r#type, span }))
    }

    /// Creates a new unit literal value with the given optional span.
    #[must_use]
    pub const fn unit(span: RelativeSpan) -> Self {
        Self::Literal(Literal::Unit(literal::Unit { span }))
    }
}
