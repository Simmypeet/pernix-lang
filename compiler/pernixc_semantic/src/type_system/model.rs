//! Contains the definition of [`Model`].

use std::{fmt::Debug, hash::Hash};

use pernixc_base::source_file::Span;

use super::term::{
    constant::Constant, lifetime::Lifetime, r#type::Type, Never, Term,
};

/// The model that the terms will be based on.
///
/// The model is used for defining the inferences that can be made in the terms.
pub trait Model:
    Debug
    + Clone
    + Copy
    + PartialEq
    + Eq
    + PartialOrd
    + Ord
    + Hash
    + std::default::Default
    + 'static
    + Send
    + Sync
{
    /// The type to use for lifetime inference.
    type LifetimeInference: Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + Hash
        + 'static
        + Send
        + Sync
        + From<Never>;

    /// The type to use for type inference.
    type TypeInference: Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + Hash
        + 'static
        + Send
        + Sync
        + From<Never>;

    /// The type to use for constant inference.
    type ConstantInference: Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + Hash
        + 'static
        + Send
        + Sync
        + From<Never>;

    /// Converts a type from the default model to the current model.
    fn from_default_type(ty: Type<Default>) -> Type<Self>;

    /// Converts a lifetime from the default model to the current model.
    fn from_default_lifetime(lifetime: Lifetime<Default>) -> Lifetime<Self>;

    /// Converts a constant from the default model to the current model.
    fn from_default_constant(constant: Constant<Default>) -> Constant<Self>;
}

/// The default model where all inferences are [`Never`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Default;

impl Model for Default {
    type LifetimeInference = Never;
    type TypeInference = Never;
    type ConstantInference = Never;

    fn from_default_type(ty: Type<Default>) -> Type<Self> { ty }
    fn from_default_lifetime(lifetime: Lifetime<Default>) -> Lifetime<Self> {
        lifetime
    }
    fn from_default_constant(constant: Constant<Default>) -> Constant<Self> {
        constant
    }
}

/// A trait for transforming terms from one model to another.
///
/// For example, this can be used when finishing the inference process to
/// transform the terms from the inference model to the final concrete model.
pub trait Transform<T: Term> {
    /// The target model to transform the terms to.
    type Target: Model;

    /// Transforms a term from the current model to the target model.
    ///
    /// # Parameters
    ///
    /// - `term`: The term to transform.
    /// - `span`: The location of the expression/declaration that have the term;
    ///   this is used for error reporting.
    fn transform(
        &mut self,
        term: T,
        span: Option<Span>,
    ) -> T::Rebind<Self::Target>;
}
