//! Defines the trait [`Transformable`]; an object primarily used for
//! transforming the inference variables in the IR to concrete types after
//! type inference has been completed.

use pernixc_query::runtime::executor::CyclicError;
use pernixc_term::{constant::Constant, lifetime::Lifetime, r#type::Type};

/// A trait implemented by terms that can be transformed by a [`Transformer`]
/// (lifetimes, types, and constants).
pub trait Transformable {
    /// A type representing where the term is coming from, useful for error
    /// reporting.
    type Source;
}

pub enum LifetimeTermSource {}

impl Transformable for Lifetime {
    type Source = LifetimeTermSource;
}

/// An enumeration of sources for type terms used in the expression.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeTermSource {
    /// From inferring the concreete type of a numeric literal.
    Numeric,

    /// From inferring the concrete type of a character literal.
    Character,

    /// From inferring the concrete type of a `phantom ?T` expression.
    Phantom,

    /// From the free type variable of an errorneous expression.
    Error,

    /// From the unreachable expression like `break`, `continue`, or `return`
    Unreachable,
}

impl Transformable for Type {
    type Source = TypeTermSource;
}

pub enum ConstantTyermSource {}

impl Transformable for Constant {
    type Source = ConstantTyermSource;
}

/// A trait for transforming terms of type `T`.
pub trait Transformer<T: Transformable> {
    /// Transforms the given term `term`, using the provided `source` for error
    /// reporting if necessary.
    #[allow(async_fn_in_trait)]
    async fn transform(
        &mut self,
        term: &mut T,
        source: T::Source,
    ) -> Result<(), CyclicError>;
}
