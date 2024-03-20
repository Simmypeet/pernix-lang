//! Contains the definition of [`Model`].

use std::{
    fmt::Debug,
    hash::Hash,
    sync::atomic::{AtomicUsize, Ordering},
};

use super::term::Never;

/// Represents a model of entities
///
/// The trait defines several types that are used to represent dependent
/// entities. Meaning that, various models might have different types for
/// representing tre same entity.
pub trait Model:
    Debug
    + Clone
    + Copy
    + PartialEq
    + Eq
    + PartialOrd
    + Ord
    + Hash
    + Default
    + 'static
    + Send
    + Sync
{
    /// The type used to represent type inference variable.
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

    /// jhe type used to represent constant inference variable.
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

    /// The type used to represent lifetime inference variable.
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

    /// The type used to represent local lifetime variable.
    type ScopedLifetime: Debug
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
}

/// Represents a for-all quantified lifetime, denoted by `for<'a>` syntax, used
/// in higher-ranked trait bounds.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Forall(pub(super) usize);

impl From<Never> for Forall {
    fn from(never: Never) -> Self { match never {} }
}

impl Forall {
    #[allow(missing_docs)]
    pub fn generate() -> Self {
        static COUNTER: AtomicUsize = AtomicUsize::new(0);

        Self(COUNTER.fetch_add(1, Ordering::SeqCst))
    }
}

impl TryInto<Never> for Forall {
    type Error = Self;

    fn try_into(self) -> Result<Never, Self::Error> { Err(self) }
}

/// A trait implemented by a type which subject to model conversion.
pub trait Entity {
    /// The model of this entity.
    type Model: Model;

    /// Rebinds this entity to another model.
    type Rebind<A: Model>: Entity;

    /// Converts this entity into another model.
    #[must_use]
    fn into_other_model<T: Model>(self) -> Self::Rebind<T>
    where
        <Self::Model as Model>::ConstantInference: Into<T::ConstantInference>,
        <Self::Model as Model>::TypeInference: Into<T::TypeInference>,
        <Self::Model as Model>::LifetimeInference: Into<T::LifetimeInference>,
        <Self::Model as Model>::ScopedLifetime: Into<T::ScopedLifetime>;

    /// Tries to convert this entity into another model.
    #[must_use]
    fn try_into_other_model<T: Model>(self) -> Option<Self::Rebind<T>>
    where
        <Self::Model as Model>::ConstantInference:
            TryInto<T::ConstantInference>,
        <Self::Model as Model>::TypeInference: TryInto<T::TypeInference>,
        <Self::Model as Model>::LifetimeInference:
            TryInto<T::LifetimeInference>,
        <Self::Model as Model>::ScopedLifetime: TryInto<T::ScopedLifetime>;
}
