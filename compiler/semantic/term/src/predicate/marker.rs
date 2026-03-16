use std::fmt::Write;

use pernixc_target::Global;
use qbice::{Decode, Encode, StableHash};

use crate::{
    generic_arguments::{GenericArguments, Symbol},
    instantiation::Instantiation,
    r#type::Type,
};

/// Predicates specifying that the marker is satisfied.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
)]
pub struct Positive(Symbol);

impl Positive {
    /// Creates a new [`Positive`] predicate from the [`Symbol`].
    #[must_use]
    pub const fn from_symbol(symbol: Symbol) -> Self { Self(symbol) }

    /// Creates a new [`Positive`] predicate
    #[must_use]
    pub fn new(
        marker_id: Global<pernixc_symbol::ID>,
        generic_arguments: GenericArguments,
    ) -> Self {
        Self(Symbol::new(marker_id, generic_arguments))
    }

    /// Checks if there's an error in the generic arguments.
    #[must_use]
    pub fn contains_error(&self) -> bool {
        self.0.generic_arguments().contains_error()
    }

    /// Applies the instantiation to the generic arguments.
    pub fn instantiate(&mut self, instantiation: &Instantiation) {
        self.0.instantiate(instantiation);
    }

    /// Returns the ID of the marker that this predicate represents.
    #[must_use]
    pub const fn marker_id(&self) -> Global<pernixc_symbol::ID> { self.0.id() }

    /// Returns the generic arguments supplied to this marker.
    #[must_use]
    pub const fn generic_arguments(&self) -> &GenericArguments {
        self.0.generic_arguments()
    }

    /// Returns the first type argument supplied to this marker, if it exists.
    #[must_use]
    pub fn first_type_argument(&self) -> Option<&Type> {
        self.0.generic_arguments().types().first()
    }

    /// Sets the first type argument of this symbol to the given type.
    #[allow(clippy::result_large_err)]
    pub fn set_first_type_argument(&mut self, ty: Type) -> Result<(), Type> {
        self.0.set_first_type_argument(ty)
    }

    /// Converts this positive marker predicate into a negative one.
    #[must_use]
    pub fn to_negative(&self) -> Negative { Negative(self.0.clone()) }
}

impl crate::display::Display for Positive {
    async fn fmt(
        &self,
        engine: &pernixc_qbice::TrackedEngine,
        formatter: &mut crate::display::Formatter<'_, '_>,
    ) -> std::fmt::Result {
        write!(formatter, "marker ")?;
        self.0.fmt(engine, formatter).await
    }
}

/// The predicate specifying that the marker will never be satisfied.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
)]
pub struct Negative(Symbol);

impl Negative {
    /// Creates a new [`Negative`] predicate from the [`Symbol`].
    #[must_use]
    pub const fn from_symbol(symbol: Symbol) -> Self { Self(symbol) }

    /// Creates a new [`Negative`] predicate
    #[must_use]
    pub fn new(
        marker_id: Global<pernixc_symbol::ID>,
        generic_arguments: GenericArguments,
    ) -> Self {
        Self(Symbol::new(marker_id, generic_arguments))
    }

    /// Checks if there's an error in the generic arguments.
    #[must_use]
    pub fn contains_error(&self) -> bool {
        self.0.generic_arguments().contains_error()
    }

    /// Applies the instantiation to the generic arguments.
    pub fn instantiate(&mut self, instantiation: &Instantiation) {
        self.0.instantiate(instantiation);
    }

    /// Returns the ID of the marker that this predicate represents.
    #[must_use]
    pub const fn marker_id(&self) -> Global<pernixc_symbol::ID> { self.0.id() }

    /// Returns the generic arguments supplied to this marker.
    #[must_use]
    pub const fn generic_arguments(&self) -> &GenericArguments {
        self.0.generic_arguments()
    }

    /// Converts this negative marker predicate into a positive one.
    #[must_use]
    pub fn to_positive(&self) -> Positive { Positive(self.0.clone()) }
}

impl crate::display::Display for Negative {
    async fn fmt(
        &self,
        engine: &pernixc_qbice::TrackedEngine,
        formatter: &mut crate::display::Formatter<'_, '_>,
    ) -> std::fmt::Result {
        write!(formatter, "not marker ")?;
        self.0.fmt(engine, formatter).await
    }
}
