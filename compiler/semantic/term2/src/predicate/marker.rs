use pernixc_target::Global;
use qbice::{Decode, Encode, StableHash, storage::intern::Interned};

use crate::{
    generic_arguments::{GenericArguments, Symbol},
    predicate::generic_arguments_contains_error,
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
    /// Creates a new positive marker predicate from a symbol application.
    #[must_use]
    pub const fn from_symbol(symbol: Symbol) -> Self { Self(symbol) }

    /// Creates a new positive marker predicate.
    #[must_use]
    pub const fn new(
        marker_id: Global<pernixc_symbol::SymbolID>,
        generic_arguments: Interned<GenericArguments>,
    ) -> Self {
        Self(Symbol::new(marker_id, generic_arguments))
    }

    /// Returns the marker id.
    #[must_use]
    pub const fn marker_id(&self) -> Global<pernixc_symbol::SymbolID> {
        self.0.id()
    }

    /// Returns the applied generic arguments.
    #[must_use]
    pub const fn generic_arguments(&self) -> &Interned<GenericArguments> {
        self.0.generic_arguments()
    }

    /// Returns the first type argument, if any.
    #[must_use]
    pub fn first_type_argument(&self) -> Option<&Interned<Type>> {
        self.generic_arguments().types().first()
    }

    /// Converts this positive marker predicate into a negative one.
    #[must_use]
    pub fn to_negative(&self) -> Negative { Negative(self.0.clone()) }

    /// Consumes the predicate and returns the wrapped symbol.
    #[must_use]
    pub fn into_symbol(self) -> Symbol { self.0 }

    /// Checks whether the predicate contains an erroneous term.
    #[must_use]
    pub fn contains_error(&self) -> bool {
        generic_arguments_contains_error(self.generic_arguments().as_ref())
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
    /// Creates a new negative marker predicate from a symbol application.
    #[must_use]
    pub const fn from_symbol(symbol: Symbol) -> Self { Self(symbol) }

    /// Creates a new negative marker predicate.
    #[must_use]
    pub const fn new(
        marker_id: Global<pernixc_symbol::SymbolID>,
        generic_arguments: Interned<GenericArguments>,
    ) -> Self {
        Self(Symbol::new(marker_id, generic_arguments))
    }

    /// Returns the marker id.
    #[must_use]
    pub const fn marker_id(&self) -> Global<pernixc_symbol::SymbolID> {
        self.0.id()
    }

    /// Returns the applied generic arguments.
    #[must_use]
    pub const fn generic_arguments(&self) -> &Interned<GenericArguments> {
        self.0.generic_arguments()
    }

    /// Converts this negative marker predicate into a positive one.
    #[must_use]
    pub fn to_positive(&self) -> Positive { Positive(self.0.clone()) }

    /// Consumes the predicate and returns the wrapped symbol.
    #[must_use]
    pub fn into_symbol(self) -> Symbol { self.0 }

    /// Checks whether the predicate contains an erroneous term.
    #[must_use]
    pub fn contains_error(&self) -> bool {
        generic_arguments_contains_error(self.generic_arguments().as_ref())
    }
}
