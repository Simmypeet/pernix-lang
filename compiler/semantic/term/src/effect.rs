//! Defines the `Effect` and `Unit` types, which represent effect sets and
//! individual effects in the type system.

use std::collections::BTreeSet;

use pernixc_qbice::TrackedEngine;
use pernixc_target::Global;
use qbice::{Decode, Encode, Identifiable, StableHash};

use crate::{
    generic_arguments::{GenericArguments, Symbol},
    instantiation::Instantiation,
};

/// Represents a single `effect Fizz` in a set of effects.
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
    Identifiable,
)]
pub struct Unit(Symbol);

impl Unit {
    /// Creates a new `Unit` with the given effect ID and generic arguments.
    #[must_use]
    pub const fn new(
        effect_id: Global<pernixc_symbol::SymbolID>,
        generic_arguments: GenericArguments,
    ) -> Self {
        Self(Symbol::new(effect_id, generic_arguments))
    }

    /// Creates a new `Unit` from the given [`Symbol`].
    #[must_use]
    pub const fn from_symbol(symbol: Symbol) -> Self { Self(symbol) }

    /// Returns the ID of the effect that this `Unit` represents.
    #[must_use]
    pub const fn effect_id(&self) -> Global<pernixc_symbol::SymbolID> {
        self.0.id()
    }

    /// Returns the generic arguments supplied to this effect.
    #[must_use]
    pub const fn generic_arguments(&self) -> &GenericArguments {
        self.0.generic_arguments()
    }

    /// Instantiates this [`Unit`] with the given instantiation.
    pub fn instantiate(&mut self, inst: &Instantiation) {
        self.0.instantiate(inst);
    }

    #[must_use]
    pub async fn create_instantiation(
        &self,
        engine: &TrackedEngine,
    ) -> Instantiation {
        self.0.create_instantiation(engine).await
    }
}

impl crate::display::Display for Unit {
    async fn fmt(
        &self,
        engine: &pernixc_qbice::TrackedEngine,
        formatter: &mut crate::display::Formatter<'_, '_>,
    ) -> std::fmt::Result {
        self.0.fmt(engine, formatter).await
    }
}

/// Represents a set of effects, such as `effect Fizz + effect Buzz`. It's
/// composed of multiple effect `Unit`s.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Default,
    StableHash,
    Encode,
    Decode,
)]
pub struct Effect {
    /// The individual effects that compose this set of effects.
    pub effects: BTreeSet<Unit>,
}
