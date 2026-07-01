use pernixc_qbice::TrackedEngine;
use pernixc_symbol::GlobalSymbolID;
use qbice::{
    Decode, Encode, Identifiable, Query, StableHash, storage::intern::Interned,
};

use crate::{
    substitution::{Substitutable, Substitution},
    r#type::{Type2, bound::Binder},
};

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Identifiable,
    Encode,
    Decode,
)]
pub struct Symbol2 {
    symbol_id: GlobalSymbolID,
    generic_arguments: Interned<[Interned<Type2>]>,
}

impl Substitutable for Symbol2 {
    fn apply(
        &self,
        subst: &Substitution,
        interner: &impl pernixc_qbice::Interner,
    ) -> Option<Self>
    where
        Self: Sized,
    {
        self.generic_arguments.apply(subst, interner).map(|generic_arguments| {
            Self { symbol_id: self.symbol_id, generic_arguments }
        })
    }
}

impl Symbol2 {
    /// Creates a new symbol reference with the given generic arguments.
    #[must_use]
    pub const fn new(
        symbol_id: GlobalSymbolID,
        generic_arguments: Interned<[Interned<Type2>]>,
    ) -> Self {
        Self { symbol_id, generic_arguments }
    }

    /// Returns the referenced symbol ID.
    #[must_use]
    pub const fn symbol_id(&self) -> GlobalSymbolID { self.symbol_id }

    /// Returns the generic arguments supplied to this symbol reference.
    #[must_use]
    pub const fn generic_arguments(&self) -> &Interned<[Interned<Type2>]> {
        &self.generic_arguments
    }

    pub async fn create_substitution(
        &self,
        engine: &TrackedEngine,
    ) -> Substitution {
        let mut subst = Substitution::new();
        subst
            .append_generic_arguments(
                self.symbol_id,
                &self.generic_arguments,
                engine,
            )
            .await;

        subst
    }
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Identifiable,
    Encode,
    Decode,
)]
pub struct TraitRef2 {
    symbol: Symbol2,
    binder: Binder,
}

impl TraitRef2 {
    /// Creates a new trait reference.
    #[must_use]
    pub const fn new(
        trait_id: GlobalSymbolID,
        generic_arguments: Interned<[Interned<Type2>]>,
        binder: Binder,
    ) -> Self {
        Self { symbol: Symbol2::new(trait_id, generic_arguments), binder }
    }

    /// Creates a new trait reference from the given symbol reference.
    #[must_use]
    pub const fn from_symbol(symbol: Symbol2, binder: Binder) -> Self {
        Self { symbol, binder }
    }

    /// Returns the referenced trait ID.
    #[must_use]
    pub const fn trait_id(&self) -> GlobalSymbolID { self.symbol.symbol_id() }

    /// Returns the generic arguments supplied to the trait.
    #[must_use]
    pub const fn generic_arguments(&self) -> &Interned<[Interned<Type2>]> {
        self.symbol.generic_arguments()
    }

    /// Returns the variables bound over the trait's generic arguments.
    #[must_use]
    pub const fn binder(&self) -> &Binder { &self.binder }

    /// Creates a substitution from the trait's generic parameters to its
    /// supplied generic arguments.
    pub async fn create_substitution(
        &self,
        engine: &TrackedEngine,
    ) -> Substitution {
        self.symbol.create_substitution(engine).await
    }
}

impl Substitutable for TraitRef2 {
    fn apply(
        &self,
        subst: &Substitution,
        interner: &impl pernixc_qbice::Interner,
    ) -> Option<Self> {
        self.symbol
            .apply(subst, interner)
            .map(|symbol| Self { symbol, binder: self.binder.clone() })
    }
}

/// Query key for retrieving the homogeneous trait reference implemented by an
/// instance declaration.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
    Query,
)]
#[value(Option<Interned<TraitRef2>>)]
#[extend(name = get_trait_ref_of_instance_symbol2, by_val)]
pub struct TraitRefKey {
    /// The global ID of the instance symbol.
    pub symbol_id: GlobalSymbolID,
}
