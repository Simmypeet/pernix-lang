//! Contains the definition of [`ForallLifetime`] component.

use enum_as_inner::EnumAsInner;
use parking_lot::{lock_api::MappedRwLockReadGuard, RwLock, RwLockReadGuard};
use pernixc_arena::{Arena, ID};
use pernixc_source_file::GlobalSpan;
use serde::{Deserialize, Serialize};

use crate::{component::Derived, table::MemberID};

/// Represents a forall lifetime declared with `for['a]` syntax.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct Named {
    /// The name given to the forall lifetime.
    pub name: String,

    /// The span where the forall lifetime was declared.
    #[serde(skip)]
    pub span: Option<GlobalSpan>,
}

/// Represents a forall lifetime; a lifetime that represents all available
/// lifetimes.
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
    EnumAsInner,
)]
#[allow(missing_docs)]
pub enum ForallLifetime {
    Named(Named),
    Anonymous,
}

/// A **presistent-derived** component representing the storage of forall
/// lifetime usjges in the compilation process.
#[derive(Debug, Default, Serialize, Deserialize)]
pub struct ForallLifetimes(RwLock<Arena<ForallLifetime>>);

impl PartialEq for ForallLifetimes {
    fn eq(&self, other: &Self) -> bool { self.0.read().eq(&*other.0.read()) }
}

impl Eq for ForallLifetimes {}

impl ForallLifetimes {
    /// Gets the [`ForallLifetime`] with the given ID.
    pub fn get(
        &self,
        id: ID<ForallLifetime>,
    ) -> Option<MappedRwLockReadGuard<'_, parking_lot::RawRwLock, ForallLifetime>>
    {
        RwLockReadGuard::try_map(self.0.read(), |x| x.get(id)).ok()
    }

    /// Inserts a new [`ForallLifetime`] into the map.
    pub fn insert(
        &self,
        forall_lifetime: ForallLifetime,
    ) -> ID<ForallLifetime> {
        self.0.write().insert(forall_lifetime)
    }
}

impl Derived for ForallLifetimes {
    fn component_name() -> &'static str { "forall lifetimes" }
}

/// Unique ID for the forall lifetime.
#[allow(clippy::module_name_repetitions)]
pub type ForallLifetimeID = MemberID<ID<ForallLifetime>>;
