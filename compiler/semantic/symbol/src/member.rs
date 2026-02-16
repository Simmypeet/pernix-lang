//! Contains the definition of [`Member`] type.

use pernixc_extend::extend;
use pernixc_hash::{HashMap, HashSet};
use pernixc_qbice::TrackedEngine;
use pernixc_target::Global;
use qbice::{
    Decode, Encode, Identifiable, Query, StableHash, storage::intern::Interned,
};

use crate::{ID, kind::get_kind};

/// Stores the members of a symbol in a form of `::Member`
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    Default,
    Encode,
    Decode,
    StableHash,
    Identifiable,
)]
pub struct Member {
    /// A map from the member name to its ID.
    ///
    /// In case of the redefinition, the firs encounter is recorded in this
    /// map. The redefinition is recorded in the [`Self::unnameds`] field.
    pub member_ids_by_name: HashMap<Interned<str>, ID>,

    /// A set of members that doesn't have a name associated to it.
    ///
    /// These IDs could be redefinitions or implements item the module.
    pub unnameds: HashSet<ID>,
}

/// The key type used with [`TrackedEngine`] to access the members of a symbol.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
    Query,
)]
#[value(Interned<Member>)]
#[extend(name = get_members, by_val)]
pub struct Key {
    /// The global ID of the symbol to get the members for.
    pub symbol_id: Global<ID>,
}

/// Optionally returns `None` if the given symbol is of a kind that does not
/// have members.
#[extend]
pub async fn try_get_members(
    self: &TrackedEngine,
    id: Global<ID>,
) -> Option<Interned<Member>> {
    if !self.get_kind(id).await.has_member() {
        return None;
    }

    Some(self.get_members(id).await)
}
