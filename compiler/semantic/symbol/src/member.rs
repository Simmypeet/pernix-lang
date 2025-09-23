//! Contains the definition of [`Member`] type.

use std::sync::Arc;

use flexstr::SharedStr;
use pernixc_extend::extend;
use pernixc_hash::{HashMap, HashSet};
use pernixc_query::TrackedEngine;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::Global;

use crate::{kind::get_kind, ID};

/// Stores the members of a symbol in a form of `::Member`
#[derive(
    Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize, StableHash,
)]
pub struct Member {
    /// A map from the member name to its ID.
    ///
    /// In case of the redefinition, the firs encounter is recorded in this
    /// map. The redefinition is recorded in the [`Self::redefinitions`] field.
    pub member_ids_by_name: HashMap<SharedStr, ID>,

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
    Serialize,
    Deserialize,
    StableHash,
    pernixc_query::Key,
)]
#[value(Arc<Member>)]
#[extend(method(get_members), no_cyclic)]
pub struct Key(pub Global<ID>);

/// Optionally returns `None` if the given symbol is of a kind that does not
/// have members.
#[extend]
pub async fn try_get_members(
    self: &TrackedEngine,
    id: Global<ID>,
) -> Option<Arc<Member>> {
    if !self.get_kind(id).await.has_member() {
        return None;
    }

    Some(self.get_members(id).await)
}
