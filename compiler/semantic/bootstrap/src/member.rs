//! Contains the definition of [`Member`] type.

use std::sync::Arc;

use flexstr::SharedStr;
use pernixc_extend::extend;
use pernixc_hash::{HashMap, HashSet};
use pernixc_query::{TrackedEngine, Value};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::Global;

use crate::{
    implemented::get_implemented,
    import::get_imports,
    kind::{get_kind, Kind},
    symbol,
};

/// Stores the members of a symbol in a form of `::Member`
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    Default,
    Serialize,
    Deserialize,
    Value,
    StableHash,
)]
#[id(Global<symbol::ID>)]
#[value(Arc<Member>)]
#[extend(method(get_members), unwrap("should have no cyclic dependencies"))]
pub struct Member {
    /// A map from the member name to its ID.
    ///
    /// In case of the redefinition, the firs encounter is recorded in this
    /// map. The redefinition is recorded in the [`Self::redefinitions`] field.
    pub member_ids_by_name: HashMap<SharedStr, symbol::ID>,

    /// A set of redefinitions of the members of this symbol.
    pub redefinitions: HashSet<symbol::ID>,
}

/// Tries retrieve the member component of the given symbol ID (if has).
#[extend]
pub fn try_get_members(
    self: &TrackedEngine<'_>,
    id: Global<symbol::ID>,
) -> Option<Arc<Member>> {
    let symbol_kind = self.get_kind(id);
    if !symbol_kind.has_member() {
        return None;
    }

    Some(self.query(&Key(id)).expect("should have no cyclic dependencies"))
}

/// Retrieves the next member of a symbol that is accessed by using the
/// `::name` syntax. This function doesn't check if the member is
/// accessible or not.
#[extend]
#[allow(missing_docs)]
pub fn get_member_of(
    self: &TrackedEngine<'_>,
    id: Global<symbol::ID>,
    use_imports: bool,
    name: &str,
) -> Option<Global<symbol::ID>> {
    // directly get the member from the symbol
    if let Some(member) = self
        .try_get_members(id)
        .and_then(|x| x.member_ids_by_name.get(name).copied())
    {
        return Some(id.target_id.make_global(member));
    }

    let symbol_kind = self.get_kind(id);

    match (symbol_kind == Kind::Module, symbol_kind.is_adt(), use_imports) {
        (true, false, true) => self.get_imports(id).0.get(name).map(|x| x.id),

        // serach for the member of implementations
        (false, true, _) => {
            let implements = self.get_implemented(id);

            for implementation_id in implements.iter().copied() {
                if let Some(id) = self
                    .get_members(implementation_id)
                    .member_ids_by_name
                    .get(name)
                {
                    return Some(implementation_id.target_id.make_global(*id));
                }
            }

            None
        }

        _ => None,
    }
}
