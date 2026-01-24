//! Contains the definition of query related to naming symbols and provides
//! basic naming resolution functionality.

use pernixc_extend::extend;
use pernixc_qbice::TrackedEngine;
use pernixc_target::{Global, get_target_map};
use qbice::{Decode, Encode, Query, StableHash, storage::intern::Interned};

use crate::{
    ID, get_target_root_module_id, kind::get_kind, member::get_members,
    parent::get_parent,
};

/// The key type used with [`TrackedEngine`] to access the name of a symbol.
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
#[value(Interned<str>)]
#[extend(name = get_name, by_val)]
pub struct Key {
    /// The global ID of the symbol to get the name for.
    pub symbol_id: Global<ID>,
}

/// Gets the qualified name of the symbol such as `module::function`.
#[extend]
pub async fn get_qualified_name(
    self: &TrackedEngine,
    mut id: Global<ID>,
) -> String {
    let mut qualified_name = String::new();

    loop {
        let current_name = self.get_name(id).await;

        if qualified_name.is_empty() {
            qualified_name.push_str(&current_name);
        } else {
            qualified_name.insert_str(0, "::");
            qualified_name.insert_str(0, &current_name);
        }

        if let Some(parent_id) = self.get_parent(id).await {
            id = Global::new(id.target_id, parent_id);
        } else {
            break;
        }
    }

    qualified_name
}

/// Gets the [`Global<symbol::ID>`] of the symbol with the given sequence of
/// qualified names.
#[extend]
pub async fn get_by_qualified_name<'a>(
    self: &TrackedEngine,
    qualified_names: impl IntoIterator<Item = &'a str>,
) -> Option<Global<ID>> {
    let mut current_id: Option<Global<ID>> = None;

    for name in qualified_names {
        match current_id {
            Some(searched_in_item_id) => {
                let has_member =
                    self.get_kind(searched_in_item_id).await.has_member();

                if !has_member {
                    return None;
                }

                current_id = Some(
                    self.get_members(searched_in_item_id)
                        .await
                        .member_ids_by_name
                        .get(name)
                        .copied()
                        .map(|x| {
                            Global::new(searched_in_item_id.target_id, x)
                        })?,
                );
            }
            None => {
                current_id = if let Some(target_id) =
                    self.get_target_map().await.get(name).copied()
                {
                    Some(Global::new(
                        target_id,
                        self.get_target_root_module_id(target_id).await,
                    ))
                } else {
                    None
                };
            }
        }
    }

    current_id
}
