//! Defines the [`Name`] type.

use extend::ext;
use flexstr::SharedStr;
use pernixc_query::{Engine, Value};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::Global;

use crate::{
    kind::Ext as _,
    member::Ext as _,
    parent::Ext as _,
    symbol::{self, ID},
    target::MapExt as _,
};

/// A simple name identifier given to a symbol.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Default,
    Serialize,
    Deserialize,
    derive_more::DerefMut,
    derive_more::Deref,
    Value,
    StableHash,
)]
#[id(Global<symbol::ID>)]
pub struct Name(pub SharedStr);

/// Extension trait related to retrieving the name of a symbol.
#[ext(name = Ext)]
pub impl Engine {
    /// Returns the name of the symbol with the given ID.
    #[must_use]
    fn get_name(&self, id: Global<symbol::ID>) -> Name {
        self.query(&Key(id)).expect("should have no cyclic dependencies")
    }

    /// Gets the qualified name of the symbol such as `module::function`.
    #[must_use]
    fn get_qualified_name(&self, mut id: Global<symbol::ID>) -> String {
        let mut qualified_name = String::new();

        loop {
            let current_name = self.get_name(id);

            if qualified_name.is_empty() {
                qualified_name.push_str(&current_name);
            } else {
                qualified_name.insert_str(0, "::");
                qualified_name.insert_str(0, &current_name);
            }

            if let Some(parent_id) = *self.get_parent(id) {
                id = Global::new(id.target_id, parent_id);
            } else {
                break;
            }
        }

        qualified_name
    }

    /// Gets the [`Global<symbol::ID>`] of the symbol with the given sequence of
    /// qualified names.
    fn get_by_qualified_name<'a>(
        &self,
        qualified_names: impl IntoIterator<Item = &'a str>,
    ) -> Option<Global<symbol::ID>> {
        let mut current_id: Option<Global<symbol::ID>> = None;

        for name in qualified_names {
            match current_id {
                Some(searched_in_item_id) => {
                    let has_member =
                        self.get_kind(searched_in_item_id).has_member();

                    if !has_member {
                        return None;
                    }

                    current_id = Some(
                        self.get_members(searched_in_item_id)
                            .member_ids_by_name
                            .get(name)
                            .copied()
                            .map(|x| {
                                Global::new(searched_in_item_id.target_id, x)
                            })?,
                    );
                }
                None => {
                    current_id = self
                        .get_target_map()
                        .get(name)
                        .map(|&x| Global::new(x, ID::ROOT_MODULE));
                }
            }
        }

        current_id
    }
}
