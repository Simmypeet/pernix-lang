//! Formats accessibility of a symbols for hover information.

use pernixc_extend::extend;
use pernixc_query::TrackedEngine;
use pernixc_symbol::{
    accessibility::get_accessibility,
    get_target_root_module_id,
    parent::{get_closest_module_id, get_parent_global},
};
use pernixc_target::Global;

/// Gets the accessibility string for the given symbol.
#[extend]
pub async fn get_accessiblity_str(
    self: &TrackedEngine,
    symbol_id: Global<pernixc_symbol::ID>,
) -> String {
    let accessibility = self.get_accessibility(symbol_id).await;
    let root_module_id =
        self.get_target_root_module_id(symbol_id.target_id).await;

    let parent_id =
        self.get_parent_global(symbol_id).await.unwrap_or(symbol_id);

    let nearest_moodule_id = self.get_closest_module_id(parent_id).await;

    let accessibility_str = match accessibility {
        pernixc_symbol::accessibility::Accessibility::Public => "public",
        pernixc_symbol::accessibility::Accessibility::Scoped(id) => {
            if id == root_module_id {
                "internal"
            } else if id == nearest_moodule_id {
                "private"
            } else {
                // should not happen
                ""
            }
        }
    };

    accessibility_str.to_string()
}
