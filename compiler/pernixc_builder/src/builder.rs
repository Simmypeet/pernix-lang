//! Contains the definition of [`Builder`].

use std::sync::Arc;

use derive_new::new;
use pernixc_table::{GlobalID, Table};

/// Builder for all the components of the symbols.
#[derive(Clone, new)]
#[allow(missing_debug_implementations, clippy::type_complexity)]
pub struct Builder {
    on_start_building:
        Option<Arc<dyn Fn(&Table, GlobalID, &'static str) + Send + Sync>>,
    on_finish_building:
        Option<Arc<dyn Fn(&Table, GlobalID, &'static str) + Send + Sync>>,
}

/// A scope object for automatically calling the finish building callback.
#[allow(clippy::type_complexity, missing_debug_implementations)]
pub struct Scope<'a> {
    table: &'a Table,
    id: GlobalID,
    name: &'static str,
    on_finish_building:
        Option<Arc<dyn Fn(&Table, GlobalID, &'static str) + Send + Sync>>,
}

impl Drop for Scope<'_> {
    fn drop(&mut self) {
        if let Some(callback) = self.on_finish_building.as_ref() {
            (callback)(self.table, self.id, self.name);
        }
    }
}

impl Builder {
    /// Starts building a symbol.
    pub fn start_building<'t>(
        &self,
        table: &'t Table,
        id: GlobalID,
        name: &'static str,
    ) -> Scope<'t> {
        if let Some(callback) = self.on_start_building.as_ref() {
            (callback)(table, id, name);
        }

        Scope {
            table,
            id,
            name,
            on_finish_building: self.on_finish_building.clone(),
        }
    }
}
