//! Contains the builder of the variance map component for the ADT.

use std::sync::Arc;

use pernixc_component::variance_map::VarianceMap;
use pernixc_handler::Handler;
use pernixc_table::{
    component::{Derived, SymbolKind},
    diagnostic::Diagnostic,
    query, GlobalID, Table,
};

use crate::builder::Builder;

impl query::Builder<VarianceMap> for Builder {
    fn build(
        &self,
        global_id: GlobalID,
        table: &Table,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Option<Arc<VarianceMap>> {
        let symbol_kind = *table.get::<SymbolKind>(global_id).unwrap();
        if !symbol_kind.has_variance_map() {
            return None;
        }

        let _scope = self.start_building(
            table,
            global_id,
            VarianceMap::component_name(),
        );

        todo!()
    }
}
