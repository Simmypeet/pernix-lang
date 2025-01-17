//! Contains the builder for the `function_signature` component.

use std::sync::Arc;

use pernixc_component::function_signature::FunctionSignature;
use pernixc_handler::Handler;
use pernixc_table::{
    component::{syntax_tree as syntax_tree_component, Derived, SymbolKind},
    diagnostic::Diagnostic,
    query, GlobalID, Table,
};

use crate::builder::Builder;

impl query::Builder<FunctionSignature> for Builder {
    fn build(
        &self,
        global_id: GlobalID,
        table: &Table,
        _handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Option<Arc<FunctionSignature>> {
        let symbol_kind = *table.get::<SymbolKind>(global_id)?;
        if !symbol_kind.has_function_signature() {
            return None;
        }

        let _scope = self.start_building(
            table,
            global_id,
            FunctionSignature::component_name(),
        );

        let _syntax_tree = table
            .get::<syntax_tree_component::FunctionSignature>(global_id)
            .unwrap();

        // let active_premise = table.get_active_premise(global_id, handler);
        // let (environment, _) = Environment::new(active_premise, table);

        todo!()
    }
}
