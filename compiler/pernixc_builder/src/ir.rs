//! Builds the IR for the function body.

use std::{borrow::Cow, sync::Arc};

use pernixc_abort::Abort;
use pernixc_handler::{Handler, Storage};
use pernixc_ir::{binding::Binder, IR};
use pernixc_syntax::syntax_tree::{
    item::{Parameter, ParameterKind},
    ConnectedList,
};
use pernixc_table::{
    component::{self, Derived, SymbolKind},
    diagnostic::Diagnostic,
    query::Builder,
    GlobalID, Table,
};
use pernixc_type_system::{
    environment::{Environment, GetActivePremiseExt},
    normalizer,
};

use crate::builder;

impl Builder<IR> for builder::Builder {
    fn build(
        &self,
        global_id: GlobalID,
        table: &Table,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Option<Arc<IR>> {
        let symbol_kind = *table.get::<SymbolKind>(global_id);

        if !symbol_kind.has_function_body() {
            return None;
        }

        let _scope =
            self.start_building(table, global_id, IR::component_name());

        let signature =
            table.get::<component::syntax_tree::FunctionSignature>(global_id);
        let body = table.get::<component::syntax_tree::FunctionBody>(global_id);

        let storage = Storage::<Box<dyn Diagnostic>>::new();

        let result = match Binder::new_function(
            table,
            global_id,
            signature
                .parameters
                .connected_list()
                .iter()
                .flat_map(ConnectedList::elements)
                .filter_map(ParameterKind::as_regular)
                .map(Parameter::irrefutable_pattern),
            &storage,
        )
        .and_then(|mut binder| {
            for statement in body.statements.tree() {
                binder.bind_statement(statement, &storage)?;
            }

            binder.finalize(&storage)
        })
        .and_then(|mut ir| {
            if storage.as_vec().is_empty() {
                pernixc_memory_checker::memory_check(
                    table, &mut ir, global_id, &storage,
                )?;
            }

            Ok(ir)
        })
        .and_then(|ir| {
            if storage.as_vec().is_empty() {
                let active_premise = table.get_active_premise(global_id);
                let environment = Environment::new(
                    Cow::Borrowed(&active_premise),
                    table,
                    normalizer::NO_OP,
                );

                pernixc_borrow_checker::borrow_check(
                    &ir,
                    global_id,
                    &environment,
                    &storage,
                )?;
            }

            Ok(ir)
        }) {
            Ok(result) => Some(Arc::new(result)),
            Err(Abort) => Some(Arc::new(IR::default())),
        };

        storage.propagate(handler);

        result
    }
}
