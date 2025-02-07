//! Builds the IR for the function body.

use std::sync::Arc;

use pernixc_handler::Handler;
use pernixc_ir::{
    binding::{AbruptError, Binder},
    IR,
};
use pernixc_syntax::syntax_tree::{item::Parameter, ConnectedList};
use pernixc_table::{
    component::{self, Derived, SymbolKind},
    diagnostic::Diagnostic,
    query::{Builder, CyclicDependencyError},
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

        match Binder::new_function(
            table,
            global_id,
            signature
                .parameters
                .connected_list()
                .iter()
                .flat_map(ConnectedList::elements)
                .map(Parameter::irrefutable_pattern),
            handler,
        )
        .and_then(|mut binder| {
            for statement in body.statements.tree() {
                binder.bind_statement(statement, handler)?;
            }

            binder.finalize(handler)
        })
        .and_then(|mut ir| {
            let environment = Environment::new(
                std::borrow::Cow::Owned(table.get_active_premise(global_id)),
                table,
                normalizer::NO_OP,
            );

            pernixc_memory_checker::memory_check(
                &mut ir,
                global_id,
                &environment,
                handler,
            )?;

            Ok(ir)
        }) {
            Ok(result) => Some(Arc::new(result)),
            Err(error) => {
                match error {
                    AbruptError::TypeSystemOverflow(type_system_overflow) => {
                        handler.receive(Box::new(type_system_overflow));
                    }
                    AbruptError::CyclicDependency(CyclicDependencyError) => {}
                }

                Some(Arc::new(IR::default()))
            }
        }
    }
}
