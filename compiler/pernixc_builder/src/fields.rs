//! Contains the builder for the struct's fields.

use std::{
    borrow::Cow,
    collections::{hash_map::Entry, HashMap},
    sync::Arc,
};

use diagnostic::FieldDuplication;
use pernixc_arena::Arena;
use pernixc_component::fields::{Field, Fields};
use pernixc_handler::Handler;
use pernixc_resolution::{Config, Ext, GetGenericParameterNamespaceExt as _};
use pernixc_source_file::SourceElement;
use pernixc_semantic::{
    component::{syntax_tree as syntax_tree_component, Derived, SymbolKind},
    diagnostic::Diagnostic,
    query, GlobalID, Table,
};
use pernixc_type_system::{
    environment::{Environment, GetActivePremiseExt},
    normalizer,
};

use crate::{builder::Builder, occurrences, type_system::EnvironmentExt};

pub mod diagnostic;

impl query::Builder<Fields> for Builder {
    fn build(
        &self,
        global_id: GlobalID,
        table: &Table,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Option<Arc<Fields>> {
        let symbol_kind = *table.get::<SymbolKind>(global_id);
        if symbol_kind != SymbolKind::Struct {
            return None;
        }

        let _scope =
            self.start_building(table, global_id, Fields::component_name());

        let syntax_tree = table.get::<syntax_tree_component::Fields>(global_id);

        let extra_namespace = table.get_generic_parameter_namepsace(global_id);

        let mut fields = Fields {
            fields: Arena::default(),
            field_ids_by_name: HashMap::default(),
            field_declaration_order: Vec::new(),
        };

        let active_premise = table.get_active_premise(global_id);
        let env = Environment::new(
            Cow::Borrowed(&active_premise),
            table,
            normalizer::NO_OP,
        );

        for field_syn in syntax_tree.fields.iter().filter_map(|x| x.as_option())
        {
            let ty = table.resolve_type(
                &field_syn.r#type,
                global_id,
                Config {
                    elided_lifetime_provider: None,
                    elided_type_provider: None,
                    elided_constant_provider: None,
                    observer: Some(&mut occurrences::Observer),
                    extra_namespace: Some(&extra_namespace),
                },
                handler,
            );
            let field_accessibility = table
                .create_accessibility(global_id, &field_syn.access_modifier)
                .unwrap();

            let field = Field {
                accessibility: field_accessibility,
                name: field_syn.identifier.span.str().to_owned(),
                r#type: env.simplify_and_check_lifetime_constraints(
                    &ty,
                    &field_syn.r#type.span(),
                    handler,
                ),
                span: Some(field_syn.identifier.span.clone()),
            };

            let field_id = fields.fields.insert(field);

            match fields
                .field_ids_by_name
                .entry(field_syn.identifier.span.str().to_owned())
            {
                Entry::Occupied(id) => {
                    handler.receive(Box::new(FieldDuplication {
                        struct_id: global_id,
                        field_id: *id.get(),
                        redeclaration_span: field_syn.identifier.span.clone(),
                    }));
                }

                Entry::Vacant(entry) => {
                    entry.insert(field_id);
                    fields.field_declaration_order.push(field_id);
                }
            }
        }

        Some(Arc::new(fields))
    }
}

#[cfg(test)]
mod test;
