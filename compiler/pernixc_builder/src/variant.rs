//! Contains the builder for the enum variant.

use std::sync::Arc;

use pernixc_component::variant::Variant;
use pernixc_handler::Handler;
use pernixc_resolution::{Config, Ext as _};
use pernixc_source_file::SourceElement;
use pernixc_table::{
    component::{syntax_tree as syntax_tree_component, Derived, SymbolKind},
    diagnostic::Diagnostic,
    query, GlobalID, Table,
};
use pernixc_term::accessibility::Ext;

use crate::{
    accessibility, builder::Builder,
    diagnostic::PrivateEntityLeakedToPublicInterface,
    generic_parameters::Ext as _, occurrences,
};

impl query::Builder<Variant> for Builder {
    fn build(
        &self,
        global_id: GlobalID,
        table: &Table,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Option<Arc<Variant>> {
        let symbol_kind = *table.get::<SymbolKind>(global_id).unwrap();
        if symbol_kind != SymbolKind::Variant {
            return None;
        }

        let _scope =
            self.start_building(table, global_id, Variant::component_name());

        let syntax_tree =
            table.get::<syntax_tree_component::Variant>(global_id).unwrap();

        let Some(syntax_tree) = syntax_tree.variant_association.as_ref() else {
            return Some(Arc::new(Variant { associated_type: None }));
        };

        let extra_namespace =
            table.get_generic_parameter_namepsace(global_id, handler);

        let associated_type = table.resolve_type(
            syntax_tree.tree(),
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

        let symbol_accessibility = table
            .get_accessibility(global_id)
            .unwrap()
            .into_global(global_id.target_id);
        let ty_accessibility =
            table.get_type_accessibility(&associated_type).unwrap();

        if accessibility::check_private_entity_leakage(
            table,
            ty_accessibility,
            symbol_accessibility,
        ) {
            handler.receive(Box::new(PrivateEntityLeakedToPublicInterface {
                entity: associated_type.clone(),
                entity_overall_accessibility: ty_accessibility,
                leaked_span: syntax_tree.tree().span(),
                public_accessibility: symbol_accessibility,
            }));
        }

        Some(Arc::new(Variant { associated_type: Some(associated_type) }))
    }
}
