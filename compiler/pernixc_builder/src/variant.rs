//! Contains the builder for the enum variant.

use std::sync::Arc;

use pernixc_component::variant::Variant;
use pernixc_handler::Handler;
use pernixc_resolution::{Config, Ext, GetGenericParameterNamespaceExt as _};
use pernixc_source_file::SourceElement;
use pernixc_table::{
    component::{syntax_tree as syntax_tree_component, Derived, SymbolKind},
    diagnostic::Diagnostic,
    query, GlobalID, Table,
};
use pernixc_type_system::{
    environment::{Environment, GetActivePremiseExt},
    normalizer,
};

use crate::{builder::Builder, occurrences, type_system::EnvironmentExt};

impl query::Builder<Variant> for Builder {
    fn build(
        &self,
        global_id: GlobalID,
        table: &Table,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Option<Arc<Variant>> {
        let symbol_kind = *table.get::<SymbolKind>(global_id);
        if symbol_kind != SymbolKind::Variant {
            return None;
        }

        let _scope =
            self.start_building(table, global_id, Variant::component_name());

        let syntax_tree =
            table.get::<syntax_tree_component::Variant>(global_id);

        let Some(syntax_tree) = syntax_tree.variant_association.as_ref() else {
            return Some(Arc::new(Variant { associated_type: None }));
        };

        let extra_namespace = table.get_generic_parameter_namepsace(global_id);

        let active_premise = table.get_active_premise(global_id);
        let env = Environment::new(
            std::borrow::Cow::Borrowed(&active_premise),
            table,
            normalizer::NO_OP,
        );

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

        Some(Arc::new(Variant {
            associated_type: Some(env.simplify_and_check_lifetime_constraints(
                &associated_type,
                &syntax_tree.tree().span(),
                handler,
            )),
        }))
    }
}
