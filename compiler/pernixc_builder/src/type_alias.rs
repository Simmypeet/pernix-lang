//! Contains the builder for the type alias.

use std::sync::Arc;

use pernixc_component::type_alias::TypeAlias;
use pernixc_handler::Handler;
use pernixc_resolution::{Config, Ext as _};
use pernixc_source_file::SourceElement;
use pernixc_table::{
    component::{syntax_tree as syntax_tree_component, Derived, SymbolKind},
    diagnostic::Diagnostic,
    query, GlobalID, Table,
};
use pernixc_type_system::environment::Environment;

use crate::{
    builder::Builder,
    generic_parameters::Ext,
    occurrences,
    type_system::{EnvironmentExt, TableExt},
};

impl query::Builder<TypeAlias> for Builder {
    fn build(
        &self,
        global_id: GlobalID,
        table: &Table,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Option<Arc<TypeAlias>> {
        let symbol_kind = *table.get::<SymbolKind>(global_id).unwrap();
        if !symbol_kind.has_type_alias() {
            return None;
        }

        let _scope =
            self.start_building(table, global_id, TypeAlias::component_name());

        let syntax_tree =
            table.get::<syntax_tree_component::TypeAlias>(global_id).unwrap();

        let extra_namespace =
            table.get_generic_parameter_namepsace(global_id, handler);

        let mut ty = table.resolve_type(
            &syntax_tree.0,
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

        let premise = table.get_active_premise(global_id, handler);
        let (env, _) = Environment::new(premise, table);

        ty = env.simplify_and_check_lifetime_constraints(
            &ty,
            &syntax_tree.span(),
            handler,
        );

        Some(Arc::new(TypeAlias(ty)))
    }
}
