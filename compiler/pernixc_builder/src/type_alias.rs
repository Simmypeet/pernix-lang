//! Contains the builder for the type alias.

use std::{borrow::Cow, sync::Arc};

use pernixc_component::type_alias::TypeAlias;
use pernixc_handler::Handler;
use pernixc_resolution::{Config, Ext, GetGenericParameterNamespaceExt as _};
use pernixc_semantic::{
    component::{syntax_tree as syntax_tree_component, Derived, SymbolKind},
    diagnostic::Diagnostic,
    query, GlobalID, Table,
};
use pernixc_source_file::SourceElement;
use pernixc_type_system::{
    environment::{Environment, GetActivePremiseExt},
    normalizer,
};

use crate::{builder::Builder, occurrences, type_system::EnvironmentExt as _};

impl query::Builder<TypeAlias> for Builder {
    fn build(
        &self,
        global_id: GlobalID,
        table: &Table,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Option<Arc<TypeAlias>> {
        let symbol_kind = *table.get::<SymbolKind>(global_id);
        if !symbol_kind.has_type_alias() {
            return None;
        }

        let _scope =
            self.start_building(table, global_id, TypeAlias::component_name());

        let syntax_tree =
            table.get::<syntax_tree_component::TypeAlias>(global_id);

        let extra_namespace = table.get_generic_parameter_namepsace(global_id);

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

        let premise = table.get_active_premise(global_id);
        let env =
            Environment::new(Cow::Borrowed(&premise), table, normalizer::NO_OP);

        ty = env.simplify_and_check_lifetime_constraints(
            &ty,
            &syntax_tree.span(),
            handler,
        );

        Some(Arc::new(TypeAlias(ty)))
    }
}
