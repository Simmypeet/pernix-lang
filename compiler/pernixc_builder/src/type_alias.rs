//! Contains the builder for the type alias.

use pernixc_component::type_alias::TypeAlias;
use pernixc_handler::Handler;
use pernixc_resolution::{Config, Ext as _};
use pernixc_source_file::SourceElement;
use pernixc_table::{
    component::{
        syntax_tree as syntax_tree_component, Accessibility, Derived,
        HierarchyRelationship, SymbolKind,
    },
    diagnostic::Diagnostic,
    query, GlobalAccessibility, GlobalID, Table,
};
use pernixc_term::{accessibility::Ext as _, r#type::Type};
use pernixc_type_system::environment::Environment;

use crate::{
    builder::Builder,
    diagnostic::PrivateEntityLeakedToPublicInterface,
    handle_term_resolution_result, occurrences,
    type_system::{EnvironmentExt, TableExt},
};

impl query::Builder<TypeAlias> for Builder {
    fn build(
        &self,
        global_id: GlobalID,
        table: &Table,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Option<TypeAlias> {
        let symbol_kind = *table.get::<SymbolKind>(global_id).unwrap();
        if !symbol_kind.has_type_alias() {
            return None;
        }

        let _scope =
            self.start_building(table, global_id, TypeAlias::component_name());

        let syntax_tree =
            table.get::<syntax_tree_component::TypeAlias>(global_id).unwrap();

        let mut ty = handle_term_resolution_result!(
            table.resolve_type(
                &syntax_tree.0,
                global_id,
                Config {
                    elided_lifetime_provider: None,
                    elided_type_provider: None,
                    elided_constant_provider: None,
                    observer: Some(&mut occurrences::Observer),
                    extra_namespace: None,
                },
                handler
            ),
            handler,
            return Some(TypeAlias(Type::Error(pernixc_term::Error)))
        );

        let ty_accessibility =
            table.get_type_accessibility(&ty).expect("should be valid");
        let symbol_accessibility =
            match table.get_accessibility(global_id).unwrap() {
                Accessibility::Public => GlobalAccessibility::Public,
                Accessibility::Scoped(id) => GlobalAccessibility::Scoped(
                    GlobalID::new(global_id.target_id, id),
                ),
            };

        // no private type in public interface
        let private_entity_leaked =
            match (ty_accessibility, symbol_accessibility) {
                (
                    GlobalAccessibility::Public,
                    GlobalAccessibility::Public
                    | GlobalAccessibility::Scoped(_),
                ) => false,

                (
                    GlobalAccessibility::Scoped(_),
                    GlobalAccessibility::Public,
                ) => true,

                (
                    GlobalAccessibility::Scoped(ty),
                    GlobalAccessibility::Scoped(sym),
                ) => {
                    assert_eq!(ty.target_id, sym.target_id);

                    table.symbol_hierarchy_relationship(
                        ty.target_id,
                        ty.id,
                        sym.id,
                    ) == HierarchyRelationship::Child
                }
            };

        if private_entity_leaked {
            handler.receive(Box::new(PrivateEntityLeakedToPublicInterface {
                entity: ty.clone(),
                leaked_span: syntax_tree.0.span(),
                public_interface_id: global_id,
                entity_overall_accessibility: ty_accessibility,
            }));
        }

        let premise = table.get_active_premise(global_id, handler);
        let (env, _) = Environment::new(premise, table);

        ty = env.simplify_and_check_lifetime_constraints(
            &ty,
            &syntax_tree.span(),
            handler,
        );

        Some(TypeAlias(ty))
    }
}
