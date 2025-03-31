//! Contains the builder for the `implements` component.

use std::sync::Arc;

use pernixc_abort::Abort;
use pernixc_component::implementation::Implementation;
use pernixc_handler::Handler;
use pernixc_resolution::{Config, Ext, GetGenericParameterNamespaceExt as _};
use pernixc_semantic::{
    component::{
        syntax_tree::ImplementationQualifiedIdentifier as SyntaxTree, Derived,
        Implements, SymbolKind,
    },
    diagnostic::Diagnostic,
    query, GlobalID, Table,
};
use pernixc_term::{
    constant::Constant, generic_arguments::GenericArguments,
    generic_parameter::GenericParameters, lifetime::Lifetime, r#type::Type,
};

use crate::{builder::Builder, occurrences};

impl query::Builder<Implementation> for Builder {
    fn build(
        &self,
        global_id: GlobalID,
        table: &Table,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Option<Arc<Implementation>> {
        let symbol_kind = *table.get::<SymbolKind>(global_id);
        if !symbol_kind.is_implementation() {
            return None;
        }

        let _scope = self.start_building(
            table,
            global_id,
            Implementation::component_name(),
        );

        // get qualified identifier syntax tree of `implements
        // QUALIFIED_IDENTIFIER`
        let syntax_tree = table.get::<SyntaxTree>(global_id);

        let generic_identifier = syntax_tree.rest.last().map_or_else(
            || {
                syntax_tree.root.as_generic_identifier().expect(
                    "`this` or `target` can't possibly refer to a symbol that \
                     can be implemented",
                )
            },
            |x| &x.1,
        );
        let implemented_id = table.get::<Implements>(global_id).0;
        let extra_namespace = table.get_generic_parameter_namepsace(global_id);

        let generic_arguments = table
            .resolve_generic_arguments_for(
                implemented_id,
                generic_identifier,
                global_id,
                Config {
                    elided_lifetime_provider: None,
                    elided_type_provider: None,
                    elided_constant_provider: None,
                    observer: Some(&mut occurrences::Observer),
                    extra_namespace: Some(&extra_namespace),
                },
                handler,
            )
            .unwrap_or_else(|Abort| {
                // try to create generic arguments for the implemented symbol
                // with every argument being an error
                let Ok(implemented_generic_parameters) =
                    table.query::<GenericParameters>(implemented_id)
                else {
                    return GenericArguments::default();
                };

                GenericArguments {
                    lifetimes: implemented_generic_parameters
                        .lifetimes()
                        .iter()
                        .map(|_| Lifetime::Error(pernixc_term::Error))
                        .collect(),
                    types: implemented_generic_parameters
                        .types()
                        .iter()
                        .map(|_| Type::Error(pernixc_term::Error))
                        .collect(),
                    constants: implemented_generic_parameters
                        .constants()
                        .iter()
                        .map(|_| Constant::Error(pernixc_term::Error))
                        .collect(),
                }
            });

        Some(Arc::new(Implementation { generic_arguments }))
    }
}
