//! Contains the builder for the `implements` component.

use pernixc_component::implementation::Implementation;
use pernixc_handler::Handler;
use pernixc_resolution::{Config, Ext};
use pernixc_table::{
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

use crate::{builder::Builder, handle_term_resolution_result, occurrences};

impl query::Builder<Implementation> for Builder {
    fn build(
        &self,
        global_id: GlobalID,
        table: &Table,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Option<Implementation> {
        let symbol_kind = *table.get::<SymbolKind>(global_id)?;
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
        let syntax_tree = table.get::<SyntaxTree>(global_id).unwrap();

        let generic_identifier = syntax_tree.rest().last().map_or_else(
            || {
                syntax_tree.root().as_generic_identifier().expect(
                    "`this` or `target` can't possibly refer to a symbol that \
                     can be implemented",
                )
            },
            |x| &x.1,
        );
        let implemented_id = table.get::<Implements>(global_id).unwrap().0;

        let generic_arguments = handle_term_resolution_result!(
            table.resolve_generic_arguments_for(
                implemented_id,
                generic_identifier,
                global_id,
                Config {
                    elided_lifetime_provider: None,
                    elided_type_provider: None,
                    elided_constant_provider: None,
                    observer: Some(&mut occurrences::Observer),
                    extra_namespace: None,
                },
                handler,
            ),
            handler,
            {
                // try to create generic arguments for the implemented symbol
                // with every argument being an error
                let implemented_generic_parameters =
                    match table.query::<GenericParameters>(implemented_id) {
                        Ok(param) => param,
                        Err(query::Error::CyclicDependency(error)) => {
                            handler.receive(Box::new(error));
                            return Some(Implementation::default());
                        }
                        Err(
                            query::Error::NoBuilderFound
                            | query::Error::SymbolNotFoundOrInvalidComponent,
                        ) => panic!("unexpected error"),
                    };

                return Some(Implementation {
                    generic_arguments: GenericArguments {
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
                    },
                });
            }
        );

        Some(Implementation { generic_arguments })
    }
}
