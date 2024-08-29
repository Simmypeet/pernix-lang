use pernixc_base::{handler::Handler, source_file::SourceElement};
use pernixc_syntax::syntax_tree::{self, ConnectedList};

use super::{builder, occurrences::Occurrences};
use crate::{
    arena::ID,
    error::Error,
    symbol::{
        table::{
            representation::{
                building::finalizing::Finalizer, Element, RwLockContainer,
            },
            resolution::{self, Observer},
            Building, Table,
        },
        Callable, CallableID, GenericID, GlobalID, Parameter,
    },
    type_system::{
        environment::Environment,
        normalizer,
        term::{self, r#type},
    },
};

impl Table<Building<RwLockContainer, Finalizer>> {
    /// Builds the function parameters and return type
    pub fn build_function_parameters<F: Callable + Element>(
        &self,
        function_id: ID<F>,
        syntax_tree: &syntax_tree::item::FunctionSignature,
        signature_occurrences: &mut Occurrences,
        handler: &dyn Handler<Box<dyn Error>>,
    ) where
        ID<F>: Into<GlobalID> + Into<GenericID> + Into<CallableID>,
    {
        let active_premise =
            self.get_active_premise(function_id.into()).unwrap();
        let definition_builder =
            builder::TypeSystem::new(function_id.into(), handler);
        let (environment, _) = Environment::new_with(
            active_premise,
            self,
            normalizer::NO_OP,
            &definition_builder,
        );

        // build the parameters
        for parameter in syntax_tree
            .parameters()
            .parameter_list()
            .iter()
            .flat_map(ConnectedList::elements)
        {
            let parameter_ty = self
                .resolve_type(
                    parameter.r#type(),
                    function_id.into(),
                    resolution::Config {
                        ellided_lifetime_provider: None,
                        ellided_type_provider: None,
                        ellided_constant_provider: None,
                        observer: Some(
                            &mut (&mut builder::Resolution::basic())
                                .chain(signature_occurrences),
                        ),
                        higher_ranked_lifetimes: None,
                    },
                    handler,
                )
                .unwrap_or(r#type::Type::Error(term::Error));

            let mut function_write =
                F::get_arena(self).get(function_id).unwrap().write();

            let parameter_id =
                function_write.parameters_mut().insert(Parameter {
                    r#type: environment
                        .simplify_and_check_lifetime_constraints(
                            &parameter_ty,
                            &parameter.r#type().span(),
                            handler,
                        ),
                    span: Some(parameter.span()),
                });
            function_write.parameter_order_mut().push(parameter_id);
        }

        // build the return type
        {
            if let Some(return_type_syn) = syntax_tree.return_type() {
                let return_type = self
                    .resolve_type(
                        return_type_syn.r#type(),
                        function_id.into(),
                        resolution::Config {
                            ellided_lifetime_provider: None,
                            ellided_type_provider: None,
                            ellided_constant_provider: None,
                            observer: Some(
                                &mut (&mut builder::Resolution::basic())
                                    .chain(signature_occurrences),
                            ),
                            higher_ranked_lifetimes: None,
                        },
                        handler,
                    )
                    .unwrap();

                *F::get_arena(self)
                    .get(function_id)
                    .unwrap()
                    .write()
                    .return_type_mut() = environment
                    .simplify_and_check_lifetime_constraints(
                        &return_type,
                        &return_type_syn.r#type().span(),
                        handler,
                    );
            } else {
                *F::get_arena(self)
                    .get(function_id)
                    .unwrap()
                    .write()
                    .return_type_mut() =
                    r#type::Type::Tuple(term::Tuple::default());
            }
        }
    }
}
