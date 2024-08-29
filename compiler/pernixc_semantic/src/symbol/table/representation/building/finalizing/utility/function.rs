use pernixc_base::{handler::Handler, source_file::SourceElement};
use pernixc_syntax::syntax_tree::{self, ConnectedList};

use super::{builder, occurrences::Occurrences};
use crate::{
    arena::ID,
    error::Error,
    symbol::{
        table::{
            representation::{
                self, building::finalizing::Finalizer, Element, RwLockContainer,
            },
            resolution::{self, Observer},
            Building, Table,
        },
        Callable, CallableID, Generic, GenericID, GlobalID, LifetimeParameter,
        LifetimeParameterID, Parameter,
    },
    type_system::{
        environment::Environment,
        model::Default,
        normalizer,
        term::{self, lifetime::Lifetime, r#type},
    },
};

#[derive(Debug)]
struct ElidedParameterLifetimeProvider<'t, G> {
    id: ID<G>,
    table: &'t Table<Building<RwLockContainer, Finalizer>>,

    created_lifetiems: Vec<ID<LifetimeParameter>>,
}

impl<'a, G: Generic + representation::Element>
    resolution::ElidedTermProvider<Lifetime<Default>>
    for ElidedParameterLifetimeProvider<'a, G>
where
    ID<G>: Into<GenericID>,
{
    fn create(&mut self) -> Lifetime<Default> {
        let id = G::get_arena(self.table)
            .get(self.id)
            .unwrap()
            .write()
            .generic_declaration_mut()
            .parameters
            .add_lifetime_parameter(LifetimeParameter {
                name: Some(format!("{}", self.created_lifetiems.len())),
                span: None,
            })
            .unwrap();

        self.created_lifetiems.push(id);

        Lifetime::Parameter(LifetimeParameterID { parent: self.id.into(), id })
    }
}

#[derive(Debug)]
struct ElidedReturnLifetimeProvider(LifetimeParameterID);

impl resolution::ElidedTermProvider<Lifetime<Default>>
    for ElidedReturnLifetimeProvider
{
    fn create(&mut self) -> Lifetime<Default> { Lifetime::Parameter(self.0) }
}

impl Table<Building<RwLockContainer, Finalizer>> {
    /// Builds the function parameters and return type
    pub fn build_function_signature<F: Callable + Element>(
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

        let mut elided_lifetime_provider = ElidedParameterLifetimeProvider {
            id: function_id,
            table: self,
            created_lifetiems: Vec::new(),
        };

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
                        elided_lifetime_provider: Some(
                            &mut elided_lifetime_provider,
                        ),
                        elided_type_provider: None,
                        elided_constant_provider: None,
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

        let mut elided_lifetime_provider =
            elided_lifetime_provider.created_lifetiems.len().eq(&1).then(
                || {
                    ElidedReturnLifetimeProvider(LifetimeParameterID {
                        parent: function_id.into(),
                        id: elided_lifetime_provider.created_lifetiems[0],
                    })
                },
            );

        // build the return type
        {
            if let Some(return_type_syn) = syntax_tree.return_type() {
                let return_type = self
                    .resolve_type(
                        return_type_syn.r#type(),
                        function_id.into(),
                        resolution::Config {
                            elided_lifetime_provider: elided_lifetime_provider
                                .as_mut()
                                .map(|x| x as _),
                            elided_type_provider: None,
                            elided_constant_provider: None,
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
