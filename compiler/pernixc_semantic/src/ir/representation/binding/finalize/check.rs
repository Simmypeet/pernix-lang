use pernixc_base::{handler::Handler, source_file::Span};

use crate::{
    arena::ID,
    error,
    ir::{
        self,
        representation::{binding::HandlerWrapper, Representation},
        value::register::{self, Register},
    },
    symbol::{
        table::{self, representation::Index},
        CallableID, GenericID,
    },
    type_system::{
        environment::Environment,
        instantiation::Instantiation,
        normalizer::Normalizer,
        observer::Observer,
        predicate::{self, PositiveTrait},
        well_formedness,
    },
};

fn report_error(
    well_formedness_error: well_formedness::Error<ir::Model>,
    instantiation_span: Span,
    handler: &HandlerWrapper,
) {
    match well_formedness_error {
        well_formedness::Error::Unsatisfied(unsatisfied) => {
            if unsatisfied.predicate.contains_error() {
                return;
            }

            handler.receive(Box::new(error::UnsatisifedPredicate {
                predicate: unsatisfied.predicate,
                instantiation_span,
                predicate_declaration_span: unsatisfied
                    .predicate_declaration_span,
            }));
        }
        well_formedness::Error::Undecidable(undecidable) => {
            if undecidable.predicate.contains_error() {
                return;
            }

            handler.receive(Box::new(error::UndecidablePredicate {
                instantiation_span,
                predicate: undecidable.predicate,
                predicate_declaration_span: undecidable
                    .predicate_declaration_span,
            }));
        }
        well_formedness::Error::ImplementationIsNotGeneralEnough(
            implementation_is_not_general_enough,
        ) => {
            if implementation_is_not_general_enough
                .generic_arguments
                .contains_error()
            {
                return;
            }

            handler.receive(Box::new(
                error::ImplementationIsNotGeneralEnough {
                    resolvable_implementation_id:
                        implementation_is_not_general_enough
                            .resolved_implementation
                            .id,
                    instantiation_span,
                    predicate_declaration_span:
                        implementation_is_not_general_enough
                            .predicate_declaration_span,
                    generic_arguments: implementation_is_not_general_enough
                        .generic_arguments,
                },
            ));
        }
        // no lifetime check occurs here, we'll do it in borrow checking stage
        well_formedness::Error::LifetimeConstraints(_) => {}
    }
}

impl Representation<ir::Model> {
    fn check_register_assignment<T: table::State>(
        &self,
        register_id: ID<Register<ir::Model>>,
        environment: &Environment<
            ir::Model,
            T,
            impl Normalizer<ir::Model, T>,
            impl Observer<ir::Model, T>,
        >,
        handler: &HandlerWrapper,
    ) {
        let register =
            self.registers.get(register_id).expect("Register not found");

        match &register.assignment {
            register::Assignment::Struct(st) => {
                let instantiation = Instantiation::from_generic_arguments(
                    st.generic_arguments.clone(),
                    st.struct_id.into(),
                    &environment
                        .table()
                        .get(st.struct_id)
                        .unwrap()
                        .generic_declaration
                        .parameters,
                )
                .unwrap();

                for error in well_formedness::check(
                    st.struct_id.into(),
                    &instantiation,
                    false,
                    environment,
                ) {
                    report_error(
                        error,
                        register.span.clone().unwrap(),
                        handler,
                    );
                }
            }
            register::Assignment::Variant(variant) => {
                let enum_id = environment
                    .table()
                    .get(variant.variant_id)
                    .unwrap()
                    .parent_enum_id();

                let instantiation = Instantiation::from_generic_arguments(
                    variant.generic_arguments.clone(),
                    enum_id.into(),
                    &environment
                        .table()
                        .get(enum_id)
                        .unwrap()
                        .generic_declaration
                        .parameters,
                )
                .unwrap();

                for error in well_formedness::check(
                    enum_id.into(),
                    &instantiation,
                    false,
                    environment,
                ) {
                    report_error(
                        error,
                        register.span.clone().unwrap(),
                        handler,
                    );
                }
            }
            register::Assignment::FunctionCall(function_call) => {
                match function_call.callable_id {
                    CallableID::Function(id) => {
                        for error in well_formedness::check(
                            id.into(),
                            &function_call.instantiation,
                            false,
                            environment,
                        ) {
                            report_error(
                                error,
                                register.span.clone().unwrap(),
                                handler,
                            );
                        }
                    }

                    CallableID::TraitFunction(id) => {
                        // parent trait requirement
                        let parent_trait_id =
                            environment.table().get(id).unwrap().parent_id();

                        let trait_arguments = function_call
                            .instantiation
                            .create_generic_arguments(
                                parent_trait_id.into(),
                                &environment
                                    .table()
                                    .get(parent_trait_id)
                                    .unwrap()
                                    .generic_declaration
                                    .parameters,
                            )
                            .unwrap();

                        // check extra trait satisfiability
                        let mut errors = well_formedness::predicate_satisfied(
                            predicate::Predicate::PositiveTrait(
                                PositiveTrait {
                                    id: parent_trait_id.into(),
                                    is_const: false, /* TODO: reflect the
                                                      * actual valuec */
                                    generic_arguments: trait_arguments,
                                },
                            ),
                            None,
                            false,
                            environment,
                        );
                        errors.extend(well_formedness::check(
                            id.into(),
                            &function_call.instantiation,
                            false,
                            environment,
                        ));

                        for error in errors {
                            report_error(
                                error,
                                register.span.clone().unwrap(),
                                handler,
                            );
                        }
                    }

                    CallableID::TraitImplementationFunction(_)
                    | CallableID::AdtImplementationFunction(_) => {
                        let parent_implementation_id: GenericID =
                            match function_call.callable_id {
                                CallableID::TraitImplementationFunction(id) => {
                                    environment
                                        .table()
                                        .get(id)
                                        .unwrap()
                                        .parent_id()
                                        .into()
                                }
                                CallableID::AdtImplementationFunction(id) => {
                                    environment
                                        .table()
                                        .get(id)
                                        .unwrap()
                                        .parent_id()
                                        .into()
                                }

                                CallableID::Function(_)
                                | CallableID::TraitFunction(_) => {
                                    unreachable!()
                                }
                            };

                        let mut errors = well_formedness::check(
                            parent_implementation_id,
                            &function_call.instantiation,
                            false,
                            environment,
                        );

                        errors.extend(well_formedness::check(
                            function_call.callable_id.into(),
                            &function_call.instantiation,
                            false,
                            environment,
                        ));

                        for error in errors {
                            report_error(
                                error,
                                register.span.clone().unwrap(),
                                handler,
                            );
                        }
                    }
                }
            }

            // TODO: tuple unpacking
            // TODO: move in not copy type on shared reference
            register::Assignment::Tuple(_)
            | register::Assignment::Load(_)
            | register::Assignment::ReferenceOf(_)
            | register::Assignment::Prefix(_)
            | register::Assignment::Binary(_)
            | register::Assignment::Array(_)
            | register::Assignment::Phi(_)
            | register::Assignment::Cast(_)
            | register::Assignment::VariantNumber(_) => {}
        }
    }

    pub(super) fn check<T: table::State>(
        &self,
        environment: &Environment<
            ir::Model,
            T,
            impl Normalizer<ir::Model, T>,
            impl Observer<ir::Model, T>,
        >,
        handler: &HandlerWrapper,
    ) {
        for register_id in self
            .control_flow_graph
            .traverse()
            .flat_map(|x| x.1.instructions())
            .filter_map(|x| x.as_register_assignment().map(|x| x.id))
        {
            self.check_register_assignment(register_id, environment, handler);
        }
    }
}
