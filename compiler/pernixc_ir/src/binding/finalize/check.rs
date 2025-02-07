use pernixc_arena::ID;
use pernixc_handler::Handler;
use pernixc_source_file::Span;
use pernixc_table::{
    component::{Parent, SymbolKind},
    diagnostic::Diagnostic,
    query::CyclicDependencyError,
    GlobalID,
};
use pernixc_term::{
    generic_arguments::GenericArguments,
    generic_parameter::GenericParameters,
    instantiation::Instantiation,
    predicate::{PositiveMarker, PositiveTrait, Predicate, Tuple},
    r#type::Qualifier,
};
use pernixc_type_system::{
    diagnostic::{
        ImplementationIsNotGeneralEnough, UndecidablePredicate,
        UnsatisfiedPredicate,
    },
    environment::Environment,
    normalizer::Normalizer,
    well_formedness,
};

use crate::{
    model,
    value::{
        register::{self, Register},
        Value,
    },
    Representation,
};

fn report_error(
    well_formedness_error: well_formedness::Error<model::Model>,
    instantiation_span: Span,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) {
    match well_formedness_error {
        well_formedness::Error::Unsatisfied(unsatisfied) => {
            if unsatisfied.predicate.contains_error() {
                return;
            }

            handler.receive(Box::new(UnsatisfiedPredicate {
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

            handler.receive(Box::new(UndecidablePredicate {
                predicate: undecidable.predicate,
                predicate_declaration_span: undecidable
                    .predicate_declaration_span,
                instantiation_span,
                overflow_error: undecidable.overflow_error,
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

            handler.receive(Box::new(ImplementationIsNotGeneralEnough {
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
            }));
        }
    }
}

impl Representation<model::Model> {
    #[allow(clippy::too_many_lines)]
    fn check_register_assignment(
        &self,
        register_id: ID<Register<model::Model>>,
        current_site: GlobalID,
        environment: &Environment<model::Model, impl Normalizer<model::Model>>,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<(), CyclicDependencyError> {
        let register =
            self.values.registers.get(register_id).expect("Register not found");

        match &register.assignment {
            register::Assignment::Struct(st) => {
                let instantiation = Instantiation::from_generic_arguments(
                    st.generic_arguments.clone(),
                    st.struct_id,
                    &*environment
                        .table()
                        .query::<GenericParameters>(st.struct_id)?,
                )
                .unwrap();

                for error in well_formedness::check(
                    st.struct_id,
                    &instantiation,
                    false,
                    environment,
                )?
                .1
                {
                    report_error(
                        error,
                        register.span.clone().unwrap(),
                        handler,
                    );
                }

                Ok(())
            }
            register::Assignment::Variant(variant) => {
                let enum_id = GlobalID::new(
                    variant.variant_id.target_id,
                    environment
                        .table()
                        .get::<Parent>(variant.variant_id)
                        .parent
                        .unwrap(),
                );

                let instantiation = Instantiation::from_generic_arguments(
                    variant.generic_arguments.clone(),
                    enum_id,
                    &*environment
                        .table()
                        .query::<GenericParameters>(enum_id)?,
                )
                .unwrap();

                for error in well_formedness::check(
                    enum_id,
                    &instantiation,
                    false,
                    environment,
                )?
                .1
                {
                    report_error(
                        error,
                        register.span.clone().unwrap(),
                        handler,
                    );
                }

                Ok(())
            }
            register::Assignment::FunctionCall(function_call) => {
                let symbol_kind = *environment
                    .table()
                    .get::<SymbolKind>(function_call.callable_id);

                match symbol_kind {
                    SymbolKind::Function | SymbolKind::ExternFunction => {
                        well_formedness::check(
                            function_call.callable_id,
                            &function_call.instantiation,
                            false,
                            environment,
                        )?
                        .1
                        .into_iter()
                        .for_each(|error| {
                            report_error(
                                error,
                                register.span.clone().unwrap(),
                                handler,
                            );
                        });

                        Ok(())
                    }

                    SymbolKind::TraitFunction => {
                        // parent trait requirement
                        let parent_trait_id = GlobalID::new(
                            function_call.callable_id.target_id,
                            environment
                                .table()
                                .get::<Parent>(function_call.callable_id)
                                .parent
                                .unwrap(),
                        );

                        let trait_arguments = function_call
                            .instantiation
                            .create_generic_arguments(
                                parent_trait_id,
                                &*environment
                                    .table()
                                    .query::<GenericParameters>(
                                        parent_trait_id,
                                    )?,
                            )
                            .unwrap();

                        // check extra trait satisfiability
                        let mut errors = well_formedness::predicate_satisfied(
                            Predicate::PositiveTrait(PositiveTrait {
                                trait_id: parent_trait_id,
                                is_const: false, /* TODO: reflect the
                                                  * actual valuec */
                                generic_arguments: trait_arguments,
                            }),
                            None,
                            false,
                            environment,
                        )?
                        .1;
                        errors.extend(
                            well_formedness::check(
                                function_call.callable_id,
                                &function_call.instantiation,
                                false,
                                environment,
                            )?
                            .1,
                        );

                        for error in errors {
                            report_error(
                                error,
                                register.span.clone().unwrap(),
                                handler,
                            );
                        }

                        Ok(())
                    }

                    SymbolKind::AdtImplementationFunction
                    | SymbolKind::TraitImplementationFunction => {
                        let parent_implementation_id = GlobalID::new(
                            function_call.callable_id.target_id,
                            environment
                                .table()
                                .get::<Parent>(function_call.callable_id)
                                .parent
                                .unwrap(),
                        );

                        let mut errors = well_formedness::check(
                            parent_implementation_id,
                            &function_call.instantiation,
                            false,
                            environment,
                        )?
                        .1;

                        errors.extend(
                            well_formedness::check(
                                function_call.callable_id,
                                &function_call.instantiation,
                                false,
                                environment,
                            )?
                            .1,
                        );

                        for error in errors {
                            report_error(
                                error,
                                register.span.clone().unwrap(),
                                handler,
                            );
                        }

                        Ok(())
                    }

                    _ => unreachable!(),
                }
            }

            // check for move behind shared reference on non-copy type
            register::Assignment::Load(load) => {
                if load.address.get_reference_qualifier()
                    == Some(Qualifier::Immutable)
                    || load.address.is_behind_index()
                {
                    let ty = self
                        .values
                        .type_of_address(
                            &load.address,
                            current_site,
                            environment,
                        )
                        .unwrap()
                        .result;

                    let copy_marker = environment
                        .table()
                        .get_by_qualified_name(["core", "Copy"])
                        .unwrap();

                    let predicate = Predicate::PositiveMarker(
                        PositiveMarker::new(copy_marker, GenericArguments {
                            lifetimes: Vec::new(),
                            types: vec![ty],
                            constants: Vec::new(),
                        }),
                    );

                    let errors = well_formedness::predicate_satisfied(
                        predicate,
                        None,
                        false,
                        environment,
                    )?
                    .1;

                    for error in errors {
                        report_error(
                            error,
                            register.span.clone().unwrap(),
                            handler,
                        );
                    }
                }

                Ok(())
            }

            // tuple unpacking
            register::Assignment::Tuple(tuple) => {
                for element in tuple.elements.iter().filter(|x| x.is_unpacked) {
                    let ty = self
                        .values
                        .type_of_value(
                            &element.value,
                            current_site,
                            environment,
                        )
                        .unwrap()
                        .result;

                    let predicate = Predicate::TupleType(Tuple(ty));
                    let errors = well_formedness::predicate_satisfied(
                        predicate,
                        None,
                        false,
                        environment,
                    )?
                    .1;

                    for error in errors {
                        report_error(
                            error,
                            match &element.value {
                                Value::Register(id) => self
                                    .values
                                    .registers
                                    .get(*id)
                                    .unwrap()
                                    .span
                                    .clone()
                                    .unwrap(),
                                Value::Literal(literal) => {
                                    literal.span().cloned().unwrap()
                                }
                            },
                            handler,
                        );
                    }
                }

                Ok(())
            }

            register::Assignment::Borrow(_)
            | register::Assignment::Prefix(_)
            | register::Assignment::Binary(_)
            | register::Assignment::Array(_)
            | register::Assignment::Phi(_)
            | register::Assignment::Cast(_)
            | register::Assignment::VariantNumber(_) => Ok(()),
        }
    }

    pub(super) fn check(
        &self,
        current_site: GlobalID,
        environment: &Environment<model::Model, impl Normalizer<model::Model>>,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<(), CyclicDependencyError> {
        for register_id in self
            .control_flow_graph
            .traverse()
            .flat_map(|x| x.1.instructions())
            .filter_map(|x| x.as_register_assignment().map(|x| x.id))
        {
            self.check_register_assignment(
                register_id,
                current_site,
                environment,
                handler,
            )?;
        }

        Ok(())
    }
}
