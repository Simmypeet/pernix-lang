use std::{
    cmp::Ordering,
    collections::{BTreeSet, HashMap, HashSet},
};

use pernixc_base::{handler::Handler, source_file::Span};

use super::{
    context::Context,
    environment::get_lifetimes_in_address,
    state::{self, SetStateSucceeded, Stack, Summary},
};
use crate::{
    arena::{Arena, ID},
    error::{
        MoveInLoop, MovedOutValueFromMutableReference, OverflowOperation,
        TypeSystemOverflow, UseAfterMove, UseBeforeInitialization,
    },
    ir::{
        self,
        address::{Address, Memory},
        alloca::Alloca,
        control_flow_graph::Block,
        instruction::{Instruction, Terminator, UnconditionalJump},
        representation::{
            binding::{
                finalize::{
                    borrow::{
                        environment::Environment,
                        transform::{
                            transform_to_borrow_model, transform_to_ir_model,
                        },
                    },
                    simplify_drop,
                },
                HandlerWrapper,
            },
            borrow::{Access, LocalRegion, Model as BorrowModel},
            Values,
        },
        value::{
            register::{
                Assignment, Borrow, FunctionCall, Load, Register, Struct,
                Variant,
            },
            Value,
        },
    },
    symbol::{
        table::{self, representation::Index, Table},
        CallableID, GlobalID,
    },
    type_system::{
        compatible::{Compatibility, Compatible},
        environment::Environment as TyEnvironment,
        instantiation::{self, Instantiation},
        normalizer::Normalizer,
        observer::Observer,
        predicate::{Outlives, PositiveMarker, Predicate},
        sub_term::TermLocation,
        term::{
            constant::Constant,
            lifetime::Lifetime,
            r#type::{Qualifier, Type},
            GenericArguments, Term,
        },
        variance::Variance,
        visitor::{self, MutableRecursive, RecursiveIterator},
        well_formedness, LifetimeConstraint, OverflowError, Succeeded,
    },
};

fn get_lifetimes_from_instantiation(
    instantiation: &Instantiation<BorrowModel>,
) -> HashSet<Lifetime<BorrowModel>> {
    instantiation
        .lifetimes
        .values()
        .cloned()
        .chain(instantiation.types.values().flat_map(|x| {
            RecursiveIterator::new(x)
                .filter_map(|x| x.0.into_lifetime().ok())
                .cloned()
        }))
        .chain(instantiation.constants.values().flat_map(|x| {
            RecursiveIterator::new(x)
                .filter_map(|x| x.0.into_lifetime().ok())
                .cloned()
        }))
        .collect::<HashSet<_>>()
}

impl Values<BorrowModel> {
    /// Handles a list of outlives constraints to populate the loans in the
    /// inference variables.
    ///
    /// # Parameters
    ///
    /// - `outlives`: The list of outlives constraints.
    /// - `priority_lifetimes`: If specified, these lifetimes will be flowed
    ///   into first.
    fn handle_outlives<'a, S: table::State>(
        &self,
        outlives: impl IntoIterator<Item = &'a Outlives<Lifetime<BorrowModel>>>
            + Clone,
        priority_lifetimes: Option<&HashSet<Lifetime<BorrowModel>>>,
        checking_span: Span,
        context: &mut Context,
        current_site: GlobalID,
        ty_environment: &TyEnvironment<
            BorrowModel,
            S,
            impl Normalizer<BorrowModel, S>,
            impl Observer<BorrowModel, S>,
        >,
        handler: &HandlerWrapper,
    ) -> Result<(), TypeSystemOverflow<ir::Model>> {
        if let Some(priority_lifetimes) = priority_lifetimes {
            let is_flow_into = |x: &Outlives<Lifetime<BorrowModel>>| {
                !priority_lifetimes.contains(&x.operand)
                    && priority_lifetimes.contains(&x.bound)
            };
            for outlive in
                outlives.clone().into_iter().filter(|x| is_flow_into(x))
            {
                context.environment.handle_outlives(
                    &outlive,
                    checking_span.clone(),
                    current_site,
                    self,
                    ty_environment,
                    handler,
                )?;
            }

            for outlive in outlives.into_iter().filter(|x| !is_flow_into(x)) {
                context.environment.handle_outlives(
                    &outlive,
                    checking_span.clone(),
                    current_site,
                    self,
                    ty_environment,
                    handler,
                )?;
            }
        } else {
            for outlive in outlives {
                context.environment.handle_outlives(
                    &outlive,
                    checking_span.clone(),
                    current_site,
                    self,
                    ty_environment,
                    handler,
                )?;
            }
        }

        Ok(())
    }

    fn handle_variant<S: table::State>(
        &self,
        variant: &Variant<BorrowModel>,
        register_span: Span,
        context: &mut Context,
        current_site: GlobalID,
        ty_environment: &TyEnvironment<
            BorrowModel,
            S,
            impl Normalizer<BorrowModel, S>,
            impl Observer<BorrowModel, S>,
        >,
        handler: &HandlerWrapper,
    ) -> Result<(), TypeSystemOverflow<ir::Model>> {
        let variant_sym =
            ty_environment.table().get(variant.variant_id).unwrap();
        let enum_id = ty_environment
            .table()
            .get(variant.variant_id)
            .unwrap()
            .parent_enum_id();
        let enum_sym = ty_environment.table().get(enum_id).unwrap();

        let instantiation = Instantiation::from_generic_arguments(
            variant.generic_arguments.clone(),
            enum_id.into(),
            &enum_sym.generic_declaration.parameters,
        )
        .unwrap();

        let mut lifetime_constraints = BTreeSet::new();

        let enum_lifetimes = get_lifetimes_from_instantiation(&instantiation);

        // compare each values in the field to the struct's field type
        if let Some(mut associated_type) = variant_sym
            .associated_type
            .as_ref()
            .map(|x| Type::from_default_model(x.clone()))
        {
            instantiation::instantiate(&mut associated_type, &instantiation);
            let associated_value = variant.associated_value.as_ref().unwrap();
            let value_span = match associated_value {
                Value::Register(id) => {
                    self.registers.get(*id).unwrap().span.clone()
                }
                Value::Literal(literal) => literal.span().clone(),
            };

            let Succeeded { result: value_ty, constraints: value_constraints } =
                self.type_of_value(
                    associated_value,
                    current_site,
                    ty_environment,
                )
                .map_err(|x| TypeSystemOverflow::<
                    ir::Model,
                > {
                    operation: OverflowOperation::TypeOf,
                    overflow_span: value_span.clone(),
                    overflow_error: x.into_overflow().unwrap(),
                })?;

            lifetime_constraints.extend(value_constraints);

            let copmatibility = value_ty
                .compatible(
                    &associated_type,
                    Variance::Covariant,
                    ty_environment,
                )
                .map_err(|overflow_error| TypeSystemOverflow::<ir::Model> {
                    operation: OverflowOperation::TypeCheck,
                    overflow_span: value_span.clone(),
                    overflow_error,
                })?;

            // append the lifetime constraints
            if let Some(Succeeded {
                result,
                constraints: compatibility_constraints,
            }) = copmatibility
            {
                assert!(result.forall_lifetime_errors.is_empty());
                assert!(result
                    .forall_lifetime_instantiations
                    .lifetimes_by_forall
                    .is_empty());

                lifetime_constraints.extend(compatibility_constraints);
            } else {
                panic!("{value_ty:#?} => {associated_type:#?}")
            }
        }

        // handle the constraints introduced by instantiating the struct
        self.handle_outlives(
            lifetime_constraints
                .iter()
                .map(|x| x.as_lifetime_outlives().unwrap()),
            Some(&enum_lifetimes),
            register_span.clone(),
            context,
            current_site,
            ty_environment,
            handler,
        )?;

        lifetime_constraints.clear();

        // handle the constraints introduced by the outlive predicates of the
        // struct

        for predicate in ty_environment
            .table()
            .get_active_premise(variant.variant_id.into())
            .unwrap()
            .predicates
            .into_iter()
            .map(|x| {
                let mut x = Predicate::from_default_model(x.clone());
                x.instantiate(&instantiation);

                x
            })
        {
            match predicate {
                Predicate::LifetimeOutlives(outlives) => {
                    lifetime_constraints.insert(
                        LifetimeConstraint::LifetimeOutlives(outlives.clone()),
                    );
                }
                Predicate::TypeOutlives(outlives) => {
                    for lt in RecursiveIterator::new(&outlives.operand)
                        .filter_map(|x| x.0.into_lifetime().ok())
                    {
                        lifetime_constraints.insert(
                            LifetimeConstraint::LifetimeOutlives(Outlives {
                                operand: lt.clone(),
                                bound: outlives.bound.clone(),
                            }),
                        );
                    }
                }

                _ => {}
            }
        }

        self.handle_outlives(
            lifetime_constraints
                .iter()
                .map(|x| x.as_lifetime_outlives().unwrap()),
            Some(&enum_lifetimes), // is it needed here?
            register_span.clone(),
            context,
            current_site,
            ty_environment,
            handler,
        )?;

        // invalidated lifetimes will not be checked here; unlike function call

        Ok(())
    }

    fn handle_struct<S: table::State>(
        &self,
        struct_lit: &Struct<BorrowModel>,
        register_span: Span,
        context: &mut Context,
        current_site: GlobalID,
        ty_environment: &TyEnvironment<
            BorrowModel,
            S,
            impl Normalizer<BorrowModel, S>,
            impl Observer<BorrowModel, S>,
        >,
        handler: &HandlerWrapper,
    ) -> Result<(), TypeSystemOverflow<ir::Model>> {
        let instantiation = Instantiation::from_generic_arguments(
            struct_lit.generic_arguments.clone(),
            struct_lit.struct_id.into(),
            &ty_environment
                .table()
                .get(struct_lit.struct_id)
                .unwrap()
                .generic_declaration
                .parameters,
        )
        .unwrap();

        let mut lifetime_constraints = BTreeSet::new();

        let struct_lifetimes = get_lifetimes_from_instantiation(&instantiation);
        let struct_sym =
            ty_environment.table().get(struct_lit.struct_id).unwrap();

        // compare each values in the field to the struct's field type
        for field_id in struct_sym.field_declaration_order().iter().copied() {
            let mut field_ty = Type::from_other_model(
                struct_sym.fields().get(field_id).unwrap().r#type.clone(),
            );
            instantiation::instantiate(&mut field_ty, &instantiation);

            let value_span =
                match &struct_lit.initializers_by_field_id.get(&field_id) {
                    Some(Value::Register(id)) => {
                        self.registers.get(*id).unwrap().span.clone()
                    }
                    Some(Value::Literal(literal)) => literal.span().clone(),
                    None => unreachable!(),
                };

            let Succeeded { result: value_ty, constraints: value_constraints } =
                self.type_of_value(
                    &struct_lit
                        .initializers_by_field_id
                        .get(&field_id)
                        .unwrap(),
                    current_site,
                    ty_environment,
                )
                .map_err(|x| TypeSystemOverflow::<
                    ir::Model,
                > {
                    operation: OverflowOperation::TypeOf,
                    overflow_span: value_span.clone(),
                    overflow_error: x.into_overflow().unwrap(),
                })?;

            lifetime_constraints.extend(value_constraints);

            let copmatibility = value_ty
                .compatible(&field_ty, Variance::Covariant, ty_environment)
                .map_err(|overflow_error| TypeSystemOverflow::<ir::Model> {
                    operation: OverflowOperation::TypeCheck,
                    overflow_span: value_span.clone(),
                    overflow_error,
                })?;

            // append the lifetime constraints
            if let Some(Succeeded {
                result,
                constraints: compatibility_constraints,
            }) = copmatibility
            {
                assert!(result.forall_lifetime_errors.is_empty());
                assert!(result
                    .forall_lifetime_instantiations
                    .lifetimes_by_forall
                    .is_empty());

                lifetime_constraints.extend(compatibility_constraints);
            } else {
                panic!("{value_ty:#?} => {field_ty:#?}")
            }
        }

        // handle the constraints introduced by instantiating the struct
        self.handle_outlives(
            lifetime_constraints
                .iter()
                .map(|x| x.as_lifetime_outlives().unwrap()),
            Some(&struct_lifetimes),
            register_span.clone(),
            context,
            current_site,
            ty_environment,
            handler,
        )?;

        lifetime_constraints.clear();

        // handle the constraints introduced by the outlive predicates of the
        // struct

        for predicate in ty_environment
            .table()
            .get_active_premise(struct_lit.struct_id.into())
            .unwrap()
            .predicates
            .into_iter()
            .map(|x| {
                let mut x = Predicate::from_default_model(x.clone());
                x.instantiate(&instantiation);

                x
            })
        {
            match predicate {
                Predicate::LifetimeOutlives(outlives) => {
                    lifetime_constraints.insert(
                        LifetimeConstraint::LifetimeOutlives(outlives.clone()),
                    );
                }
                Predicate::TypeOutlives(outlives) => {
                    for lt in RecursiveIterator::new(&outlives.operand)
                        .filter_map(|x| x.0.into_lifetime().ok())
                    {
                        lifetime_constraints.insert(
                            LifetimeConstraint::LifetimeOutlives(Outlives {
                                operand: lt.clone(),
                                bound: outlives.bound.clone(),
                            }),
                        );
                    }
                }

                _ => {}
            }
        }

        self.handle_outlives(
            lifetime_constraints
                .iter()
                .map(|x| x.as_lifetime_outlives().unwrap()),
            Some(&struct_lifetimes), // is it needed here?
            register_span.clone(),
            context,
            current_site,
            ty_environment,
            handler,
        )?;

        // invalidated lifetimes will not be checked here; unlike function call

        Ok(())
    }

    fn handle_function_call<S: table::State>(
        &self,
        function_call: &FunctionCall<BorrowModel>,
        register_span: Span,
        context: &mut Context,
        accesses: &Arena<Access>,
        current_site: GlobalID,
        ty_environment: &TyEnvironment<
            BorrowModel,
            S,
            impl Normalizer<BorrowModel, S>,
            impl Observer<BorrowModel, S>,
        >,
        handler: &HandlerWrapper,
    ) -> Result<(), TypeSystemOverflow<ir::Model>> {
        let callable = ty_environment
            .table()
            .get_callable(function_call.callable_id)
            .unwrap();

        let mut lifetime_constraints = BTreeSet::new();

        for (parameter, argument) in callable
            .parameter_order()
            .iter()
            .copied()
            .map(|x| callable.parameters().get(x).unwrap())
            .zip(&function_call.arguments)
        {
            assert_eq!(
                callable.parameter_order().len(),
                function_call.arguments.len()
            );

            let mut parameter_ty =
                Type::from_other_model(parameter.r#type.clone());
            instantiation::instantiate(
                &mut parameter_ty,
                &function_call.instantiation,
            );

            // obtains the type of argument ty
            let argument_span = match argument {
                Value::Register(id) => {
                    self.registers.get(*id).unwrap().span.clone()
                }
                Value::Literal(literal) => literal.span().clone(),
            };
            let Succeeded {
                result: argument_ty,
                constraints: argument_ty_constraints,
            } = self
                .type_of_value(argument, current_site, ty_environment)
                .map_err(|x| TypeSystemOverflow::<ir::Model> {
                    operation: OverflowOperation::TypeOf,
                    overflow_span: argument_span.clone(),
                    overflow_error: x.into_overflow().unwrap(),
                })?;

            lifetime_constraints.extend(argument_ty_constraints);

            let copmatibility = argument_ty
                .compatible(&parameter_ty, Variance::Covariant, ty_environment)
                .map_err(|overflow_error| TypeSystemOverflow::<ir::Model> {
                    operation: OverflowOperation::TypeCheck,
                    overflow_span: argument_span,
                    overflow_error,
                })?;

            // append the lifetime constraints
            if let Some(Succeeded {
                result,
                constraints: compatibility_constraints,
            }) = copmatibility
            {
                assert!(result.forall_lifetime_errors.is_empty());
                assert!(result
                    .forall_lifetime_instantiations
                    .lifetimes_by_forall
                    .is_empty());

                lifetime_constraints.extend(compatibility_constraints);
            } else {
                panic!("{argument_ty:#?} => {parameter_ty:#?}")
            }
        }

        let function_lifetimes =
            get_lifetimes_from_instantiation(&function_call.instantiation);

        self.handle_outlives(
            lifetime_constraints
                .iter()
                .map(|x| x.as_lifetime_outlives().unwrap()),
            Some(&function_lifetimes),
            register_span.clone(),
            context,
            current_site,
            ty_environment,
            handler,
        )?;

        lifetime_constraints.clear();

        for predicate in ty_environment
            .table()
            .get_active_premise(function_call.callable_id.into())
            .unwrap()
            .predicates
            .into_iter()
            .map(|x| {
                let mut x = Predicate::from_default_model(x.clone());
                x.instantiate(&function_call.instantiation);

                x
            })
        {
            match predicate {
                Predicate::LifetimeOutlives(outlives) => {
                    lifetime_constraints.insert(
                        LifetimeConstraint::LifetimeOutlives(outlives.clone()),
                    );
                }
                Predicate::TypeOutlives(outlives) => {
                    for lt in RecursiveIterator::new(&outlives.operand)
                        .filter_map(|x| x.0.into_lifetime().ok())
                    {
                        lifetime_constraints.insert(
                            LifetimeConstraint::LifetimeOutlives(Outlives {
                                operand: lt.clone(),
                                bound: outlives.bound.clone(),
                            }),
                        );
                    }
                }

                _ => {}
            }
        }

        self.handle_outlives(
            lifetime_constraints
                .iter()
                .map(|x| x.as_lifetime_outlives().unwrap()),
            Some(&function_lifetimes),
            register_span.clone(),
            context,
            current_site,
            ty_environment,
            handler,
        )?;

        context.environment.check_lifetime_usages(
            function_lifetimes.iter(),
            self,
            &context.stack,
            register_span,
            accesses,
            current_site,
            ty_environment,
            handler,
        );

        Ok(())
    }

    fn handle_reference_of<S: table::State>(
        &self,
        register_id: ID<Register<BorrowModel>>,
        reference_of: &Borrow<BorrowModel>,
        register_span: Span,
        context: &mut Context,
        accesses: &mut Arena<Access>,
        current_site: GlobalID,
        ty_environment: &TyEnvironment<
            BorrowModel,
            S,
            impl Normalizer<BorrowModel, S>,
            impl Observer<BorrowModel, S>,
        >,
        handler: &HandlerWrapper,
    ) -> Result<(), TypeSystemOverflow<ir::Model>> {
        let state = context
            .stack
            .get_state(&reference_of.address)
            .expect("should found")
            .get_state_summary();

        match state {
            Summary::Uninitialized => {
                handler.receive(Box::new(UseBeforeInitialization {
                    use_span: register_span.clone(),
                }));
            }

            Summary::Moved(span) => {
                handler.receive(Box::new(UseAfterMove {
                    use_span: register_span.clone(),
                    move_span: span,
                }));
            }

            Summary::Initialized => {}
        }

        // check the access
        context.environment.handle_access(
            &reference_of.address,
            reference_of.qualifier,
            register_span.clone(),
            &context.stack,
            self,
            accesses,
            current_site,
            ty_environment,
            handler,
        )?;

        let reference_of_origin_id =
            reference_of.lifetime.clone().into_inference().unwrap();

        let constraints_in_type_of = self
            .type_of_address(
                &reference_of.address,
                current_site,
                ty_environment,
            )
            .map_err(|x| TypeSystemOverflow::<ir::Model> {
                operation: OverflowOperation::TypeOf,
                overflow_span: register_span.clone(),
                overflow_error: x.into_overflow().unwrap(),
            })?
            .constraints;

        let constraints_in_address = get_lifetimes_in_address(
            &reference_of.address,
            register_span.clone(),
            self,
            current_site,
            ty_environment,
        )?
        .into_iter()
        .map(|x| Outlives::new(x, Lifetime::Inference(reference_of_origin_id)))
        .collect::<Vec<_>>();

        // handle outlives constraints of the reference
        self.handle_outlives(
            constraints_in_type_of
                .iter()
                .map(|x| x.as_lifetime_outlives().unwrap())
                .chain(constraints_in_address.iter()),
            None,
            register_span,
            context,
            current_site,
            ty_environment,
            handler,
        )?;

        context.environment.attach_borrow(register_id, reference_of_origin_id);

        Ok(())
    }

    fn handle_store<S: table::State>(
        &self,
        store_address: &Address<BorrowModel>,
        value_type: Option<Succeeded<Type<BorrowModel>, BorrowModel>>,
        store_span: Span,
        context: &mut Context,
        accesses: &mut Arena<Access>,
        current_site: GlobalID,
        ty_environment: &TyEnvironment<
            BorrowModel,
            S,
            impl Normalizer<BorrowModel, S>,
            impl Observer<BorrowModel, S>,
        >,
        handler: &HandlerWrapper,
    ) -> Result<Vec<Instruction<BorrowModel>>, TypeSystemOverflow<ir::Model>>
    {
        let state = context.stack.set_initialized(
            store_address,
            store_span.clone(),
            ty_environment,
        )?;

        let (state, target_address) = match state {
            SetStateSucceeded::Unchanged(initialized, address) => {
                (state::State::Total(initialized), address)
            }
            SetStateSucceeded::Updated(state) => (state, store_address.clone()),
        };

        // check if there's left-over mutable reference
        if let Some(span) = state.get_moved_out_mutable_reference() {
            handler.receive(Box::new(MovedOutValueFromMutableReference {
                moved_out_value_span: span.clone(),
                reassignment_span: Some(store_span.clone()),
            }));
        }

        let drop_instructions = state
            .get_drop_instructions(&target_address, ty_environment.table())
            .unwrap();

        if let Some(Succeeded {
            result: value_ty,
            constraints: value_constraints,
        }) = value_type
        {
            let Succeeded {
                result: address_ty,
                constraints: address_constraints,
            } = self
                .type_of_address(store_address, current_site, ty_environment)
                .map_err(|x| TypeSystemOverflow::<ir::Model> {
                    operation: OverflowOperation::TypeOf,
                    overflow_span: store_span.clone(),
                    overflow_error: x.into_overflow().unwrap(),
                })?;

            match value_ty.compatible(
                &address_ty,
                Variance::Covariant,
                ty_environment,
            ) {
                Ok(Some(Succeeded {
                    result:
                        Compatibility {
                            forall_lifetime_instantiations,
                            forall_lifetime_errors,
                        },
                    constraints: compatibility_constraints,
                })) => {
                    assert!(forall_lifetime_instantiations
                        .lifetimes_by_forall
                        .is_empty());
                    assert!(forall_lifetime_errors.is_empty());

                    // reset all the origin that appears in the store address
                    let address_lifetimes = RecursiveIterator::new(&address_ty)
                        .filter_map(|x| x.0.into_lifetime().ok())
                        .cloned()
                        .collect::<HashSet<_>>();

                    // detach the subset-relation
                    for lifetime in address_lifetimes.iter() {
                        context.environment.detach_subset_relation(lifetime);
                    }

                    // apply the compatibility constraints
                    self.handle_outlives(
                        value_constraints
                            .iter()
                            .chain(address_constraints.iter())
                            .chain(compatibility_constraints.iter())
                            .map(|x| x.as_lifetime_outlives().unwrap()),
                        Some(&address_lifetimes),
                        store_span.clone(),
                        context,
                        current_site,
                        ty_environment,
                        handler,
                    )?;
                }
                Ok(None) => {
                    panic!("{value_ty:#?} => {address_ty:#?}")
                }
                Err(OverflowError) => {
                    return Err(TypeSystemOverflow {
                        operation: OverflowOperation::TypeCheck,
                        overflow_span: store_span.clone(),
                        overflow_error: OverflowError,
                    })
                }
            }
        }

        context.environment.handle_access(
            store_address,
            Qualifier::Mutable,
            store_span,
            &context.stack,
            self,
            accesses,
            current_site,
            ty_environment,
            handler,
        )?;

        Ok(drop_instructions)
    }

    fn handle_load<S: table::State>(
        &self,
        load: &Load<BorrowModel>,
        register_span: Span,
        context: &mut Context,
        accesses: &mut Arena<Access>,
        current_site: GlobalID,
        ty_environment: &TyEnvironment<
            BorrowModel,
            S,
            impl Normalizer<BorrowModel, S>,
            impl Observer<BorrowModel, S>,
        >,
        handler: &HandlerWrapper,
    ) -> Result<(), TypeSystemOverflow<ir::Model>> {
        let ty = self
            .type_of_address(&load.address, current_site, ty_environment)
            .unwrap();

        // has been checked previously
        let memory_state = if load.address.get_reference_qualifier()
            == Some(Qualifier::Immutable)
            || load.address.is_behind_index()
        {
            context
                .stack
                .get_state(&load.address)
                .expect("should found")
                .get_state_summary()
        } else {
            let copy_marker = ty_environment
                .table()
                .get_by_qualified_name(["core", "Copy"].into_iter())
                .unwrap()
                .into_marker()
                .unwrap();

            // no need to move
            if well_formedness::predicate_satisfied(
                Predicate::PositiveMarker(PositiveMarker::new(
                    copy_marker,
                    GenericArguments {
                        lifetimes: Vec::new(),
                        types: vec![ty.result],
                        constants: Vec::new(),
                    },
                )),
                None,
                false,
                ty_environment,
            )
            .iter()
            .all(well_formedness::Error::is_lifetime_constraints)
            {
                context.environment.handle_access(
                    &load.address,
                    Qualifier::Immutable,
                    register_span,
                    &context.stack,
                    self,
                    accesses,
                    current_site,
                    ty_environment,
                    handler,
                )?;
                return Ok(());
            }

            let state = context.stack.set_uninitialized(
                &load.address,
                register_span.clone(),
                ty_environment,
            )?;

            match state {
                SetStateSucceeded::Unchanged(initialized, _) => initialized
                    .as_false()
                    .and_then(|x| x.latest_accessor().as_ref())
                    .map_or(Summary::Initialized, |x| {
                        Summary::Moved(x.clone())
                    }),

                SetStateSucceeded::Updated(state) => state.get_state_summary(),
            }
        };

        match memory_state {
            Summary::Uninitialized => {
                handler.receive(Box::new(UseBeforeInitialization {
                    use_span: register_span.clone(),
                }));
            }

            Summary::Moved(span) => {
                handler.receive(Box::new(UseAfterMove {
                    use_span: register_span.clone(),
                    move_span: span,
                }));
            }

            Summary::Initialized => {}
        };

        context.environment.handle_access(
            &load.address,
            Qualifier::Immutable,
            register_span,
            &context.stack,
            self,
            accesses,
            current_site,
            ty_environment,
            handler,
        )
    }
}

fn sort_drop_addresses(
    addresses: &mut [Memory<BorrowModel>],
    allocas: &Arena<Alloca<BorrowModel>>,
    current_site: GlobalID,
    table: &Table<impl table::State>,
) {
    let callable = CallableID::try_from(current_site)
        .ok()
        .map(|x| table.get_callable(x).unwrap());

    addresses.sort_by(|x, y| match (x, y) {
        (Memory::Parameter(x_id), Memory::Parameter(y_id)) => {
            let x = callable
                .as_ref()
                .unwrap()
                .parameter_order()
                .iter()
                .position(|y| y == x_id)
                .unwrap();
            let y = callable
                .as_ref()
                .unwrap()
                .parameter_order()
                .iter()
                .position(|y| y == y_id)
                .unwrap();

            x.cmp(&y).reverse()
        }

        (Memory::Alloca(x_id), Memory::Alloca(y_id)) => {
            let x = allocas.get(*x_id).unwrap().declaration_order;
            let y = allocas.get(*y_id).unwrap().declaration_order;

            x.cmp(&y).reverse()
        }

        (Memory::Parameter(_), Memory::Alloca(_)) => Ordering::Greater,
        (Memory::Alloca(_), Memory::Parameter(_)) => Ordering::Less,
    });
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct WalkResult {
    /// The context after checking the whole block
    context: Context,
    looped_blocks: Vec<ID<Block<BorrowModel>>>,
}

#[derive(Debug, Clone)]
struct Checker<
    'a,
    S: table::State,
    N: Normalizer<BorrowModel, S>,
    O: Observer<BorrowModel, S>,
> {
    representation: ir::Representation<BorrowModel>,

    /// The key represents the block ID that needs to be checked/explored.
    ///
    /// - `None` value means the block is being processed.
    /// - `Some` value means the block has been processed
    /// - No value means the block has not been explored
    walk_results_by_block_id:
        HashMap<ID<Block<BorrowModel>>, Option<WalkResult>>,

    /// If the block id appears in this map, it means the block is a looped
    /// block and the value is the starting environment of the looped block.
    target_contexts_by_block_id: HashMap<ID<Block<BorrowModel>>, Context>,

    /// List of access to the address occurred so far.
    accesses: Arena<Access>,

    /// Contains the starting borrow origins. (Contains all the id with empty
    /// values)
    #[allow(unused)]
    starting_origin: Arena<LocalRegion>,

    current_site: GlobalID,
    ty_environment: &'a TyEnvironment<'a, BorrowModel, S, N, O>,
}

impl<
        S: table::State,
        N: Normalizer<BorrowModel, S>,
        O: Observer<BorrowModel, S>,
    > Checker<'_, S, N, O>
{
    fn walk_instructions(
        &mut self,
        block_id: ID<Block<BorrowModel>>,
        context: &mut Context,
        handler: &HandlerWrapper,
    ) -> Result<(), TypeSystemOverflow<ir::Model>> {
        let mut current_index = 0;
        let block = self
            .representation
            .control_flow_graph
            .get_block_mut(block_id)
            .unwrap();

        while current_index < block.instructions().len() {
            let step = match &block.instructions()[current_index] {
                Instruction::Store(store) => {
                    let span = match &store.value {
                        Value::Register(id) => self
                            .representation
                            .values
                            .registers
                            .get(*id)
                            .unwrap()
                            .span
                            .clone(),
                        Value::Literal(literal) => literal.span().clone(),
                    };
                    let value_type = self
                        .representation
                        .values
                        .type_of_value(
                            &store.value,
                            self.current_site,
                            self.ty_environment,
                        )
                        .map_err(|x| TypeSystemOverflow::<ir::Model> {
                            operation: OverflowOperation::TypeOf,
                            overflow_span: span.clone(),
                            overflow_error: x.into_overflow().unwrap(),
                        })?;
                    let instructions =
                        self.representation.values.handle_store(
                            &store.address,
                            Some(value_type),
                            store.span.clone(),
                            context,
                            &mut self.accesses,
                            self.current_site,
                            self.ty_environment,
                            handler,
                        )?;

                    let instructions = simplify_drop::simplify_drops(
                        instructions,
                        &self.representation.values,
                        self.current_site,
                        self.ty_environment,
                    )?;
                    let instructions_len = instructions.len();

                    let _ =
                        block.insert_instructions(current_index, instructions);

                    instructions_len + 1
                }

                Instruction::RegisterAssignment(register_assignment) => {
                    let register = self
                        .representation
                        .values
                        .registers
                        .get(register_assignment.id)
                        .unwrap();

                    match &register.assignment {
                        Assignment::Load(load) => {
                            self.representation.values.handle_load(
                                load,
                                register.span.clone(),
                                context,
                                &mut self.accesses,
                                self.current_site,
                                self.ty_environment,
                                handler,
                            )?;

                            1
                        }

                        Assignment::Borrow(reference_of) => {
                            self.representation.values.handle_reference_of(
                                register_assignment.id,
                                reference_of,
                                register.span.clone(),
                                context,
                                &mut self.accesses,
                                self.current_site,
                                self.ty_environment,
                                handler,
                            )?;

                            1
                        }

                        Assignment::FunctionCall(function_call) => {
                            self.representation.values.handle_function_call(
                                function_call,
                                register.span.clone(),
                                context,
                                &self.accesses,
                                self.current_site,
                                self.ty_environment,
                                handler,
                            )?;

                            1
                        }

                        Assignment::Struct(struct_lit) => {
                            self.representation.values.handle_struct(
                                struct_lit,
                                register.span.clone(),
                                context,
                                self.current_site,
                                self.ty_environment,
                                handler,
                            )?;

                            1
                        }

                        Assignment::Variant(variant) => {
                            self.representation.values.handle_variant(
                                variant,
                                register.span.clone(),
                                context,
                                self.current_site,
                                self.ty_environment,
                                handler,
                            )?;

                            1
                        }

                        Assignment::Prefix(_)
                        | Assignment::Tuple(_)
                        | Assignment::Binary(_)
                        | Assignment::Array(_)
                        | Assignment::Phi(_)
                        | Assignment::Cast(_)
                        | Assignment::VariantNumber(_) => 1,
                    }
                }

                Instruction::TuplePack(tuple_pack) => {
                    let moved_tuple_ty = self
                        .representation
                        .values
                        .type_of_address(
                            &tuple_pack.tuple_address,
                            self.current_site,
                            self.ty_environment,
                        )
                        .unwrap()
                        .result
                        .into_tuple()
                        .unwrap();

                    let moved_range = tuple_pack.before_packed_element_count
                        ..(moved_tuple_ty.elements.len()
                            - tuple_pack.after_packed_element_count);

                    for i in moved_range {
                        let address = Address::Tuple(ir::address::Tuple {
                            tuple_address: Box::new(
                                tuple_pack.tuple_address.clone(),
                            ),
                            offset: ir::address::Offset::FromStart(i),
                        });

                        // mark as moved out
                        let state = context.stack.set_uninitialized(
                            &address,
                            tuple_pack.packed_tuple_span.clone().unwrap(),
                            self.ty_environment,
                        )?;

                        let sumamry = match state {
                            SetStateSucceeded::Unchanged(initialized, _) => {
                                initialized
                                    .as_false()
                                    .and_then(|x| x.latest_accessor().as_ref())
                                    .map_or(Summary::Initialized, |x| {
                                        Summary::Moved(x.clone())
                                    })
                            }

                            SetStateSucceeded::Updated(state) => {
                                state.get_state_summary()
                            }
                        };

                        match sumamry {
                            Summary::Uninitialized => {
                                handler.receive(Box::new(
                                    UseBeforeInitialization {
                                        use_span: tuple_pack
                                            .packed_tuple_span
                                            .clone()
                                            .unwrap(),
                                    },
                                ));
                            }

                            Summary::Moved(span) => {
                                handler.receive(Box::new(UseAfterMove {
                                    use_span: tuple_pack
                                        .packed_tuple_span
                                        .clone()
                                        .unwrap(),
                                    move_span: span,
                                }));
                            }

                            Summary::Initialized => {}
                        }
                    }

                    let instructions =
                        self.representation.values.handle_store(
                            &tuple_pack.store_address,
                            None,
                            tuple_pack.packed_tuple_span.clone().unwrap(),
                            context,
                            &mut self.accesses,
                            self.current_site,
                            self.ty_environment,
                            handler,
                        )?;

                    let instructions = simplify_drop::simplify_drops(
                        instructions,
                        &self.representation.values,
                        self.current_site,
                        self.ty_environment,
                    )?;
                    let instructions_len = instructions.len();

                    let _ =
                        block.insert_instructions(current_index, instructions);

                    instructions_len + 1
                }

                Instruction::RegisterDiscard(_)
                | Instruction::DropUnpackTuple(_)
                | Instruction::Drop(_) => 1,

                Instruction::ScopePush(scope_push) => {
                    context.stack.new_scope(scope_push.0);

                    'out: {
                        // if we're in the function and at the root scope,
                        // we need to initialize the parameters
                        if scope_push.0
                            == self.representation.scope_tree.root_scope_id()
                        {
                            let Ok(callable_id) =
                                CallableID::try_from(self.current_site)
                            else {
                                break 'out;
                            };

                            let callable_symbol = self
                                .ty_environment
                                .table()
                                .get_callable(callable_id)
                                .unwrap();

                            for (parameter_id, parameter) in callable_symbol
                                .parameter_order()
                                .iter()
                                .copied()
                                .map(|x| {
                                    (
                                        x,
                                        callable_symbol
                                            .parameters()
                                            .get(x)
                                            .unwrap(),
                                    )
                                })
                            {
                                assert!(context.stack.current_mut().new_state(
                                    Memory::Parameter(parameter_id),
                                    true,
                                    Type::from_default_model(
                                        parameter.r#type.clone(),
                                    ),
                                ));
                            }
                        }
                    }

                    let allocas = self
                        .representation
                        .values
                        .allocas
                        .iter()
                        .filter_map(|(id, alloca)| {
                            (alloca.declared_in_scope_id == scope_push.0)
                                .then_some(id)
                        })
                        .collect::<Vec<_>>();

                    for id in allocas {
                        assert!(context.stack.current_mut().new_state(
                            Memory::Alloca(id),
                            false,
                            self.representation
                                .values
                                .allocas
                                .get(id)
                                .unwrap()
                                .r#type
                                .clone(),
                        ));
                    }

                    1
                }

                Instruction::ScopePop(scope_pop) => {
                    // record the stack state
                    let poped_scope = context.stack.pop_scope().unwrap();
                    assert_eq!(poped_scope.scope_id(), scope_pop.0);

                    let mut memories = poped_scope
                        .memories_by_address()
                        .keys()
                        .copied()
                        .collect::<Vec<_>>();

                    sort_drop_addresses(
                        &mut memories,
                        &self.representation.values.allocas,
                        self.current_site,
                        self.ty_environment.table(),
                    );

                    let mut drop_instructions = Vec::new();

                    for memory in memories {
                        let state = poped_scope
                            .get_state(&Address::Memory(memory))
                            .unwrap();

                        if let Some(moved_out) =
                            state.get_moved_out_mutable_reference()
                        {
                            handler.receive(Box::new(
                                MovedOutValueFromMutableReference {
                                    moved_out_value_span: moved_out.clone(),
                                    reassignment_span: None,
                                },
                            ));
                        }

                        drop_instructions.extend(
                            state
                                .get_drop_instructions(
                                    &Address::Memory(memory),
                                    self.ty_environment.table(),
                                )
                                .unwrap(),
                        );
                    }

                    let drop_instructions = simplify_drop::simplify_drops(
                        drop_instructions,
                        &self.representation.values,
                        self.current_site,
                        self.ty_environment,
                    )?;
                    let len = drop_instructions.len();

                    let _ = block
                        .insert_instructions(current_index, drop_instructions);

                    1 + len
                }
            };

            current_index += step;
        }

        Ok(())
    }

    fn get_predecessor_context(
        &mut self,
        block_id: ID<Block<BorrowModel>>,
        handler: &HandlerWrapper,
    ) -> Result<Option<Context>, TypeSystemOverflow<ir::Model>> {
        let Some(mut walk_result) = self.walk_block(block_id, handler)? else {
            return Ok(None);
        };

        for block_id in walk_result.looped_blocks {
            let Some(looped_context) =
                self.get_predecessor_context(block_id, handler)?
            else {
                continue;
            };

            walk_result.context.environment.merge(&looped_context.environment);
        }

        Ok(Some(walk_result.context))
    }

    fn walk_block(
        &mut self,
        block_id: ID<Block<BorrowModel>>,
        handler: &HandlerWrapper,
    ) -> Result<Option<WalkResult>, TypeSystemOverflow<ir::Model>> {
        // skip if already processed
        if let Some(walk_result) = self.walk_results_by_block_id.get(&block_id)
        {
            return Ok(walk_result.clone());
        }

        // mark as processing
        self.walk_results_by_block_id.insert(block_id, None);

        let block =
            self.representation.control_flow_graph.get_block(block_id).unwrap();

        let (mut context, looped_blocks) = if block.is_entry() {
            assert!(block.predecessors().is_empty());

            let mut starting_environment = Environment::default();
            let predicates = self
                .ty_environment
                .table()
                .get_active_premise::<BorrowModel>(self.current_site.into())
                .unwrap()
                .predicates;

            for predicate in predicates {
                match predicate {
                    Predicate::LifetimeOutlives(outlives) => {
                        let (Some(operand), Some(bound)) = (
                            outlives.operand.try_into().ok(),
                            outlives.bound.try_into().ok(),
                        ) else {
                            continue;
                        };

                        starting_environment
                            .insert_known_subset_relation(operand, bound);
                    }

                    Predicate::TypeOutlives(outlives) => {
                        let Some(bound) = outlives.bound.try_into().ok() else {
                            continue;
                        };

                        for operand in RecursiveIterator::new(&outlives.operand)
                            .filter_map(|x| x.0.into_lifetime().ok())
                            .filter_map(|x| x.clone().try_into().ok())
                        {
                            starting_environment
                                .insert_known_subset_relation(operand, bound);
                        }
                    }

                    _ => {}
                }
            }

            (
                Context {
                    stack: Stack::new(),
                    environment: Environment::default(),
                },
                Vec::new(),
            )
        } else {
            let predecessors =
                block.predecessors().iter().copied().collect::<Vec<_>>();

            let mut merging_contexts = Vec::new();
            let mut looped_block_ids = Vec::new();

            for predecessor_id in predecessors.iter().copied() {
                if let Some(stack) =
                    self.get_predecessor_context(predecessor_id, handler)?
                {
                    merging_contexts.push((predecessor_id, stack));
                } else {
                    looped_block_ids.push(predecessor_id);
                }
            }

            if merging_contexts.is_empty() {
                // try again later
                self.walk_results_by_block_id.remove(&block_id);

                return Ok(None);
            }

            // Sanity check
            if merging_contexts.len() > 1 {
                for i in merging_contexts.iter().map(|x| x.0) {
                    assert_eq!(
                        *self
                            .representation
                            .control_flow_graph
                            .blocks()
                            .get(i)
                            .unwrap()
                            .terminator(),
                        Some(Terminator::Jump(
                            ir::instruction::Jump::Unconditional(
                                UnconditionalJump { target: block_id }
                            )
                        )),
                        "merging block `{i:#?}` should directly jump to the \
                         `block_id` {:#?} {:#?}",
                        self.representation.control_flow_graph,
                        self.representation.values
                    );
                }
            }

            // merge the contexts
            let mut context = merging_contexts.pop().unwrap().1;
            context = merging_contexts.into_iter().fold(
                context,
                |mut acc, (_, env)| {
                    acc.merge(&env);
                    acc
                },
            );

            // mark the looped block
            for looped in looped_block_ids.iter().copied() {
                self.target_contexts_by_block_id
                    .insert(looped, context.clone());
            }

            for predecessor in predecessors
                .iter()
                .copied()
                .filter(|x| !looped_block_ids.contains(x))
            {
                let alternate_environment = self
                    .walk_results_by_block_id
                    .get(&predecessor)
                    .unwrap()
                    .as_ref()
                    .unwrap();

                // drop instructions to insert at the end of the block
                let mut drop_instructions = Vec::new();

                for (this, alternate) in
                    context.stack.scopes().iter().rev().zip(
                        alternate_environment
                            .context
                            .stack
                            .scopes()
                            .iter()
                            .rev(),
                    )
                {
                    assert_eq!(this.scope_id(), alternate.scope_id());

                    let mut memories = this
                        .memories_by_address()
                        .keys()
                        .copied()
                        .collect::<Vec<_>>();

                    sort_drop_addresses(
                        &mut memories,
                        &self.representation.values.allocas,
                        self.current_site,
                        self.ty_environment.table(),
                    );

                    for memory_to_drop in memories {
                        let this_state = this
                            .get_state(&Address::Memory(memory_to_drop))
                            .unwrap();
                        let alternate_state = alternate
                            .get_state(&Address::Memory(memory_to_drop))
                            .unwrap();

                        drop_instructions.extend(
                            this_state
                                .get_alternate_drop_instructions(
                                    alternate_state,
                                    &Address::Memory(memory_to_drop),
                                    self.ty_environment.table(),
                                )
                                .unwrap(),
                        );
                    }
                }

                let block = self
                    .representation
                    .control_flow_graph
                    .get_block_mut(predecessor)
                    .unwrap();

                let _ = block.insert_instructions(
                    block.instructions().len(),
                    simplify_drop::simplify_drops(
                        drop_instructions,
                        &self.representation.values,
                        self.current_site,
                        self.ty_environment,
                    )?,
                );
            }

            (context, looped_block_ids)
        };

        self.walk_instructions(block_id, &mut context, handler)?;

        // handle loop
        if let Some(target_stack) =
            self.target_contexts_by_block_id.get(&block_id)
        {
            assert_eq!(
                target_stack.stack.scopes().len(),
                context.stack.scopes().len()
            );

            for (this_scope, target_scope) in context
                .stack
                .scopes()
                .iter()
                .zip(target_stack.stack.scopes().iter())
            {
                assert_eq!(this_scope.scope_id(), target_scope.scope_id());

                let mut memories = this_scope
                    .memories_by_address()
                    .keys()
                    .copied()
                    .collect::<Vec<_>>();

                sort_drop_addresses(
                    &mut memories,
                    &self.representation.values.allocas,
                    self.current_site,
                    self.ty_environment.table(),
                );

                for memory in memories {
                    let this_state =
                        this_scope.get_state(&Address::Memory(memory)).unwrap();
                    let target_state = target_scope
                        .get_state(&Address::Memory(memory))
                        .unwrap();

                    // get the drop instructions that will make this scope the
                    // same as the target scope
                    let drop_instructions = target_state
                        .get_alternate_drop_instructions(
                            this_state,
                            &Address::Memory(memory),
                            self.ty_environment.table(),
                        )
                        .unwrap();

                    let block = self
                        .representation
                        .control_flow_graph
                        .get_block_mut(block_id)
                        .unwrap();
                    let _ = block.insert_instructions(
                        block.instructions().len(),
                        simplify_drop::simplify_drops(
                            drop_instructions,
                            &self.representation.values,
                            self.current_site,
                            self.ty_environment,
                        )?,
                    );

                    for move_span in target_state
                        .get_uninitialized_diff(this_state)
                        .unwrap()
                        .into_iter()
                        .map(|x| x.get_state_summary().into_moved().unwrap())
                    {
                        handler.receive(Box::new(MoveInLoop {
                            moved_value_span: move_span,
                        }));
                    }
                }
            }

            context.stack = target_stack.stack.clone();
        }

        let result = WalkResult { context, looped_blocks };

        // mark as done
        assert!(self
            .walk_results_by_block_id
            .insert(block_id, Some(result.clone()))
            .unwrap()
            .is_none());

        Ok(Some(result))
    }
}

#[derive(Debug, PartialEq, Eq)]
struct ReplaceWithFreshInference<'a> {
    origins: &'a mut Arena<LocalRegion>,
}

impl MutableRecursive<Lifetime<BorrowModel>> for ReplaceWithFreshInference<'_> {
    fn visit(
        &mut self,
        term: &mut Lifetime<BorrowModel>,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        if term.is_inference() {
            return true;
        }

        *term =
            Lifetime::Inference(self.origins.insert(LocalRegion::default()));

        true
    }
}

impl MutableRecursive<Type<BorrowModel>> for ReplaceWithFreshInference<'_> {
    fn visit(
        &mut self,
        _: &mut Type<BorrowModel>,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        true
    }
}

impl MutableRecursive<Constant<BorrowModel>> for ReplaceWithFreshInference<'_> {
    fn visit(
        &mut self,
        _: &mut Constant<BorrowModel>,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        true
    }
}

impl ir::Representation<BorrowModel> {
    fn replace_with_fresh_lifetimes(
        &mut self,
        origins: &mut Arena<LocalRegion>,
    ) {
        let mut visitor = ReplaceWithFreshInference { origins };

        // replace the lifetime in allocas
        for alloca in self.values.allocas.items_mut() {
            visitor::accept_recursive_mut(&mut alloca.r#type, &mut visitor);
        }

        // replace the lifetime in registers
        for register in self.values.registers.items_mut() {
            // these assignments merge multiple lifetimes, therefore we create
            // a new lifetime for each of them
            match &mut register.assignment {
                Assignment::Phi(phi) => {
                    visitor::accept_recursive_mut(
                        &mut phi.r#type,
                        &mut visitor,
                    );
                }

                Assignment::Array(array) => {
                    visitor::accept_recursive_mut(
                        &mut array.element_type,
                        &mut visitor,
                    );
                }

                Assignment::Struct(structure) => {
                    for lifetime in &mut structure.generic_arguments.lifetimes {
                        visitor::accept_recursive_mut(lifetime, &mut visitor);
                    }

                    for ty in &mut structure.generic_arguments.types {
                        visitor::accept_recursive_mut(ty, &mut visitor);
                    }

                    for con in &mut structure.generic_arguments.constants {
                        visitor::accept_recursive_mut(con, &mut visitor);
                    }
                }

                Assignment::Variant(variant) => {
                    for lifetime in &mut variant.generic_arguments.lifetimes {
                        visitor::accept_recursive_mut(lifetime, &mut visitor);
                    }

                    for ty in &mut variant.generic_arguments.types {
                        visitor::accept_recursive_mut(ty, &mut visitor);
                    }

                    for con in &mut variant.generic_arguments.constants {
                        visitor::accept_recursive_mut(con, &mut visitor);
                    }
                }

                _ => {}
            }
        }
    }
}

impl ir::Representation<ir::Model> {
    /// The entry point of the borrow checker.
    pub(in super::super) fn borrow_checker<S: table::State>(
        &mut self,
        current_site: GlobalID,
        ty_environment: &TyEnvironment<
            BorrowModel,
            S,
            impl Normalizer<BorrowModel, S>,
            impl Observer<BorrowModel, S>,
        >,
        handler: &HandlerWrapper,
    ) -> Result<(), TypeSystemOverflow<ir::Model>> {
        let (mut ir, mut arena) =
            transform_to_borrow_model(self.clone(), ty_environment.table());

        let all_block_ids =
            ir.control_flow_graph.blocks().ids().collect::<Vec<_>>();

        ir.replace_with_fresh_lifetimes(&mut arena);

        let mut checker = Checker {
            representation: ir,
            walk_results_by_block_id: HashMap::new(),
            target_contexts_by_block_id: HashMap::new(),
            starting_origin: arena,
            accesses: Arena::new(),
            current_site,
            ty_environment,
        };

        for block_id in all_block_ids {
            checker.walk_block(block_id, handler)?;
        }

        assert!(checker.walk_results_by_block_id.values().all(Option::is_some));

        *self = transform_to_ir_model(
            checker.representation,
            ty_environment.table(),
        );

        Ok(())
    }
}
