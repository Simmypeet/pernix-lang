use std::{
    cmp::Ordering,
    collections::{BTreeSet, HashMap, HashSet},
    ops::Not,
};

use pernixc_base::{handler::Handler, source_file::Span};

use super::{
    context::Context,
    state::{self, SetStateSucceeded, Stack, Summary},
};
use crate::{
    arena::{Arena, ID},
    error::{
        MoveInLoop, MovedOutValueFromMutableReference, MovedOutWhileBorrowed,
        OverflowOperation, TypeSystemOverflow, UseAfterMove,
        UseBeforeInitialization, VariableDoesNotLiveLongEnough,
    },
    ir::{
        self,
        address::{self, Address, Memory},
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
            borrow::{Loan, Model as BorrowModel, Origin},
            Values,
        },
        value::{
            register::{Assignment, FunctionCall, Load, ReferenceOf, Register},
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
        instantiation,
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

fn find_reference_address_usage(
    mut address: &Address<BorrowModel>,
) -> Option<&address::Reference<BorrowModel>> {
    loop {
        match address {
            Address::Memory(_) => return None,
            Address::Field(field) => address = &field.struct_address,
            Address::Tuple(tuple) => address = &tuple.tuple_address,
            Address::Index(index) => address = &index.array_address,
            Address::Variant(variant) => address = &variant.enum_address,
            Address::Reference(reference) => return Some(reference),
        }
    }
}

impl Values<BorrowModel> {
    /// Checks if the access is allowed e.g., access to droped value, mutably
    /// borrow more than once.
    fn handle_access<S: table::State>(
        &self,
        address: &Address<BorrowModel>,
        qualifier: Qualifier,
        access_span: Span,
        context: &Context,
        current_site: GlobalID,
        ty_environment: &TyEnvironment<
            BorrowModel,
            S,
            impl Normalizer<BorrowModel, S>,
            impl Observer<BorrowModel, S>,
        >,
        handler: &HandlerWrapper,
    ) -> Result<(), TypeSystemOverflow<ir::Model>> {
        // check if the variable is still alive
        let Succeeded { result: address_ty, constraints } = self
            .type_of_address(&address, current_site, ty_environment)
            .map_err(|x| TypeSystemOverflow::<ir::Model> {
                operation: OverflowOperation::TypeOf,
                overflow_span: access_span.clone(),
                overflow_error: x.into_overflow().unwrap(),
            })?;

        let loans = context.environment.get_loans(&address_ty);

        dbg!(&access_span, &loans, &address_ty);

        for borrow in loans.iter().filter_map(|x| x.as_borrow()) {
            // check if the variable is still alive
            let borrow_address = &self
                .registers
                .get(*borrow)
                .unwrap()
                .assignment
                .as_reference_of()
                .unwrap()
                .address;

            let Some(state) = context.stack.get_state(borrow_address) else {
                // the variable has been dropped
                handler.receive(Box::new(VariableDoesNotLiveLongEnough::<
                    ir::Model,
                > {
                    variable_span: match *borrow_address.get_root_memory() {
                        Memory::Parameter(id) => ty_environment
                            .table()
                            .get_callable(current_site.try_into().unwrap())
                            .unwrap()
                            .parameters()
                            .get(id)
                            .unwrap()
                            .span
                            .clone()
                            .unwrap(),
                        Memory::Alloca(id) => {
                            self.allocas.get(id).unwrap().span.clone()
                        }
                    },
                    for_lifetime: None,
                    instantiation_span: access_span.clone(),
                }));
                continue;
            };

            match state.get_state_summary() {
                Summary::Initialized => {}
                Summary::Uninitialized => {}
                Summary::Moved(span) => {
                    handler.receive(Box::new(MovedOutWhileBorrowed {
                        borrow_usage_span: access_span.clone(),
                        moved_out_span: span,
                    }));
                }
            }
        }

        Ok(())
    }

    fn handle_outlives<'a, S: table::State>(
        &self,
        outlives: impl IntoIterator<Item = &'a Outlives<Lifetime<BorrowModel>>>
            + Clone,
        priority_lifetimes: Option<HashSet<Lifetime<BorrowModel>>>,
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
            for outlive in outlives
                .clone()
                .into_iter()
                .filter(|x| priority_lifetimes.contains(&x.bound))
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

            for outlive in outlives
                .into_iter()
                .filter(|x| !priority_lifetimes.contains(&x.bound))
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

    fn handle_function_call<S: table::State>(
        &self,
        register_id: ID<Register<BorrowModel>>,
        function_call: &FunctionCall<BorrowModel>,
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
            }
        }

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

        let function_lifetimes = function_call
            .instantiation
            .lifetimes
            .values()
            .cloned()
            .chain(function_call.instantiation.types.values().flat_map(|x| {
                RecursiveIterator::new(x)
                    .filter_map(|x| x.0.into_lifetime().ok())
                    .cloned()
            }))
            .chain(function_call.instantiation.constants.values().flat_map(
                |x| {
                    RecursiveIterator::new(x)
                        .filter_map(|x| x.0.into_lifetime().ok())
                        .cloned()
                },
            ))
            .collect::<HashSet<_>>();

        self.handle_outlives(
            lifetime_constraints
                .iter()
                .map(|x| x.as_lifetime_outlives().unwrap()),
            Some(function_lifetimes),
            register_span,
            context,
            current_site,
            ty_environment,
            handler,
        )?;

        Ok(())
    }

    fn handle_reference_of<S: table::State>(
        &self,
        register_id: ID<Register<BorrowModel>>,
        reference_of: &ReferenceOf<BorrowModel>,
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

        let Succeeded { result: ty, constraints } = self
            .type_of_address(
                &reference_of.address,
                current_site,
                ty_environment,
            )
            .map_err(|x| TypeSystemOverflow::<ir::Model> {
                operation: OverflowOperation::TypeOf,
                overflow_span: register_span.clone(),
                overflow_error: x.into_overflow().unwrap(),
            })?;

        // handle outlives constraints of the reference
        self.handle_outlives(
            constraints.iter().map(|x| x.as_lifetime_outlives().unwrap()),
            None,
            register_span.clone(),
            context,
            current_site,
            ty_environment,
            handler,
        )?;

        let reference_of_origin_id =
            reference_of.lifetime.clone().into_inference().unwrap();

        let mut loans = context.environment.get_loans(&ty);
        loans.insert(Loan::Borrow(register_id));

        context
            .environment
            .origins
            .get_mut(reference_of_origin_id)
            .unwrap()
            .loans = loans;

        // check the access
        self.handle_access(
            &reference_of.address,
            reference_of.qualifier,
            register_span,
            context,
            current_site,
            ty_environment,
            handler,
        )?;

        Ok(())
    }

    fn handle_store<S: table::State>(
        &self,
        store_address: &Address<BorrowModel>,
        value_type: Option<Succeeded<Type<BorrowModel>, BorrowModel>>,
        store_span: Span,
        context: &mut Context,
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

                    for origin_id in address_lifetimes
                        .iter()
                        .filter_map(|x| x.as_inference())
                        .copied()
                    {
                        context
                            .environment
                            .origins
                            .get_mut(origin_id)
                            .unwrap()
                            .loans
                            .clear();
                    }

                    // apply the compatibility constraints
                    self.handle_outlives(
                        value_constraints
                            .iter()
                            .chain(address_constraints.iter())
                            .chain(compatibility_constraints.iter())
                            .map(|x| x.as_lifetime_outlives().unwrap()),
                        Some(address_lifetimes),
                        store_span,
                        context,
                        current_site,
                        ty_environment,
                        handler,
                    )?;
                }
                Ok(None) => {}
                Err(OverflowError) => {
                    return Err(TypeSystemOverflow {
                        operation: OverflowOperation::TypeCheck,
                        overflow_span: store_span.clone(),
                        overflow_error: OverflowError,
                    })
                }
            }
        }

        Ok(drop_instructions)
    }

    fn handle_load<S: table::State>(
        &self,
        load: &Load<BorrowModel>,
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
                self.handle_access(
                    &load.address,
                    Qualifier::Immutable,
                    register_span,
                    context,
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

        self.handle_access(
            &load.address,
            Qualifier::Immutable,
            register_span,
            context,
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
    contexts_by_block_id: HashMap<ID<Block<BorrowModel>>, Option<Context>>,

    /// If the block id appears in this map, it means the block is a looped
    /// block and the value is the starting environment of the looped block.
    target_contexts_by_block_id: HashMap<ID<Block<BorrowModel>>, Context>,

    /// Contains the starting borrow origins. (Contains all the id with empty
    /// values)
    starting_origin: Arena<Origin>,

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
                            span,
                            context,
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
                                self.current_site,
                                self.ty_environment,
                                handler,
                            )?;

                            1
                        }

                        Assignment::ReferenceOf(reference_of) => {
                            self.representation.values.handle_reference_of(
                                register_assignment.id,
                                reference_of,
                                register.span.clone(),
                                context,
                                self.current_site,
                                self.ty_environment,
                                handler,
                            )?;

                            1
                        }

                        Assignment::FunctionCall(function_call) => {
                            self.representation.values.handle_function_call(
                                register_assignment.id,
                                function_call,
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
                        | Assignment::Struct(_)
                        | Assignment::Variant(_)
                        | Assignment::Binary(_)
                        | Assignment::Array(_)
                        | Assignment::Phi(_)
                        | Assignment::Cast(_)
                        | Assignment::VariantNumber(_) => 1,
                    }
                }

                Instruction::AllocaDeclaration(alloca_declaration) => {
                    assert!(context.stack.current_mut().new_state(
                        Memory::Alloca(alloca_declaration.id),
                        false,
                        self.representation
                            .values
                            .allocas
                            .get(alloca_declaration.id)
                            .unwrap()
                            .r#type
                            .clone(),
                    ));

                    1
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

    fn walk_block(
        &mut self,
        block_id: ID<Block<BorrowModel>>,
        handler: &HandlerWrapper,
    ) -> Result<Option<Context>, TypeSystemOverflow<ir::Model>> {
        // skip if already processed
        if let Some(stack) = self.contexts_by_block_id.get(&block_id) {
            return Ok(stack.clone());
        }

        // mark as processing
        self.contexts_by_block_id.insert(block_id, None);

        let block =
            self.representation.control_flow_graph.get_block(block_id).unwrap();

        let mut starting_context = if block.is_entry() {
            Context {
                stack: Stack::new(),
                environment: Environment {
                    origins: self.starting_origin.clone(),
                },
            }
        } else {
            let predecessors =
                block.predecessors().iter().copied().collect::<Vec<_>>();

            let mut merging_contexts = Vec::new();
            let mut looped_block_ids = Vec::new();

            for predecessor_id in predecessors.iter().copied() {
                if let Some(stack) = self.walk_block(predecessor_id, handler)? {
                    merging_contexts.push((predecessor_id, stack));
                } else {
                    looped_block_ids.push(predecessor_id);
                }
            }

            if merging_contexts.is_empty() {
                // try again later
                self.contexts_by_block_id.remove(&block_id);

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
                    .contexts_by_block_id
                    .get(&predecessor)
                    .unwrap()
                    .as_ref()
                    .unwrap();

                // drop instructions to insert at the end of the block
                let mut drop_instructions = Vec::new();

                for (this, alternate) in context
                    .stack
                    .scopes()
                    .iter()
                    .rev()
                    .zip(alternate_environment.stack.scopes().iter().rev())
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

            context
        };

        self.walk_instructions(block_id, &mut starting_context, handler)?;

        // handle loop
        if let Some(target_stack) =
            self.target_contexts_by_block_id.get(&block_id)
        {
            assert_eq!(
                target_stack.stack.scopes().len(),
                starting_context.stack.scopes().len()
            );

            for (this_scope, target_scope) in starting_context
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

            starting_context = target_stack.clone();
        }

        // mark as done
        assert!(self
            .contexts_by_block_id
            .insert(block_id, Some(starting_context.clone()))
            .unwrap()
            .is_none());

        Ok(Some(starting_context))
    }
}

#[derive(Debug, PartialEq, Eq)]
struct ReplaceWithFreshInference<'a> {
    origins: &'a mut Arena<Origin>,
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

        *term = Lifetime::Inference(self.origins.insert(Origin::default()));

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
    fn replace_with_fresh_lifetimes(&mut self, origins: &mut Arena<Origin>) {
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
            contexts_by_block_id: HashMap::new(),
            target_contexts_by_block_id: HashMap::new(),
            starting_origin: arena,
            current_site,
            ty_environment,
        };

        for block_id in all_block_ids {
            checker.walk_block(block_id, handler)?;
        }

        assert!(checker.contexts_by_block_id.values().all(Option::is_some));

        *self = transform_to_ir_model(
            checker.representation,
            ty_environment.table(),
        );

        dbg!(self);

        Ok(())
    }
}
