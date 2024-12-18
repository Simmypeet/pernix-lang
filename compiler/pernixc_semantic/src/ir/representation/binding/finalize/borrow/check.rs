use std::{
    collections::{BTreeSet, HashMap, HashSet},
    sync::Arc,
};

use pernixc_base::source_file::Span;

use super::{
    environment::{get_lifetimes_in_address, RegionInfo},
    local_region_generator::LocalRegionGenerator,
};
use crate::{
    arena::ID,
    error::{OverflowOperation, TypeSystemOverflow},
    ir::{
        self,
        address::{Address, Memory},
        control_flow_graph::{Block, Point},
        instruction::{Instruction, Return, Terminator, UnconditionalJump},
        representation::{
            binding::{
                finalize::borrow::{
                    environment::Environment,
                    transform::transform_to_borrow_model,
                },
                HandlerWrapper,
            },
            borrow::Model as BorrowModel,
        },
        value::{
            register::{
                Array, Assignment, Borrow, FunctionCall, Load, Phi, Register,
                Struct, Variant,
            },
            Value,
        },
    },
    symbol::{
        table::{self, representation::Index},
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

impl<
        'a,
        S: table::State,
        N: Normalizer<BorrowModel, S>,
        O: Observer<BorrowModel, S>,
    > Environment<'a, S, N, O>
{
    fn handle_return(
        &mut self,
        ret: &Return<BorrowModel>,
        handler: &HandlerWrapper,
    ) -> Result<(), TypeSystemOverflow<ir::Model>> {
        let return_ty = Type::from_default_model(
            self.ty_environment()
                .table()
                .get_callable(self.current_site().try_into().unwrap())
                .unwrap()
                .return_type()
                .clone(),
        );

        let Succeeded { result: value_ty, mut constraints } = self
            .representation()
            .values
            .type_of_value(
                &ret.value,
                self.current_site(),
                self.ty_environment(),
            )
            .map_err(|x| TypeSystemOverflow::<ir::Model> {
                operation: OverflowOperation::TypeOf,
                overflow_span: ret.span.clone(),
                overflow_error: x.into_overflow().unwrap(),
            })?;

        match value_ty.compatible(
            &return_ty,
            Variance::Covariant,
            self.ty_environment(),
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

                constraints.extend(compatibility_constraints);

                self.handle_outlives_constraints(
                    constraints
                        .iter()
                        .map(|x| x.as_lifetime_outlives().unwrap()),
                    ret.span.clone(),
                    handler,
                )?;
            }
            Ok(None) => {
                panic!("{value_ty:#?} => {return_ty:#?}")
            }
            Err(OverflowError) => {
                return Err(TypeSystemOverflow {
                    operation: OverflowOperation::TypeCheck,
                    overflow_span: ret.span.clone(),
                    overflow_error: OverflowError,
                })
            }
        }

        Ok(())
    }

    fn handle_variant(
        &mut self,
        variant: &Variant<BorrowModel>,
        register_span: Span,
        handler: &HandlerWrapper,
    ) -> Result<(), TypeSystemOverflow<ir::Model>> {
        let variant_sym =
            self.ty_environment().table().get(variant.variant_id).unwrap();
        let enum_id = self
            .ty_environment()
            .table()
            .get(variant.variant_id)
            .unwrap()
            .parent_enum_id();
        let enum_sym = self.ty_environment().table().get(enum_id).unwrap();

        let instantiation = Instantiation::from_generic_arguments(
            variant.generic_arguments.clone(),
            enum_id.into(),
            &enum_sym.generic_declaration.parameters,
        )
        .unwrap();

        let mut lifetime_constraints = BTreeSet::new();

        // compare each values in the field to the struct's field type
        if let Some(mut associated_type) = variant_sym
            .associated_type
            .as_ref()
            .map(|x| Type::from_default_model(x.clone()))
        {
            instantiation::instantiate(&mut associated_type, &instantiation);
            let associated_value = variant.associated_value.as_ref().unwrap();
            let value_span = match associated_value {
                Value::Register(id) => self
                    .representation()
                    .values
                    .registers
                    .get(*id)
                    .unwrap()
                    .span
                    .clone(),
                Value::Literal(literal) => literal.span().clone(),
            };

            let Succeeded { result: value_ty, constraints: value_constraints } =
                self.representation()
                    .values
                    .type_of_value(
                        associated_value,
                        self.current_site(),
                        self.ty_environment(),
                    )
                    .map_err(|x| TypeSystemOverflow::<ir::Model> {
                        operation: OverflowOperation::TypeOf,
                        overflow_span: value_span.clone(),
                        overflow_error: x.into_overflow().unwrap(),
                    })?;

            lifetime_constraints.extend(value_constraints);

            let copmatibility = value_ty
                .compatible(
                    &associated_type,
                    Variance::Covariant,
                    self.ty_environment(),
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
        self.handle_outlives_constraints(
            lifetime_constraints
                .iter()
                .map(|x| x.as_lifetime_outlives().unwrap()),
            register_span.clone(),
            handler,
        )?;

        lifetime_constraints.clear();

        // handle the constraints introduced by the outlive predicates of the
        // struct

        for predicate in self
            .ty_environment()
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

        self.handle_outlives_constraints(
            lifetime_constraints
                .iter()
                .map(|x| x.as_lifetime_outlives().unwrap()),
            register_span.clone(),
            handler,
        )?;

        Ok(())
    }

    fn handle_phi(
        &mut self,
        phi: &Phi<BorrowModel>,
        register_span: Span,
        handler: &HandlerWrapper,
    ) -> Result<(), TypeSystemOverflow<ir::Model>> {
        let mut lifetime_constraints = BTreeSet::new();

        for value in phi.incoming_values.values() {
            let value_span = match value {
                Value::Register(id) => self
                    .representation()
                    .values
                    .registers
                    .get(*id)
                    .unwrap()
                    .span
                    .clone(),
                Value::Literal(literal) => literal.span().clone(),
            };

            let Succeeded { result: value_ty, constraints } = self
                .representation()
                .values
                .type_of_value(
                    value,
                    self.current_site(),
                    self.ty_environment(),
                )
                .map_err(|x| TypeSystemOverflow::<ir::Model> {
                    operation: OverflowOperation::TypeOf,
                    overflow_span: value_span.clone(),
                    overflow_error: x.into_overflow().unwrap(),
                })?;

            lifetime_constraints.extend(constraints);

            let copmatibility = value_ty
                .compatible(
                    &phi.r#type,
                    Variance::Covariant,
                    self.ty_environment(),
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
                panic!("{value_ty:#?} => {:#?}", phi.r#type)
            }
        }

        self.handle_outlives_constraints(
            lifetime_constraints
                .iter()
                .map(|x| x.as_lifetime_outlives().unwrap()),
            register_span.clone(),
            handler,
        )?;

        Ok(())
    }

    fn handle_array(
        &mut self,
        array: &Array<BorrowModel>,
        register_span: Span,
        handler: &HandlerWrapper,
    ) -> Result<(), TypeSystemOverflow<ir::Model>> {
        let array_ty = array.element_type.clone();
        let mut lifetime_constraints = BTreeSet::new();

        for value in &array.elements {
            let value_span = match value {
                Value::Register(id) => self
                    .representation()
                    .values
                    .registers
                    .get(*id)
                    .unwrap()
                    .span
                    .clone(),
                Value::Literal(literal) => literal.span().clone(),
            };

            let Succeeded { result: value_ty, constraints } = self
                .representation()
                .values
                .type_of_value(
                    value,
                    self.current_site(),
                    self.ty_environment(),
                )
                .map_err(|x| TypeSystemOverflow::<ir::Model> {
                    operation: OverflowOperation::TypeOf,
                    overflow_span: value_span.clone(),
                    overflow_error: x.into_overflow().unwrap(),
                })?;

            lifetime_constraints.extend(constraints);

            let copmatibility = value_ty
                .compatible(
                    &array_ty,
                    Variance::Covariant,
                    self.ty_environment(),
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
                panic!("{value_ty:#?} => {array_ty:#?}")
            }
        }

        self.handle_outlives_constraints(
            lifetime_constraints
                .iter()
                .map(|x| x.as_lifetime_outlives().unwrap()),
            register_span.clone(),
            handler,
        )?;

        Ok(())
    }

    fn handle_struct(
        &mut self,
        struct_lit: &Struct<BorrowModel>,
        register_span: Span,
        handler: &HandlerWrapper,
    ) -> Result<(), TypeSystemOverflow<ir::Model>> {
        let instantiation = Instantiation::from_generic_arguments(
            struct_lit.generic_arguments.clone(),
            struct_lit.struct_id.into(),
            &self
                .ty_environment()
                .table()
                .get(struct_lit.struct_id)
                .unwrap()
                .generic_declaration
                .parameters,
        )
        .unwrap();

        let mut lifetime_constraints = BTreeSet::new();

        let struct_sym =
            self.ty_environment().table().get(struct_lit.struct_id).unwrap();

        // compare each values in the field to the struct's field type
        for field_id in struct_sym.field_declaration_order().iter().copied() {
            let mut field_ty = Type::from_other_model(
                struct_sym.fields().get(field_id).unwrap().r#type.clone(),
            );
            instantiation::instantiate(&mut field_ty, &instantiation);

            let value_span =
                match &struct_lit.initializers_by_field_id.get(&field_id) {
                    Some(Value::Register(id)) => self
                        .representation()
                        .values
                        .registers
                        .get(*id)
                        .unwrap()
                        .span
                        .clone(),
                    Some(Value::Literal(literal)) => literal.span().clone(),
                    None => unreachable!(),
                };

            let Succeeded { result: value_ty, constraints: value_constraints } =
                self.representation()
                    .values
                    .type_of_value(
                        &struct_lit
                            .initializers_by_field_id
                            .get(&field_id)
                            .unwrap(),
                        self.current_site(),
                        self.ty_environment(),
                    )
                    .map_err(|x| TypeSystemOverflow::<ir::Model> {
                        operation: OverflowOperation::TypeOf,
                        overflow_span: value_span.clone(),
                        overflow_error: x.into_overflow().unwrap(),
                    })?;

            lifetime_constraints.extend(value_constraints);

            let copmatibility = value_ty
                .compatible(
                    &field_ty,
                    Variance::Covariant,
                    self.ty_environment(),
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
                panic!("{value_ty:#?} => {field_ty:#?}")
            }
        }

        // handle the constraints introduced by instantiating the struct
        self.handle_outlives_constraints(
            lifetime_constraints
                .iter()
                .map(|x| x.as_lifetime_outlives().unwrap()),
            register_span.clone(),
            handler,
        )?;

        lifetime_constraints.clear();

        // handle the constraints introduced by the outlive predicates of the
        // struct

        for predicate in self
            .ty_environment()
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

        self.handle_outlives_constraints(
            lifetime_constraints
                .iter()
                .map(|x| x.as_lifetime_outlives().unwrap()),
            register_span.clone(),
            handler,
        )?;

        // invalidated lifetimes will not be checked here; unlike function call

        Ok(())
    }

    fn handle_function_call(
        &mut self,
        function_call: &FunctionCall<BorrowModel>,
        register_span: Span,
        handler: &HandlerWrapper,
    ) -> Result<(), TypeSystemOverflow<ir::Model>> {
        let callable = self
            .ty_environment()
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
                Value::Register(id) => self
                    .representation()
                    .values
                    .registers
                    .get(*id)
                    .unwrap()
                    .span
                    .clone(),
                Value::Literal(literal) => literal.span().clone(),
            };
            let Succeeded {
                result: argument_ty,
                constraints: argument_ty_constraints,
            } = self
                .representation()
                .values
                .type_of_value(
                    argument,
                    self.current_site(),
                    self.ty_environment(),
                )
                .map_err(|x| TypeSystemOverflow::<ir::Model> {
                    operation: OverflowOperation::TypeOf,
                    overflow_span: argument_span.clone(),
                    overflow_error: x.into_overflow().unwrap(),
                })?;

            lifetime_constraints.extend(argument_ty_constraints);

            let copmatibility = argument_ty
                .compatible(
                    &parameter_ty,
                    Variance::Covariant,
                    self.ty_environment(),
                )
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

        self.handle_outlives_constraints(
            lifetime_constraints
                .iter()
                .map(|x| x.as_lifetime_outlives().unwrap()),
            register_span.clone(),
            handler,
        )?;

        lifetime_constraints.clear();

        for predicate in self
            .ty_environment()
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

        self.handle_outlives_constraints(
            lifetime_constraints
                .iter()
                .map(|x| x.as_lifetime_outlives().unwrap()),
            register_span.clone(),
            handler,
        )?;

        Ok(())
    }

    fn handle_reference_of(
        &mut self,
        register_id: ID<Register<BorrowModel>>,
        reference_of: &Borrow<BorrowModel>,
        register_span: Span,
        point: Point<BorrowModel>,
        handler: &HandlerWrapper,
    ) -> Result<(), TypeSystemOverflow<ir::Model>> {
        // check the access
        self.handle_access(
            &reference_of.address,
            reference_of.qualifier,
            register_span.clone(),
            point,
            handler,
        )?;

        let reference_of_origin_id =
            reference_of.lifetime.clone().into_inference().unwrap();

        let constraints_in_type_of = self
            .representation()
            .values
            .type_of_address(
                &reference_of.address,
                self.current_site(),
                self.ty_environment(),
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
            &self.representation().values,
            self.current_site(),
            self.ty_environment(),
        )?
        .into_iter()
        .map(|x| Outlives::new(x, Lifetime::Inference(reference_of_origin_id)))
        .collect::<Vec<_>>();

        // handle outlives constraints of the reference
        self.handle_outlives_constraints(
            constraints_in_type_of
                .iter()
                .map(|x| x.as_lifetime_outlives().unwrap())
                .chain(constraints_in_address.iter()),
            register_span,
            handler,
        )?;

        self.attach_borrow(register_id, reference_of_origin_id);

        Ok(())
    }

    fn handle_store(
        &mut self,
        store_address: &Address<BorrowModel>,
        value_type: Succeeded<Type<BorrowModel>, BorrowModel>,
        store_span: Span,
        point: Point<BorrowModel>,
        handler: &HandlerWrapper,
    ) -> Result<(), TypeSystemOverflow<ir::Model>> {
        let Succeeded { result: address_ty, constraints: address_constraints } =
            self.representation()
                .values
                .type_of_address(
                    store_address,
                    self.current_site(),
                    self.ty_environment(),
                )
                .map_err(|x| TypeSystemOverflow::<ir::Model> {
                    operation: OverflowOperation::TypeOf,
                    overflow_span: store_span.clone(),
                    overflow_error: x.into_overflow().unwrap(),
                })?;

        match value_type.result.compatible(
            &address_ty,
            Variance::Covariant,
            self.ty_environment(),
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

                self.detach_subset_relations(
                    address_lifetimes
                        .into_iter()
                        .filter_map(|x| x.try_into().ok()),
                );

                // apply the compatibility constraints
                self.handle_outlives_constraints(
                    value_type
                        .constraints
                        .iter()
                        .chain(address_constraints.iter())
                        .chain(compatibility_constraints.iter())
                        .map(|x| x.as_lifetime_outlives().unwrap()),
                    store_span.clone(),
                    handler,
                )?;
            }
            Ok(None) => {
                panic!("{:#?} => {address_ty:#?}", value_type.result)
            }
            Err(OverflowError) => {
                return Err(TypeSystemOverflow {
                    operation: OverflowOperation::TypeCheck,
                    overflow_span: store_span.clone(),
                    overflow_error: OverflowError,
                })
            }
        }

        self.handle_access(
            store_address,
            Qualifier::Mutable,
            store_span,
            point,
            handler,
        )
    }

    fn handle_load(
        &mut self,
        load: &Load<BorrowModel>,
        register_span: Span,
        point: Point<BorrowModel>,
        handler: &HandlerWrapper,
    ) -> Result<(), TypeSystemOverflow<ir::Model>> {
        let ty = self
            .representation()
            .values
            .type_of_address(
                &load.address,
                self.current_site(),
                self.ty_environment(),
            )
            .unwrap();

        // has been checked previously
        'out: {
            if load.address.get_reference_qualifier()
                == Some(Qualifier::Immutable)
                || load.address.is_behind_index()
            {
                // TODO: check copy marker
            } else {
                let copy_marker = self
                    .ty_environment()
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
                    self.ty_environment(),
                )
                .iter()
                .all(well_formedness::Error::is_lifetime_constraints)
                {
                    break 'out;
                }

                // TODO: check the usage of borrow after move
            }
        };

        self.handle_access(
            &load.address,
            Qualifier::Immutable,
            register_span,
            point,
            handler,
        )
    }
}

impl<
        'a,
        S: table::State,
        N: Normalizer<BorrowModel, S>,
        O: Observer<BorrowModel, S>,
    > Environment<'a, S, N, O>
{
    fn walk_instructions(
        &mut self,
        block_id: ID<Block<BorrowModel>>,
        handler: &HandlerWrapper,
    ) -> Result<(), TypeSystemOverflow<ir::Model>> {
        let block = self
            .representation()
            .control_flow_graph
            .get_block(block_id)
            .unwrap();

        for (index, inst) in block.instructions().iter().enumerate() {
            let current_point = Point { block_id, instruction_index: index };

            match inst {
                Instruction::Store(store) => {
                    let span = match &store.value {
                        Value::Register(id) => self
                            .representation()
                            .values
                            .registers
                            .get(*id)
                            .unwrap()
                            .span
                            .clone(),
                        Value::Literal(literal) => literal.span().clone(),
                    };
                    let value_type = self
                        .representation()
                        .values
                        .type_of_value(
                            &store.value,
                            self.current_site(),
                            self.ty_environment(),
                        )
                        .map_err(|x| TypeSystemOverflow::<ir::Model> {
                            operation: OverflowOperation::TypeOf,
                            overflow_span: span.clone(),
                            overflow_error: x.into_overflow().unwrap(),
                        })?;

                    self.handle_store(
                        &store.address,
                        value_type,
                        store.span.clone(),
                        current_point,
                        handler,
                    )?;
                }

                Instruction::RegisterAssignment(register_assignment) => {
                    let register = self
                        .representation()
                        .values
                        .registers
                        .get(register_assignment.id)
                        .unwrap();

                    match &register.assignment {
                        Assignment::Load(load) => {
                            self.handle_load(
                                load,
                                register.span.clone(),
                                current_point,
                                handler,
                            )?;
                        }

                        Assignment::Borrow(reference_of) => {
                            self.handle_reference_of(
                                register_assignment.id,
                                reference_of,
                                register.span.clone(),
                                current_point,
                                handler,
                            )?;
                        }

                        Assignment::FunctionCall(function_call) => {
                            self.handle_function_call(
                                function_call,
                                register.span.clone(),
                                handler,
                            )?;
                        }

                        Assignment::Struct(struct_lit) => {
                            self.handle_struct(
                                struct_lit,
                                register.span.clone(),
                                handler,
                            )?;
                        }

                        Assignment::Variant(variant) => {
                            self.handle_variant(
                                variant,
                                register.span.clone(),
                                handler,
                            )?;
                        }

                        Assignment::Array(array) => {
                            self.handle_array(
                                array,
                                register.span.clone(),
                                handler,
                            )?;
                        }

                        Assignment::Phi(phi) => {
                            self.handle_phi(
                                phi,
                                register.span.clone(),
                                handler,
                            )?;
                        }

                        Assignment::Prefix(_)
                        | Assignment::Tuple(_)
                        | Assignment::Binary(_)
                        | Assignment::Cast(_)
                        | Assignment::VariantNumber(_) => {}
                    }
                }

                Instruction::TuplePack(tuple_pack) => {
                    let tuple_ty = self
                        .representation()
                        .values
                        .type_of_address(
                            &tuple_pack.tuple_address,
                            self.current_site(),
                            self.ty_environment(),
                        )
                        .map_err(|x| TypeSystemOverflow::<ir::Model> {
                            operation: OverflowOperation::TypeOf,
                            overflow_span: tuple_pack.packed_tuple_span.clone(),
                            overflow_error: x.into_overflow().unwrap(),
                        })?;

                    self.handle_store(
                        &tuple_pack.store_address,
                        tuple_ty.map(|x| {
                            x.into_tuple()
                                .unwrap()
                                .elements
                                .into_iter()
                                .find_map(|x| x.is_unpacked.then_some(x.term))
                                .unwrap()
                        }),
                        tuple_pack.packed_tuple_span.clone(),
                        current_point,
                        handler,
                    )?;
                }

                Instruction::RegisterDiscard(_)
                | Instruction::DropUnpackTuple(_)
                | Instruction::ScopePush(_)
                | Instruction::Drop(_) => {}

                Instruction::ScopePop(scope_pop) => {
                    println!("scope pop {:?}", scope_pop);
                    // TODO: check use of goes out of scope borrows
                    let mut dropped_memories = self
                        .representation()
                        .values
                        .allocas
                        .iter()
                        .filter_map(|(id, x)| {
                            (x.declared_in_scope_id == scope_pop.0)
                                .then_some(Memory::Alloca(id))
                        })
                        .collect::<Vec<_>>();

                    // add parameters to the dropped memories
                    if let Ok(callable_id) =
                        CallableID::try_from(self.current_site())
                    {
                        if scope_pop.0
                            == self.representation().scope_tree.root_scope_id()
                        {
                            let callable = self
                                .ty_environment()
                                .table()
                                .get_callable(callable_id)
                                .unwrap();

                            dropped_memories.extend(
                                callable
                                    .parameters()
                                    .ids()
                                    .map(|x| Memory::Parameter(x)),
                            );
                        }
                    }

                    for memory in dropped_memories {
                        println!("dropping {:?}", memory);

                        self.handle_drop_memory(
                            memory,
                            current_point,
                            handler,
                        )?;
                    }
                }
            }
        }

        let Some(terminator) = block.terminator() else {
            return Ok(());
        };

        Ok(())
    }
}

#[derive(Debug)]
struct WalkResult<
    'a,
    S: table::State,
    N: Normalizer<BorrowModel, S>,
    O: Observer<BorrowModel, S>,
> {
    /// The context after checking the whole block
    environment: Environment<'a, S, N, O>,
    looped_blocks: Vec<ID<Block<BorrowModel>>>,
}

impl<
        'a,
        S: table::State,
        N: Normalizer<BorrowModel, S>,
        O: Observer<BorrowModel, S>,
    > Clone for WalkResult<'a, S, N, O>
{
    fn clone(&self) -> Self {
        Self {
            environment: self.environment.clone(),
            looped_blocks: self.looped_blocks.clone(),
        }
    }
}

#[derive(Debug, Clone)]
struct Checker<
    'a,
    S: table::State,
    N: Normalizer<BorrowModel, S>,
    O: Observer<BorrowModel, S>,
> {
    representation: &'a ir::Representation<BorrowModel>,

    /// The key represents the block ID that needs to be checked/explored.
    ///
    /// - `None` value means the block is being processed.
    /// - `Some` value means the block has been processed
    /// - No value means the block has not been explored
    walk_results_by_block_id:
        HashMap<ID<Block<BorrowModel>>, Option<WalkResult<'a, S, N, O>>>,

    /// If the block id appears in this map, it means the block is a looped
    /// block and the value is the starting environment of the looped block.
    target_environments_by_block_id:
        HashMap<ID<Block<BorrowModel>>, Environment<'a, S, N, O>>,

    region_info: Arc<RegionInfo>,

    current_site: GlobalID,
    ty_environment: &'a TyEnvironment<'a, BorrowModel, S, N, O>,
}

impl<
        'a,
        S: table::State,
        N: Normalizer<BorrowModel, S>,
        O: Observer<BorrowModel, S>,
    > Checker<'a, S, N, O>
{
    fn get_predecessor_environment(
        &mut self,
        block_id: ID<Block<BorrowModel>>,
        handler: &HandlerWrapper,
    ) -> Result<Option<Environment<'a, S, N, O>>, TypeSystemOverflow<ir::Model>>
    {
        let Some(mut walk_result) = self.walk_block(block_id, handler)? else {
            return Ok(None);
        };

        for block_id in walk_result.looped_blocks {
            let Some(looped_environment) =
                self.get_predecessor_environment(block_id, handler)?
            else {
                continue;
            };

            walk_result.environment.merge(&looped_environment);
        }

        Ok(Some(walk_result.environment))
    }

    fn walk_block(
        &mut self,
        block_id: ID<Block<BorrowModel>>,
        handler: &HandlerWrapper,
    ) -> Result<Option<WalkResult<'a, S, N, O>>, TypeSystemOverflow<ir::Model>>
    {
        // skip if already processed
        if let Some(walk_result) = self.walk_results_by_block_id.get(&block_id)
        {
            return Ok(walk_result.clone());
        }

        // mark as processing
        self.walk_results_by_block_id.insert(block_id, None);

        let block =
            self.representation.control_flow_graph.get_block(block_id).unwrap();

        let (mut environment, looped_blocks) = if block.is_entry() {
            assert!(block.predecessors().is_empty());

            let mut starting_environment = Environment::new(
                self.region_info.clone(),
                &self.representation,
                self.current_site,
                self.ty_environment,
            )?;

            let predicates = self
                .ty_environment
                .table()
                .get_active_premise::<BorrowModel>(self.current_site.into())
                .unwrap()
                .predicates;

            let mut adding_edges = Vec::new();

            for predicate in predicates {
                match predicate {
                    Predicate::LifetimeOutlives(outlives) => {
                        let (Some(operand), Some(bound)) = (
                            outlives.operand.try_into().ok(),
                            outlives.bound.try_into().ok(),
                        ) else {
                            continue;
                        };

                        adding_edges.push((operand, bound));
                    }

                    Predicate::TypeOutlives(outlives) => {
                        let Some(bound) = outlives.bound.try_into().ok() else {
                            continue;
                        };

                        for operand in RecursiveIterator::new(&outlives.operand)
                            .filter_map(|x| x.0.into_lifetime().ok())
                            .filter_map(|x| x.clone().try_into().ok())
                        {
                            adding_edges.push((operand, bound));
                        }
                    }

                    _ => {}
                }
            }

            starting_environment.insert_known_subset_relation(adding_edges);

            (starting_environment, Vec::new())
        } else {
            let predecessors =
                block.predecessors().iter().copied().collect::<Vec<_>>();

            let mut merging_environments = Vec::new();
            let mut looped_block_ids = Vec::new();

            for predecessor_id in predecessors.iter().copied() {
                if let Some(stack) =
                    self.get_predecessor_environment(predecessor_id, handler)?
                {
                    merging_environments.push((predecessor_id, stack));
                } else {
                    looped_block_ids.push(predecessor_id);
                }
            }

            if merging_environments.is_empty() {
                // try again later
                self.walk_results_by_block_id.remove(&block_id);

                return Ok(None);
            }

            // Sanity check
            if merging_environments.len() > 1 {
                for i in merging_environments.iter().map(|x| x.0) {
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
            let mut environment = merging_environments.pop().unwrap().1;
            environment = merging_environments.into_iter().fold(
                environment,
                |mut acc, (_, env)| {
                    acc.merge(&env);
                    acc
                },
            );

            // mark the looped block
            for looped in looped_block_ids.iter().copied() {
                self.target_environments_by_block_id
                    .insert(looped, environment.clone());
            }

            (environment, looped_block_ids)
        };

        environment.walk_instructions(block_id, handler)?;

        let result = WalkResult { environment, looped_blocks };

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
    generator: &'a mut LocalRegionGenerator,
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

        *term = Lifetime::Inference(self.generator.next());

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
        origins: &mut LocalRegionGenerator,
    ) {
        let mut visitor = ReplaceWithFreshInference { generator: origins };

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
    pub(in super::super) fn borrow_check<S: table::State>(
        &self,
        current_site: GlobalID,
        ty_environment: &TyEnvironment<
            BorrowModel,
            S,
            impl Normalizer<BorrowModel, S>,
            impl Observer<BorrowModel, S>,
        >,
        handler: &HandlerWrapper,
    ) -> Result<(), TypeSystemOverflow<ir::Model>> {
        let (mut ir, mut generator) =
            transform_to_borrow_model(self.clone(), ty_environment.table());

        let all_block_ids =
            ir.control_flow_graph.blocks().ids().collect::<Vec<_>>();

        ir.replace_with_fresh_lifetimes(&mut generator);

        let mut checker = Checker {
            representation: &ir,
            walk_results_by_block_id: HashMap::new(),
            target_environments_by_block_id: HashMap::new(),
            region_info: Arc::new(RegionInfo::new(
                generator,
                current_site,
                ty_environment.table(),
            )),
            current_site,
            ty_environment,
        };

        for block_id in all_block_ids {
            checker.walk_block(block_id, handler)?;
        }

        assert!(checker.walk_results_by_block_id.values().all(Option::is_some));

        dbg!(ir);

        Ok(())
    }
}
