use std::collections::HashSet;

use pernixc_base::{
    handler::{Dummy, Handler, Storage},
    source_file::{SourceElement, Span},
};
use pernixc_syntax::syntax_tree::{
    self,
    expression::{AccessKind, PostfixOperator},
    ConnectedList,
};

use super::{Bind, Config, Expression, LValue};
use crate::{
    arena::ID,
    error::{
        self, AdtImplementationFunctionCannotBeUsedAsMethod,
        AmbiguousMethodCall, CannotIndexPastUnpackedTuple, ExpectArray,
        ExpectStructType, ExpressionIsNotCallable, FieldIsNotAccessible,
        FieldNotFound, InvalidCastType, MethodNotFound,
        MismatchedArgumentCount, MismatchedQualifierForReferenceOf,
        OverflowOperation, SymbolIsNotAccessible, SymbolIsNotCallable,
        SymbolNotFound, TooLargeTupleIndex, TupleExpected,
        TupleIndexOutOfBOunds, TypeSystemOverflow,
        UnexpectedGenericArgumentsInField,
    },
    ir::{
        self,
        address::{self, Address, Memory},
        instruction::{Instruction, Store},
        representation::{
            binding::{
                expression::Target,
                infer::{self, InferenceVariable},
                Binder, Error, InferenceProvider, InternalError, SemanticError,
            },
            borrow,
        },
        value::{
            literal::{self, Literal},
            register::{
                Assignment, Cast, FunctionCall, Load, ReferenceOf, Register,
                Variant,
            },
            Value,
        },
        Erased, NoConstraint,
    },
    symbol::{
        table::{
            self,
            representation::Index,
            resolution::{self, MemberGenericID},
        },
        AdtID, CallableID, ConstantParameterID, LifetimeParameterID,
        TraitMemberID, TypeParameterID,
    },
    type_system::{
        self,
        instantiation::{self, Instantiation},
        model::{self, Model},
        simplify,
        term::{
            constant::Constant,
            lifetime::Lifetime,
            r#type::{self, Constraint, Expected, Primitive, Qualifier, Type},
            GenericArguments, Symbol,
        },
    },
};

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>
            + type_system::observer::Observer<borrow::Model, S>,
    > Bind<&syntax_tree::expression::Postfix> for Binder<'t, S, RO, TO>
{
    #[allow(clippy::too_many_lines)]
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::Postfix,
        config: Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        match syntax_tree.operator() {
            syntax_tree::expression::PostfixOperator::Call(call) => {
                self.handle_call(call, syntax_tree, handler)
            }

            syntax_tree::expression::PostfixOperator::Cast(cast_syn) => {
                // resolve the type
                let mut cast_type = self
                    .table
                    .resolve_type(
                        cast_syn.r#type(),
                        self.current_site,
                        resolution::Config {
                            elided_lifetime_provider: Some(
                                &mut InferenceProvider::default(),
                            ),
                            elided_type_provider: None,
                            elided_constant_provider: None,
                            observer: Some(&mut self.resolution_observer),
                            higher_ranked_lifetimes: None,
                        },
                        handler,
                    )
                    .map_err(|_| {
                        Error::Semantic(SemanticError(syntax_tree.span()))
                    })?;

                cast_type =
                    simplify::simplify(&cast_type, &self.create_environment())
                        .map_err(|overflow_error| TypeSystemOverflow {
                            operation: OverflowOperation::TypeOf,
                            overflow_span: cast_syn.r#type().span(),
                            overflow_error,
                        })?
                        .result;

                // can only cast between numeric types
                if !matches!(
                    cast_type,
                    Type::Primitive(
                        Primitive::Float32
                            | Primitive::Float64
                            | Primitive::Int8
                            | Primitive::Int16
                            | Primitive::Int32
                            | Primitive::Int64
                            | Primitive::Uint8
                            | Primitive::Uint16
                            | Primitive::Uint32
                            | Primitive::Uint64
                            | Primitive::Usize
                            | Primitive::Isize
                    )
                ) {
                    self.create_handler_wrapper(handler).receive(Box::new(
                        InvalidCastType {
                            r#type: self
                                .inference_context
                                .transform_type_into_constraint_model(cast_type)
                                .map_err(|overflow_error| {
                                    TypeSystemOverflow {
                                        operation: OverflowOperation::TypeOf,
                                        overflow_span: cast_syn.r#type().span(),
                                        overflow_error,
                                    }
                                })?,
                            span: cast_syn.r#type().span(),
                        },
                    ));

                    return Err(Error::Semantic(SemanticError(
                        cast_syn.r#type().span(),
                    )));
                }

                // the value to be casted
                let value = self.bind_value_or_error(
                    &**syntax_tree.postfixable(),
                    handler,
                )?;

                // type check the value
                let type_of_value = self.type_of_value(&value)?;
                let _ = self.type_check(
                    &type_of_value,
                    Expected::Constraint(Constraint::Number),
                    syntax_tree.postfixable().span(),
                    true,
                    handler,
                )?;

                Ok(Expression::RValue(Value::Register(
                    self.create_register_assignmnet(
                        Assignment::Cast(Cast { value, r#type: cast_type }),
                        syntax_tree.span(),
                    ),
                )))
            }

            syntax_tree::expression::PostfixOperator::Access(access_syn) => {
                self.handle_access(access_syn, syntax_tree, config, handler)
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum RecieverKind {
    Value,
    Reference(Qualifier),
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>
            + type_system::observer::Observer<borrow::Model, S>,
    > Binder<'t, S, RO, TO>
{
    fn is_method(
        first_parameter_type: &Type<model::Default>,
        implemented_type: &Type<model::Default>,
    ) -> Option<RecieverKind> {
        if first_parameter_type == implemented_type {
            return Some(RecieverKind::Value);
        }

        if let Type::Reference(parameter_type) = first_parameter_type {
            if &*parameter_type.pointee == implemented_type {
                return Some(RecieverKind::Reference(parameter_type.qualifier));
            }

            return None;
        }

        None
    }

    #[allow(clippy::too_many_lines, clippy::type_complexity)]
    fn search_trait_method_candidates(
        &mut self,
        method_ident: &syntax_tree::GenericIdentifier,
        generic_arguments: &GenericArguments<infer::Model>,
        access_type: &Type<infer::Model>,
        arguments: &[(Span, Value<infer::Model>)],
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<
        Vec<(
            CallableID,
            Instantiation<infer::Model>,
            infer::Context,
            RecieverKind,
        )>,
        TypeSystemOverflow,
    > {
        let mut candidates = Vec::new();

        let current_module_id =
            self.table.get_closet_module_id(self.current_site).unwrap();
        let current_module_sym = self.table.get(current_module_id).unwrap();

        let visible_traits = current_module_sym
            .imports()
            .iter()
            .filter_map(|(_, (id, _))| (*id).into_trait().ok())
            .chain(
                current_module_sym
                    .member_ids_by_name()
                    .values()
                    .copied()
                    .filter_map(|x| x.into_trait().ok()),
            )
            .chain(
                self.premise
                    .predicates
                    .iter()
                    .filter_map(|x| x.as_positive_trait().map(|x| x.id)),
            )
            .collect::<HashSet<_>>();

        drop(current_module_sym);
        let starting_inference_context = self.inference_context.clone();

        'candidate: for trait_id in visible_traits {
            // looking for a trait function
            let Some(TraitMemberID::Function(trait_function_id)) = self
                .table
                .get(trait_id)
                .unwrap()
                .member_ids_by_name()
                .get(method_ident.identifier().span.str())
                .copied()
            else {
                continue;
            };

            // invoke the resolution observer
            self.resolution_observer.on_global_id_resolved(
                self.table,
                self.current_site,
                &self.create_handler_wrapper(handler),
                trait_function_id.into(),
                &method_ident.identifier().span,
            );

            let trait_function_sym = self.table.get(trait_function_id).unwrap();

            // argument count mismatched, skip
            if trait_function_sym.parameters().len() != arguments.len() + 1 {
                continue;
            }

            // check accessibility, skip
            if !self
                .table
                .is_accessible_from(
                    self.current_site,
                    self.table
                        .get_accessibility(trait_function_id.into())
                        .unwrap(),
                )
                .unwrap()
            {
                continue;
            }

            let trait_sym = self.table.get(trait_id).unwrap();

            // trait must at least have one type parameter
            let Some(first_ty_parameter_id) = trait_sym
                .generic_declaration
                .parameters
                .type_parameters_as_order()
                .next()
                .map(|x| x.0)
            else {
                continue;
            };

            // check if it can be used as a method
            let Some(kind) = Self::is_method(
                &trait_function_sym
                    .parameter_as_order()
                    .next()
                    .unwrap()
                    .1
                    .r#type,
                &Type::Parameter(TypeParameterID {
                    parent: trait_id.into(),
                    id: first_ty_parameter_id,
                }),
            ) else {
                continue;
            };

            drop(trait_function_sym);
            drop(trait_sym);

            self.inference_context = starting_inference_context.clone();

            let storage = Storage::<Box<dyn error::Error>>::default();
            let function_generic_arguments = self
                .verify_generic_arguments_for_with_inference(
                    generic_arguments.clone(),
                    trait_function_id.into(),
                    method_ident.span(),
                    false,
                    &storage,
                );

            if !storage.as_vec().is_empty() {
                continue;
            }

            let trait_sym = self.table.get(trait_id).unwrap();

            let trait_generic_arguments = GenericArguments {
                lifetimes: trait_sym
                    .generic_declaration
                    .parameters
                    .lifetime_order()
                    .iter()
                    .map(|_| Lifetime::Inference(Erased))
                    .collect(),
                types: trait_sym
                    .generic_declaration
                    .parameters
                    .type_order()
                    .iter()
                    .enumerate()
                    .map(|(order, _)| {
                        if order == 0 {
                            access_type.clone()
                        } else {
                            let inference_variable = InferenceVariable::new();

                            assert!(self.inference_context.register(
                                inference_variable,
                                Constraint::All(false)
                            ));
                            Type::Inference(inference_variable)
                        }
                    })
                    .collect(),
                constants: {
                    trait_sym
                        .generic_declaration
                        .parameters
                        .constant_order()
                        .iter()
                        .map(|_| {
                            let inference_variable = InferenceVariable::new();

                            assert!(self
                                .inference_context
                                .register(inference_variable, NoConstraint));

                            Constant::Inference(inference_variable)
                        })
                        .collect()
                },
            };

            let trait_function_resolution =
                resolution::Resolution::MemberGeneric(
                    resolution::MemberGeneric {
                        id: trait_function_id.into(),
                        parent_generic_arguments: trait_generic_arguments,
                        generic_arguments: function_generic_arguments,
                    },
                );

            self.resolution_observer.on_resolution_resolved(
                self.table,
                self.current_site,
                &self.create_handler_wrapper(handler),
                &trait_function_resolution,
                &method_ident.span(),
            );

            let resolution::Resolution::MemberGeneric(
                resolution::MemberGeneric {
                    id:
                        resolution::MemberGenericID::TraitFunction(
                            trait_function_id,
                        ),
                    parent_generic_arguments: trait_generic_arguments,
                    generic_arguments: function_generic_arguments,
                },
            ) = trait_function_resolution
            else {
                unreachable!()
            };

            let mut instantiation = Instantiation::from_generic_arguments(
                trait_generic_arguments.clone(),
                trait_id.into(),
                &trait_sym.generic_declaration.parameters,
            )
            .unwrap();
            assert!(instantiation
                .append_from_generic_arguments(
                    function_generic_arguments,
                    trait_function_id.into(),
                    &self
                        .table
                        .get(trait_function_id)
                        .unwrap()
                        .generic_declaration
                        .parameters
                )
                .unwrap()
                .is_empty());

            let trait_function_sym = self.table.get(trait_function_id).unwrap();

            for ((argument_span, argument_value), parameter) in
                arguments.iter().zip(
                    trait_function_sym
                        .parameter_as_order()
                        .skip(1)
                        .map(|x| x.1),
                )
            {
                let mut parameter_ty =
                    infer::Model::from_default_type(parameter.r#type.clone());

                instantiation::instantiate(&mut parameter_ty, &instantiation);

                if !self.type_check(
                    &self.type_of_value(argument_value).unwrap(),
                    Expected::Known(parameter_ty),
                    argument_span.clone(),
                    false,
                    &Dummy,
                )? {
                    continue 'candidate;
                }
            }

            // check if trait is implemented
            candidates.push((
                CallableID::TraitFunction(trait_function_id),
                instantiation,
                self.inference_context.clone(),
                kind,
            ));
        }

        if !candidates.is_empty() {
            self.inference_context = starting_inference_context;
        }

        Ok(candidates)
    }

    fn bind_reciever_argument(
        &mut self,
        reciever: Expression,
        reciever_kind: RecieverKind,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Value<infer::Model>, Error> {
        match (reciever_kind, reciever) {
            (RecieverKind::Value, Expression::RValue(value)) => Ok(value),
            (
                RecieverKind::Value,
                Expression::LValue(LValue { address, span, .. }),
            ) => {
                let register_id = self.create_register_assignmnet(
                    Assignment::Load(Load { address }),
                    span,
                );

                Ok(Value::Register(register_id))
            }

            (
                RecieverKind::Reference(qualifieer),
                Expression::RValue(value),
            ) => {
                let span = match &value {
                    Value::Register(id) => self
                        .intermediate_representation
                        .values
                        .registers
                        .get(*id)
                        .unwrap()
                        .span
                        .clone(),
                    Value::Literal(literal) => literal.span().clone(),
                };
                let type_of_value = self.type_of_value(&value)?;
                let alloca_id = self.create_alloca(type_of_value, span.clone());

                // initialize
                let _ = self.current_block_mut().add_instruction(
                    Instruction::Store(Store {
                        address: Address::Memory(Memory::Alloca(alloca_id)),
                        value,
                    }),
                );

                Ok(Value::Register(self.create_register_assignmnet(
                    Assignment::ReferenceOf(ReferenceOf {
                        address: address::Address::Memory(Memory::Alloca(
                            alloca_id,
                        )),
                        qualifier: qualifieer,
                        lifetime: Lifetime::Inference(Erased),
                    }),
                    span,
                )))
            }

            (
                RecieverKind::Reference(qualifier),
                Expression::LValue(address),
            ) => {
                if qualifier > address.qualifier {
                    self.create_handler_wrapper(handler).receive(Box::new(
                        MismatchedQualifierForReferenceOf {
                            reference_of_span: address.span.clone(),
                            found_qualifier: address.qualifier,
                            expected_qualifier: qualifier,
                            is_behind_reference: address
                                .address
                                .is_behind_reference(),
                        },
                    ));
                }

                let reference_of = self.create_register_assignmnet(
                    Assignment::ReferenceOf(ReferenceOf {
                        address: address.address,
                        qualifier,
                        lifetime: Lifetime::Inference(Erased),
                    }),
                    address.span,
                );

                Ok(Value::Register(reference_of))
            }
        }
    }

    fn on_binding_adt_method_error<
        I: Iterator<Item = Box<dyn error::Error>>,
    >(
        &mut self,
        method_call_span: Span,
        reciever: Expression,
        mut arguments: Vec<(Span, Value<infer::Model>)>,
        trait_method_candidates: Vec<(
            CallableID,
            Instantiation<infer::Model>,
            infer::Context,
            RecieverKind,
        )>,
        error: impl FnOnce(&mut Self) -> I,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Value<infer::Model>, Error> {
        if trait_method_candidates.len() == 1 {
            let (callable_id, instantiation, inference_context, kind) =
                trait_method_candidates.into_iter().next().unwrap();

            let span = match &reciever {
                Expression::RValue(Value::Register(register_id)) => self
                    .intermediate_representation
                    .values
                    .registers
                    .get(*register_id)
                    .unwrap()
                    .span
                    .clone(),
                Expression::RValue(Value::Literal(literal)) => {
                    literal.span().clone()
                }
                Expression::LValue(lvalue) => lvalue.span.clone(),
            };
            self.inference_context = inference_context;

            let reciever_argument =
                self.bind_reciever_argument(reciever, kind, handler)?;
            arguments.insert(0, (span, reciever_argument));

            return Ok(Value::Register(self.bind_function_call(
                &arguments,
                callable_id,
                instantiation,
                method_call_span,
                handler,
            )?));
        } else if trait_method_candidates.is_empty() {
            let wrapper = self.create_handler_wrapper(handler);

            for error in error(self) {
                wrapper.receive(error);
            }

            return Err(Error::Semantic(SemanticError(method_call_span)));
        }

        self.create_handler_wrapper(handler).receive(Box::new(
            AmbiguousMethodCall {
                method_call_span: method_call_span.clone(),
                callable_candidates: trait_method_candidates
                    .into_iter()
                    .map(|x| x.0)
                    .collect(),
            },
        ));

        Err(Error::Semantic(SemanticError(method_call_span)))
    }

    #[allow(clippy::too_many_lines)]
    fn bind_method_call(
        &mut self,
        postfix: &syntax_tree::expression::Postfix,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Value<infer::Model>, Error> {
        let struct_expression_wth_access =
            postfix.postfixable().as_postfix().unwrap();
        let call = postfix.operator().as_call().unwrap();
        let struct_expression = struct_expression_wth_access.postfixable();
        let access =
            struct_expression_wth_access.operator().as_access().unwrap();
        let method_ident = access.kind().as_generic_identifier().unwrap();
        let is_arrow = access.operator().is_arrow();

        let (reciever_expression, access_type) = if is_arrow
        // expect the lvalue address
        {
            let lvalue = self
                .bind_dereference(
                    &**struct_expression,
                    Config { target: Target::LValue },
                    struct_expression.span(),
                    handler,
                )?
                .into_l_value()
                .unwrap();

            let ty = self.type_of_address(&lvalue.address)?;

            (Expression::LValue(lvalue), ty)
        }
        // expect the rvalue reference value
        else {
            let value = self.bind(
                &**struct_expression,
                Config { target: Target::LValue },
                handler,
            )?;

            let ty = match &value {
                Expression::RValue(val) => self.type_of_value(val).unwrap(),
                Expression::LValue(val) => {
                    self.type_of_address(&val.address).unwrap()
                }
            };

            (value, ty)
        };

        let mut arguments = call
            .arguments()
            .connected_list()
            .as_ref()
            .into_iter()
            .flat_map(ConnectedList::elements)
            .map(|arg| {
                self.bind_value_or_error(&**arg, handler)
                    .map(|x| (arg.span(), x))
            })
            .collect::<Result<Vec<_>, _>>()?;

        let generic_arguments = method_ident
            .generic_arguments()
            .as_ref()
            .map_or_else(GenericArguments::default, |x| {
                self.resolve_generic_arguments_with_inference(x, true, handler)
                    .expect("should have no invalid referring site")
            });

        let trait_method_candidates = self.search_trait_method_candidates(
            method_ident,
            &generic_arguments,
            &access_type,
            &arguments,
            handler,
        )?;

        let Type::Symbol(Symbol {
            id: r#type::SymbolID::Adt(adt_id),
            generic_arguments: adt_generic_arguments,
        }) = access_type
        else {
            return self.on_binding_adt_method_error(
                postfix.span(),
                reciever_expression,
                arguments,
                trait_method_candidates,
                |_| {
                    std::iter::once(Box::new(MethodNotFound {
                        method_call_span: postfix.span(),
                    })
                        as Box<dyn error::Error>)
                },
                handler,
            );
        };

        // find the method id
        let Some(adt_implementation_function_id) = self
            .table
            .get_adt(adt_id)
            .unwrap()
            .implementations()
            .iter()
            .find_map(|x| {
                self.table
                    .get(*x)
                    .unwrap()
                    .member_ids_by_name()
                    .get(method_ident.identifier().span.str())
                    .copied()
            })
        else {
            return self.on_binding_adt_method_error(
                postfix.span(),
                reciever_expression,
                arguments,
                trait_method_candidates,
                |_| {
                    std::iter::once(Box::new(SymbolNotFound {
                        searched_global_id: Some(adt_id.into()),
                        resolution_span: method_ident.span(),
                    })
                        as Box<dyn error::Error>)
                },
                handler,
            );
        };

        // this is considered as an global id resolution, notify the observer
        self.resolution_observer.on_global_id_resolved(
            self.table,
            self.current_site,
            handler,
            adt_implementation_function_id.into(),
            &method_ident.identifier().span,
        );

        let storage = Storage::<Box<dyn error::Error>>::default();
        let function_generic_arguments = self
            .verify_generic_arguments_for_with_inference(
                generic_arguments,
                adt_implementation_function_id.into(),
                method_ident.span(),
                true,
                handler,
            );

        let storage = storage.into_vec();
        if !storage.is_empty() {
            return self.on_binding_adt_method_error(
                postfix.span(),
                reciever_expression,
                arguments,
                trait_method_candidates,
                |_| storage.into_iter(),
                handler,
            );
        }

        let adt_implementation_id =
            self.table.get(adt_implementation_function_id).unwrap().parent_id();
        let adt_implementation = self.table.get(adt_implementation_id).unwrap();

        let mut type_inferences = Vec::new();
        let mut constant_inferences = Vec::new();

        let mut instantiation = Instantiation::<infer::Model> {
            lifetimes: adt_implementation
                .generic_declaration
                .parameters
                .lifetime_parameters_as_order()
                .map(|(id, _)| {
                    (
                        Lifetime::Parameter(LifetimeParameterID {
                            parent: adt_implementation_id.into(),
                            id,
                        }),
                        Lifetime::Inference(Erased),
                    )
                })
                .collect(),
            types: adt_implementation
                .generic_declaration
                .parameters
                .type_parameters_as_order()
                .map(|(id, _)| {
                    let inference_variable = InferenceVariable::new();
                    type_inferences.push(inference_variable);

                    (
                        Type::Parameter(TypeParameterID {
                            parent: adt_implementation_id.into(),
                            id,
                        }),
                        Type::Inference(inference_variable),
                    )
                })
                .collect(),
            constants: adt_implementation
                .generic_declaration
                .parameters
                .constant_parameters_as_order()
                .map(|(id, _)| {
                    let inference_variable = InferenceVariable::new();
                    constant_inferences.push(inference_variable);

                    (
                        Constant::Parameter(ConstantParameterID {
                            parent: adt_implementation_id.into(),
                            id,
                        }),
                        Constant::Inference(inference_variable),
                    )
                })
                .collect(),
        };

        for x in type_inferences {
            assert!(self.inference_context.register(x, Constraint::All(false)));
        }
        for x in constant_inferences {
            assert!(self.inference_context.register(x, NoConstraint));
        }

        let _ = self.type_check(
            &Type::Symbol(Symbol {
                id: adt_implementation.implemented_id().into(),
                generic_arguments: adt_generic_arguments.clone(),
            }),
            Expected::Known(Type::Symbol(Symbol {
                id: adt_implementation.implemented_id().into(),
                generic_arguments: {
                    let mut adt_generic_arguments =
                        GenericArguments::from_default_model(
                            adt_implementation.arguments.clone(),
                        );

                    adt_generic_arguments.instantiate(&instantiation);

                    adt_generic_arguments
                },
            })),
            struct_expression_wth_access.postfixable().span(),
            true,
            handler,
        )?;

        assert!(instantiation
            .append_from_generic_arguments(
                function_generic_arguments.clone(),
                adt_implementation_function_id.into(),
                &self
                    .table
                    .get(adt_implementation_function_id)
                    .unwrap()
                    .generic_declaration
                    .parameters,
            )
            .unwrap()
            .is_empty());

        drop(adt_implementation);

        self.resolution_observer.on_resolution_resolved(
            self.table,
            self.current_site,
            handler,
            &resolution::Resolution::MemberGeneric(resolution::MemberGeneric {
                id: MemberGenericID::AdtImplementationFunction(
                    adt_implementation_function_id,
                ),
                parent_generic_arguments: adt_generic_arguments,
                generic_arguments: function_generic_arguments,
            }),
            &method_ident.span(),
        );

        let adt_implementation_function =
            self.table.get(adt_implementation_function_id).unwrap();

        if adt_implementation_function.parameters().is_empty() {
            return self.on_binding_adt_method_error(
                postfix.span(),
                reciever_expression,
                arguments,
                trait_method_candidates,
                |_| {
                    std::iter::once(Box::new(
                        AdtImplementationFunctionCannotBeUsedAsMethod {
                            adt_implementation_function_id,
                            span: method_ident.span(),
                        },
                    )
                        as Box<dyn error::Error>)
                },
                handler,
            );
        }

        let implemented_type = Type::Symbol(Symbol {
            id: r#type::SymbolID::Adt(adt_id),
            generic_arguments: self
                .table
                .get(adt_implementation_id)
                .unwrap()
                .arguments
                .clone(),
        });
        let first_parameter_type = &adt_implementation_function
            .parameters()
            .get(
                adt_implementation_function
                    .parameter_as_order()
                    .next()
                    .unwrap()
                    .0,
            )
            .unwrap()
            .r#type;

        let Some(reciever_kind) =
            Self::is_method(first_parameter_type, &implemented_type)
        else {
            return self.on_binding_adt_method_error(
                postfix.span(),
                reciever_expression,
                arguments,
                trait_method_candidates,
                |_| {
                    std::iter::once(Box::new(
                        AdtImplementationFunctionCannotBeUsedAsMethod {
                            adt_implementation_function_id,
                            span: method_ident.span(),
                        },
                    )
                        as Box<dyn error::Error>)
                },
                handler,
            );
        };

        if adt_implementation_function.parameters().len() != arguments.len() + 1
        {
            let found_argument_count = arguments.len();
            return self.on_binding_adt_method_error(
                postfix.span(),
                reciever_expression,
                arguments,
                trait_method_candidates,
                |_| {
                    std::iter::once(Box::new(MismatchedArgumentCount {
                        called_id: adt_implementation_function_id.into(),
                        expected_count: adt_implementation_function
                            .parameters()
                            .len()
                            - 1,
                        found_count: found_argument_count,
                        span: call.span(),
                    })
                        as Box<dyn error::Error>)
                },
                handler,
            );
        }

        if !self
            .table
            .is_accessible_from(
                self.current_site,
                self.table
                    .get_accessibility(adt_implementation_function_id.into())
                    .unwrap(),
            )
            .unwrap()
        {
            self.create_handler_wrapper(handler).receive(Box::new(
                SymbolIsNotAccessible {
                    referring_site: self.current_site,
                    referred: adt_implementation_function_id.into(),
                    referred_span: method_ident.span(),
                },
            ));
        }

        let reciever_argument = self.bind_reciever_argument(
            reciever_expression,
            reciever_kind,
            handler,
        )?;

        arguments.insert(0, (struct_expression.span(), reciever_argument));
        drop(adt_implementation_function);

        Ok(Value::Register(self.bind_function_call(
            &arguments,
            CallableID::AdtImplementationFunction(
                adt_implementation_function_id,
            ),
            instantiation,
            postfix.span(),
            handler,
        )?))
    }

    #[allow(clippy::too_many_lines)]
    fn handle_access(
        &mut self,
        access: &syntax_tree::expression::Access,
        syntax_tree: &syntax_tree::expression::Postfix,
        config: Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        let (lvalue, ty) = match access.operator() {
            // expect the lvalue address
            syntax_tree::expression::AccessOperator::Dot(_) => {
                let lvalue = self.bind_as_lvalue(
                    &**syntax_tree.postfixable(),
                    true,
                    handler,
                )?;

                let ty = self.type_of_address(&lvalue.address)?;

                (lvalue, ty)
            }

            // expect the rvalue reference value
            syntax_tree::expression::AccessOperator::Arrow(_, _) => {
                let lvalue = self
                    .bind_dereference(
                        &**syntax_tree.postfixable(),
                        Config { target: Target::LValue },
                        syntax_tree.span(),
                        handler,
                    )?
                    .into_l_value()
                    .unwrap();

                let ty = self.type_of_address(&lvalue.address)?;

                (lvalue, ty)
            }
        };

        let address = match access.kind() {
            // struct field access
            syntax_tree::expression::AccessKind::GenericIdentifier(
                generic_ident,
            ) => {
                let struct_id = match ty {
                    Type::Symbol(Symbol {
                        id: r#type::SymbolID::Adt(AdtID::Struct(struct_id)),
                        ..
                    }) => struct_id,

                    found_type => {
                        self.create_handler_wrapper(handler).receive(Box::new(
                            ExpectStructType {
                                span: syntax_tree.postfixable().span(),
                                r#type: self
                                    .inference_context
                                    .transform_type_into_constraint_model(
                                        found_type,
                                    )
                                    .map_err(|overflow_error| {
                                        TypeSystemOverflow {
                                            operation:
                                                OverflowOperation::TypeOf,
                                            overflow_span: syntax_tree
                                                .postfixable()
                                                .span(),
                                            overflow_error,
                                        }
                                    })?,
                            },
                        ));

                        return Err(Error::Semantic(SemanticError(
                            syntax_tree.postfixable().span(),
                        )));
                    }
                };

                // soft error, keep going
                if generic_ident.generic_arguments().is_some() {
                    self.create_handler_wrapper(handler).receive(Box::new(
                        UnexpectedGenericArgumentsInField {
                            field_access_span: syntax_tree.span(),
                        },
                    ));
                }

                // find field id
                let Some(field_id) = self
                    .table
                    .get(struct_id)
                    .unwrap()
                    .fields()
                    .get_id(generic_ident.identifier().span.str())
                else {
                    self.create_handler_wrapper(handler).receive(Box::new(
                        FieldNotFound {
                            struct_id,
                            identifier_span: generic_ident
                                .identifier()
                                .span
                                .clone(),
                        },
                    ));

                    return Err(Error::Semantic(SemanticError(
                        generic_ident.identifier().span.clone(),
                    )));
                };

                let field_accessibility = self
                    .table
                    .get(struct_id)
                    .unwrap()
                    .fields()
                    .get(field_id)
                    .unwrap()
                    .accessibility;

                // field is not accessible, soft error, keep going
                if !self
                    .table
                    .is_accessible_from(self.current_site, field_accessibility)
                    .unwrap()
                {
                    self.create_handler_wrapper(handler).receive(Box::new(
                        FieldIsNotAccessible {
                            field_id,
                            struct_id,
                            referring_site: self.current_site,
                            referring_identifier_span: generic_ident
                                .identifier()
                                .span
                                .clone(),
                        },
                    ));
                }

                Address::Field(address::Field {
                    struct_address: Box::new(lvalue.address),
                    id: field_id,
                })
            }

            syntax_tree::expression::AccessKind::Tuple(syn) => {
                let tuple_ty = match ty {
                    Type::Tuple(tuple_ty) => tuple_ty,
                    found_type => {
                        self.create_handler_wrapper(handler).receive(Box::new(
                            TupleExpected {
                                span: syntax_tree.postfixable().span(),
                                r#type: self
                                    .inference_context
                                    .transform_type_into_constraint_model(
                                        found_type,
                                    )
                                    .map_err(|overflow_error| {
                                        TypeSystemOverflow {
                                            operation:
                                                OverflowOperation::TypeOf,
                                            overflow_span: syntax_tree
                                                .postfixable()
                                                .span(),
                                            overflow_error,
                                        }
                                    })?,
                            },
                        ));

                        return Err(Error::Semantic(SemanticError(
                            syntax_tree.postfixable().span(),
                        )));
                    }
                };

                // sanity check
                if tuple_ty.elements.iter().filter(|x| x.is_unpacked).count()
                    > 1
                {
                    return Err(Error::Semantic(SemanticError(
                        syntax_tree.span(),
                    )));
                }

                // used to chec if the index is past the unpacked index
                let unpacked_index =
                    tuple_ty.elements.iter().position(|x| x.is_unpacked);

                let index = match syn.index().span.str().parse::<usize>() {
                    Ok(number) => number,
                    Err(err) => match err.kind() {
                        std::num::IntErrorKind::NegOverflow
                        | std::num::IntErrorKind::PosOverflow => {
                            self.create_handler_wrapper(handler).receive(
                                Box::new(TooLargeTupleIndex {
                                    access_span: access.span(),
                                }),
                            );

                            return Err(Error::Semantic(SemanticError(
                                syn.index().span.clone(),
                            )));
                        }

                        _ => {
                            unreachable!()
                        }
                    },
                };

                if index >= tuple_ty.elements.len() {
                    self.create_handler_wrapper(handler).receive(Box::new(
                        TupleIndexOutOfBOunds {
                            access_span: access.span(),
                            tuple_type: self
                                .inference_context
                                .transform_type_into_constraint_model(
                                    Type::Tuple(tuple_ty),
                                )
                                .map_err(|overflow_error| TypeSystemOverflow {
                                    operation: OverflowOperation::TypeOf,
                                    overflow_span: syntax_tree
                                        .postfixable()
                                        .span(),
                                    overflow_error,
                                })?
                                .into_tuple()
                                .unwrap(),
                        },
                    ));

                    return Err(Error::Semantic(SemanticError(
                        syn.index().span.clone(),
                    )));
                }

                if let Some(unpacked_index) = unpacked_index {
                    // can't access past the unpacked index
                    let pass_unpacked = if syn.minus().is_some() {
                        index >= (tuple_ty.elements.len() - unpacked_index - 1)
                    } else {
                        index >= unpacked_index
                    };

                    // report error
                    if pass_unpacked {
                        self.create_handler_wrapper(handler).receive(Box::new(
                            CannotIndexPastUnpackedTuple {
                                index_span: syn.span(),
                                tuple_type: self
                                    .inference_context
                                    .transform_type_into_constraint_model(
                                        Type::Tuple(tuple_ty),
                                    )
                                    .map_err(|overflow_error| {
                                        TypeSystemOverflow {
                                            operation:
                                                OverflowOperation::TypeOf,
                                            overflow_span: syntax_tree
                                                .postfixable()
                                                .span(),
                                            overflow_error,
                                        }
                                    })?
                                    .into_tuple()
                                    .unwrap(),
                            },
                        ));

                        return Err(Error::Semantic(SemanticError(
                            syn.index().span.clone(),
                        )));
                    }
                }

                Address::Tuple(address::Tuple {
                    tuple_address: Box::new(lvalue.address),
                    offset: if syn.minus().is_some() {
                        address::Offset::FromEnd(index)
                    } else {
                        address::Offset::FromStart(index)
                    },
                })
            }

            syntax_tree::expression::AccessKind::Index(index) => {
                let value = self.bind_value_or_error(
                    &**index.expression().tree(),
                    handler,
                )?;

                if !matches!(ty, Type::Array(_)) {
                    self.create_handler_wrapper(handler).receive(Box::new(
                        ExpectArray {
                            span: syntax_tree.postfixable().span(),
                            r#type: self
                                .inference_context
                                .transform_type_into_constraint_model(ty)
                                .map_err(|overflow_error| {
                                    TypeSystemOverflow {
                                        operation: OverflowOperation::TypeOf,
                                        overflow_span: syntax_tree
                                            .postfixable()
                                            .span(),
                                        overflow_error,
                                    }
                                })?,
                        },
                    ));

                    return Err(Error::Semantic(SemanticError(
                        syntax_tree.postfixable().span(),
                    )));
                }

                // expected a `usize` type
                let index_ty = self.type_of_value(&value)?;
                let _ = self.type_check(
                    &index_ty,
                    Expected::Known(Type::Primitive(r#type::Primitive::Usize)),
                    index.expression().span(),
                    true,
                    handler,
                )?;

                Address::Index(address::Index {
                    array_address: Box::new(lvalue.address),
                    indexing_value: value,
                })
            }
        };

        match config.target {
            Target::RValue => {
                // will be optimized to move later
                let register_id = self.create_register_assignmnet(
                    Assignment::Load(Load { address }),
                    syntax_tree.span(),
                );

                Ok(Expression::RValue(Value::Register(register_id)))
            }

            // qualifier should've been checked earlier
            Target::Statement | Target::LValue { .. } => {
                Ok(Expression::LValue(LValue {
                    address,
                    span: syntax_tree.span(),
                    qualifier: lvalue.qualifier,
                }))
            }
        }
    }

    fn handle_call(
        &mut self,
        call: &syntax_tree::expression::Call,
        syntax_tree: &syntax_tree::expression::Postfix,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        let arguments = call
            .arguments()
            .connected_list()
            .iter()
            .flat_map(ConnectedList::elements)
            .map(|arg| {
                self.bind_value_or_error(&**arg, handler)
                    .map(|x| (arg.span(), x))
            })
            .collect::<Result<Vec<_>, _>>()?;

        match &**syntax_tree.postfixable() {
            // possibly method call
            syntax_tree::expression::Postfixable::Postfix(postfix)
                // is an access to a field
                if matches!(
                    postfix.operator(),
                    PostfixOperator::Access(
                        access
                    )
                    if matches!(access.kind(),
                        AccessKind::GenericIdentifier(_)
                    )
                ) =>
            {
                self.bind_method_call(
                    syntax_tree,
                    handler
                ).map(Expression::RValue)
            }

            // function call
            syntax_tree::expression::Postfixable::Unit(
                syntax_tree::expression::Unit::QualifiedIdentifier(
                    qualified_identifier,
                ),
            ) => {
                let resolution = self
                    .resolve_with_inference(
                        qualified_identifier,
                        handler,
                    )
                    .ok_or(Error::Semantic(SemanticError(
                        syntax_tree.span(),
                    )))?;

                self.bind_postfix_call_with_resolution(
                    arguments,
                    qualified_identifier.span(),
                    resolution,
                    syntax_tree.span(),
                    handler,
                )
            }

            // currently only supports functions and enum variants
            postfixable => {
                let _ = self.bind_value_or_error(postfixable, handler);

                self.create_handler_wrapper(handler).receive(Box::new(
                    ExpressionIsNotCallable {
                        span: postfixable.span(),
                    },
                ));

                Err(Error::Semantic(SemanticError(postfixable.span())))
            }
        }
    }

    fn bind_variant_call(
        &mut self,
        arguments: Vec<(Span, Value<infer::Model>)>,
        variant: resolution::Variant<infer::Model>,
        syntax_tree_span: Span,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<ID<Register<infer::Model>>, InternalError> {
        let variant_sym = self.table.get(variant.variant).unwrap();
        let enum_sym = self.table.get(variant_sym.parent_enum_id()).unwrap();

        let instantiation = Instantiation::from_generic_arguments(
            variant.generic_arguments.clone(),
            variant_sym.parent_enum_id().into(),
            &enum_sym.generic_declaration.parameters,
        )
        .unwrap();

        if let Some(mut variant_type) = variant_sym
            .associated_type
            .clone()
            .map(infer::Model::from_default_type)
        {
            instantiation::instantiate(&mut variant_type, &instantiation);

            let associated_value = match arguments.len() {
                0 => {
                    self.create_handler_wrapper(handler).receive(Box::new(
                        MismatchedArgumentCount {
                            called_id: variant.variant.into(),
                            expected_count: 1,
                            found_count: 0,
                            span: syntax_tree_span.clone(),
                        },
                    ));

                    Value::Literal(Literal::Error(literal::Error {
                        r#type: variant_type,
                        span: (syntax_tree_span.clone()),
                    }))
                }
                len => {
                    if len != 1 {
                        self.create_handler_wrapper(handler).receive(Box::new(
                            MismatchedArgumentCount {
                                called_id: variant.variant.into(),
                                expected_count: 1,
                                found_count: len,
                                span: syntax_tree_span.clone(),
                            },
                        ));
                    }

                    let argument = arguments.into_iter().next().unwrap();

                    let _ = self.type_check(
                        &self.type_of_value(&argument.1)?,
                        Expected::Known(variant_type),
                        argument.0,
                        true,
                        &self.create_handler_wrapper(handler),
                    )?;

                    argument.1
                }
            };

            Ok(self.create_register_assignmnet(
                Assignment::Variant(Variant {
                    variant_id: variant.variant,
                    associated_value: Some(associated_value),
                    generic_arguments: variant.generic_arguments,
                }),
                syntax_tree_span,
            ))
        } else {
            if !arguments.is_empty() {
                self.create_handler_wrapper(handler).receive(Box::new(
                    MismatchedArgumentCount {
                        called_id: variant.variant.into(),
                        expected_count: 0,
                        found_count: arguments.len(),
                        span: syntax_tree_span.clone(),
                    },
                ));
            }

            Ok(self.create_register_assignmnet(
                Assignment::Variant(Variant {
                    variant_id: variant.variant,
                    associated_value: None,
                    generic_arguments: variant.generic_arguments,
                }),
                syntax_tree_span,
            ))
        }
    }

    fn bind_function_call(
        &mut self,
        arguments: &[(Span, Value<infer::Model>)],
        callable_id: CallableID,
        instantiation: Instantiation<infer::Model>,
        syntax_tree_span: Span,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<ID<Register<infer::Model>>, TypeSystemOverflow> {
        let callable = self.table.get_callable(callable_id).unwrap();
        let mut acutal_arguments = Vec::new();

        // mismatched arguments count
        if arguments.len() != callable.parameter_order().len() {
            self.create_handler_wrapper(handler).receive(Box::new(
                MismatchedArgumentCount {
                    called_id: callable_id.into(),
                    expected_count: callable.parameter_order().len(),
                    found_count: arguments.len(),
                    span: syntax_tree_span.clone(),
                },
            ));
        }

        // type check the arguments
        for ((argument_span, argument_value), parameter) in
            arguments.iter().zip(
                callable
                    .parameter_order()
                    .iter()
                    .map(|x| callable.parameters().get(*x).unwrap()),
            )
        {
            let mut parameter_ty =
                infer::Model::from_default_type(parameter.r#type.clone());

            instantiation::instantiate(&mut parameter_ty, &instantiation);

            let _ = self.type_check(
                &self.type_of_value(argument_value).unwrap(),
                Expected::Known(parameter_ty),
                argument_span.clone(),
                true,
                handler,
            )?;

            acutal_arguments.push(argument_value.clone());
        }

        // fill the unsupplied arguments with error values
        let rest_parameter_ids =
            callable.parameter_order().iter().skip(arguments.len());

        for parameter in
            rest_parameter_ids.map(|x| callable.parameters().get(*x).unwrap())
        {
            let mut parameter_ty =
                infer::Model::from_default_type(parameter.r#type.clone());

            instantiation::instantiate(&mut parameter_ty, &instantiation);

            let error_value = Value::Literal(Literal::Error(literal::Error {
                r#type: parameter_ty,
                span: (syntax_tree_span.clone()),
            }));

            acutal_arguments.push(error_value);
        }

        let mut return_type =
            infer::Model::from_default_type(callable.return_type().clone());

        instantiation::instantiate(&mut return_type, &instantiation);

        Ok(self.create_register_assignmnet(
            Assignment::FunctionCall(FunctionCall {
                callable_id,
                arguments: acutal_arguments,
                instantiation,
            }),
            syntax_tree_span,
        ))
    }

    #[allow(clippy::too_many_lines)]
    fn bind_postfix_call_with_resolution(
        &mut self,
        arguments: Vec<(Span, Value<infer::Model>)>,
        resolution_span: Span,
        resolution: resolution::Resolution<infer::Model>,
        syntax_tree_span: Span,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        // can be enum variant and functin call
        match resolution {
            // bind as variant
            resolution::Resolution::Variant(variant) => {
                let value = Value::Register(self.bind_variant_call(
                    arguments,
                    variant,
                    syntax_tree_span,
                    handler,
                )?);

                Ok(Expression::RValue(value))
            }

            resolution::Resolution::Generic(resolution::Generic {
                id,
                generic_arguments,
            }) if id.is_trait_implementation_function() || id.is_function() => {
                let (callable_id, instantiation) = match id {
                    resolution::GenericID::Function(id) => {
                        let inst = Instantiation::from_generic_arguments(
                            generic_arguments,
                            id.into(),
                            &self
                                .table
                                .get(id)
                                .unwrap()
                                .generic_declaration
                                .parameters,
                        )
                        .unwrap();

                        (CallableID::Function(id), inst)
                    }
                    resolution::GenericID::TraitImplementationFunction(id) => {
                        let trait_implementation_sym_id =
                            self.table.get(id).unwrap().parent_id();
                        let trait_implementation_sym = self
                            .table
                            .get(trait_implementation_sym_id)
                            .unwrap();

                        let mut inst = Instantiation {
                            lifetimes: trait_implementation_sym
                                .generic_declaration
                                .parameters
                                .lifetime_order()
                                .iter()
                                .copied()
                                .map(|x| {
                                    let lifetime_parameter =
                                        Lifetime::Parameter(
                                            LifetimeParameterID {
                                                parent:
                                                    trait_implementation_sym_id
                                                        .into(),
                                                id: x,
                                            },
                                        );

                                    (
                                        lifetime_parameter.clone(),
                                        lifetime_parameter,
                                    )
                                })
                                .collect(),

                            types: trait_implementation_sym
                                .generic_declaration
                                .parameters
                                .type_order()
                                .iter()
                                .copied()
                                .map(|x| {
                                    let type_parameter =
                                        Type::Parameter(TypeParameterID {
                                            parent: trait_implementation_sym_id
                                                .into(),
                                            id: x,
                                        });

                                    (type_parameter.clone(), type_parameter)
                                })
                                .collect(),

                            constants: trait_implementation_sym
                                .generic_declaration
                                .parameters
                                .constant_order()
                                .iter()
                                .copied()
                                .map(|x| {
                                    let constant_parameter =
                                        Constant::Parameter(
                                            ConstantParameterID {
                                                parent:
                                                    trait_implementation_sym_id
                                                        .into(),
                                                id: x,
                                            },
                                        );

                                    (
                                        constant_parameter.clone(),
                                        constant_parameter,
                                    )
                                })
                                .collect(),
                        };

                        assert!(inst
                            .append_from_generic_arguments(
                                generic_arguments,
                                id.into(),
                                &self
                                    .table
                                    .get(id)
                                    .unwrap()
                                    .generic_declaration
                                    .parameters,
                            )
                            .unwrap()
                            .is_empty());

                        (CallableID::TraitImplementationFunction(id), inst)
                    }

                    _ => unreachable!(),
                };

                let value = Value::Register(self.bind_function_call(
                    &arguments,
                    callable_id,
                    instantiation,
                    syntax_tree_span,
                    handler,
                )?);

                Ok(Expression::RValue(value))
            }

            resolution::Resolution::MemberGeneric(
                resolution::MemberGeneric {
                    id,
                    parent_generic_arguments,
                    generic_arguments,
                },
            ) if id.is_trait_function()
                || id.is_adt_implementation_function() =>
            {
                let (callable_id, inst) = match id {
                    MemberGenericID::TraitFunction(id) => {
                        let trait_sym_id =
                            self.table.get(id).unwrap().parent_id();
                        let trait_sym = self.table.get(trait_sym_id).unwrap();

                        let mut inst = Instantiation::from_generic_arguments(
                            parent_generic_arguments,
                            trait_sym_id.into(),
                            &trait_sym.generic_declaration.parameters,
                        )
                        .unwrap();

                        assert!(inst
                            .append_from_generic_arguments(
                                generic_arguments,
                                id.into(),
                                &self
                                    .table
                                    .get(id)
                                    .unwrap()
                                    .generic_declaration
                                    .parameters,
                            )
                            .unwrap()
                            .is_empty());

                        (CallableID::TraitFunction(id), inst)
                    }

                    MemberGenericID::AdtImplementationFunction(id) => {
                        let adt_implementation_id =
                            self.table.get(id).unwrap().parent_id();
                        let adt_implementation =
                            self.table.get(adt_implementation_id).unwrap();

                        let mut type_inferences = Vec::new();
                        let mut constant_inferences = Vec::new();

                        let mut instantiation = Instantiation::<infer::Model> {
                            lifetimes: adt_implementation
                                .generic_declaration
                                .parameters
                                .lifetime_parameters_as_order()
                                .map(|(id, _)| {
                                    (
                                        Lifetime::Parameter(
                                            LifetimeParameterID {
                                                parent: adt_implementation_id
                                                    .into(),
                                                id,
                                            },
                                        ),
                                        Lifetime::Inference(Erased),
                                    )
                                })
                                .collect(),
                            types: adt_implementation
                                .generic_declaration
                                .parameters
                                .type_parameters_as_order()
                                .map(|(id, _)| {
                                    let inference_variable =
                                        InferenceVariable::new();
                                    type_inferences.push(inference_variable);

                                    (
                                        Type::Parameter(TypeParameterID {
                                            parent: adt_implementation_id
                                                .into(),
                                            id,
                                        }),
                                        Type::Inference(inference_variable),
                                    )
                                })
                                .collect(),
                            constants: adt_implementation
                                .generic_declaration
                                .parameters
                                .constant_parameters_as_order()
                                .map(|(id, _)| {
                                    let inference_variable =
                                        InferenceVariable::new();
                                    constant_inferences
                                        .push(inference_variable);

                                    (
                                        Constant::Parameter(
                                            ConstantParameterID {
                                                parent: adt_implementation_id
                                                    .into(),
                                                id,
                                            },
                                        ),
                                        Constant::Inference(inference_variable),
                                    )
                                })
                                .collect(),
                        };

                        for x in type_inferences {
                            assert!(self
                                .inference_context
                                .register(x, Constraint::All(false)));
                        }
                        for x in constant_inferences {
                            assert!(self
                                .inference_context
                                .register(x, NoConstraint));
                        }

                        let _ = self.type_check(
                            &Type::Symbol(Symbol {
                                id: adt_implementation.implemented_id().into(),
                                generic_arguments: parent_generic_arguments,
                            }),
                            Expected::Known(Type::Symbol(Symbol {
                                id: adt_implementation.implemented_id().into(),
                                generic_arguments: {
                                    let mut adt_generic_arguments =
                                        GenericArguments::from_default_model(
                                            adt_implementation
                                                .arguments
                                                .clone(),
                                        );

                                    adt_generic_arguments
                                        .instantiate(&instantiation);

                                    adt_generic_arguments
                                },
                            })),
                            resolution_span,
                            true,
                            handler,
                        )?;

                        assert!(instantiation
                            .append_from_generic_arguments(
                                generic_arguments,
                                id.into(),
                                &self
                                    .table
                                    .get(id)
                                    .unwrap()
                                    .generic_declaration
                                    .parameters,
                            )
                            .unwrap()
                            .is_empty());

                        (
                            CallableID::AdtImplementationFunction(id),
                            instantiation,
                        )
                    }

                    _ => unreachable!(),
                };

                let value = Value::Register(self.bind_function_call(
                    &arguments,
                    callable_id,
                    inst,
                    syntax_tree_span,
                    handler,
                )?);

                Ok(Expression::RValue(value))
            }

            resolution => {
                // report symbol is not callable
                self.create_handler_wrapper(handler).receive(Box::new(
                    SymbolIsNotCallable {
                        called_id: resolution.global_id(),
                        span: syntax_tree_span.clone(),
                    },
                ));

                Err(Error::Semantic(SemanticError(syntax_tree_span)))
            }
        }
    }
}
