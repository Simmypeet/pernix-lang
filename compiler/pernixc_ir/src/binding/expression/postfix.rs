use std::collections::HashSet;

use pernixc_arena::ID;
use pernixc_component::{
    function_signature::FunctionSignature, implementation::Implementation,
};
use pernixc_handler::{Handler, Storage};
use pernixc_resolution::qualified_identifier;
use pernixc_source_file::{SourceElement, Span};
use pernixc_syntax::syntax_tree::{
    self,
    expression::{AccessKind, PostfixOperator},
    ConnectedList,
};
use pernixc_table::{
    component::{Implemented, Implements, Import, Member, Parent, SymbolKind},
    diagnostic::Diagnostic,
    resolution::diagnostic::{SymbolIsNotAccessible, SymbolNotFound},
    GlobalID,
};
use pernixc_term::{
    constant::Constant,
    elided_lifetimes::{ElidedLifetimeID, ElidedLifetimes},
    generic_arguments::GenericArguments,
    generic_parameter::{
        ConstantParameterID, GenericParameters, LifetimeParameterID,
        TypeParameterID,
    },
    instantiation::{self, Instantiation},
    lifetime::Lifetime,
    r#type::{Primitive, Qualifier, Type},
    Default, Model, Symbol,
};
use pernixc_type_system::diagnostic::OverflowOperation;

use super::{Bind, Config, Expression, LValue};
use crate::{
    address::{self, Address, Memory},
    binding::{
        diagnostic::{
            AdtImplementationFunctionCannotBeUsedAsMethod, AmbiguousMethodCall,
            CannotIndexPastUnpackedTuple, ExpectArray, ExpectedStructType,
            ExpectedTuple, ExpressionIsNotCallable, FieldIsNotAccessible,
            FieldNotFound, InvalidCastType, MethodNotFound,
            MismatchedArgumentCount, MismatchedQualifierForReferenceOf,
            SymbolIsNotCallable, TooLargeTupleIndex, TupleIndexOutOfBOunds,
            UnexpectedGenericArgumentsInField,
        },
        expression::Target,
        infer::{self, Expected, InferenceVariable},
        AbruptError, AddContextExt, Binder, Error, SemanticError,
    },
    instruction::{Instruction, Store},
    model::{Constraint, Erased, NoConstraint},
    value::{
        literal::{self, Literal},
        register::{
            Assignment, Borrow, Cast, FunctionCall, Load, Register, Variant,
        },
        Value,
    },
};

// FIXME: the algorithm for searching methods are such a mess

impl Bind<&syntax_tree::expression::Postfix> for Binder<'_> {
    #[allow(clippy::too_many_lines)]
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::Postfix,
        config: Config,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<Expression, Error> {
        match syntax_tree.operator() {
            syntax_tree::expression::PostfixOperator::Call(call) => {
                self.handle_call(call, syntax_tree, handler)
            }

            syntax_tree::expression::PostfixOperator::Cast(cast_syn) => {
                // resolve the type
                let cast_type = self
                    .resolve_type_with_inference(cast_syn.r#type(), handler);

                let cast_type = self
                    .create_environment()
                    .simplify(cast_type)
                    .map_err(|x| {
                    x.into_type_system_overflow(
                        OverflowOperation::TypeOf,
                        cast_syn.r#type().span(),
                    )
                })?;

                // can only cast between numeric types
                if !matches!(
                    &cast_type.result,
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
                                .transform_type_into_constraint_model(
                                    cast_type.result.clone(),
                                    self.table,
                                )
                                .map_err(|x| {
                                    x.into_type_system_overflow(
                                        OverflowOperation::TypeOf,
                                        cast_syn.r#type().span(),
                                    )
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
                        Assignment::Cast(Cast {
                            value,
                            r#type: cast_type.result.clone(),
                        }),
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

impl Binder<'_> {
    fn is_method(
        first_parameter_type: &Type<Default>,
        implemented_type: &Type<Default>,
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
    ) -> Result<
        Vec<(
            GlobalID,
            Instantiation<infer::Model>,
            infer::Context,
            RecieverKind,
        )>,
        AbruptError,
    > {
        let mut candidates = Vec::new();

        let current_module_id = GlobalID::new(
            self.current_site.target_id,
            self.table.get_closet_module_id(self.current_site),
        );

        let module_import = self.table.get::<Import>(current_module_id);
        let module_member = self.table.get::<Member>(current_module_id);

        let visible_traits = module_import
            .iter()
            .map(|x| x.1.id)
            .chain(
                module_member
                    .values()
                    .copied()
                    .map(|x| GlobalID::new(current_module_id.target_id, x)),
            )
            .filter(|x| {
                let symbol_kind = *self.table.get::<SymbolKind>(*x);

                symbol_kind == SymbolKind::Trait
            })
            .chain(
                self.premise
                    .predicates
                    .iter()
                    .filter_map(|x| x.as_positive_trait().map(|x| x.trait_id)),
            )
            .collect::<HashSet<_>>();

        let starting_inference_context = self.inference_context.clone();

        'candidate: for trait_id in visible_traits {
            // looking for a trait function
            let Some(trait_function_id) = self
                .table
                .get::<Member>(trait_id)
                .get(method_ident.identifier().span.str())
                .copied()
                .map(|x| GlobalID::new(trait_id.target_id, x))
            else {
                continue;
            };

            let symbol_kind = *self.table.get::<SymbolKind>(trait_function_id);

            if symbol_kind != SymbolKind::TraitFunction {
                continue;
            }

            // invoke the resolution observer
            let trait_function_signature =
                self.table.query::<FunctionSignature>(trait_function_id)?;

            // argument count mismatched, skip
            if trait_function_signature.parameters.len() != arguments.len() + 1
            {
                continue;
            }

            // check accessibility, skip
            if !self.table.is_accessible_from_globally(
                self.current_site,
                self.table
                    .get_accessibility(trait_function_id)
                    .into_global(trait_function_id.target_id),
            ) {
                continue;
            }

            let trait_generic_parameter =
                self.table.query::<GenericParameters>(trait_id)?;

            // trait must at least have one type parameter
            let Some(first_ty_parameter_id) = trait_generic_parameter
                .type_parameters_as_order()
                .next()
                .map(|x| x.0)
            else {
                continue;
            };

            // check if it can be used as a method
            let Some(kind) = Self::is_method(
                trait_function_signature
                    .parameter_order
                    .iter()
                    .copied()
                    .map(|x| &trait_function_signature.parameters[x].r#type)
                    .next()
                    .unwrap(),
                &Type::Parameter(TypeParameterID {
                    parent: trait_id,
                    id: first_ty_parameter_id,
                }),
            ) else {
                continue;
            };

            self.inference_context = starting_inference_context.clone();

            let storage = Storage::<Box<dyn Diagnostic>>::default();
            let function_generic_arguments = self
                .verify_generic_arguments_for_with_inference(
                    generic_arguments.clone(),
                    trait_function_id,
                    method_ident.span(),
                    false,
                    &storage,
                )?;

            if !storage.as_vec().is_empty() {
                continue;
            }

            let trait_generic_arguments = GenericArguments {
                lifetimes: trait_generic_parameter
                    .lifetime_order()
                    .iter()
                    .map(|_| Lifetime::Inference(Erased))
                    .collect(),
                types: trait_generic_parameter
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
                    trait_generic_parameter
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

            let trait_function_generic_params =
                self.table.query::<GenericParameters>(trait_function_id)?;
            let mut instantiation = Instantiation::from_generic_arguments(
                trait_generic_arguments.clone(),
                trait_id,
                &trait_generic_parameter,
            )
            .unwrap();

            assert!(instantiation
                .append_from_generic_arguments(
                    function_generic_arguments,
                    trait_function_id,
                    &trait_function_generic_params
                )
                .unwrap()
                .is_empty());

            let trait_function_elided_lifetimes =
                self.table.query::<ElidedLifetimes>(trait_function_id)?;

            instantiation.lifetimes.extend(
                trait_function_elided_lifetimes.elided_lifetimes.ids().map(
                    |x| {
                        (
                            Lifetime::Elided(ElidedLifetimeID::new(
                                trait_function_id,
                                x,
                            )),
                            Lifetime::Inference(Erased),
                        )
                    },
                ),
            );

            for ((argument_span, argument_value), parameter) in
                arguments.iter().zip(
                    trait_function_signature
                        .parameter_order
                        .iter()
                        .copied()
                        .skip(1)
                        .map(|x| &trait_function_signature.parameters[x]),
                )
            {
                let mut parameter_ty =
                    infer::Model::from_default_type(parameter.r#type.clone());

                instantiation::instantiate(&mut parameter_ty, &instantiation);

                let storage = Storage::<Box<dyn Diagnostic>>::default();
                if !self.type_check(
                    &self.type_of_value(argument_value).unwrap(),
                    Expected::Known(parameter_ty),
                    argument_span.clone(),
                    false,
                    &storage,
                )? {
                    continue 'candidate;
                }

                if !storage.as_vec().is_empty() {
                    continue;
                }
            }

            // check if trait is implemented
            candidates.push((
                trait_function_id,
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
        handler: &dyn Handler<Box<dyn Diagnostic>>,
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
                        .clone()
                        .unwrap(),
                    Value::Literal(literal) => literal.span().cloned().unwrap(),
                };
                let type_of_value = self.type_of_value(&value)?;
                let alloca_id = self.create_alloca(type_of_value, span.clone());

                // initialize
                let _ = self.current_block_mut().add_instruction(
                    Instruction::Store(Store {
                        address: Address::Memory(Memory::Alloca(alloca_id)),
                        span: Some(span.clone()),
                        value,
                    }),
                );

                Ok(Value::Register(self.create_register_assignmnet(
                    Assignment::Borrow(Borrow {
                        address: Address::Memory(Memory::Alloca(alloca_id)),
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
                    Assignment::Borrow(Borrow {
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

    fn on_binding_adt_method_error<I: Iterator<Item = Box<dyn Diagnostic>>>(
        &mut self,
        method_call_span: Span,
        reciever: Expression,
        mut arguments: Vec<(Span, Value<infer::Model>)>,
        trait_method_candidates: Vec<(
            GlobalID,
            Instantiation<infer::Model>,
            infer::Context,
            RecieverKind,
        )>,
        error: impl FnOnce(&mut Self) -> I,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
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
                    .clone()
                    .unwrap(),
                Expression::RValue(Value::Literal(literal)) => {
                    literal.span().cloned().unwrap()
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
        handler: &dyn Handler<Box<dyn Diagnostic>>,
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

            let ty = self.type_of_address(&lvalue.address).map_err(|x| {
                x.into_type_system_overflow(
                    OverflowOperation::TypeOf,
                    lvalue.span.clone(),
                )
            })?;

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
            });

        let trait_method_candidates = self.search_trait_method_candidates(
            method_ident,
            &generic_arguments,
            &access_type,
            &arguments,
        )?;

        let Some(symbol) = access_type.into_symbol().into_iter().find(|x| {
            let symbol_kind = *self.table.get::<SymbolKind>(x.id);
            symbol_kind.is_adt()
        }) else {
            return self.on_binding_adt_method_error(
                postfix.span(),
                reciever_expression,
                arguments,
                trait_method_candidates,
                |_| {
                    std::iter::once(Box::new(MethodNotFound {
                        method_call_span: postfix.span(),
                    })
                        as Box<dyn Diagnostic>)
                },
                handler,
            );
        };

        let Symbol { id: adt_id, generic_arguments: adt_generic_arguments } =
            symbol;

        // find the method id
        let Some(adt_implementation_function_id) =
            self.table.get::<Implemented>(adt_id).iter().copied().find_map(
                |x| {
                    self.table
                        .get::<Member>(x)
                        .get(method_ident.identifier().span.str())
                        .copied()
                        .map(|y| GlobalID::new(x.target_id, y))
                        .filter(|x| {
                            let symbol_kind = *self.table.get::<SymbolKind>(*x);

                            symbol_kind == SymbolKind::AdtImplementationFunction
                        })
                },
            )
        else {
            return self.on_binding_adt_method_error(
                postfix.span(),
                reciever_expression,
                arguments,
                trait_method_candidates,
                |_| {
                    std::iter::once(Box::new(SymbolNotFound {
                        searched_item_id: Some(adt_id),
                        resolution_span: method_ident.span(),
                    })
                        as Box<dyn Diagnostic>)
                },
                handler,
            );
        };

        let storage = Storage::<Box<dyn Diagnostic>>::default();
        let function_generic_arguments = self
            .verify_generic_arguments_for_with_inference(
                generic_arguments,
                adt_implementation_function_id,
                method_ident.span(),
                false,
                &storage,
            )?;

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

        let adt_implementation_id = GlobalID::new(
            adt_implementation_function_id.target_id,
            self.table
                .get::<Parent>(adt_implementation_function_id)
                .parent
                .unwrap(),
        );
        let adt_implementation_generic_params =
            self.table.query::<GenericParameters>(adt_implementation_id)?;
        let adt_implementation_generic_arguments =
            self.table.query::<Implementation>(adt_implementation_id)?;

        let mut type_inferences = Vec::new();
        let mut constant_inferences = Vec::new();

        let mut instantiation = Instantiation::<infer::Model> {
            lifetimes: adt_implementation_generic_params
                .lifetime_parameters_as_order()
                .map(|(id, _)| {
                    (
                        Lifetime::Parameter(LifetimeParameterID {
                            parent: adt_implementation_id,
                            id,
                        }),
                        Lifetime::Inference(Erased),
                    )
                })
                .collect(),
            types: adt_implementation_generic_params
                .type_parameters_as_order()
                .map(|(id, _)| {
                    let inference_variable = InferenceVariable::new();
                    type_inferences.push(inference_variable);

                    (
                        Type::Parameter(TypeParameterID {
                            parent: adt_implementation_id,
                            id,
                        }),
                        Type::Inference(inference_variable),
                    )
                })
                .collect(),
            constants: adt_implementation_generic_params
                .constant_parameters_as_order()
                .map(|(id, _)| {
                    let inference_variable = InferenceVariable::new();
                    constant_inferences.push(inference_variable);

                    (
                        Constant::Parameter(ConstantParameterID {
                            parent: adt_implementation_id,
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
                id: adt_id,
                generic_arguments: adt_generic_arguments,
            }),
            Expected::Known(Type::Symbol(Symbol {
                id: adt_id,
                generic_arguments: {
                    let mut adt_generic_arguments =
                        GenericArguments::from_default_model(
                            adt_implementation_generic_arguments
                                .generic_arguments
                                .clone(),
                        );

                    adt_generic_arguments.instantiate(&instantiation);

                    adt_generic_arguments
                },
            })),
            struct_expression_wth_access.postfixable().span(),
            true,
            handler,
        )?;

        let adt_implementation_function_generic_params =
            self.table
                .query::<GenericParameters>(adt_implementation_function_id)?;

        assert!(instantiation
            .append_from_generic_arguments(
                function_generic_arguments,
                adt_implementation_function_id,
                &adt_implementation_function_generic_params
            )
            .unwrap()
            .is_empty());

        let adt_implementation_function_signature =
            self.table
                .query::<FunctionSignature>(adt_implementation_function_id)?;

        if adt_implementation_function_signature.parameters.is_empty() {
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
                        as Box<dyn Diagnostic>)
                },
                handler,
            );
        }

        let implemented_type = Type::Symbol(Symbol {
            id: adt_id,
            generic_arguments: adt_implementation_generic_arguments
                .generic_arguments
                .clone(),
        });
        let first_parameter_type = &adt_implementation_function_signature
            .parameters[*adt_implementation_function_signature
            .parameter_order
            .first()
            .unwrap()]
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
                        as Box<dyn Diagnostic>)
                },
                handler,
            );
        };

        if adt_implementation_function_signature.parameters.len()
            != arguments.len() + 1
        {
            let found_argument_count = arguments.len();
            return self.on_binding_adt_method_error(
                postfix.span(),
                reciever_expression,
                arguments,
                trait_method_candidates,
                |_| {
                    std::iter::once(Box::new(MismatchedArgumentCount {
                        called_id: adt_implementation_function_id,
                        expected_count: adt_implementation_function_signature
                            .parameters
                            .len()
                            - 1,
                        found_count: found_argument_count,
                        span: call.span(),
                    })
                        as Box<dyn Diagnostic>)
                },
                handler,
            );
        }

        if !self.table.is_accessible_from_globally(
            self.current_site,
            self.table
                .get_accessibility(adt_implementation_function_id)
                .into_global(adt_implementation_function_id.target_id),
        ) {
            self.create_handler_wrapper(handler).receive(Box::new(
                SymbolIsNotAccessible {
                    referring_site: self.current_site,
                    referred: adt_implementation_function_id,
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
        drop(adt_implementation_function_signature);

        Ok(Value::Register(self.bind_function_call(
            &arguments,
            adt_implementation_function_id,
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
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<Expression, Error> {
        let (lvalue, ty) = match access.operator() {
            // expect the lvalue address
            syntax_tree::expression::AccessOperator::Dot(_) => {
                let lvalue = self.bind_as_lvalue(
                    &**syntax_tree.postfixable(),
                    true,
                    handler,
                )?;

                let ty =
                    self.type_of_address(&lvalue.address).map_err(|x| {
                        x.into_type_system_overflow(
                            OverflowOperation::TypeOf,
                            lvalue.span.clone(),
                        )
                    })?;

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

                let ty =
                    self.type_of_address(&lvalue.address).map_err(|x| {
                        x.into_type_system_overflow(
                            OverflowOperation::TypeOf,
                            lvalue.span.clone(),
                        )
                    })?;

                (lvalue, ty)
            }
        };

        let address = match access.kind() {
            // struct field access
            syntax_tree::expression::AccessKind::GenericIdentifier(
                generic_ident,
            ) => {
                let struct_id = match ty {
                    Type::Symbol(Symbol { id: struct_id, .. })
                        if {
                            let symbol_kind =
                                *self.table.get::<SymbolKind>(struct_id);

                            symbol_kind.is_adt()
                        } =>
                    {
                        struct_id
                    }

                    found_type => {
                        self.create_handler_wrapper(handler).receive(Box::new(
                            ExpectedStructType {
                                span: syntax_tree.postfixable().span(),
                                r#type: self
                                    .inference_context
                                    .transform_type_into_constraint_model(
                                        found_type, self.table,
                                    )
                                    .map_err(|x| {
                                        x.into_type_system_overflow(
                                            OverflowOperation::TypeOf,
                                            syntax_tree.postfixable().span(),
                                        )
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
                let fields = self
                    .table
                    .query::<pernixc_component::fields::Fields>(struct_id)?;

                let Some(field_id) = fields
                    .field_ids_by_name
                    .get(generic_ident.identifier().span.str())
                    .copied()
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

                let field_accessibility = fields.fields[field_id].accessibility;

                // field is not accessible, soft error, keep going
                if !self.table.is_accessible_from_globally(
                    self.current_site,
                    field_accessibility.into_global(struct_id.target_id),
                ) {
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
                            ExpectedTuple {
                                span: syntax_tree.postfixable().span(),
                                r#type: self
                                    .inference_context
                                    .transform_type_into_constraint_model(
                                        found_type, self.table,
                                    )
                                    .map_err(|x| {
                                        x.into_type_system_overflow(
                                            OverflowOperation::TypeOf,
                                            syntax_tree.postfixable().span(),
                                        )
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
                                    self.table,
                                )
                                .map_err(|x| {
                                    x.into_type_system_overflow(
                                        OverflowOperation::TypeOf,
                                        syntax_tree.postfixable().span(),
                                    )
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
                                        self.table,
                                    )
                                    .map_err(|x| {
                                        x.into_type_system_overflow(
                                            OverflowOperation::TypeOf,
                                            syntax_tree.postfixable().span(),
                                        )
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
                                .transform_type_into_constraint_model(
                                    ty, self.table,
                                )
                                .map_err(|x| {
                                    x.into_type_system_overflow(
                                        OverflowOperation::TypeOf,
                                        syntax_tree.postfixable().span(),
                                    )
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
                    Expected::Known(Type::Primitive(Primitive::Usize)),
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
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<Expression, Error> {
        use qualified_identifier::Error::{CyclicDependency, FatalSemantic};

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


                let resolution = self
                    .resolve_qualified_identifier_with_inference(
                        qualified_identifier,
                        handler,
                    )
                    .map_err(|x| {
                        match x{
                            FatalSemantic => {
                                Error::Semantic(
                                    SemanticError(qualified_identifier.span())
                                )
                            },
                            CyclicDependency(x) => {
                                Error::Abrupt(AbruptError::CyclicDependency(x))
                            }
                        }
                    })?;

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
        variant: qualified_identifier::Variant<infer::Model>,
        syntax_tree_span: Span,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<ID<Register<infer::Model>>, AbruptError> {
        let enum_id = GlobalID::new(
            variant.variant_id.target_id,
            self.table.get::<Parent>(variant.variant_id).parent.unwrap(),
        );
        let enum_generic_params =
            self.table.query::<GenericParameters>(enum_id)?;
        let variant_symbol = self
            .table
            .query::<pernixc_component::variant::Variant>(variant.variant_id)?;

        let instantiation = Instantiation::from_generic_arguments(
            variant.generic_arguments.clone(),
            enum_id,
            &enum_generic_params,
        )
        .unwrap();

        if let Some(mut variant_type) = variant_symbol
            .associated_type
            .clone()
            .map(infer::Model::from_default_type)
        {
            instantiation::instantiate(&mut variant_type, &instantiation);

            let associated_value = match arguments.len() {
                0 => {
                    self.create_handler_wrapper(handler).receive(Box::new(
                        MismatchedArgumentCount {
                            called_id: variant.variant_id,
                            expected_count: 1,
                            found_count: 0,
                            span: syntax_tree_span.clone(),
                        },
                    ));

                    Value::Literal(Literal::Error(literal::Error {
                        r#type: variant_type,
                        span: Some(syntax_tree_span.clone()),
                    }))
                }
                len => {
                    if len != 1 {
                        self.create_handler_wrapper(handler).receive(Box::new(
                            MismatchedArgumentCount {
                                called_id: variant.variant_id,
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
                    variant_id: variant.variant_id,
                    associated_value: Some(associated_value),
                    generic_arguments: variant.generic_arguments,
                }),
                syntax_tree_span,
            ))
        } else {
            if !arguments.is_empty() {
                self.create_handler_wrapper(handler).receive(Box::new(
                    MismatchedArgumentCount {
                        called_id: variant.variant_id,
                        expected_count: 0,
                        found_count: arguments.len(),
                        span: syntax_tree_span.clone(),
                    },
                ));
            }

            Ok(self.create_register_assignmnet(
                Assignment::Variant(Variant {
                    variant_id: variant.variant_id,
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
        callable_id: GlobalID,
        mut instantiation: Instantiation<infer::Model>,
        syntax_tree_span: Span,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<ID<Register<infer::Model>>, AbruptError> {
        let function_signature =
            self.table.query::<FunctionSignature>(callable_id)?;
        let elided_lifetimes =
            self.table.query::<ElidedLifetimes>(callable_id)?;

        instantiation.lifetimes.extend(
            elided_lifetimes.elided_lifetimes.ids().map(|x| {
                (
                    Lifetime::Elided(ElidedLifetimeID::new(callable_id, x)),
                    Lifetime::Inference(Erased),
                )
            }),
        );

        let mut acutal_arguments = Vec::new();

        // mismatched arguments count
        if arguments.len() != function_signature.parameter_order.len() {
            self.create_handler_wrapper(handler).receive(Box::new(
                MismatchedArgumentCount {
                    called_id: callable_id,
                    expected_count: function_signature.parameter_order.len(),
                    found_count: arguments.len(),
                    span: syntax_tree_span.clone(),
                },
            ));
        }

        // type check the arguments
        for ((argument_span, argument_value), parameter) in
            arguments.iter().zip(
                function_signature
                    .parameter_order
                    .iter()
                    .copied()
                    .map(|x| &function_signature.parameters[x]),
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
        let rest_parameter_ids = function_signature
            .parameter_order
            .iter()
            .skip(arguments.len())
            .copied();

        for parameter in
            rest_parameter_ids.map(|x| &function_signature.parameters[x])
        {
            let mut parameter_ty =
                infer::Model::from_default_type(parameter.r#type.clone());

            instantiation::instantiate(&mut parameter_ty, &instantiation);

            let error_value = Value::Literal(Literal::Error(literal::Error {
                r#type: parameter_ty,
                span: Some(syntax_tree_span.clone()),
            }));

            acutal_arguments.push(error_value);
        }

        let mut return_type = infer::Model::from_default_type(
            function_signature.return_type.clone(),
        );

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
        resolution: qualified_identifier::Resolution<infer::Model>,
        syntax_tree_span: Span,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<Expression, Error> {
        // can be enum variant and functin call
        match resolution {
            // bind as variant
            qualified_identifier::Resolution::Variant(variant) => {
                let value = Value::Register(self.bind_variant_call(
                    arguments,
                    variant,
                    syntax_tree_span,
                    handler,
                )?);

                Ok(Expression::RValue(value))
            }

            qualified_identifier::Resolution::Generic(
                qualified_identifier::Generic { id, generic_arguments },
            ) if matches!(
                *self.table.get::<SymbolKind>(id),
                SymbolKind::TraitImplementationFunction
                    | SymbolKind::Function
                    | SymbolKind::ExternFunction
            ) =>
            {
                let symbol_kind = *self.table.get::<SymbolKind>(id);

                let (callable_id, instantiation) = match symbol_kind {
                    SymbolKind::Function | SymbolKind::ExternFunction => {
                        let function_generic_params =
                            self.table.query::<GenericParameters>(id)?;

                        let inst = Instantiation::from_generic_arguments(
                            generic_arguments,
                            id,
                            &function_generic_params,
                        )
                        .unwrap();

                        (id, inst)
                    }
                    SymbolKind::TraitImplementationFunction => {
                        let trait_implementation_sym_id = GlobalID::new(
                            id.target_id,
                            self.table.get::<Parent>(id).parent.unwrap(),
                        );
                        let trait_implementation_generic_parameters =
                            self.table.query::<GenericParameters>(
                                trait_implementation_sym_id,
                            )?;

                        let mut inst = Instantiation {
                            lifetimes: trait_implementation_generic_parameters
                                .lifetime_order()
                                .iter()
                                .copied()
                                .map(|x| {
                                    let lifetime_parameter =
                                        Lifetime::Parameter(
                                            LifetimeParameterID {
                                                parent:
                                                    trait_implementation_sym_id,
                                                id: x,
                                            },
                                        );

                                    (lifetime_parameter, lifetime_parameter)
                                })
                                .collect(),

                            types: trait_implementation_generic_parameters
                                .type_order()
                                .iter()
                                .copied()
                                .map(|x| {
                                    let type_parameter =
                                        Type::Parameter(TypeParameterID {
                                            parent: trait_implementation_sym_id,
                                            id: x,
                                        });

                                    (type_parameter.clone(), type_parameter)
                                })
                                .collect(),

                            constants: trait_implementation_generic_parameters
                                .constant_order()
                                .iter()
                                .copied()
                                .map(|x| {
                                    let constant_parameter =
                                        Constant::Parameter(
                                            ConstantParameterID {
                                                parent:
                                                    trait_implementation_sym_id,
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

                        let trait_function_generic_params =
                            self.table.query::<GenericParameters>(id)?;
                        assert!(inst
                            .append_from_generic_arguments(
                                generic_arguments,
                                id,
                                &trait_function_generic_params,
                            )
                            .unwrap()
                            .is_empty());

                        (id, inst)
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

            qualified_identifier::Resolution::MemberGeneric(
                qualified_identifier::MemberGeneric {
                    id,
                    parent_generic_arguments,
                    member_generic_arguments,
                },
            ) if matches!(
                *self.table.get::<SymbolKind>(id),
                SymbolKind::TraitFunction
                    | SymbolKind::AdtImplementationFunction
            ) =>
            {
                let symbol_kind = *self.table.get::<SymbolKind>(id);

                let (callable_id, inst) = match symbol_kind {
                    SymbolKind::TraitFunction => {
                        let trait_sym_id = GlobalID::new(
                            id.target_id,
                            self.table.get::<Parent>(id).parent.unwrap(),
                        );
                        let trait_generic_params =
                            self.table
                                .query::<GenericParameters>(trait_sym_id)?;

                        let mut inst = Instantiation::from_generic_arguments(
                            parent_generic_arguments,
                            trait_sym_id,
                            &trait_generic_params,
                        )
                        .unwrap();

                        let trait_func_generic_params =
                            self.table.query::<GenericParameters>(id)?;

                        assert!(inst
                            .append_from_generic_arguments(
                                member_generic_arguments,
                                id,
                                &trait_func_generic_params
                            )
                            .unwrap()
                            .is_empty());

                        (id, inst)
                    }

                    SymbolKind::AdtImplementationFunction => {
                        let adt_implementation_id = GlobalID::new(
                            id.target_id,
                            self.table.get::<Parent>(id).parent.unwrap(),
                        );
                        let adt_implementation_generic_params =
                            self.table.query::<GenericParameters>(
                                adt_implementation_id,
                            )?;

                        let mut type_inferences = Vec::new();
                        let mut constant_inferences = Vec::new();

                        let mut instantiation = Instantiation::<infer::Model> {
                            lifetimes: adt_implementation_generic_params
                                .lifetime_parameters_as_order()
                                .map(|(id, _)| {
                                    (
                                        Lifetime::Parameter(
                                            LifetimeParameterID {
                                                parent: adt_implementation_id,
                                                id,
                                            },
                                        ),
                                        Lifetime::Inference(Erased),
                                    )
                                })
                                .collect(),
                            types: adt_implementation_generic_params
                                .type_parameters_as_order()
                                .map(|(id, _)| {
                                    let inference_variable =
                                        InferenceVariable::new();
                                    type_inferences.push(inference_variable);

                                    (
                                        Type::Parameter(TypeParameterID {
                                            parent: adt_implementation_id,
                                            id,
                                        }),
                                        Type::Inference(inference_variable),
                                    )
                                })
                                .collect(),
                            constants: adt_implementation_generic_params
                                .constant_parameters_as_order()
                                .map(|(id, _)| {
                                    let inference_variable =
                                        InferenceVariable::new();
                                    constant_inferences
                                        .push(inference_variable);

                                    (
                                        Constant::Parameter(
                                            ConstantParameterID {
                                                parent: adt_implementation_id,
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

                        let adt_implementation_sig =
                            self.table.query::<Implementation>(
                                adt_implementation_id,
                            )?;
                        let implemented_id = self
                            .table
                            .get::<Implements>(adt_implementation_id)
                            .0;

                        let _ = self.type_check(
                            &Type::Symbol(Symbol {
                                id: implemented_id,
                                generic_arguments: parent_generic_arguments,
                            }),
                            Expected::Known(Type::Symbol(Symbol {
                                id: implemented_id,
                                generic_arguments: {
                                    let mut adt_generic_arguments =
                                        GenericArguments::from_default_model(
                                            adt_implementation_sig
                                                .generic_arguments
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

                        let adt_impl_func_generic_params =
                            self.table.query::<GenericParameters>(id)?;
                        assert!(instantiation
                            .append_from_generic_arguments(
                                member_generic_arguments,
                                id,
                                &adt_impl_func_generic_params,
                            )
                            .unwrap()
                            .is_empty());

                        (id, instantiation)
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

#[cfg(test)]
mod test;
