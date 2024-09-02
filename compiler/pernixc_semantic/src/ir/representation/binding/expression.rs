//! Contains the code for binding the expression syntax tree.

use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    num::NonZeroUsize,
};

use enum_as_inner::EnumAsInner;
use pernixc_base::{
    handler::Handler,
    source_file::{SourceElement, Span},
};
use pernixc_lexical::token;
use pernixc_syntax::syntax_tree::{
    self,
    expression::{BlockOrIfElse, Loop},
    ConnectedList, Label,
};

use super::{
    infer::{self, Erased},
    stack::Scope,
    Binder, Error, InferenceProvider, InternalError, LoopKind, SemanticError,
};
use crate::{
    arena::ID,
    error::{
        self, BlockWithGivenLableNameNotFound, CannotDereference,
        CannotIndexPastUnpackedTuple, DuplicatedFieldInitialization,
        ExpectArray, ExpectLValue, ExpectStructType, ExpressOutsideBlock,
        ExpressionIsNotCallable, FieldIsNotAccessible, FieldNotFound,
        FloatingPointLiteralHasIntegralSuffix, InvalidCastType,
        InvalidNumericSuffix, InvalidRelationalOperation, LoopControlFlow,
        LoopControlFlowOutsideLoop, LoopWithGivenLabelNameNotFound,
        MismatchedArgumentCount, MismatchedMutability,
        MismatchedReferenceQualifier, MoreThanOneUnpackedInTupleExpression,
        NotAllFlowPathsExpressValue, ReturnIsNotAllowed, SymbolIsNotCallable,
        TooLargeTupleIndex, TupleExpected, TupleIndexOutOfBOunds,
    },
    ir::{
        address::{self, Address, Memory},
        control_flow_graph::{Block, InsertTerminatorError},
        instruction::{
            self, Instruction, Jump, ScopePop, ScopePush, Store, Terminator,
            UnconditionalJump,
        },
        representation::binding::{
            infer::{InferenceVariable, NoConstraint},
            BlockState, LoopState,
        },
        scope,
        value::{
            literal::{self, Boolean, Literal, Numeric, Unit, Unreachable},
            register::{
                self, ArithmeticOperator, Array, Assignment, Binary,
                BinaryOperator, BitwiseOperator, Cast, FunctionCall, Load,
                LoadKind, Phi, Prefix, PrefixOperator, ReferenceOf, Register,
                Struct, Variant,
            },
            Value,
        },
        TypeOfError,
    },
    symbol::{
        table::{
            self,
            representation::Index,
            resolution::{self, MemberGenericID},
        },
        AdtID, CallableID, ConstantParameterID, Field, LifetimeParameterID,
        TypeParameterID,
    },
    type_system::{
        self,
        instantiation::{self, Instantiation},
        model::Model,
        simplify,
        term::{
            self,
            constant::Constant,
            lifetime::Lifetime,
            r#type::{
                self, Constraint, Expected, Primitive, Qualifier, SymbolID,
                Type,
            },
            GenericArguments, Local, Symbol,
        },
    },
};

/// An enumeration describes the intended purpose of binding the expression.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Target {
    /// Binds the expression syntax tree for the value.
    Value,

    /// Binds the syntax tree for the underlying address.
    ///
    /// This is used for obtaining the address of some l-value.
    Address {
        /// The expected qualifier of the underlying address.
        expected_qualifier: Qualifier,
    },

    /// The expression is being bound for a statement, therefore the produced
    /// value will be discarded right away.
    Statement,
}

/// The configuration object for binding the expression syntax tree.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Config {
    /// The intended purpose of binding the expression.
    pub target: Target,
}

/// The result of binding the expression syntax tree.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Expression {
    /// The expression is bound as an r-value.
    Value(Value<infer::Model>),

    /// The expression is bound as an l-value.
    Address {
        /// The address of the l-value.
        address: Address<infer::Model>,

        /// The span of the expression.
        span: Span,
    },

    /// The expression is bound as a statement.
    SideEffect,
}

/// The trait for binding the expression syntax tree.
pub trait Bind<T> {
    /// Binds the given syntax tree to the [`Expression`].
    ///
    /// # Errors
    ///
    /// If an error occurs during the binding process, an [`Error`] is returned
    /// with the span of the syntax tree that caused the error.
    fn bind(
        &mut self,
        syntax_tree: T,
        config: Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error>;
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>,
    > Bind<&syntax_tree::expression::Numeric> for Binder<'t, S, RO, TO>
{
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::Numeric,
        _: Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        let numeric_ty = match syntax_tree.suffix() {
            Some(suffix) => {
                // the literal type is specified, no need to infer
                let primitive_type = match suffix.span.str() {
                    "i8" => r#type::Primitive::Int8,
                    "i16" => r#type::Primitive::Int16,
                    "i32" => r#type::Primitive::Int32,
                    "i64" => r#type::Primitive::Int64,
                    "u8" => r#type::Primitive::Uint8,
                    "u16" => r#type::Primitive::Uint16,
                    "u32" => r#type::Primitive::Uint32,
                    "u64" => r#type::Primitive::Uint64,
                    "f32" => r#type::Primitive::Float32,
                    "f64" => r#type::Primitive::Float64,
                    "us" => r#type::Primitive::Usize,
                    "is" => r#type::Primitive::Isize,
                    _ => {
                        self.create_handler_wrapper(handler).receive(Box::new(
                            InvalidNumericSuffix { suffix_span: suffix.span() },
                        ));

                        return Err(Error::Semantic(SemanticError(
                            syntax_tree.span(),
                        )));
                    }
                };

                let primitive_type_is_integral = matches!(
                    primitive_type,
                    r#type::Primitive::Int8
                        | r#type::Primitive::Int16
                        | r#type::Primitive::Int32
                        | r#type::Primitive::Int64
                        | r#type::Primitive::Uint8
                        | r#type::Primitive::Uint16
                        | r#type::Primitive::Uint32
                        | r#type::Primitive::Uint64
                        | r#type::Primitive::Usize
                        | r#type::Primitive::Isize
                );

                // check if the type is integer but the numeric literal has
                // decimal point

                if syntax_tree.decimal().is_some() && primitive_type_is_integral
                {
                    self.create_handler_wrapper(handler).receive(Box::new(
                        FloatingPointLiteralHasIntegralSuffix {
                            numeric_literal_span: syntax_tree.span(),
                        },
                    ));
                    return Err(Error::Semantic(SemanticError(
                        syntax_tree.span(),
                    )));
                }

                Type::Primitive(primitive_type)
            }
            None => {
                // infer the type
                let constraint = if syntax_tree.decimal().is_some() {
                    r#type::Constraint::Floating
                } else {
                    r#type::Constraint::Number
                };

                Type::Inference(self.create_type_inference(constraint))
            }
        };

        Ok(Expression::Value(Value::Literal(Literal::Numeric(Numeric {
            integer_string: syntax_tree.numeric().span.str().to_string(),
            decimal_stirng: syntax_tree
                .decimal()
                .as_ref()
                .map(|x| x.numeric().span.str().to_owned()),
            r#type: numeric_ty,
            span: Some(syntax_tree.span()),
        }))))
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>,
    > Bind<&syntax_tree::expression::Boolean> for Binder<'t, S, RO, TO>
{
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::Boolean,
        _: Config,
        _: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        let value = match syntax_tree {
            syntax_tree::expression::Boolean::True(_) => true,
            syntax_tree::expression::Boolean::False(_) => false,
        };

        Ok(Expression::Value(Value::Literal(Literal::Boolean(Boolean {
            value,
            span: Some(syntax_tree.span()),
        }))))
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>,
    > Binder<'t, S, RO, TO>
{
    /// Binds the given syntax tree as an address.
    ///
    /// If the expression cannot be bound as an address, a variable will be
    /// created an the value is stored in the variable; the address of the
    /// variable is returned.
    fn bind_as_address<'a, T>(
        &mut self,
        syntax_tree: &'a T,
        qualifier: Qualifier,
        create_temporary: bool,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Address<infer::Model>, Error>
    where
        T: SourceElement,
        Self: Bind<&'a T>,
    {
        match self.bind(
            syntax_tree,
            Config {
                target: Target::Address { expected_qualifier: qualifier },
            },
            handler,
        )? {
            Expression::Value(value) => {
                if create_temporary {
                    let type_of_value = self.type_of_value(&value)?;
                    let alloca_id = self
                        .create_alloca(type_of_value, Some(syntax_tree.span()));

                    // initialize
                    let _ = self.current_block_mut().insert_instruction(
                        Instruction::Store(Store {
                            address: Address::Memory(Memory::Alloca(alloca_id)),
                            value,
                        }),
                    );

                    Ok(Address::Memory(Memory::Alloca(alloca_id)))
                } else {
                    handler.receive(Box::new(ExpectLValue {
                        expression_span: syntax_tree.span(),
                    }));

                    Err(Error::Semantic(SemanticError(syntax_tree.span())))
                }
            }
            Expression::Address { address, .. } => Ok(address),
            Expression::SideEffect => {
                unreachable!()
            }
        }
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>,
    > Bind<&syntax_tree::expression::Prefix> for Binder<'t, S, RO, TO>
{
    #[allow(clippy::too_many_lines)]
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::Prefix,
        config: Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        match syntax_tree.operator() {
            syntax_tree::expression::PrefixOperator::Local(_)
            | syntax_tree::expression::PrefixOperator::LogicalNot(_)
            | syntax_tree::expression::PrefixOperator::Negate(_)
            | syntax_tree::expression::PrefixOperator::Unlocal(_)
            | syntax_tree::expression::PrefixOperator::BitwiseNot(_) => {
                let (expected_type, operator) = match syntax_tree.operator() {
                    syntax_tree::expression::PrefixOperator::Local(_) => {
                        (None, PrefixOperator::Local)
                    }
                    syntax_tree::expression::PrefixOperator::LogicalNot(_) => (
                        Some(Expected::Known(Type::Primitive(
                            r#type::Primitive::Bool,
                        ))),
                        PrefixOperator::LogicalNot,
                    ),
                    syntax_tree::expression::PrefixOperator::Unlocal(_) => (
                        Some(Expected::Known(Type::Local(Local(Box::new(
                            Type::Inference(self.create_type_inference(
                                r#type::Constraint::All(false),
                            )),
                        ))))),
                        PrefixOperator::Unlocal,
                    ),
                    syntax_tree::expression::PrefixOperator::Negate(_) => (
                        Some(Expected::Constraint(r#type::Constraint::Signed)),
                        PrefixOperator::Negate,
                    ),
                    syntax_tree::expression::PrefixOperator::BitwiseNot(_) => (
                        Some(Expected::Constraint(r#type::Constraint::Integer)),
                        PrefixOperator::BitwiseNot,
                    ),
                    _ => unreachable!(),
                };

                let operand = self
                    .bind(
                        &**syntax_tree.prefixable(),
                        Config { target: Target::Value },
                        handler,
                    )?
                    .into_value()
                    .unwrap();

                // if required, type check the operand
                if let Some(expected_type) = expected_type {
                    self.type_check(
                        self.type_of_value(&operand)?,
                        expected_type,
                        syntax_tree.span(),
                        handler,
                    )?;
                }
                let register_id = self.create_register_assignmnet(
                    Assignment::Prefix(Prefix { operand, operator }),
                    Some(syntax_tree.span()),
                );

                Ok(Expression::Value(Value::Register(register_id)))
            }

            syntax_tree::expression::PrefixOperator::Dereference(_) => {
                let operand = self
                    .bind(
                        &**syntax_tree.prefixable(),
                        Config { target: Target::Value },
                        handler,
                    )?
                    .into_value()
                    .unwrap();

                // expected a reference type
                let mut operand_type = self.type_of_value(&operand)?;

                operand_type = simplify::simplify(
                    &operand_type,
                    &self.create_environment(),
                )
                .result;

                let reference_type = match operand_type {
                    Type::Reference(reference) => reference,
                    found_type => {
                        self.create_handler_wrapper(handler).receive(Box::new(
                            CannotDereference {
                                found_type: self
                                    .inference_context
                                    .into_constraint_model(found_type)
                                    .unwrap(),
                                span: syntax_tree.span(),
                            },
                        ));
                        return Err(Error::Semantic(SemanticError(
                            syntax_tree.span(),
                        )));
                    }
                };

                match config.target {
                    Target::Value | Target::Statement => {
                        let register_id = self.create_register_assignmnet(
                            Assignment::Load(Load {
                                address: Address::Memory(
                                    address::Memory::ReferenceValue(operand),
                                ),
                                kind: LoadKind::Move,
                            }),
                            Some(syntax_tree.span()),
                        );

                        Ok(Expression::Value(Value::Register(register_id)))
                    }
                    Target::Address { expected_qualifier, .. } => {
                        // soft error, keep going
                        if reference_type.qualifier < expected_qualifier {
                            self.create_handler_wrapper(handler).receive(
                                Box::new(MismatchedReferenceQualifier {
                                    found_reference_type: self
                                        .inference_context
                                        .into_constraint_model(Type::Reference(
                                            reference_type,
                                        ))
                                        .unwrap(),
                                    expected_qualifier,
                                    span: syntax_tree.span(),
                                }),
                            );
                        }

                        Ok(Expression::Address {
                            address: Address::Memory(
                                address::Memory::ReferenceValue(operand),
                            ),
                            span: syntax_tree.span(),
                        })
                    }
                }
            }

            syntax_tree::expression::PrefixOperator::ReferenceOf(
                reference_of,
            ) => {
                let qualifier = reference_of.qualifier().as_ref().map_or(
                    r#type::Qualifier::Immutable,
                    |qualifier| match qualifier {
                        syntax_tree::Qualifier::Mutable(_) => {
                            r#type::Qualifier::Mutable
                        }
                        syntax_tree::Qualifier::Unique(_) => {
                            r#type::Qualifier::Unique
                        }
                    },
                );

                let address = self.bind_as_address(
                    &**syntax_tree.prefixable(),
                    qualifier,
                    true,
                    handler,
                )?;

                let register_id = self.create_register_assignmnet(
                    Assignment::ReferenceOf(ReferenceOf {
                        address,
                        qualifier,
                        lifetime: Lifetime::Inference(Erased),
                    }),
                    Some(syntax_tree.span()),
                );

                Ok(Expression::Value(Value::Register(register_id)))
            }
        }
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>,
    > Bind<&syntax_tree::expression::Prefixable> for Binder<'t, S, RO, TO>
{
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::Prefixable,
        config: Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        match syntax_tree {
            syntax_tree::expression::Prefixable::Postfixable(syn) => {
                self.bind(syn, config, handler)
            }
            syntax_tree::expression::Prefixable::Prefix(syn) => {
                self.bind(syn, config, handler)
            }
        }
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>,
    > Binder<'t, S, RO, TO>
{
    fn bind_variant_call(
        &mut self,
        arguments: Vec<(Span, Value<infer::Model>)>,
        variant: resolution::Variant<infer::Model>,
        syntax_tree_span: Span,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<ID<Register<infer::Model>>, TypeOfError<infer::Model>> {
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
                        span: Some(syntax_tree_span.clone()),
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
                        self.type_of_value(&argument.1)?,
                        Expected::Known(variant_type),
                        argument.0,
                        &self.create_handler_wrapper(handler),
                    );

                    argument.1
                }
            };

            Ok(self.create_register_assignmnet(
                Assignment::Variant(Variant {
                    variant_id: variant.variant,
                    associated_value: Some(associated_value),
                    generic_arguments: variant.generic_arguments,
                }),
                Some(syntax_tree_span),
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
                Some(syntax_tree_span),
            ))
        }
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>,
    > Binder<'t, S, RO, TO>
{
    fn bind_function_call(
        &mut self,
        arguments: &[(Span, Value<infer::Model>)],
        callable_id: CallableID,
        instantiation: Instantiation<infer::Model>,
        syntax_tree_span: Span,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<ID<Register<infer::Model>>, TypeOfError<infer::Model>> {
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
                self.type_of_value(argument_value)?,
                Expected::Known(parameter_ty),
                argument_span.clone(),
                handler,
            );

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
                span: Some(syntax_tree_span.clone()),
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
            Some(syntax_tree_span),
        ))
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>,
    > Binder<'t, S, RO, TO>
{
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

                Ok(Expression::Value(value))
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

                Ok(Expression::Value(value))
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
                            Type::Symbol(Symbol {
                                id: match adt_implementation.implemented_id() {
                                    AdtID::Struct(id) => SymbolID::Struct(id),
                                    AdtID::Enum(id) => SymbolID::Enum(id),
                                },
                                generic_arguments: parent_generic_arguments,
                            }),
                            Expected::Known(Type::Symbol(Symbol {
                                id: match adt_implementation.implemented_id() {
                                    AdtID::Struct(id) => SymbolID::Struct(id),
                                    AdtID::Enum(id) => SymbolID::Enum(id),
                                },
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
                            handler,
                        );

                        assert!(instantiation
                            .append_from_generic_arguments(
                                generic_arguments,
                                adt_implementation_id.into(),
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

                Ok(Expression::Value(value))
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

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>,
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
                let arguments = call
                    .arguments()
                    .iter()
                    .flat_map(ConnectedList::elements)
                    .map(|arg| {
                        self.bind_value_or_error(&**arg, handler)
                            .map(|x| (arg.span(), x))
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                match &**syntax_tree.postfixable() {
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

            syntax_tree::expression::PostfixOperator::Cast(cast_syn) => {
                // resolve the type
                let mut cast_type = self
                    .table
                    .resolve_type(
                        cast_syn.r#type(),
                        self.current_site,
                        resolution::Config {
                            elided_lifetime_provider: Some(
                                &mut InferenceProvider,
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
                                .into_constraint_model(cast_type)
                                .unwrap(),
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
                    type_of_value,
                    Expected::Constraint(Constraint::Number),
                    syntax_tree.postfixable().span(),
                    handler,
                );

                Ok(Expression::Value(Value::Register(
                    self.create_register_assignmnet(
                        Assignment::Cast(Cast { value, r#type: cast_type }),
                        Some(syntax_tree.span()),
                    ),
                )))
            }

            syntax_tree::expression::PostfixOperator::Access(access_syn) => {
                let required_qualifier = match config.target {
                    Target::Statement | Target::Value => Qualifier::Immutable,
                    Target::Address { expected_qualifier } => {
                        expected_qualifier
                    }
                };

                let (address, mut access_type) = match access_syn.operator() {
                    // expect the lvalue address
                    syntax_tree::expression::AccessOperator::Dot(_) => {
                        let address = self.bind_as_address(
                            &**syntax_tree.postfixable(),
                            required_qualifier,
                            true,
                            handler,
                        )?;

                        let address_type = self.type_of_address(&address)?;

                        (address, address_type)
                    }

                    // expect the rvalue reference value
                    syntax_tree::expression::AccessOperator::Arrow(_, _) => {
                        let value = self.bind_value_or_error(
                            &**syntax_tree.postfixable(),
                            handler,
                        )?;

                        let mut type_of_value = self.type_of_value(&value)?;
                        type_of_value = simplify::simplify(
                            &type_of_value,
                            &self.create_environment(),
                        )
                        .result;

                        let type_of_value = match type_of_value {
                            Type::Reference(reference) => reference,
                            found_type => {
                                self.create_handler_wrapper(handler).receive(
                                    Box::new(CannotDereference {
                                        found_type: self
                                            .inference_context
                                            .into_constraint_model(found_type)
                                            .unwrap(),
                                        span: syntax_tree.span(),
                                    }),
                                );
                                return Err(Error::Semantic(SemanticError(
                                    syntax_tree.span(),
                                )));
                            }
                        };

                        // soft error, keep going
                        if type_of_value.qualifier < required_qualifier {
                            self.create_handler_wrapper(handler).receive(
                                Box::new(MismatchedReferenceQualifier {
                                    found_reference_type: self
                                        .inference_context
                                        .into_constraint_model(Type::Reference(
                                            type_of_value.clone(),
                                        ))
                                        .unwrap(),
                                    expected_qualifier: required_qualifier,
                                    span: syntax_tree.span(),
                                }),
                            );
                        }

                        (
                            Address::Memory(address::Memory::ReferenceValue(
                                value,
                            )),
                            *type_of_value.pointee,
                        )
                    }
                };

                access_type = simplify::simplify(
                    &access_type,
                    &self.create_environment(),
                )
                .result;

                let address = match access_syn.kind() {
                    // struct field access
                    syntax_tree::expression::AccessKind::Identifier(ident) => {
                        let struct_id = match access_type {
                            Type::Symbol(Symbol {
                                id: SymbolID::Struct(struct_id),
                                ..
                            }) => struct_id,

                            found_type => {
                                self.create_handler_wrapper(handler).receive(
                                    Box::new(ExpectStructType {
                                        span: syntax_tree.postfixable().span(),
                                        r#type: self
                                            .inference_context
                                            .into_constraint_model(found_type)
                                            .unwrap(),
                                    }),
                                );

                                return Err(Error::Semantic(SemanticError(
                                    syntax_tree.postfixable().span(),
                                )));
                            }
                        };

                        // find field id
                        let Some(field_id) = self
                            .table
                            .get(struct_id)
                            .unwrap()
                            .fields()
                            .get_id(ident.span.str())
                        else {
                            self.create_handler_wrapper(handler).receive(
                                Box::new(FieldNotFound {
                                    struct_id,
                                    identifier_span: ident.span.clone(),
                                }),
                            );

                            return Err(Error::Semantic(SemanticError(
                                ident.span.clone(),
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
                            .is_accessible_from(
                                self.current_site,
                                field_accessibility,
                            )
                            .unwrap()
                        {
                            self.create_handler_wrapper(handler).receive(
                                Box::new(FieldIsNotAccessible {
                                    field_id,
                                    struct_id,
                                    referring_site: self.current_site,
                                    referring_identifier_span: ident
                                        .span
                                        .clone(),
                                }),
                            );
                        }

                        Address::Field(address::Field {
                            struct_address: Box::new(address),
                            id: field_id,
                        })
                    }

                    syntax_tree::expression::AccessKind::Tuple(syn) => {
                        let tuple_ty = match access_type {
                            Type::Tuple(tuple_ty) => tuple_ty,
                            found_type => {
                                self.create_handler_wrapper(handler).receive(
                                    Box::new(TupleExpected {
                                        span: syntax_tree.postfixable().span(),
                                        r#type: self
                                            .inference_context
                                            .into_constraint_model(found_type)
                                            .unwrap(),
                                    }),
                                );

                                return Err(Error::Semantic(SemanticError(
                                    syntax_tree.postfixable().span(),
                                )));
                            }
                        };

                        // sanity check
                        if tuple_ty
                            .elements
                            .iter()
                            .filter(|x| x.is_unpacked)
                            .count()
                            > 1
                        {
                            return Err(Error::Semantic(SemanticError(
                                syntax_tree.span(),
                            )));
                        }

                        // used to chec if the index is past the unpacked index
                        let unpacked_index = tuple_ty
                            .elements
                            .iter()
                            .position(|x| x.is_unpacked);

                        let index =
                            match syn.index().span.str().parse::<usize>() {
                                Ok(number) => number,
                                Err(err) => match err.kind() {
                                    std::num::IntErrorKind::NegOverflow
                                    | std::num::IntErrorKind::PosOverflow => {
                                        self.create_handler_wrapper(handler)
                                            .receive(Box::new(
                                                TooLargeTupleIndex {
                                                    access_span: access_syn
                                                        .span(),
                                                },
                                            ));

                                        return Err(Error::Semantic(
                                            SemanticError(
                                                syn.index().span.clone(),
                                            ),
                                        ));
                                    }

                                    _ => {
                                        unreachable!()
                                    }
                                },
                            };

                        if index >= tuple_ty.elements.len() {
                            self.create_handler_wrapper(handler).receive(
                                Box::new(TupleIndexOutOfBOunds {
                                    access_span: access_syn.span(),
                                    tuple_type: self
                                        .inference_context
                                        .into_constraint_model(Type::Tuple(
                                            tuple_ty,
                                        ))
                                        .unwrap()
                                        .into_tuple()
                                        .unwrap(),
                                }),
                            );

                            return Err(Error::Semantic(SemanticError(
                                syn.index().span.clone(),
                            )));
                        }

                        if let Some(unpacked_index) = unpacked_index {
                            // can't access past the unpacked index
                            let pass_unpacked = if syn.minus().is_some() {
                                index
                                    >= (tuple_ty.elements.len()
                                        - unpacked_index
                                        - 1)
                            } else {
                                index >= unpacked_index
                            };

                            // report error
                            if pass_unpacked {
                                self.create_handler_wrapper(handler).receive(
                                    Box::new(CannotIndexPastUnpackedTuple {
                                        index_span: syn.span(),
                                        tuple_type: self
                                            .inference_context
                                            .into_constraint_model(Type::Tuple(
                                                tuple_ty,
                                            ))
                                            .unwrap()
                                            .into_tuple()
                                            .unwrap(),
                                    }),
                                );

                                return Err(Error::Semantic(SemanticError(
                                    syn.index().span.clone(),
                                )));
                            }
                        }

                        Address::Tuple(address::Tuple {
                            tuple_address: Box::new(address),
                            offset: if syn.minus().is_some() {
                                address::Offset::FromEnd(index)
                            } else {
                                address::Offset::FromStart(index)
                            },
                        })
                    }

                    syntax_tree::expression::AccessKind::Index(index) => {
                        let value = self.bind_value_or_error(
                            &**index.expression(),
                            handler,
                        )?;

                        if !matches!(access_type, Type::Array(_)) {
                            self.create_handler_wrapper(handler).receive(
                                Box::new(ExpectArray {
                                    span: syntax_tree.postfixable().span(),
                                    r#type: self
                                        .inference_context
                                        .into_constraint_model(access_type)
                                        .unwrap(),
                                }),
                            );

                            return Err(Error::Semantic(SemanticError(
                                syntax_tree.postfixable().span(),
                            )));
                        }

                        // expected a `usize` type
                        let index_ty = self.type_of_value(&value)?;
                        let _ = self.type_check(
                            index_ty,
                            Expected::Known(Type::Primitive(
                                r#type::Primitive::Usize,
                            )),
                            index.expression().span(),
                            handler,
                        );

                        Address::Index(address::Index {
                            array_address: Box::new(address),
                            indexing_value: value,
                        })
                    }
                };

                match config.target {
                    Target::Value => {
                        // will be optimized to move later
                        let register_id = self.create_register_assignmnet(
                            Assignment::Load(Load {
                                address,
                                kind: LoadKind::Move,
                            }),
                            Some(syntax_tree.span()),
                        );

                        Ok(Expression::Value(Value::Register(register_id)))
                    }

                    // qualifier should've been checked earlier
                    Target::Address { .. } => Ok(Expression::Address {
                        address,
                        span: syntax_tree.span(),
                    }),

                    Target::Statement => Ok(Expression::SideEffect),
                }
            }
        }
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>,
    > Bind<&syntax_tree::expression::Postfixable> for Binder<'t, S, RO, TO>
{
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::Postfixable,
        config: Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        match syntax_tree {
            syntax_tree::expression::Postfixable::Unit(syn) => {
                self.bind(syn, config, handler)
            }
            syntax_tree::expression::Postfixable::Postfix(syn) => {
                self.bind(syn, config, handler)
            }
        }
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>,
    > Bind<&syntax_tree::QualifiedIdentifier> for Binder<'t, S, RO, TO>
{
    #[allow(clippy::too_many_lines)]
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::QualifiedIdentifier,
        config: Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        let is_simple_identifier =
            syntax_tree.leading_scope_separator().is_none()
                && syntax_tree.rest().is_empty()
                && syntax_tree.first().generic_arguments().is_none();

        // search for the variable/parameter in the stack
        #[allow(clippy::unnecessary_operation)]
        'out: {
            if is_simple_identifier {
                let Some(name) = self
                    .stack
                    .search(syntax_tree.first().identifier().span.str())
                else {
                    break 'out;
                };

                let address = name.load_address.clone();

                match config.target {
                    Target::Statement | Target::Value => {
                        let register_id = self.create_register_assignmnet(
                            Assignment::Load(Load {
                                address,
                                kind: LoadKind::Move,
                            }),
                            Some(syntax_tree.span()),
                        );

                        return Ok(match config.target {
                            Target::Statement => Expression::SideEffect,
                            Target::Value => {
                                Expression::Value(Value::Register(register_id))
                            }
                            Target::Address { .. } => unreachable!(),
                        });
                    }
                    Target::Address { expected_qualifier, .. } => {
                        // report mutability error, soft error, keep going
                        if matches!(
                            expected_qualifier,
                            Qualifier::Mutable | Qualifier::Unique
                        ) && !name.mutable
                        {
                            self.create_handler_wrapper(handler).receive(
                                Box::new(MismatchedMutability {
                                    span: syntax_tree.span(),
                                }),
                            );
                        }

                        return Ok(Expression::Address {
                            address,
                            span: syntax_tree.span(),
                        });
                    }
                }
            }
        };

        let resolution = self
            .resolve_with_inference(syntax_tree, handler)
            .ok_or(Error::Semantic(SemanticError(syntax_tree.span())))?;

        match resolution {
            resolution::Resolution::Variant(variant_res) => {
                let variant = self.table.get(variant_res.variant).unwrap();

                // expected a variant type
                if variant.associated_type.is_some() {
                    self.create_handler_wrapper(handler).receive(Box::new(
                        error::ExpectAssociatedValue {
                            span: syntax_tree.span(),
                            variant_id: variant_res.variant,
                        },
                    ));
                }

                let associated_value = if let Some(associated_type) =
                    variant.associated_type.clone()
                {
                    let mut associated_type =
                        infer::Model::from_default_type(associated_type);

                    let instantiation = Instantiation::from_generic_arguments(
                        variant_res.generic_arguments.clone(),
                        variant.parent_enum_id().into(),
                        &self
                            .table
                            .get(variant.parent_enum_id())
                            .unwrap()
                            .generic_declaration
                            .parameters,
                    )
                    .unwrap();

                    instantiation::instantiate(
                        &mut associated_type,
                        &instantiation,
                    );

                    let associated_value =
                        Value::Literal(Literal::Error(literal::Error {
                            r#type: associated_type,
                            span: Some(syntax_tree.span()),
                        }));

                    Some(associated_value)
                } else {
                    None
                };

                let register_id = self.create_register_assignmnet(
                    Assignment::Variant(Variant {
                        variant_id: variant_res.variant,
                        associated_value,
                        generic_arguments: variant_res.generic_arguments,
                    }),
                    Some(syntax_tree.span()),
                );

                Ok(Expression::Value(Value::Register(register_id)))
            }

            resolution::Resolution::Generic(resolution::Generic {
                id: resolution::GenericID::Constant(_id),
                generic_arguments: _,
            }) => todo!("handle constant evaluation"),

            resolution::Resolution::Generic(resolution::Generic {
                id: resolution::GenericID::TraitImplementationConstant(_id),
                generic_arguments: _,
            }) => todo!("handle constant evaluation"),

            resolution => {
                self.create_handler_wrapper(handler).receive(Box::new(
                    error::SymbolCannotBeUsedAsAnExpression {
                        span: syntax_tree.span(),
                        symbol: resolution.global_id(),
                    },
                ));

                Err(Error::Semantic(SemanticError(syntax_tree.span())))
            }
        }
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>,
    > Bind<&syntax_tree::expression::Struct> for Binder<'t, S, RO, TO>
{
    #[allow(clippy::too_many_lines)]
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::Struct,
        _: Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        let resolution = self
            .resolve_with_inference(syntax_tree.qualified_identifier(), handler)
            .ok_or(Error::Semantic(SemanticError(syntax_tree.span())))?;

        // must be struct type
        let resolution::Resolution::Generic(resolution::Generic {
            id: resolution::GenericID::Struct(struct_id),
            generic_arguments,
        }) = resolution
        else {
            self.create_handler_wrapper(handler).receive(Box::new(
                error::ExpectStruct { span: syntax_tree.span() },
            ));
            return Err(Error::Semantic(SemanticError(syntax_tree.span())));
        };

        let instantiation = Instantiation::from_generic_arguments(
            generic_arguments.clone(),
            struct_id.into(),
            &self.table.get(struct_id).unwrap().generic_declaration.parameters,
        )
        .unwrap();

        let mut initializers_by_field_id =
            HashMap::<ID<Field>, (Value<infer::Model>, Span)>::new();

        for field_syn in syntax_tree
            .field_initializers()
            .as_ref()
            .into_iter()
            .flat_map(ConnectedList::elements)
        {
            // get the field ID by name
            let value =
                self.bind_value_or_error(&**field_syn.expression(), handler)?;

            let (field_id, field_ty, field_accessibility) = {
                let struct_sym = self.table.get(struct_id).unwrap();
                let Some(field_id) = struct_sym
                    .fields()
                    .get_id(field_syn.identifier().span.str())
                else {
                    self.create_handler_wrapper(handler).receive(Box::new(
                        FieldNotFound {
                            identifier_span: field_syn
                                .identifier()
                                .span
                                .clone(),
                            struct_id,
                        },
                    ));
                    continue;
                };

                let field = struct_sym.fields().get(field_id).unwrap();
                let mut field_ty =
                    infer::Model::from_default_type(field.r#type.clone());

                instantiation::instantiate(&mut field_ty, &instantiation);

                (field_id, field_ty, field.accessibility)
            };

            // field accessibility check
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
                        referring_identifier_span: field_syn
                            .identifier()
                            .span
                            .clone(),
                    },
                ));
            }

            // type check the field
            let _ = self.type_check(
                self.type_of_value(&value)?,
                Expected::Known(field_ty),
                field_syn.expression().span(),
                handler,
            );

            match initializers_by_field_id.entry(field_id) {
                Entry::Occupied(entry) => self
                    .create_handler_wrapper(handler)
                    .receive(Box::new(DuplicatedFieldInitialization {
                        field_id,
                        struct_id,
                        prior_initialization_span: entry.get().1.clone(),
                        duplicate_initialization_span: field_syn.span(),
                    })),
                Entry::Vacant(entry) => {
                    entry.insert((value, field_syn.span()));
                }
            }
        }

        // check for uninitialized fields
        let struct_sym = self.table.get(struct_id).unwrap();
        let uninitialized_fields = struct_sym
            .fields()
            .ids()
            .filter(|field_id| !initializers_by_field_id.contains_key(field_id))
            .collect::<HashSet<_>>();

        if !uninitialized_fields.is_empty() {
            self.create_handler_wrapper(handler).receive(Box::new(
                error::UninitializedFields {
                    struct_id,
                    uninitialized_fields,
                    struct_expression_span: syntax_tree.span(),
                },
            ));
        }

        let value = Value::Register(
            self.create_register_assignmnet(
                Assignment::Struct(Struct {
                    struct_id,
                    initializers_by_field_id: initializers_by_field_id
                        .into_iter()
                        .map(|(id, (register_id, _))| (id, register_id))
                        .collect(),
                    generic_arguments,
                }),
                Some(syntax_tree.span()),
            ),
        );

        Ok(Expression::Value(value))
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>,
    > Bind<&token::String> for Binder<'t, S, RO, TO>
{
    fn bind(
        &mut self,
        syntax_tree: &token::String,
        _: Config,
        _: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        let value = if let Some(value) = &syntax_tree.value {
            let value = value.as_bytes().to_vec();

            Value::Literal(Literal::String(literal::String {
                value,
                span: Some(syntax_tree.span()),
            }))
        } else {
            let constant_inference = InferenceVariable::new();

            assert!(self
                .inference_context
                .register::<Constant<_>>(constant_inference, NoConstraint));

            Value::Literal(Literal::Error(literal::Error {
                // &'static [uint8: len]
                r#type: Type::Reference(r#type::Reference {
                    qualifier: Qualifier::Immutable,
                    lifetime: Lifetime::Static,
                    pointee: Box::new(Type::Array(r#type::Array {
                        length: Constant::Inference(constant_inference),
                        r#type: Box::new(Type::Primitive(
                            r#type::Primitive::Uint8,
                        )),
                    })),
                }),
                span: Some(syntax_tree.span()),
            }))
        };

        Ok(Expression::Value(value))
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>,
    > Bind<&token::Character> for Binder<'t, S, RO, TO>
{
    fn bind(
        &mut self,
        syntax_tree: &token::Character,
        _: Config,
        _: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        let inference_variable = InferenceVariable::new();

        assert!(self.inference_context.register::<Type<_>>(
            inference_variable,
            Constraint::UnsignedInteger
        ));

        let value = syntax_tree.value.map_or_else(
            || {
                Value::Literal(Literal::Error(literal::Error {
                    r#type: Type::Inference(inference_variable),
                    span: Some(syntax_tree.span()),
                }))
            },
            |character| {
                Value::Literal(Literal::Character(literal::Character {
                    character,
                    r#type: Type::Inference(inference_variable),
                    span: Some(syntax_tree.span()),
                }))
            },
        );

        Ok(Expression::Value(value))
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>,
    > Bind<&syntax_tree::expression::Parenthesized> for Binder<'t, S, RO, TO>
{
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::Parenthesized,
        config: Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        let bind_as_tuple =
            syntax_tree.expression().as_ref().map_or(true, |x| {
                !x.rest().is_empty()
                    || x.trailing_separator().is_some()
                    || x.first().ellipsis().is_some()
            });

        if bind_as_tuple {
            let mut elements = Vec::new();

            for element_syn in syntax_tree
                .expression()
                .as_ref()
                .into_iter()
                .flat_map(ConnectedList::elements)
            {
                let element = self.bind_value_or_error(
                    &**element_syn.expression(),
                    handler,
                )?;

                elements.push(register::TupleElement {
                    value: element,
                    is_unpacked: element_syn.ellipsis().is_some(),
                });
            }

            let tuple_type = simplify::simplify(
                &Type::<infer::Model>::Tuple(r#type::Tuple {
                    elements: elements
                        .iter()
                        .map(|x| {
                            self.type_of_value(&x.value).map(|ty| {
                                term::TupleElement {
                                    term: ty,
                                    is_unpacked: x.is_unpacked,
                                }
                            })
                        })
                        .collect::<Result<Vec<_>, _>>()?,
                }),
                &self.create_environment(),
            )
            .result
            .into_tuple()
            .unwrap();

            // more than one unpacked elements
            if tuple_type.elements.iter().filter(|x| x.is_unpacked).count() > 1
            {
                self.create_handler_wrapper(handler).receive(Box::new(
                    MoreThanOneUnpackedInTupleExpression {
                        span: syntax_tree.span(),
                        r#type: self
                            .inference_context
                            .into_constraint_model(Type::Tuple(tuple_type))
                            .unwrap(),
                    },
                ));

                Err(Error::Semantic(SemanticError(syntax_tree.span())))
            } else {
                let value = if elements.is_empty() {
                    // return unit Tuple
                    Value::Literal(Literal::Unit(literal::Unit {
                        span: Some(syntax_tree.span()),
                    }))
                } else {
                    let create_register_assignmnet = self
                        .create_register_assignmnet(
                            Assignment::Tuple(register::Tuple { elements }),
                            Some(syntax_tree.span()),
                        );
                    Value::Register(create_register_assignmnet)
                };

                Ok(Expression::Value(value))
            }
        } else {
            // propagate the target
            self.bind(
                &**syntax_tree
                    .expression()
                    .as_ref()
                    .unwrap()
                    .first()
                    .expression(),
                config,
                handler,
            )
        }
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>,
    > Bind<&syntax_tree::expression::Phantom> for Binder<'t, S, RO, TO>
{
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::Phantom,
        _: Config,
        _: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        let inference = InferenceVariable::new();

        assert!(self
            .inference_context
            .register::<Type<_>>(inference, Constraint::All(false)));

        Ok(Expression::Value(Value::Literal(Literal::Phantom(
            literal::Phantom {
                r#type: Type::Inference(inference),
                span: Some(syntax_tree.span()),
            },
        ))))
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>,
    > Bind<&syntax_tree::expression::Array> for Binder<'t, S, RO, TO>
{
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::Array,
        _: Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        let Some(arguments) = syntax_tree.arguments() else {
            let inference = InferenceVariable::new();

            assert!(self
                .inference_context
                .register::<Type<_>>(inference, Constraint::All(false)));

            let register_id = self.create_register_assignmnet(
                Assignment::Array(Array {
                    elements: Vec::new(),
                    element_type: Type::Inference(inference),
                }),
                Some(syntax_tree.span()),
            );

            return Ok(Expression::Value(Value::Register(register_id)));
        };

        let mut elements = Vec::new();

        for element_syn in arguments.elements().map(|x| &**x) {
            elements.push(self.bind_value_or_error(element_syn, handler)?);
        }

        // do type check against the first elemen
        let mut iter = elements.iter();

        let first_element = iter.next().unwrap();
        let first_ty = self.type_of_value(first_element)?;

        for element in iter {
            let element_ty = self.type_of_value(element)?;

            let _ = self.type_check(
                element_ty,
                Expected::Known(first_ty.clone()),
                match element {
                    Value::Register(register_id) => self
                        .intermediate_representation
                        .registers
                        .get(*register_id)
                        .unwrap()
                        .span
                        .clone()
                        .unwrap(),
                    Value::Literal(literal) => literal.span().cloned().unwrap(),
                },
                handler,
            );
        }

        let value = Value::Register(self.create_register_assignmnet(
            Assignment::Array(Array { elements, element_type: first_ty }),
            Some(syntax_tree.span()),
        ));

        Ok(Expression::Value(value))
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>,
    > Bind<&syntax_tree::expression::Unit> for Binder<'t, S, RO, TO>
{
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::Unit,
        config: Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        match syntax_tree {
            syntax_tree::expression::Unit::Boolean(syn) => {
                self.bind(syn, config, handler)
            }
            syntax_tree::expression::Unit::Numeric(syn) => {
                self.bind(syn, config, handler)
            }
            syntax_tree::expression::Unit::QualifiedIdentifier(syn) => {
                self.bind(syn, config, handler)
            }
            syntax_tree::expression::Unit::Parenthesized(syn) => {
                self.bind(syn, config, handler)
            }
            syntax_tree::expression::Unit::Struct(syn) => {
                self.bind(syn, config, handler)
            }
            syntax_tree::expression::Unit::Array(syn) => {
                self.bind(syn, config, handler)
            }
            syntax_tree::expression::Unit::Phantom(syn) => {
                self.bind(syn, config, handler)
            }
            syntax_tree::expression::Unit::String(syn) => {
                self.bind(syn, config, handler)
            }
            syntax_tree::expression::Unit::Character(syn) => {
                self.bind(syn, config, handler)
            }
        }
    }
}

enum BinaryNode<'a> {
    Binary(Box<BinaryTree<'a>>),
    Expression(&'a syntax_tree::expression::Prefixable),
}

impl SourceElement for BinaryNode<'_> {
    fn span(&self) -> Span {
        match self {
            BinaryNode::Binary(tree) => tree.span(),
            BinaryNode::Expression(exp) => exp.span(),
        }
    }
}

struct BinaryTree<'a> {
    left: BinaryNode<'a>,
    right: BinaryNode<'a>,
    operator: &'a syntax_tree::expression::BinaryOperator,
}

impl SourceElement for BinaryTree<'_> {
    fn span(&self) -> Span {
        self.left.span().join(&self.right.span()).unwrap()
    }
}

const fn operator_precedence(
    operator: &syntax_tree::expression::BinaryOperator,
) -> u32 {
    use syntax_tree::expression::BinaryOperator;
    match operator {
        BinaryOperator::Multiply(_)
        | BinaryOperator::Divide(_)
        | BinaryOperator::Modulo(_) => 10,

        BinaryOperator::Add(_) | BinaryOperator::Subtract(_) => 9,

        BinaryOperator::BitwiseLeftShift(..)
        | BinaryOperator::BitwiseRightShift(..) => 8,

        BinaryOperator::LessThan(_)
        | BinaryOperator::LessThanOrEqual(..)
        | BinaryOperator::GreaterThan(_)
        | BinaryOperator::GreaterThanOrEqual(..) => 7,

        BinaryOperator::Equal(..) | BinaryOperator::NotEqual(..) => 6,

        BinaryOperator::BitwiseAnd(_) => 5,

        BinaryOperator::BitwiseXor(_) => 4,

        BinaryOperator::BitwiseOr(_) => 3,

        BinaryOperator::LogicalAnd(_) => 2,

        BinaryOperator::LogicalOr(_) => 1,

        BinaryOperator::Assign(_)
        | BinaryOperator::CompoundAdd(..)
        | BinaryOperator::CompoundSubtract(..)
        | BinaryOperator::CompoundMultiply(..)
        | BinaryOperator::CompoundDivide(..)
        | BinaryOperator::CompoundModulo(..)
        | BinaryOperator::CompoundBitwiseAnd(..)
        | BinaryOperator::CompoundBitwiseOr(..)
        | BinaryOperator::CompoundBitwiseXor(..)
        | BinaryOperator::CompoundBitwiseLeftShift(..)
        | BinaryOperator::CompoundBitwiseRightShift(..) => 0,
    }
}

fn to_binary_tree(syntax_tree: &syntax_tree::expression::Binary) -> BinaryNode {
    let mut first = BinaryNode::Expression(syntax_tree.first());
    let mut expressions = syntax_tree
        .chain()
        .iter()
        .map(|(op, exp)| (op, Some(BinaryNode::Expression(exp))))
        .collect::<Vec<_>>();

    while !expressions.is_empty() {
        let candidate_operator_precedence = expressions
            .iter()
            .map(|(op, _)| operator_precedence(op))
            .max()
            .unwrap();

        // every operators except the assignments are left associative
        let is_left_associative = candidate_operator_precedence != 1;

        let fold_index = if is_left_associative {
            expressions.iter().position(|(op, _)| {
                operator_precedence(op) == candidate_operator_precedence
            })
        } else {
            expressions.iter().rposition(|(op, _)| {
                operator_precedence(op) == candidate_operator_precedence
            })
        }
        .unwrap();

        // replace the first expression with the binary tree
        if fold_index == 0 {
            let (operator, right_expression) = expressions.remove(0);

            // replace the first expression with the binary tree
            first = BinaryNode::Binary(Box::new(BinaryTree {
                left: first,
                right: right_expression.unwrap(),
                operator,
            }));
        } else {
            let (operator, right_expression) = expressions.remove(fold_index);

            // replace the first expression with the binary tree
            expressions[fold_index - 1].1 =
                Some(BinaryNode::Binary(Box::new(BinaryTree {
                    left: expressions[fold_index - 1].1.take().unwrap(),
                    right: right_expression.unwrap(),
                    operator,
                })));
        }
    }

    first
}

const fn into_binary_operator(
    syntax_tree: &syntax_tree::expression::BinaryOperator,
) -> Result<(BinaryOperator, bool), &syntax_tree::expression::BinaryOperator> {
    use syntax_tree::expression::BinaryOperator as BinOpSyn;

    use crate::ir::value::register::{
        ArithmeticOperator as ArithOp, BinaryOperator as BinOp,
        BitwiseOperator as BitOp, RelationalOperator as RelaOp,
    };

    match syntax_tree {
        BinOpSyn::Add(_) => Ok((BinOp::Arithmetic(ArithOp::Add), false)),
        BinOpSyn::Subtract(_) => {
            Ok((BinOp::Arithmetic(ArithOp::Subtract), false))
        }
        BinOpSyn::Multiply(_) => {
            Ok((BinOp::Arithmetic(ArithOp::Multiply), false))
        }
        BinOpSyn::Divide(_) => Ok((BinOp::Arithmetic(ArithOp::Divide), false)),
        BinOpSyn::Modulo(_) => Ok((BinOp::Arithmetic(ArithOp::Modulo), false)),

        BinOpSyn::Equal(_, _) => Ok((BinOp::Relational(RelaOp::Equal), false)),
        BinOpSyn::NotEqual(_, _) => {
            Ok((BinOp::Relational(RelaOp::NotEqual), false))
        }
        BinOpSyn::LessThan(_) => {
            Ok((BinOp::Relational(RelaOp::LessThan), false))
        }
        BinOpSyn::LessThanOrEqual(_, _) => {
            Ok((BinOp::Relational(RelaOp::LessThanOrEqual), false))
        }
        BinOpSyn::GreaterThan(_) => {
            Ok((BinOp::Relational(RelaOp::GreaterThan), false))
        }
        BinOpSyn::GreaterThanOrEqual(_, _) => {
            Ok((BinOp::Relational(RelaOp::GreaterThanOrEqual), false))
        }

        BinOpSyn::BitwiseAnd(_) => Ok((BinOp::Bitwise(BitOp::And), false)),
        BinOpSyn::BitwiseOr(_) => Ok((BinOp::Bitwise(BitOp::Or), false)),
        BinOpSyn::BitwiseXor(_) => Ok((BinOp::Bitwise(BitOp::Xor), false)),
        BinOpSyn::BitwiseLeftShift(_, _) => {
            Ok((BinOp::Bitwise(BitOp::LeftShift), false))
        }
        BinOpSyn::BitwiseRightShift(_, _) => {
            Ok((BinOp::Bitwise(BitOp::RightShift), false))
        }

        BinOpSyn::CompoundAdd(_, _) => {
            Ok((BinOp::Arithmetic(ArithOp::Add), true))
        }
        BinOpSyn::CompoundSubtract(_, _) => {
            Ok((BinOp::Arithmetic(ArithOp::Subtract), true))
        }
        BinOpSyn::CompoundMultiply(_, _) => {
            Ok((BinOp::Arithmetic(ArithOp::Multiply), true))
        }
        BinOpSyn::CompoundDivide(_, _) => {
            Ok((BinOp::Arithmetic(ArithOp::Divide), true))
        }
        BinOpSyn::CompoundModulo(_, _) => {
            Ok((BinOp::Arithmetic(ArithOp::Modulo), true))
        }

        BinOpSyn::CompoundBitwiseAnd(_, _) => {
            Ok((BinOp::Bitwise(BitOp::And), true))
        }
        BinOpSyn::CompoundBitwiseOr(_, _) => {
            Ok((BinOp::Bitwise(BitOp::Or), true))
        }
        BinOpSyn::CompoundBitwiseLeftShift(_, _, _) => {
            Ok((BinOp::Bitwise(BitOp::LeftShift), true))
        }
        BinOpSyn::CompoundBitwiseRightShift(_, _, _) => {
            Ok((BinOp::Bitwise(BitOp::RightShift), true))
        }
        BinOpSyn::CompoundBitwiseXor(_, _) => {
            Ok((BinOp::Bitwise(BitOp::Xor), true))
        }

        BinOpSyn::Assign(_)
        | BinOpSyn::LogicalAnd(_)
        | BinOpSyn::LogicalOr(_) => Err(syntax_tree),
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>,
    > Binder<'t, S, RO, TO>
{
    fn bind_assignment(
        &mut self,
        tree: &BinaryTree,
        config: Config,
        lhs_address: Address<infer::Model>,
        rhs_value: Value<infer::Model>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        let lhs_ty = self.type_of_address(&lhs_address)?;
        let rhs_ty = self.type_of_value(&rhs_value)?;

        let _ = self.type_check(
            rhs_ty,
            Expected::Known(lhs_ty),
            tree.right.span(),
            handler,
        );

        let _ =
            self.current_block_mut().insert_instruction(Instruction::Store(
                Store { address: lhs_address.clone(), value: rhs_value },
            ));

        Ok(match config.target {
            Target::Value => {
                let register_id = self.create_register_assignmnet(
                    Assignment::Load(Load {
                        address: lhs_address,
                        kind: LoadKind::Move,
                    }),
                    Some(tree.span()),
                );

                Expression::Value(Value::Register(register_id))
            }

            // qualifier is not checked here since the address is already bound
            // as Qualifier::Unique, which has the highest priority
            Target::Address { .. } => {
                Expression::Address { address: lhs_address, span: tree.span() }
            }

            Target::Statement => Expression::SideEffect,
        })
    }

    #[allow(clippy::too_many_lines)]
    fn bind_normal_binary(
        &mut self,
        syntax_tree: &BinaryTree,
        config: Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        let (op, is_compound) =
            into_binary_operator(syntax_tree.operator).unwrap();

        let (lhs_address, lhs_value) = 'out: {
            if is_compound {
                let lhs_address = match self.bind_as_address(
                    &syntax_tree.left,
                    Qualifier::Unique,
                    false,
                    handler,
                ) {
                    Ok(address) => address,

                    Err(Error::Semantic(SemanticError(span))) => {
                        let inference = self.create_type_inference(
                            r#type::Constraint::All(false),
                        );

                        break 'out (
                            None,
                            Value::Literal(Literal::Error(literal::Error {
                                r#type: Type::Inference(inference),
                                span: Some(span),
                            })),
                        );
                    }

                    Err(Error::Internal(internal_error)) => {
                        return Err(Error::Internal(internal_error))
                    }
                };

                let lhs_register = self.create_register_assignmnet(
                    Assignment::Load(Load {
                        address: lhs_address.clone(),
                        kind: LoadKind::Move,
                    }),
                    Some(syntax_tree.left.span()),
                );

                (Some(lhs_address), Value::Register(lhs_register))
            } else {
                (None, self.bind_value_or_error(&syntax_tree.left, handler)?)
            }
        };

        let rhs_value =
            self.bind_value_or_error(&syntax_tree.right, handler)?;

        let lhs_register_ty = self.type_of_value(&lhs_value)?;

        // left shift and right shift doesn't necessarily require the same type
        // for both operands
        if !matches!(
            op,
            BinaryOperator::Bitwise(
                BitwiseOperator::LeftShift | BitwiseOperator::RightShift
            )
        ) {
            let rhs_register_ty = self.type_of_value(&rhs_value)?;

            let _ = self.type_check(
                rhs_register_ty,
                Expected::Known(lhs_register_ty.clone()),
                syntax_tree.right.span(),
                handler,
            );
        }

        match op {
            BinaryOperator::Arithmetic(arith_op) => {
                let expected_constraints = match arith_op {
                    ArithmeticOperator::Add
                    | ArithmeticOperator::Subtract
                    | ArithmeticOperator::Multiply
                    | ArithmeticOperator::Divide => r#type::Constraint::Number,

                    ArithmeticOperator::Modulo => r#type::Constraint::Integer,
                };

                let _ = self.type_check(
                    lhs_register_ty,
                    Expected::Constraint(expected_constraints),
                    syntax_tree.span(),
                    handler,
                );
            }
            BinaryOperator::Relational(_) => {
                let lhs_register_ty = simplify::simplify(
                    &lhs_register_ty,
                    &self.create_environment(),
                )
                .result;

                let valid = match lhs_register_ty {
                    Type::Primitive(_) => true,
                    Type::Inference(inference) => {
                        let constraint_id = *self
                            .inference_context
                            .get_inference(inference)
                            .unwrap()
                            .as_inferring()
                            .unwrap();

                        let constraint = *self
                            .inference_context
                            .get_constraint::<Type<_>>(constraint_id)
                            .unwrap();

                        match constraint {
                            Constraint::Number
                            | Constraint::Integer
                            | Constraint::SignedInteger
                            | Constraint::Signed
                            | Constraint::UnsignedInteger
                            | Constraint::Floating => true,

                            Constraint::All(_) => false,
                        }
                    }
                    _ => false,
                };

                if !valid {
                    self.create_handler_wrapper(handler).receive(Box::new(
                        InvalidRelationalOperation {
                            found_type: self
                                .inference_context
                                .into_constraint_model(lhs_register_ty)
                                .unwrap(),
                            span: syntax_tree.span(),
                        },
                    ));
                }
            }
            BinaryOperator::Bitwise(bitwise) => match bitwise {
                BitwiseOperator::And
                | BitwiseOperator::Or
                | BitwiseOperator::Xor => {
                    let lhs_register_ty = simplify::simplify(
                        &lhs_register_ty,
                        &self.create_environment(),
                    )
                    .result;

                    let valid = match lhs_register_ty {
                        Type::Primitive(_) => true,
                        Type::Inference(inference) => {
                            let constraint_id = *self
                                .inference_context
                                .get_inference(inference)
                                .unwrap()
                                .as_inferring()
                                .unwrap();

                            let constraint = *self
                                .inference_context
                                .get_constraint::<Type<_>>(constraint_id)
                                .unwrap();

                            match constraint {
                                Constraint::Number
                                | Constraint::Integer
                                | Constraint::SignedInteger
                                | Constraint::UnsignedInteger
                                | Constraint::Signed => true,

                                Constraint::Floating | Constraint::All(_) => {
                                    false
                                }
                            }
                        }
                        _ => false,
                    };

                    if !valid {
                        self.create_handler_wrapper(handler).receive(Box::new(
                            InvalidRelationalOperation {
                                found_type: self
                                    .inference_context
                                    .into_constraint_model(lhs_register_ty)
                                    .unwrap(),
                                span: syntax_tree.span(),
                            },
                        ));
                    }
                }

                BitwiseOperator::LeftShift | BitwiseOperator::RightShift => {
                    let expected_constraints = r#type::Constraint::Integer;

                    let _ = self.type_check(
                        lhs_register_ty,
                        Expected::Constraint(expected_constraints),
                        syntax_tree.left.span(),
                        handler,
                    );

                    let rhs_value_ty = self.type_of_value(&rhs_value)?;

                    let _ = self.type_check(
                        rhs_value_ty,
                        Expected::Constraint(expected_constraints),
                        syntax_tree.right.span(),
                        handler,
                    );
                }
            },
        }

        let binary_register = self.create_register_assignmnet(
            Assignment::Binary(Binary {
                operator: op,
                lhs: lhs_value,
                rhs: rhs_value,
            }),
            Some(syntax_tree.span()),
        );

        if let (Some(lhs_address), true) = (lhs_address, is_compound) {
            self.bind_assignment(
                syntax_tree,
                config,
                lhs_address,
                Value::Register(binary_register),
                handler,
            )
        } else {
            Ok(Expression::Value(Value::Register(binary_register)))
        }
    }
}

impl<
        'x,
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>,
    > Bind<&BinaryTree<'x>> for Binder<'t, S, RO, TO>
{
    #[allow(clippy::too_many_lines)]
    fn bind(
        &mut self,
        syntax_tree: &BinaryTree<'x>,
        config: Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        match syntax_tree.operator {
            syntax_tree::expression::BinaryOperator::Assign(_) => {
                let rhs =
                    self.bind_value_or_error(&syntax_tree.right, handler)?;
                let lhs = self.bind_as_address(
                    &syntax_tree.left,
                    Qualifier::Unique,
                    false,
                    handler,
                )?;

                self.bind_assignment(syntax_tree, config, lhs, rhs, handler)
            }

            syntax_tree::expression::BinaryOperator::LogicalAnd(_)
            | syntax_tree::expression::BinaryOperator::LogicalOr(_) => {
                let successor_block_id = self
                    .intermediate_representation
                    .control_flow_graph
                    .new_block();

                let lhs =
                    self.bind_value_or_error(&syntax_tree.left, handler)?;

                // must be a boolean
                let lhs_ty = self.type_of_value(&lhs)?;
                let _ = self.type_check(
                    lhs_ty,
                    Expected::Known(Type::Primitive(r#type::Primitive::Bool)),
                    syntax_tree.left.span(),
                    handler,
                );

                let true_block_id = self
                    .intermediate_representation
                    .control_flow_graph
                    .new_block();
                let false_block_id = self
                    .intermediate_representation
                    .control_flow_graph
                    .new_block();

                if let Err(InsertTerminatorError::InvalidBlockID(_)) = self
                    .intermediate_representation
                    .control_flow_graph
                    .insert_terminator(
                        self.current_block_id,
                        Terminator::Jump(Jump::Conditional(
                            instruction::ConditionalJump {
                                condition: lhs,
                                true_target: true_block_id,
                                false_target: false_block_id,
                            },
                        )),
                    )
                {
                    panic!("invalid block id");
                }

                let (true_branch, false_branch) = {
                    let result = self
                        .intermediate_representation
                        .scope_tree
                        .new_child_branch(
                            self.stack.current_scope().scope_id(),
                            NonZeroUsize::new(2).unwrap(),
                        )
                        .unwrap();

                    (result[0], result[1])
                };

                // true branch
                let (true_branch_value, true_branch_last_block_id) = {
                    self.current_block_id = true_block_id;

                    // push the scope
                    self.stack.push_scope(true_branch);
                    let _ = self.current_block_mut().insert_instruction(
                        Instruction::ScopePush(ScopePush(true_branch)),
                    );

                    let value = if matches!(
                        syntax_tree.operator,
                        syntax_tree::expression::BinaryOperator::LogicalOr(_)
                    ) {
                        Value::Literal(Literal::Boolean(Boolean {
                            value: true,
                            span: Some(syntax_tree.left.span()),
                        }))
                    } else {
                        let rhs = self
                            .bind_value_or_error(&syntax_tree.right, handler)?;

                        let rhs_ty = self.type_of_value(&rhs)?;
                        let _ = self.type_check(
                            rhs_ty,
                            Expected::Known(Type::Primitive(
                                r#type::Primitive::Bool,
                            )),
                            syntax_tree.right.span(),
                            handler,
                        );

                        rhs
                    };

                    // pop the scope
                    assert_eq!(
                        self.stack.pop_scope().map(|x| x.scope_id()),
                        Some(true_branch)
                    );
                    let _ = self.current_block_mut().insert_instruction(
                        Instruction::ScopePop(ScopePop(true_branch)),
                    );

                    // jump to the successor block
                    if let Err(InsertTerminatorError::InvalidBlockID(_)) = self
                        .intermediate_representation
                        .control_flow_graph
                        .insert_terminator(
                            self.current_block_id,
                            Terminator::Jump(Jump::Unconditional(
                                instruction::UnconditionalJump {
                                    target: successor_block_id,
                                },
                            )),
                        )
                    {
                        panic!("invalid block id");
                    }

                    (value, self.current_block_id)
                };

                // false block
                let (false_branch_value, false_branch_last_block_id) = {
                    self.current_block_id = false_block_id;

                    // push the scope
                    self.stack.push_scope(false_branch);
                    let _ = self.current_block_mut().insert_instruction(
                        Instruction::ScopePush(ScopePush(false_branch)),
                    );

                    let value = if matches!(
                        syntax_tree.operator,
                        syntax_tree::expression::BinaryOperator::LogicalAnd(_)
                    ) {
                        Value::Literal(Literal::Boolean(Boolean {
                            value: false,
                            span: Some(syntax_tree.left.span()),
                        }))
                    } else {
                        let rhs = self
                            .bind_value_or_error(&syntax_tree.right, handler)?;

                        let rhs_ty = self.type_of_value(&rhs)?;
                        let _ = self.type_check(
                            rhs_ty,
                            Expected::Known(Type::Primitive(
                                r#type::Primitive::Bool,
                            )),
                            syntax_tree.right.span(),
                            handler,
                        );

                        rhs
                    };

                    // pop the scope
                    assert_eq!(
                        self.stack.pop_scope().map(|x| x.scope_id()),
                        Some(false_branch)
                    );
                    let _ = self.current_block_mut().insert_instruction(
                        Instruction::ScopePop(ScopePop(false_branch)),
                    );

                    // jump to the successor block
                    if let Err(InsertTerminatorError::InvalidBlockID(_)) = self
                        .intermediate_representation
                        .control_flow_graph
                        .insert_terminator(
                            self.current_block_id,
                            Terminator::Jump(Jump::Unconditional(
                                instruction::UnconditionalJump {
                                    target: successor_block_id,
                                },
                            )),
                        )
                    {
                        panic!("invalid block id");
                    }

                    (value, self.current_block_id)
                };

                // set the current block to the successor block
                self.current_block_id = successor_block_id;

                // shouldn't have more than 2 predecessors
                assert!(self.current_block().predecessors().len() <= 2);

                let value = match self.current_block().predecessors().len() {
                    0 => {
                        // unreachable
                        Value::Literal(Literal::Unreachable(
                            literal::Unreachable {
                                r#type: Type::Primitive(
                                    r#type::Primitive::Bool,
                                ),
                                span: Some(syntax_tree.span()),
                            },
                        ))
                    }

                    1 => {
                        // only one predecessor
                        if self
                            .current_block()
                            .predecessors()
                            .contains(&true_branch_last_block_id)
                        {
                            true_branch_value
                        } else {
                            assert!(self
                                .current_block()
                                .predecessors()
                                .contains(&false_branch_last_block_id));

                            false_branch_value
                        }
                    }

                    2 => {
                        // both predecessors

                        let register_id = self.create_register_assignmnet(
                            Assignment::Phi(Phi {
                                r#type: Type::Primitive(
                                    r#type::Primitive::Bool,
                                ),
                                incoming_values: [
                                    (
                                        true_branch_last_block_id,
                                        true_branch_value,
                                    ),
                                    (
                                        false_branch_last_block_id,
                                        false_branch_value,
                                    ),
                                ]
                                .into_iter()
                                .collect(),
                            }),
                            Some(syntax_tree.span()),
                        );

                        Value::Register(register_id)
                    }

                    _ => unreachable!(),
                };

                Ok(Expression::Value(value))
            }

            _ => self.bind_normal_binary(syntax_tree, config, handler),
        }
    }
}

impl<
        'x,
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>,
    > Bind<&BinaryNode<'x>> for Binder<'t, S, RO, TO>
{
    fn bind(
        &mut self,
        syntax_tree: &BinaryNode<'x>,
        config: Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        match syntax_tree {
            BinaryNode::Binary(binary) => self.bind(&**binary, config, handler),
            BinaryNode::Expression(prefixable) => {
                self.bind(*prefixable, config, handler)
            }
        }
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>,
    > Bind<&syntax_tree::expression::Binary> for Binder<'t, S, RO, TO>
{
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::Binary,
        config: Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        // transform the syntax tree into a binary tree
        let binary_node = to_binary_tree(syntax_tree);

        self.bind(&binary_node, config, handler)
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>,
    > Bind<&syntax_tree::expression::Return> for Binder<'t, S, RO, TO>
{
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::Return,
        _: Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        let Some(callable_id) = CallableID::try_from(self.current_site).ok()
        else {
            self.create_handler_wrapper(handler).receive(Box::new(
                ReturnIsNotAllowed { span: syntax_tree.span() },
            ));

            return Err(Error::Semantic(SemanticError(syntax_tree.span())));
        };

        let callable = self.table.get_callable(callable_id).unwrap();
        let return_type =
            infer::Model::from_default_type(callable.return_type().clone());

        let value = syntax_tree.binary().as_ref().map_or_else(
            || {
                Ok(Value::Literal(Literal::Unit(Unit {
                    span: Some(syntax_tree.span()),
                })))
            },
            |syn| self.bind_value_or_error(syn, handler),
        )?;
        let value_ty = self.type_of_value(&value)?;

        // do type check
        let _ = self.type_check(
            value_ty,
            Expected::Known(return_type),
            syntax_tree.span(),
            handler,
        );

        // pop all the needed scopes
        for popping_scope in
            self.stack.scopes().iter().map(Scope::scope_id).rev()
        {
            let _ = self
                .intermediate_representation
                .control_flow_graph
                .get_block_mut(self.current_block_id)
                .unwrap()
                .insert_instruction(Instruction::ScopePop(ScopePop(
                    popping_scope,
                )));
        }

        // insert the return instruction
        if let Err(InsertTerminatorError::InvalidBlockID(_)) = self
            .intermediate_representation
            .control_flow_graph
            .insert_terminator(
                self.current_block_id,
                Terminator::Return(instruction::Return { value }),
            )
        {
            panic!("invalid block id");
        }

        let value =
            Value::Literal(Literal::Unreachable(literal::Unreachable {
                r#type: {
                    let inference = InferenceVariable::new();

                    assert!(self
                        .inference_context
                        .register(inference, Constraint::All(true)));

                    Type::Inference(inference)
                },
                span: Some(syntax_tree.span()),
            }));

        Ok(Expression::Value(value))
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>,
    > Binder<'t, S, RO, TO>
{
    /// Finds a [`ID<scope::Scope>`] to operate a control flow on based on the
    /// location and label.
    fn find_loop_scope_id(
        &self,
        control_flow: LoopControlFlow,
        label: Option<&token::Identifier>,
        syntax_tree_span: Span,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<ID<scope::Scope>, Error> {
        let mut loop_scope_id = None;

        // find the loop state
        for scope in self.stack.scopes().iter().rev() {
            let Some(get_loop_state) =
                self.loop_states_by_scope_id.get(&scope.scope_id())
            else {
                continue;
            };

            if let Some(label) = label.as_ref() {
                if get_loop_state.label.as_deref() != Some(label.span.str()) {
                    continue;
                }
            }

            loop_scope_id = Some(scope.scope_id());
            break;
        }

        // loop state not found report the error
        let Some(loop_scope_id) = loop_scope_id else {
            if let Some(label) = label {
                self.create_handler_wrapper(handler).receive(Box::new(
                    LoopWithGivenLabelNameNotFound { span: label.span.clone() },
                ));
            } else {
                self.create_handler_wrapper(handler).receive(Box::new(
                    LoopControlFlowOutsideLoop {
                        span: syntax_tree_span.clone(),
                        control_flow,
                    },
                ));
            };

            return Err(Error::Semantic(SemanticError(syntax_tree_span)));
        };

        Ok(loop_scope_id)
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>,
    > Bind<&syntax_tree::expression::Continue> for Binder<'t, S, RO, TO>
{
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::Continue,
        _: Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        let loop_scope_id = self.find_loop_scope_id(
            LoopControlFlow::Continue,
            syntax_tree.label().as_ref().map(Label::identifier),
            syntax_tree.span(),
            handler,
        )?;

        // pop all the needed scopes
        for popping_scope in self
            .stack
            .scopes()
            .iter()
            .rev()
            .map(Scope::scope_id)
            .take_while(|x| *x != loop_scope_id)
            .chain(std::iter::once(loop_scope_id))
        {
            let _ = self
                .intermediate_representation
                .control_flow_graph
                .get_block_mut(self.current_block_id)
                .unwrap()
                .insert_instruction(Instruction::ScopePop(ScopePop(
                    popping_scope,
                )));
        }

        // jump to the loop block
        if let Err(InsertTerminatorError::InvalidBlockID(_)) = self
            .intermediate_representation
            .control_flow_graph
            .insert_terminator(
                self.current_block_id,
                Terminator::Jump(Jump::Unconditional(UnconditionalJump {
                    target: self
                        .loop_states_by_scope_id
                        .get(&loop_scope_id)
                        .unwrap()
                        .loop_block_id,
                })),
            )
        {
            panic!("invalid block id");
        }

        let value = Value::Literal(Literal::Unreachable(Unreachable {
            r#type: {
                let inference = InferenceVariable::new();

                assert!(self
                    .inference_context
                    .register(inference, Constraint::All(true)));

                Type::Inference(inference)
            },
            span: Some(syntax_tree.span()),
        }));

        Ok(Expression::Value(value))
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>,
    > Bind<&syntax_tree::expression::Break> for Binder<'t, S, RO, TO>
{
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::Break,
        _: Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        let value = syntax_tree
            .binary()
            .as_ref()
            .map(|x| self.bind_value_or_error(x, handler))
            .transpose()?;

        let loop_scope_id = self.find_loop_scope_id(
            LoopControlFlow::Break,
            syntax_tree.label().as_ref().map(Label::identifier),
            syntax_tree.span(),
            handler,
        )?;

        let value_type = value.as_ref().map_or_else(
            || Ok(Type::Tuple(term::Tuple { elements: Vec::new() })),
            |x| self.type_of_value(x),
        )?;

        match &self.loop_states_by_scope_id.get(&loop_scope_id).unwrap().kind {
            // can only be unit type
            LoopKind::While => {
                let _ = self.type_check(
                    value_type,
                    Expected::Known(Type::Tuple(term::Tuple {
                        elements: Vec::new(),
                    })),
                    syntax_tree
                        .binary()
                        .as_ref()
                        .map_or_else(|| syntax_tree.span(), |x| x.span()),
                    handler,
                );
            }

            // can use any type
            LoopKind::Loop { break_type, .. } => {
                if let Some(break_type) = break_type {
                    let _ = self.type_check(
                        value_type,
                        Expected::Known(break_type.clone()),
                        syntax_tree.span(),
                        handler,
                    );
                } else {
                    *self
                        .loop_states_by_scope_id
                        .get_mut(&loop_scope_id)
                        .unwrap()
                        .kind
                        .as_loop_mut()
                        .unwrap()
                        .1 = Some(value_type);
                }

                // insert incoming value
                if let Entry::Vacant(entry) = self
                    .loop_states_by_scope_id
                    .get_mut(&loop_scope_id)
                    .unwrap()
                    .kind
                    .as_loop_mut()
                    .unwrap()
                    .0
                    .entry(self.current_block_id)
                {
                    entry.insert(value.unwrap_or(Value::Literal(
                        Literal::Unit(literal::Unit {
                            span: Some(syntax_tree.span()),
                        }),
                    )));
                }
            }
        }

        // pop all the needed scopes
        for popping_scope in self
            .stack
            .scopes()
            .iter()
            .rev()
            .map(Scope::scope_id)
            .take_while(|x| *x != loop_scope_id)
            .chain(std::iter::once(loop_scope_id))
        {
            let _ = self
                .intermediate_representation
                .control_flow_graph
                .get_block_mut(self.current_block_id)
                .unwrap()
                .insert_instruction(Instruction::ScopePop(ScopePop(
                    popping_scope,
                )));
        }

        // jump to the exit block
        if let Err(InsertTerminatorError::InvalidBlockID(_)) = self
            .intermediate_representation
            .control_flow_graph
            .insert_terminator(
                self.current_block_id,
                Terminator::Jump(Jump::Unconditional(UnconditionalJump {
                    target: self
                        .loop_states_by_scope_id
                        .get(&loop_scope_id)
                        .unwrap()
                        .exit_block_id,
                })),
            )
        {
            panic!("invalid block id");
        }

        let value = Value::Literal(Literal::Unreachable(Unreachable {
            r#type: {
                let inference = InferenceVariable::new();

                assert!(self
                    .inference_context
                    .register(inference, Constraint::All(true)));

                Type::Inference(inference)
            },
            span: Some(syntax_tree.span()),
        }));

        Ok(Expression::Value(value))
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>,
    > Bind<&syntax_tree::expression::Terminator> for Binder<'t, S, RO, TO>
{
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::Terminator,
        config: Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        match syntax_tree {
            syntax_tree::expression::Terminator::Return(syn) => {
                self.bind(syn, config, handler)
            }
            syntax_tree::expression::Terminator::Continue(syn) => {
                self.bind(syn, config, handler)
            }
            syntax_tree::expression::Terminator::Express(syn) => {
                self.bind(syn, config, handler)
            }
            syntax_tree::expression::Terminator::Break(syn) => {
                self.bind(syn, config, handler)
            }
        }
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>,
    > Bind<&syntax_tree::expression::Express> for Binder<'t, S, RO, TO>
{
    #[allow(clippy::too_many_lines)]
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::Express,
        _: Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        let label = syntax_tree
            .label()
            .as_ref()
            .map(|x| x.identifier().span.str().to_owned());

        let value = syntax_tree
            .binary()
            .as_ref()
            .map(|x| self.bind_value_or_error(x, handler))
            .transpose()?;

        let mut scope_id = None;

        // find the block state
        for scope in self.stack.scopes().iter().rev() {
            let Some(get_block_state) =
                self.block_states_by_scope_id.get(&scope.scope_id())
            else {
                continue;
            };

            if let Some(label) = &label {
                if get_block_state.label.as_ref() != Some(label) {
                    continue;
                }
            }

            scope_id = Some(scope.scope_id());
            break;
        }

        // block state not found report the error
        let Some(scope_id) = scope_id else {
            if label.is_some() {
                self.create_handler_wrapper(handler).receive(Box::new(
                    BlockWithGivenLableNameNotFound {
                        span: syntax_tree
                            .label()
                            .as_ref()
                            .map(|x| x.identifier().span.clone())
                            .unwrap(),
                    },
                ));
            } else {
                self.create_handler_wrapper(handler).receive(Box::new(
                    ExpressOutsideBlock { span: syntax_tree.span() },
                ));
            };

            return Err(Error::Semantic(SemanticError(syntax_tree.span())));
        };

        let value_type = value.as_ref().map_or_else(
            || Ok(Type::Tuple(term::Tuple { elements: Vec::new() })),
            |x| self.type_of_value(x),
        )?;

        if let Some(express_type) =
            &self.block_states_by_scope_id.get(&scope_id).unwrap().express_type
        {
            let _ = self.type_check(
                value_type,
                Expected::Known(express_type.clone()),
                syntax_tree.span(),
                handler,
            );
        } else {
            // have no express before, gets to decide the type.
            self.block_states_by_scope_id
                .get_mut(&scope_id)
                .unwrap()
                .express_type = Some(value_type);
        };

        if let Entry::Vacant(entry) = self
            .block_states_by_scope_id
            .get_mut(&scope_id)
            .unwrap()
            .incoming_values
            .entry(self.current_block_id)
        {
            entry.insert(value.unwrap_or(Value::Literal(Literal::Unit(
                literal::Unit { span: Some(syntax_tree.span()) },
            ))));
        }

        // pop all the needed scopes
        for popping_scope in self
            .stack
            .scopes()
            .iter()
            .rev()
            .map(Scope::scope_id)
            .take_while(|x| *x != scope_id)
            .chain(std::iter::once(scope_id))
        {
            let _ = self
                .intermediate_representation
                .control_flow_graph
                .get_block_mut(self.current_block_id)
                .unwrap()
                .insert_instruction(Instruction::ScopePop(ScopePop(
                    popping_scope,
                )));
        }

        // jump to the block
        if let Err(InsertTerminatorError::InvalidBlockID(_)) = self
            .intermediate_representation
            .control_flow_graph
            .insert_terminator(
                self.current_block_id,
                Terminator::Jump(Jump::Unconditional(UnconditionalJump {
                    target: self
                        .block_states_by_scope_id
                        .get(&scope_id)
                        .unwrap()
                        .successor_block_id,
                })),
            )
        {
            panic!("invalid block id");
        }

        let value =
            Value::Literal(Literal::Unreachable(literal::Unreachable {
                r#type: {
                    let inference = self
                        .create_type_inference(r#type::Constraint::All(true));

                    Type::Inference(inference)
                },
                span: Some(syntax_tree.span()),
            }));

        Ok(Expression::Value(value))
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>,
    > Bind<&syntax_tree::expression::IfElse> for Binder<'t, S, RO, TO>
{
    #[allow(clippy::too_many_lines)]
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::IfElse,
        _: Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        let condition =
            self.bind_value_or_error(&**syntax_tree.condition(), handler)?;

        // expect the type boolean
        let _ = self.type_check(
            self.type_of_value(&condition)?,
            Expected::Known(Type::Primitive(r#type::Primitive::Bool)),
            syntax_tree.condition().span(),
            handler,
        );

        let then_block_id =
            self.intermediate_representation.control_flow_graph.new_block();
        let else_block_id =
            self.intermediate_representation.control_flow_graph.new_block();
        let if_else_successor_block_id =
            self.intermediate_representation.control_flow_graph.new_block();

        /*
        wrapper_scope_id: {
            if_scope_id: {} && else_scope_id: {}
        }
         */

        let wrapper_scope_id = {
            let scopes = self
                .intermediate_representation
                .scope_tree
                .new_child_branch(
                    self.stack.current_scope().scope_id(),
                    NonZeroUsize::new(1).unwrap(),
                )
                .unwrap();

            scopes[0]
        };

        // push wrapper scope
        let _ = self.current_block_mut().insert_instruction(
            Instruction::ScopePush(ScopePush(wrapper_scope_id)),
        );
        self.stack.push_scope(wrapper_scope_id);

        let (then_scope_id, else_scope_id) = {
            let scopes = self
                .intermediate_representation
                .scope_tree
                .new_child_branch(
                    self.stack.current_scope().scope_id(),
                    NonZeroUsize::new(2).unwrap(),
                )
                .unwrap();

            (scopes[0], scopes[1])
        };

        if let Err(InsertTerminatorError::InvalidBlockID(_)) = self
            .intermediate_representation
            .control_flow_graph
            .insert_terminator(
                self.current_block_id,
                Terminator::Jump(Jump::Conditional(
                    crate::ir::instruction::ConditionalJump {
                        condition,
                        true_target: then_block_id,
                        false_target: else_block_id,
                    },
                )),
            )
        {
            panic!("invalid block id");
        }

        // bind the then block
        self.current_block_id = then_block_id;
        let (then_value, successor_then_block_id) = {
            let successor_then_block_id =
                self.intermediate_representation.control_flow_graph.new_block();

            let block_state = self.bind_block(
                syntax_tree.then_expression(),
                then_scope_id,
                successor_then_block_id,
                handler,
            )?;

            let value = self.bind_value_or_error(block_state, handler)?;

            if let Err(InsertTerminatorError::InvalidBlockID(_)) = self
                .intermediate_representation
                .control_flow_graph
                .insert_terminator(
                    successor_then_block_id,
                    Terminator::Jump(Jump::Unconditional(UnconditionalJump {
                        target: if_else_successor_block_id,
                    })),
                )
            {
                panic!("invalid id")
            }

            (value, successor_then_block_id)
        };

        // bind the else block
        self.current_block_id = else_block_id;
        let (else_value, successor_else_block_id) = match syntax_tree
            .else_expression()
            .as_ref()
            .map(|x| &**x.expression())
        {
            Some(BlockOrIfElse::Block(block)) => {
                let successor_else_block_id = self
                    .intermediate_representation
                    .control_flow_graph
                    .new_block();

                let block_state = self.bind_block(
                    block,
                    else_scope_id,
                    successor_else_block_id,
                    handler,
                )?;

                let value = self.bind_value_or_error(block_state, handler)?;

                (value, successor_else_block_id)
            }
            Some(BlockOrIfElse::IfElse(if_else)) => {
                let expression = self.bind_value_or_error(
                    if_else,
                    &self.create_handler_wrapper(handler),
                )?;

                (expression, self.current_block_id)
            }
            None => (
                Value::Literal(Literal::Unit(literal::Unit {
                    span: Some(syntax_tree.span()),
                })),
                self.current_block_id,
            ),
        };

        if let Err(InsertTerminatorError::InvalidBlockID(_)) = self
            .intermediate_representation
            .control_flow_graph
            .insert_terminator(
                successor_else_block_id,
                Terminator::Jump(Jump::Unconditional(UnconditionalJump {
                    target: if_else_successor_block_id,
                })),
            )
        {
            panic!("invalid id")
        }

        // change the current block to the if else successor block
        self.current_block_id = if_else_successor_block_id;

        // pop the wrapper scope
        let _ = self.current_block_mut().insert_instruction(
            Instruction::ScopePop(ScopePop(wrapper_scope_id)),
        );
        assert_eq!(
            self.stack.pop_scope().map(|x| x.scope_id()),
            Some(wrapper_scope_id)
        );

        let value = match self.current_block().predecessors().len() {
            0 => Value::Literal(Literal::Unreachable(Unreachable {
                r#type: {
                    let inference = InferenceVariable::new();
                    assert!(self
                        .inference_context
                        .register(inference, Constraint::All(true)));

                    Type::Inference(inference)
                },
                span: Some(syntax_tree.span()),
            })),
            1 => {
                if self
                    .current_block()
                    .predecessors()
                    .contains(&successor_then_block_id)
                {
                    then_value
                } else {
                    assert!(self
                        .current_block()
                        .predecessors()
                        .contains(&successor_else_block_id));
                    else_value
                }
            }
            2 => {
                assert!(
                    self.current_block()
                        .predecessors()
                        .contains(&successor_else_block_id)
                        && self
                            .current_block()
                            .predecessors()
                            .contains(&successor_then_block_id)
                );

                let then_type = self.type_of_value(&then_value)?;
                let else_type = self.type_of_value(&else_value)?;

                if syntax_tree.else_expression().is_some() {
                    let _ = self.type_check(
                        else_type,
                        Expected::Known(then_type.clone()),
                        syntax_tree
                            .else_expression()
                            .as_ref()
                            .map_or(syntax_tree.span(), SourceElement::span),
                        handler,
                    );
                } else {
                    let _ = self.type_check(
                        then_type.clone(),
                        Expected::Known(else_type),
                        syntax_tree.span(),
                        handler,
                    );
                }

                let phi_register_id = self.create_register_assignmnet(
                    Assignment::Phi(Phi {
                        r#type: then_type,
                        incoming_values: {
                            let mut incoming_values = HashMap::new();
                            incoming_values
                                .insert(successor_then_block_id, then_value);
                            incoming_values
                                .insert(successor_else_block_id, else_value);

                            incoming_values
                        },
                    }),
                    Some(syntax_tree.span()),
                );

                Value::Register(phi_register_id)
            }
            _ => unreachable!(),
        };

        Ok(Expression::Value(value))
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>,
    > Bind<&syntax_tree::expression::Loop> for Binder<'t, S, RO, TO>
{
    #[allow(clippy::too_many_lines)]
    fn bind(
        &mut self,
        syntax_tree: &Loop,
        _: Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        let label = syntax_tree
            .block()
            .label_specifier()
            .as_ref()
            .map(|x| x.label().identifier().span.str().to_owned());

        let loop_block_id =
            self.intermediate_representation.control_flow_graph.new_block();
        let exit_block_id =
            self.intermediate_representation.control_flow_graph.new_block();

        let loop_scope_id = {
            let result = self
                .intermediate_representation
                .scope_tree
                .new_child_branch(
                    self.stack.current_scope().scope_id(),
                    NonZeroUsize::new(1).unwrap(),
                )
                .unwrap();

            result[0]
        };

        // jump to the loop header block
        if let Err(InsertTerminatorError::InvalidBlockID(_)) = self
            .intermediate_representation
            .control_flow_graph
            .insert_terminator(
                self.current_block_id,
                Terminator::Jump(Jump::Unconditional(UnconditionalJump {
                    target: loop_block_id,
                })),
            )
        {
            panic!("invalid block id");
        }

        // set the current block to the loop header block
        self.current_block_id = loop_block_id;
        let _ = self.current_block_mut().insert_instruction(
            Instruction::ScopePush(ScopePush(loop_scope_id)),
        );
        self.stack.push_scope(loop_scope_id);
        self.loop_states_by_scope_id.insert(loop_scope_id, LoopState {
            label,
            kind: LoopKind::Loop {
                incoming_values: HashMap::new(),
                break_type: None,
            },
            loop_block_id,
            exit_block_id,
            span: syntax_tree.span(),
        });

        // bind the loop block
        for statement in syntax_tree.block().statements().statements() {
            self.bind_statement(statement, handler)?;
        }

        // pop the loop scope
        assert_eq!(
            self.stack.pop_scope().map(|x| x.scope_id()),
            Some(loop_scope_id)
        );
        let _ = self
            .current_block_mut()
            .insert_instruction(Instruction::ScopePop(ScopePop(loop_scope_id)));
        let loop_state =
            self.loop_states_by_scope_id.remove(&loop_scope_id).unwrap();

        // jump to the loop header block
        if let Err(InsertTerminatorError::InvalidBlockID(_)) = self
            .intermediate_representation
            .control_flow_graph
            .insert_terminator(
                self.current_block_id,
                Terminator::Jump(Jump::Unconditional(UnconditionalJump {
                    target: loop_block_id,
                })),
            )
        {
            panic!("invalid block id");
        }

        // set the current block to the exit block
        self.current_block_id = loop_state.exit_block_id;
        let (mut incoming_values, break_type) =
            loop_state.kind.into_loop().unwrap();

        let value = if let Some(break_type) = break_type {
            assert!(self
                .current_block()
                .predecessors()
                .iter()
                .copied()
                .all(|block_id| incoming_values.contains_key(&block_id)));

            // filter out the incoming values that are unreachable
            #[allow(clippy::needless_collect)]
            for remove in incoming_values
                .keys()
                .copied()
                .filter(|x| !self.current_block().predecessors().contains(x))
                .collect::<Vec<_>>()
            {
                incoming_values.remove(&remove);
            }

            match incoming_values.len() {
                0 => Value::Literal(Literal::Unreachable(Unreachable {
                    r#type: break_type,
                    span: Some(syntax_tree.span()),
                })),

                // only one incoming value, just return it
                1 => incoming_values.into_iter().next().unwrap().1,

                // multiple incoming values, create a phi node
                _ => {
                    let phi_register_id = self.create_register_assignmnet(
                        Assignment::Phi(Phi {
                            r#type: break_type,
                            incoming_values,
                        }),
                        Some(syntax_tree.span()),
                    );

                    Value::Register(phi_register_id)
                }
            }
        } else {
            assert!(incoming_values.is_empty());
            assert!(self.current_block().is_unreachable_or_terminated());

            Value::Literal(Literal::Unreachable(Unreachable {
                r#type: {
                    let inference = InferenceVariable::new();

                    assert!(self
                        .inference_context
                        .register(inference, Constraint::All(true)));

                    Type::Inference(inference)
                },
                span: Some(syntax_tree.span()),
            }))
        };

        Ok(Expression::Value(value))
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>,
    > Bind<&syntax_tree::expression::Brace> for Binder<'t, S, RO, TO>
{
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::Brace,
        config: Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        match syntax_tree {
            syntax_tree::expression::Brace::Block(syn) => {
                self.bind(syn, config, handler)
            }
            syntax_tree::expression::Brace::IfElse(syn) => {
                self.bind(syn, config, handler)
            }
            syntax_tree::expression::Brace::Loop(syn) => {
                self.bind(syn, config, handler)
            }
            syntax_tree::expression::Brace::Match(syn) => {
                self.bind(syn, config, handler)
            }
            syntax_tree::expression::Brace::While(syn) => {
                self.bind(syn, config, handler)
            }
        }
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>,
    > Bind<&syntax_tree::expression::While> for Binder<'t, S, RO, TO>
{
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::While,
        _: Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        let label = syntax_tree
            .block()
            .label_specifier()
            .as_ref()
            .map(|x| x.label().identifier().span.str().to_owned());

        let loop_block_id =
            self.intermediate_representation.control_flow_graph.new_block();
        let loop_body_block_id =
            self.intermediate_representation.control_flow_graph.new_block();
        let condition_fail_block_id =
            self.intermediate_representation.control_flow_graph.new_block();
        let exit_block_id =
            self.intermediate_representation.control_flow_graph.new_block();

        let while_scope_id = {
            let result = self
                .intermediate_representation
                .scope_tree
                .new_child_branch(
                    self.stack.current_scope().scope_id(),
                    NonZeroUsize::new(1).unwrap(),
                )
                .unwrap();

            result[0]
        };

        /*
        current:
            ...

        loop_block:
            scope push $while_scope_id
            branch condition, loop_body_block, condition_fail_block

        loop_body_block:
            ...
            scope pop $while_scope_id
            jump loop_block

        condition_fail_block:
            scope pop $while_scope_id
            jump exit_block

        exit:
            ...
        */

        // jump to the loop header block
        if let Err(InsertTerminatorError::InvalidBlockID(_)) = self
            .intermediate_representation
            .control_flow_graph
            .insert_terminator(
                self.current_block_id,
                Terminator::Jump(Jump::Unconditional(UnconditionalJump {
                    target: loop_block_id,
                })),
            )
        {
            panic!("invalid block id");
        }

        // set the current block to the loop header block
        self.current_block_id = loop_block_id;
        let _ = self.current_block_mut().insert_instruction(
            Instruction::ScopePush(ScopePush(while_scope_id)),
        );
        self.stack.push_scope(while_scope_id);
        self.loop_states_by_scope_id.insert(while_scope_id, LoopState {
            label,
            kind: LoopKind::While,
            loop_block_id,
            exit_block_id,
            span: syntax_tree.span(),
        });

        // bind the conditional value
        let condition =
            self.bind_value_or_error(&**syntax_tree.condition(), handler)?;
        let _ = self.type_check(
            self.type_of_value(&condition)?,
            Expected::Known(Type::Primitive(r#type::Primitive::Bool)),
            syntax_tree.condition().span(),
            handler,
        );

        // based on the condition, jump to the loop body block or the condition
        // fail block
        assert!(
            !self
                .intermediate_representation
                .control_flow_graph
                .insert_terminator(
                    loop_block_id,
                    Terminator::Jump(Jump::Conditional(
                        crate::ir::instruction::ConditionalJump {
                            condition,
                            true_target: loop_body_block_id,
                            false_target: condition_fail_block_id,
                        },
                    )),
                )
                .err()
                .map_or(false, |x| x.is_invalid_block_id()),
            "invalid block id"
        );

        // handle condition fail block
        self.current_block_id = condition_fail_block_id;

        // pop the loop scope
        let _ = self.current_block_mut().insert_instruction(
            Instruction::ScopePop(ScopePop(while_scope_id)),
        );
        // jump to the exit block
        assert!(
            !self
                .intermediate_representation
                .control_flow_graph
                .insert_terminator(
                    condition_fail_block_id,
                    Terminator::Jump(Jump::Unconditional(UnconditionalJump {
                        target: exit_block_id,
                    })),
                )
                .err()
                .map_or(false, |x| x.is_invalid_block_id()),
            "invalid block id"
        );

        // handle loop body block
        self.current_block_id = loop_body_block_id;

        // bind the loop block
        for statement in syntax_tree.block().statements().statements() {
            self.bind_statement(statement, handler)?;
        }

        // pop the loop scope
        let _ = self.current_block_mut().insert_instruction(
            Instruction::ScopePop(ScopePop(while_scope_id)),
        );

        // jump to the loop header block
        assert!(
            !self
                .intermediate_representation
                .control_flow_graph
                .insert_terminator(
                    self.current_block_id,
                    Terminator::Jump(Jump::Unconditional(UnconditionalJump {
                        target: loop_block_id,
                    })),
                )
                .err()
                .map_or(false, |x| x.is_invalid_block_id()),
            "invalid block id"
        );

        // pop the loop scope
        assert_eq!(
            self.stack.pop_scope().map(|x| x.scope_id()),
            Some(while_scope_id)
        );

        // set the current block to the exit block
        self.current_block_id = exit_block_id;

        let value = if self.current_block().predecessors().is_empty() {
            Value::Literal(Literal::Unreachable(Unreachable {
                r#type: {
                    let inference = InferenceVariable::new();
                    assert!(self
                        .inference_context
                        .register(inference, Constraint::All(true)));

                    Type::Inference(inference)
                },
                span: Some(syntax_tree.span()),
            }))
        } else {
            Value::Literal(Literal::Unit(literal::Unit {
                span: Some(syntax_tree.span()),
            }))
        };

        Ok(Expression::Value(value))
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>,
    > Bind<&syntax_tree::expression::Match> for Binder<'t, S, RO, TO>
{
    fn bind(
        &mut self,
        _syntax_tree: &syntax_tree::expression::Match,
        _config: Config,
        _handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        todo!()
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>,
    > Bind<&syntax_tree::expression::Expression> for Binder<'t, S, RO, TO>
{
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::Expression,
        config: Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        match syntax_tree {
            syntax_tree::expression::Expression::Binary(syn) => {
                self.bind(syn, config, handler)
            }
            syntax_tree::expression::Expression::Terminator(syn) => {
                self.bind(syn, config, handler)
            }
            syntax_tree::expression::Expression::Brace(syn) => {
                self.bind(syn, config, handler)
            }
        }
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>,
    > Bind<BlockState> for Binder<'t, S, RO, TO>
{
    fn bind(
        &mut self,
        mut block_state: BlockState,
        _: Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        let value = if let Some(express_type) = block_state.express_type {
            // the block id that doesn't have incoming values
            let missing_value_block_ids = self
                .current_block()
                .predecessors()
                .iter()
                .copied()
                .filter(|block_id| {
                    !block_state.incoming_values.contains_key(block_id)
                })
                .collect::<Vec<_>>();

            // filter out the incoming values that are unreachable
            #[allow(clippy::needless_collect)]
            for remove in block_state
                .incoming_values
                .keys()
                .copied()
                .filter(|x| !self.current_block().predecessors().contains(x))
                .collect::<Vec<_>>()
            {
                block_state.incoming_values.remove(&remove);
            }

            // add the missing values
            for block_id in missing_value_block_ids.iter().copied() {
                assert!(block_state
                    .incoming_values
                    .insert(
                        block_id,
                        Value::Literal(Literal::Error(literal::Error {
                            r#type: express_type.clone(),
                            span: Some(block_state.span.clone()),
                        })),
                    )
                    .is_none());
            }

            if !missing_value_block_ids.is_empty() {
                self.create_handler_wrapper(handler).receive(Box::new(
                    NotAllFlowPathsExpressValue {
                        span: block_state.span.clone(),
                    },
                ));
            }

            match block_state.incoming_values.len() {
                // no incoming values, unreachable
                0 => Value::Literal(Literal::Unreachable(Unreachable {
                    r#type: express_type,
                    span: Some(block_state.span.clone()),
                })),

                // only one incoming value, just return it
                1 => block_state.incoming_values.into_iter().next().unwrap().1,

                // multiple incoming values, create a phi node
                _ => {
                    let phi_register_id = self.create_register_assignmnet(
                        Assignment::Phi(Phi {
                            r#type: express_type,
                            incoming_values: block_state.incoming_values,
                        }),
                        Some(block_state.span.clone()),
                    );

                    Value::Register(phi_register_id)
                }
            }
        } else {
            // should have had no incoming values
            assert!(block_state.incoming_values.is_empty());

            if self.current_block().is_unreachable_or_terminated() {
                let inference_variable = InferenceVariable::new();

                assert!(self
                    .inference_context
                    .register(inference_variable, Constraint::All(true)));

                Value::Literal(Literal::Unreachable(Unreachable {
                    r#type: Type::Inference(inference_variable),
                    span: Some(block_state.span.clone()),
                }))
            } else {
                Value::Literal(Literal::Unit(Unit {
                    span: Some(block_state.span.clone()),
                }))
            }
        };

        Ok(Expression::Value(value))
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>,
    > Binder<'t, S, RO, TO>
{
    /// Binds the basic block of the given syntax tree.
    ///
    /// When binding a block, a new scope push instruction will be inserted
    /// right away in the `current_block`. Then, the list of instructions will
    /// be bound. Finally, a scope pop instruction will be followed as well
    /// as a jump instruction to the successor block.
    ///
    /// The function returns the [`BlockState`] that can be bound as a value by
    /// calling the bind block state function.
    fn bind_block(
        &mut self,
        syntax_tree: &syntax_tree::expression::Block,
        scope_id: ID<scope::Scope>,
        successor_block_id: ID<Block<infer::Model>>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<BlockState, InternalError> {
        // add the scope push instruction
        let _ = self
            .current_block_mut()
            .insert_instruction(Instruction::ScopePush(ScopePush(scope_id)));

        self.block_states_by_scope_id.insert(scope_id, BlockState {
            label: syntax_tree
                .label_specifier()
                .as_ref()
                .map(|x| x.label().identifier().span().str().to_owned()),
            incoming_values: HashMap::new(),
            successor_block_id,
            express_type: None,
            span: syntax_tree.span(),
        });

        // push a new scope
        self.stack.push_scope(scope_id);

        // bind list of statements
        for statement in syntax_tree.statements().statements() {
            self.bind_statement(statement, handler)?;
        }

        // ends the scope
        assert_eq!(
            self.stack.pop_scope().map(|x| x.scope_id()),
            Some(scope_id)
        );
        let _ = self
            .current_block_mut()
            .insert_instruction(Instruction::ScopePop(ScopePop(scope_id)));

        if let Err(InsertTerminatorError::InvalidBlockID(_)) = self
            .intermediate_representation
            .control_flow_graph
            .insert_terminator(
                self.current_block_id,
                Terminator::Jump(Jump::Unconditional(UnconditionalJump {
                    target: successor_block_id,
                })),
            )
        {
            panic!("should've been a valid ID");
        }

        // set the current block to the successor block
        self.current_block_id = successor_block_id;

        // return the block state
        Ok(self.block_states_by_scope_id.remove(&scope_id).unwrap())
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>,
    > Bind<&syntax_tree::expression::Block> for Binder<'t, S, RO, TO>
{
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::Block,
        config: Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        let scope_id = self
            .intermediate_representation
            .scope_tree
            .new_child_branch(
                self.stack.current_scope().scope_id(),
                NonZeroUsize::new(1).unwrap(),
            )
            .unwrap()[0];

        let successor_block_id =
            self.intermediate_representation.control_flow_graph.new_block();

        let block_state = self.bind_block(
            syntax_tree,
            scope_id,
            successor_block_id,
            handler,
        )?;

        // bind the block state as value
        self.bind(block_state, config, handler)
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>,
    > Binder<'t, S, RO, TO>
{
    /// Binds the given syntax tree as a value. In case of an error, an error
    /// register is returned.
    ///
    /// # Errors
    ///
    /// Returns [`InternalError`] that is returned by the [`Bind::bind()`]
    /// function.
    pub fn bind_value_or_error<T>(
        &mut self,
        syntax_tree: T,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Value<infer::Model>, InternalError>
    where
        Self: Bind<T>,
    {
        match self.bind(syntax_tree, Config { target: Target::Value }, handler)
        {
            Ok(value) => Ok(value.into_value().unwrap()),
            Err(Error::Semantic(semantic_error)) => {
                let inference =
                    self.create_type_inference(r#type::Constraint::All(false));

                Ok(Value::Literal(Literal::Error(literal::Error {
                    r#type: Type::Inference(inference),
                    span: Some(semantic_error.0),
                })))
            }
            Err(Error::Internal(internal_error)) => Err(internal_error),
        }
    }
}

#[cfg(test)]
mod test;
