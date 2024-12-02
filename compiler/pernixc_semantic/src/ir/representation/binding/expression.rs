//! Contains the code for binding the expression syntax tree.

use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    num::NonZeroUsize,
};

use enum_as_inner::EnumAsInner;
use pernixc_base::{
    handler::{Dummy, Handler, Storage},
    source_file::{SourceElement, Span},
};
use pernixc_lexical::token;
use pernixc_syntax::syntax_tree::{
    self,
    expression::{AccessKind, BlockOrIfElse, Loop, PostfixOperator},
    ConnectedList, Label,
};

use super::{
    infer, pattern::Path, stack::Scope, Binder, Error, InferenceProvider,
    InternalError, LoopKind, SemanticError,
};
use crate::{
    arena::ID,
    error::{
        self, AdtImplementationFunctionCannotBeUsedAsMethod,
        AmbiguousMethodCall, BlockWithGivenLableNameNotFound,
        CannotDereference, CannotIndexPastUnpackedTuple,
        DuplicatedFieldInitialization, ExpectArray, ExpectLValue,
        ExpectStructType, ExpressOutsideBlock, ExpressionIsNotCallable,
        FieldIsNotAccessible, FieldNotFound,
        FloatingPointLiteralHasIntegralSuffix, InvalidCastType,
        InvalidNumericSuffix, InvalidRelationalOperation, LoopControlFlow,
        LoopControlFlowOutsideLoop, LoopWithGivenLabelNameNotFound,
        MethodNotFound, MismatchedArgumentCount,
        MismatchedQualifierForReferenceOf,
        MoreThanOneUnpackedInTupleExpression, NotAllFlowPathsExpressValue,
        ReturnIsNotAllowed, SymbolIsNotAccessible, SymbolIsNotCallable,
        SymbolNotFound, TooLargeTupleIndex, TupleExpected,
        TupleIndexOutOfBOunds, UnexpectedGenericArgumentsInField,
    },
    ir::{
        self,
        address::{self, Address, Memory, Reference},
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
                BinaryOperator, BitwiseOperator, Cast, FunctionCall, Load, Phi,
                Prefix, PrefixOperator, ReferenceOf, Register, Struct, Variant,
            },
            Value,
        },
        Erased, TypeOfError,
    },
    symbol::{
        table::{
            self,
            representation::Index,
            resolution::{self, MemberGenericID},
        },
        AdtID, Callable, CallableID, ConstantParameterID, Field,
        LifetimeParameterID, TraitMemberID, TypeParameterID,
    },
    type_system::{
        self,
        instantiation::{self, Instantiation},
        model::{self, Model},
        simplify,
        term::{
            self,
            constant::Constant,
            lifetime::Lifetime,
            r#type::{self, Constraint, Expected, Primitive, Qualifier, Type},
            GenericArguments, Local, Symbol,
        },
    },
};

mod r#match;

/// An enumeration describes the intended purpose of binding the expression.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Target {
    /// Binds the expression syntax tree for the r-value.
    ///
    /// All the expressions can be bound as a r-value.
    RValue,

    /// Binds the syntax tree for the underlying address (l-value).
    ///
    /// This is a *request* to bind the expression as an l-value not strictly
    /// required. If the expression cannot be bound as an l-value, the r-value
    /// is returned instead.
    LValue,

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

/// The result of binding the expression as an l-value. (The value has an
/// address where it is stored.)
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LValue {
    /// The address of the l-value.
    pub address: Address<infer::Model>,

    /// The span of the expression that produces this l-value.
    pub span: Span,

    /// The qualifier of the l-value.
    pub qualifier: Qualifier,
}

/// The result of binding the expression syntax tree.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Expression {
    /// The expression is bound as an r-value.
    RValue(Value<infer::Model>),

    /// The expression is bound as an l-value.
    LValue(LValue),
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
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>,
    > Bind<&syntax_tree::expression::Numeric> for Binder<'t, S, RO, TO>
{
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::Numeric,
        _: Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        let numeric_ty = if let Some(suffix) = syntax_tree.suffix() {
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

            if syntax_tree.decimal().is_some() && primitive_type_is_integral {
                self.create_handler_wrapper(handler).receive(Box::new(
                    FloatingPointLiteralHasIntegralSuffix {
                        numeric_literal_span: syntax_tree.span(),
                    },
                ));
                return Err(Error::Semantic(SemanticError(syntax_tree.span())));
            }

            Type::Primitive(primitive_type)
        } else {
            // infer the type
            let constraint = if syntax_tree.decimal().is_some() {
                r#type::Constraint::Floating
            } else {
                r#type::Constraint::Number
            };

            Type::Inference(self.create_type_inference(constraint))
        };

        Ok(Expression::RValue(Value::Literal(Literal::Numeric(Numeric {
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
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>,
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

        Ok(Expression::RValue(Value::Literal(Literal::Boolean(Boolean {
            value,
            span: Some(syntax_tree.span()),
        }))))
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>,
    > Binder<'t, S, RO, TO>
{
    /// Binds the given syntax tree as an address.
    ///
    /// If the expression cannot be bound as an address, a variable will be
    /// created an the value is stored in the variable; the address of the
    /// variable is returned.
    fn bind_as_lvalue<'a, T>(
        &mut self,
        syntax_tree: &'a T,
        create_temporary: bool,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<LValue, Error>
    where
        T: SourceElement,
        Self: Bind<&'a T>,
    {
        match self.bind(
            syntax_tree,
            Config { target: Target::LValue },
            handler,
        )? {
            Expression::RValue(value) => {
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

                    Ok(LValue {
                        address: Address::Memory(Memory::Alloca(alloca_id)),
                        span: syntax_tree.span(),
                        qualifier: Qualifier::Mutable,
                    })
                } else {
                    handler.receive(Box::new(ExpectLValue {
                        expression_span: syntax_tree.span(),
                    }));

                    Err(Error::Semantic(SemanticError(syntax_tree.span())))
                }
            }
            Expression::LValue(lvalue) => Ok(lvalue),
        }
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>,
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
                        Config { target: Target::RValue },
                        handler,
                    )?
                    .into_r_value()
                    .unwrap();

                // if required, type check the operand
                if let Some(expected_type) = expected_type {
                    self.type_check(
                        &self.type_of_value(&operand)?,
                        expected_type,
                        syntax_tree.span(),
                        true,
                        handler,
                    )?;
                }
                let register_id = self.create_register_assignmnet(
                    Assignment::Prefix(Prefix { operand, operator }),
                    Some(syntax_tree.span()),
                );

                Ok(Expression::RValue(Value::Register(register_id)))
            }

            syntax_tree::expression::PrefixOperator::Dereference(_) => self
                .bind_dereference(
                    &**syntax_tree.prefixable(),
                    config,
                    syntax_tree.span(),
                    handler,
                ),

            syntax_tree::expression::PrefixOperator::ReferenceOf(
                reference_of,
            ) => {
                let qualifier = if reference_of.mutable_keyword().is_some() {
                    Qualifier::Mutable
                } else {
                    Qualifier::Immutable
                };

                let lvalue = self.bind_as_lvalue(
                    &**syntax_tree.prefixable(),
                    true,
                    handler,
                )?;

                if lvalue.qualifier < qualifier {
                    self.create_handler_wrapper(handler).receive(Box::new(
                        MismatchedQualifierForReferenceOf {
                            reference_of_span: syntax_tree.span(),
                            found_qualifier: lvalue.qualifier,
                            expected_qualifier: qualifier,
                            is_behind_reference: lvalue
                                .address
                                .is_behind_reference(),
                        },
                    ));
                }

                let register_id = self.create_register_assignmnet(
                    Assignment::ReferenceOf(ReferenceOf {
                        address: lvalue.address,
                        qualifier,
                        lifetime: Lifetime::Inference(Erased),
                    }),
                    Some(syntax_tree.span()),
                );

                Ok(Expression::RValue(Value::Register(register_id)))
            }
        }
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>,
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
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>,
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
                        &self.type_of_value(&argument.1)?,
                        Expected::Known(variant_type),
                        argument.0,
                        true,
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
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>,
    > Binder<'t, S, RO, TO>
{
    fn bind_function_call(
        &mut self,
        arguments: &[(Span, Value<infer::Model>)],
        callable_id: CallableID,
        instantiation: Instantiation<infer::Model>,
        syntax_tree_span: Span,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> ID<Register<infer::Model>> {
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

        self.create_register_assignmnet(
            Assignment::FunctionCall(FunctionCall {
                callable_id,
                arguments: acutal_arguments,
                instantiation,
            }),
            Some(syntax_tree_span),
        )
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>,
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
                ));

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
                        );

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
                ));

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

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>,
    > Binder<'t, S, RO, TO>
{
    fn bind_dereference<'a, T>(
        &mut self,
        dereference: &'a T,
        config: Config,
        final_span: Span,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error>
    where
        Self: Bind<&'a T>,
        T: SourceElement,
    {
        let operand = self.bind_as_lvalue(dereference, true, handler)?;

        // expected a reference type
        let operand_type = self.type_of_address(&operand.address).unwrap();

        let reference_type = match operand_type {
            Type::Reference(reference) => reference,
            found_type => {
                self.create_handler_wrapper(handler).receive(Box::new(
                    CannotDereference {
                        found_type: self
                            .inference_context
                            .into_constraint_model(found_type)
                            .unwrap(),
                        span: dereference.span(),
                    },
                ));
                return Err(Error::Semantic(SemanticError(dereference.span())));
            }
        };

        match config.target {
            Target::RValue | Target::Statement => {
                let register_id = self.create_register_assignmnet(
                    Assignment::Load(Load {
                        address: Address::Reference(Reference {
                            qualifier: reference_type.qualifier,
                            reference_address: Box::new(operand.address),
                        }),
                    }),
                    Some(dereference.span()),
                );

                Ok(Expression::RValue(Value::Register(register_id)))
            }
            Target::LValue => {
                let new_qualifier = reference_type.qualifier.min(
                    if operand.address.is_behind_reference() {
                        operand.qualifier
                    } else {
                        Qualifier::Mutable
                    },
                );

                Ok(Expression::LValue(LValue {
                    address: Address::Reference(Reference {
                        qualifier: reference_type.qualifier,
                        reference_address: Box::new(operand.address),
                    }),
                    span: final_span,
                    qualifier: new_qualifier,
                }))
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
            + type_system::observer::Observer<ir::Model, S>,
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

    #[allow(clippy::too_many_lines)]
    fn search_trait_method_candidates(
        &mut self,
        method_ident: &syntax_tree::GenericIdentifier,
        generic_arguments: &GenericArguments<infer::Model>,
        access_type: &Type<infer::Model>,
        arguments: &[(Span, Value<infer::Model>)],
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Vec<(
        CallableID,
        Instantiation<infer::Model>,
        infer::Context,
        RecieverKind,
    )> {
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
            .collect::<Vec<_>>();

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

                if self
                    .type_check(
                        &self.type_of_value(argument_value).unwrap(),
                        Expected::Known(parameter_ty),
                        argument_span.clone(),
                        false,
                        &Dummy,
                    )
                    .is_err()
                {
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

        candidates
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
                    Some(span),
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
                    Value::Literal(literal) => literal.span().unwrap().clone(),
                };
                let type_of_value = self.type_of_value(&value)?;
                let alloca_id =
                    self.create_alloca(type_of_value, Some(span.clone()));

                // initialize
                let _ = self.current_block_mut().insert_instruction(
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
                    Some(span),
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
                    Some(address.span),
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
                    .clone()
                    .unwrap(),
                Expression::RValue(Value::Literal(literal)) => {
                    literal.span().unwrap().clone()
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
            )));
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
        );

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
        );

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
        )))
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>,
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
                    &type_of_value,
                    Expected::Constraint(Constraint::Number),
                    syntax_tree.postfixable().span(),
                    true,
                    handler,
                );

                Ok(Expression::RValue(Value::Register(
                    self.create_register_assignmnet(
                        Assignment::Cast(Cast { value, r#type: cast_type }),
                        Some(syntax_tree.span()),
                    ),
                )))
            }

            syntax_tree::expression::PostfixOperator::Access(access_syn) => {
                let (lvalue, ty) = match access_syn.operator() {
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

                let address = match access_syn.kind() {
                    // struct field access
                    syntax_tree::expression::AccessKind::GenericIdentifier(
                        generic_ident,
                    ) => {
                        let struct_id = match ty {
                            Type::Symbol(Symbol {
                                id:
                                    r#type::SymbolID::Adt(AdtID::Struct(struct_id)),
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

                        // soft error, keep going
                        if generic_ident.generic_arguments().is_some() {
                            self.create_handler_wrapper(handler).receive(
                                Box::new(UnexpectedGenericArgumentsInField {
                                    field_access_span: syntax_tree.span(),
                                }),
                            );
                        }

                        // find field id
                        let Some(field_id) = self
                            .table
                            .get(struct_id)
                            .unwrap()
                            .fields()
                            .get_id(generic_ident.identifier().span.str())
                        else {
                            self.create_handler_wrapper(handler).receive(
                                Box::new(FieldNotFound {
                                    struct_id,
                                    identifier_span: generic_ident
                                        .identifier()
                                        .span
                                        .clone(),
                                }),
                            );

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
                                    referring_identifier_span: generic_ident
                                        .identifier()
                                        .span
                                        .clone(),
                                }),
                            );
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
                            self.create_handler_wrapper(handler).receive(
                                Box::new(ExpectArray {
                                    span: syntax_tree.postfixable().span(),
                                    r#type: self
                                        .inference_context
                                        .into_constraint_model(ty)
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
                            &index_ty,
                            Expected::Known(Type::Primitive(
                                r#type::Primitive::Usize,
                            )),
                            index.expression().span(),
                            true,
                            handler,
                        );

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
                            Some(syntax_tree.span()),
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
        }
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>,
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
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>,
    > Bind<&syntax_tree::QualifiedIdentifier> for Binder<'t, S, RO, TO>
{
    #[allow(clippy::too_many_lines)]
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::QualifiedIdentifier,
        config: Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        let is_simple_identifier = syntax_tree.rest().is_empty()
            && syntax_tree
                .root()
                .as_generic_identifier()
                .map_or(false, |x| x.generic_arguments().is_none());

        // search for the variable/parameter in the stack
        #[allow(clippy::unnecessary_operation)]
        'out: {
            if is_simple_identifier {
                let Some(name) = self.stack.search(
                    syntax_tree
                        .root()
                        .as_generic_identifier()
                        .unwrap()
                        .identifier()
                        .span
                        .str(),
                ) else {
                    break 'out;
                };

                let address = name.load_address.clone();

                match config.target {
                    Target::RValue => {
                        let register_id = self.create_register_assignmnet(
                            Assignment::Load(Load { address }),
                            Some(syntax_tree.span()),
                        );

                        return Ok(Expression::RValue(Value::Register(
                            register_id,
                        )));
                    }
                    Target::Statement | Target::LValue => {
                        let name_qualifier = if name.mutable {
                            Qualifier::Mutable
                        } else {
                            Qualifier::Immutable
                        };

                        let final_qualifier = self
                            .get_behind_reference_qualifier(&name.load_address)
                            .map_or(name_qualifier, |x| x.min(name_qualifier));

                        return Ok(Expression::LValue(LValue {
                            address: name.load_address.clone(),
                            span: syntax_tree.span(),
                            qualifier: final_qualifier,
                        }));
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

                Ok(Expression::RValue(Value::Register(register_id)))
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
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>,
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
            .connected_list()
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
                &self.type_of_value(&value)?,
                Expected::Known(field_ty),
                field_syn.expression().span(),
                true,
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

        Ok(Expression::RValue(value))
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>,
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

        Ok(Expression::RValue(value))
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>,
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

        Ok(Expression::RValue(value))
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>,
    > Bind<&syntax_tree::expression::Parenthesized> for Binder<'t, S, RO, TO>
{
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::Parenthesized,
        config: Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        let bind_as_tuple =
            syntax_tree.connected_list().as_ref().map_or(true, |x| {
                !x.rest().is_empty()
                    || x.trailing_separator().is_some()
                    || x.first().ellipsis().is_some()
            });

        if bind_as_tuple {
            let mut elements = Vec::new();

            for element_syn in syntax_tree
                .connected_list()
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

                Ok(Expression::RValue(value))
            }
        } else {
            // propagate the target
            self.bind(
                &**syntax_tree
                    .connected_list()
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
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>,
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

        Ok(Expression::RValue(Value::Literal(Literal::Phantom(
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
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>,
    > Bind<&syntax_tree::expression::Array> for Binder<'t, S, RO, TO>
{
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::Array,
        _: Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        let Some(arguments) = syntax_tree.arguments().connected_list() else {
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

            return Ok(Expression::RValue(Value::Register(register_id)));
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
                &element_ty,
                Expected::Known(first_ty.clone()),
                match element {
                    Value::Register(register_id) => self
                        .intermediate_representation
                        .values
                        .registers
                        .get(*register_id)
                        .unwrap()
                        .span
                        .clone()
                        .unwrap(),
                    Value::Literal(literal) => literal.span().cloned().unwrap(),
                },
                true,
                handler,
            );
        }

        let value = Value::Register(self.create_register_assignmnet(
            Assignment::Array(Array { elements, element_type: first_ty }),
            Some(syntax_tree.span()),
        ));

        Ok(Expression::RValue(value))
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>,
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
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>,
    > Binder<'t, S, RO, TO>
{
    fn bind_assignment(
        &mut self,
        tree: &BinaryTree,
        config: Config,
        lhs_address: LValue,
        rhs_value: Value<infer::Model>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        let lhs_ty = self.type_of_address(&lhs_address.address)?;
        let rhs_ty = self.type_of_value(&rhs_value)?;

        let _ = self.type_check(
            &rhs_ty,
            Expected::Known(lhs_ty),
            tree.right.span(),
            true,
            handler,
        );

        if lhs_address.qualifier != Qualifier::Mutable {
            self.create_handler_wrapper(handler).receive(Box::new(
                error::AssignToNonMutable {
                    span: tree.span(),
                    qualifier: lhs_address.qualifier,
                },
            ));
        }

        let _ = self.current_block_mut().insert_instruction(
            Instruction::Store(Store {
                address: lhs_address.address.clone(),
                value: rhs_value,
            }),
        );

        Ok(match config.target {
            Target::RValue => {
                let register_id = self.create_register_assignmnet(
                    Assignment::Load(Load { address: lhs_address.address }),
                    Some(tree.span()),
                );

                Expression::RValue(Value::Register(register_id))
            }

            // qualifier is not checked here since the address is already bound
            // as Qualifier::Unique, which has the highest priority
            Target::Statement | Target::LValue { .. } => {
                Expression::LValue(LValue {
                    address: lhs_address.address,
                    span: tree.span(),
                    qualifier: lhs_address.qualifier,
                })
            }
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
                let lhs_lvalue = match self.bind_as_lvalue(
                    &syntax_tree.left,
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
                        address: lhs_lvalue.address.clone(),
                    }),
                    Some(syntax_tree.left.span()),
                );

                (Some(lhs_lvalue), Value::Register(lhs_register))
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
                &rhs_register_ty,
                Expected::Known(lhs_register_ty.clone()),
                syntax_tree.right.span(),
                true,
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
                    &lhs_register_ty,
                    Expected::Constraint(expected_constraints),
                    syntax_tree.span(),
                    true,
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
                        &lhs_register_ty,
                        Expected::Constraint(expected_constraints),
                        syntax_tree.left.span(),
                        true,
                        handler,
                    );

                    let rhs_value_ty = self.type_of_value(&rhs_value)?;

                    let _ = self.type_check(
                        &rhs_value_ty,
                        Expected::Constraint(expected_constraints),
                        syntax_tree.right.span(),
                        true,
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
            Ok(Expression::RValue(Value::Register(binary_register)))
        }
    }
}

impl<
        'x,
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>,
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
                let lhs =
                    self.bind_as_lvalue(&syntax_tree.left, false, handler)?;

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
                    &lhs_ty,
                    Expected::Known(Type::Primitive(r#type::Primitive::Bool)),
                    syntax_tree.left.span(),
                    true,
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
                            &rhs_ty,
                            Expected::Known(Type::Primitive(
                                r#type::Primitive::Bool,
                            )),
                            syntax_tree.right.span(),
                            true,
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
                            &rhs_ty,
                            Expected::Known(Type::Primitive(
                                r#type::Primitive::Bool,
                            )),
                            syntax_tree.right.span(),
                            true,
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

                Ok(Expression::RValue(value))
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
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>,
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
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>,
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
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>,
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
            &value_ty,
            Expected::Known(return_type),
            syntax_tree.span(),
            true,
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

        Ok(Expression::RValue(value))
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>,
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
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>,
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

        Ok(Expression::RValue(value))
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>,
    > Bind<&syntax_tree::expression::Break> for Binder<'t, S, RO, TO>
{
    #[allow(clippy::too_many_lines)]
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
                    &value_type,
                    Expected::Known(Type::Tuple(term::Tuple {
                        elements: Vec::new(),
                    })),
                    syntax_tree.binary().as_ref().map_or_else(
                        || syntax_tree.span(),
                        SourceElement::span,
                    ),
                    true,
                    handler,
                );
            }

            // can use any type
            LoopKind::Loop { break_type, .. } => {
                if let Some(break_type) = break_type {
                    let _ = self.type_check(
                        &value_type,
                        Expected::Known(break_type.clone()),
                        syntax_tree.span(),
                        true,
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

        Ok(Expression::RValue(value))
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>,
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
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>,
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
                &value_type,
                Expected::Known(express_type.clone()),
                syntax_tree.span(),
                true,
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

        Ok(Expression::RValue(value))
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>,
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
            self.bind_value_or_error(syntax_tree.parenthesized(), handler)?;

        // expect the type boolean
        let _ = self.type_check(
            &self.type_of_value(&condition)?,
            Expected::Known(Type::Primitive(r#type::Primitive::Bool)),
            syntax_tree.parenthesized().span(),
            true,
            handler,
        );

        let then_block_id =
            self.intermediate_representation.control_flow_graph.new_block();
        let else_block_id =
            self.intermediate_representation.control_flow_graph.new_block();
        let if_else_successor_block_id =
            self.intermediate_representation.control_flow_graph.new_block();

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
                        &else_type,
                        Expected::Known(then_type.clone()),
                        syntax_tree
                            .else_expression()
                            .as_ref()
                            .map_or(syntax_tree.span(), SourceElement::span),
                        true,
                        handler,
                    );
                } else {
                    let _ = self.type_check(
                        &then_type,
                        Expected::Known(else_type),
                        syntax_tree.span(),
                        true,
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

        Ok(Expression::RValue(value))
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>,
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
        for statement in syntax_tree.block().statements().tree() {
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

        Ok(Expression::RValue(value))
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>,
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
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>,
    > Bind<&syntax_tree::expression::While> for Binder<'t, S, RO, TO>
{
    #[allow(clippy::too_many_lines)]
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
            self.bind_value_or_error(syntax_tree.parenthesized(), handler)?;
        let _ = self.type_check(
            &self.type_of_value(&condition)?,
            Expected::Known(Type::Primitive(r#type::Primitive::Bool)),
            syntax_tree.parenthesized().span(),
            true,
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
        for statement in syntax_tree.block().statements().tree() {
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

        Ok(Expression::RValue(value))
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>,
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
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>,
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

        Ok(Expression::RValue(value))
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>,
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
        for statement in syntax_tree.statements().tree() {
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
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>,
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
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>,
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
        match self.bind(syntax_tree, Config { target: Target::RValue }, handler)
        {
            Ok(value) => Ok(value.into_r_value().unwrap()),
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
