//! Contains the code for binding the expression syntax tree.

use std::collections::{hash_map::Entry, HashMap, HashSet};

use enum_as_inner::EnumAsInner;
use pernixc_base::{
    diagnostic::Handler,
    source_file::{SourceElement, Span},
};
use pernixc_syntax::syntax_tree::{self, ConnectedList};

use super::{
    infer::{self, Erased},
    Binder, Error, HandlerWrapper, InternalError, SemanticError,
};
use crate::{
    arena::ID,
    error::{
        self, CannotDereference, DuplicatedFieldInitialization, ExpectedLValue,
        ExpressionIsNotCallable, FieldIsNotAccessible, FieldNotFound,
        FloatingPointLiteralHasIntegralSuffix, InvalidNumericSuffix,
        InvalidRelationalOperation, MismatchedArgumentCount,
        MismatchedMutability, MismatchedReferenceQualifier,
        SymbolIsNotCallable,
    },
    ir::{
        address::{self, Address, Memory},
        instruction::{Instruction, Store},
        register::{
            ArithmeticOperator, Assignment, Binary, BinaryOperator,
            BitwiseOperator, Boolean, FunctionCall, Load, LoadKind, Numeric,
            Prefix, PrefixOperator, ReferenceOf, Register, Struct, Variant,
        },
        representation::binding::infer::{InferenceVariable, NoConstraint},
        TypeOfError,
    },
    symbol::{
        table::{
            self,
            representation::Index,
            resolution::{self, MemberGenericID, Observer},
        },
        AdtID, CallableID, ConstantParameterID, Field, LifetimeParameterID,
        TypeParameterID,
    },
    type_system::{
        instantiation::{self, Instantiation},
        model::Model,
        simplify,
        term::{
            constant::Constant,
            lifetime::Lifetime,
            r#type::{self, Constraint, Expected, Qualifier, SymbolID, Type},
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

impl Target {
    /// Handles the `register_id` as an r-value.
    fn return_rvalue(
        self,
        register_id: ID<Register<infer::Model>>,
        rvalue_span: Span,
        handler: &HandlerWrapper,
    ) -> Result<Expression, SemanticError> {
        match self {
            Self::Value => Ok(Expression::Value(register_id)),
            Self::Address { .. } => {
                handler.receive(Box::new(ExpectedLValue {
                    expression_span: rvalue_span.clone(),
                }));
                Err(SemanticError(rvalue_span))
            }
            Self::Statement => Ok(Expression::SideEffect),
        }
    }
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
    Value(ID<Register<infer::Model>>),

    /// The expression is bound as an l-value.
    Address {
        /// The address of the l-value.
        address: Address<Memory<infer::Model>>,

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
        syntax_tree: &T,
        config: Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error>;
}

impl<'t, S: table::State, O: Observer<S, infer::Model>>
    Bind<syntax_tree::expression::Numeric> for Binder<'t, S, O>
{
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::Numeric,
        config: Config,
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

        let register_id = self.create_register_assignmnet(
            Assignment::Numeric(Numeric {
                integer_string: syntax_tree.numeric().span.str().to_string(),
                decimal_stirng: syntax_tree
                    .decimal()
                    .as_ref()
                    .map(|x| x.numeric().span.str().to_owned()),
                r#type: numeric_ty,
            }),
            Some(syntax_tree.span()),
        );

        Ok(config.target.return_rvalue(
            register_id,
            syntax_tree.span(),
            &self.create_handler_wrapper(handler),
        )?)
    }
}

impl<'t, S: table::State, O: Observer<S, infer::Model>>
    Bind<syntax_tree::expression::Boolean> for Binder<'t, S, O>
{
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::Boolean,
        config: Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        let value = match syntax_tree {
            syntax_tree::expression::Boolean::True(_) => true,
            syntax_tree::expression::Boolean::False(_) => false,
        };

        let register_id = self.create_register_assignmnet(
            Assignment::Boolean(Boolean { value }),
            Some(syntax_tree.span()),
        );

        Ok(config.target.return_rvalue(
            register_id,
            syntax_tree.span(),
            &self.create_handler_wrapper(handler),
        )?)
    }
}

impl<'t, S: table::State, O: Observer<S, infer::Model>>
    Bind<syntax_tree::expression::Prefix> for Binder<'t, S, O>
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
                                r#type::Constraint::All,
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
                        self.type_of_register(operand)?,
                        expected_type,
                        syntax_tree.span(),
                        handler,
                    )?;
                }
                let register_id = self.create_register_assignmnet(
                    Assignment::Prefix(Prefix { operand, operator }),
                    Some(syntax_tree.span()),
                );

                Ok(config.target.return_rvalue(
                    register_id,
                    syntax_tree.span(),
                    &self.create_handler_wrapper(handler),
                )?)
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
                let mut operand_type = self.type_of_register(operand)?;

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
                                found_type,
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
                                address: Address::Base(
                                    address::Memory::ReferenceValue(operand),
                                ),
                                kind: LoadKind::Copy,
                            }),
                            Some(syntax_tree.span()),
                        );

                        Ok(config.target.return_rvalue(
                            register_id,
                            syntax_tree.span(),
                            &self.create_handler_wrapper(handler),
                        )?)
                    }
                    Target::Address { expected_qualifier } => {
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
                            address: Address::Base(
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

                // bind the operand
                let Expression::Address { address, .. } = self.bind(
                    &**syntax_tree.prefixable(),
                    Config {
                        target: Target::Address {
                            expected_qualifier: qualifier,
                        },
                    },
                    handler,
                )?
                else {
                    panic!("Expected an address");
                };

                // check if the address is mutable
                let expected = match reference_of.kind() {
                    syntax_tree::expression::ReferenceOfKind::Local(_) => {
                        let inference_variable =
                            self.create_type_inference(r#type::Constraint::All);

                        Some(Type::Local(Local(Box::new(Type::Inference(
                            inference_variable,
                        )))))
                    }
                    syntax_tree::expression::ReferenceOfKind::Regular(_) => {
                        None
                    }
                };

                if let Some(expected) = expected {
                    self.type_check(
                        self.type_of_address(&address)?,
                        Expected::Known(expected),
                        syntax_tree.span(),
                        handler,
                    )?;
                }

                let register_id = self.create_register_assignmnet(
                    Assignment::ReferenceOf(ReferenceOf {
                        address,
                        qualifier,
                        is_local: reference_of.kind().is_local(),
                        lifetime: Lifetime::Inference(Erased),
                    }),
                    Some(syntax_tree.span()),
                );

                Ok(config.target.return_rvalue(
                    register_id,
                    syntax_tree.span(),
                    &self.create_handler_wrapper(handler),
                )?)
            }
        }
    }
}

impl<'t, S: table::State, O: Observer<S, infer::Model>>
    Bind<syntax_tree::expression::Prefixable> for Binder<'t, S, O>
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

impl<'t, S: table::State, O: Observer<S, infer::Model>> Binder<'t, S, O> {
    fn bind_variant_call(
        &mut self,
        arguments: Vec<(Span, ID<Register<infer::Model>>)>,
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
                    handler.receive(Box::new(MismatchedArgumentCount {
                        called_id: variant.variant.into(),
                        expected_count: 1,
                        found_count: 0,
                        span: syntax_tree_span.clone(),
                    }));

                    self.create_register_assignmnet(
                        Assignment::Errored(variant_type),
                        Some(syntax_tree_span.clone()),
                    )
                }
                len => {
                    if len != 1 {
                        handler.receive(Box::new(MismatchedArgumentCount {
                            called_id: variant.variant.into(),
                            expected_count: 1,
                            found_count: len,
                            span: syntax_tree_span.clone(),
                        }));
                    }

                    let argument = arguments.into_iter().next().unwrap();

                    let _ = self.type_check(
                        self.type_of_register(argument.1)?,
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
                handler.receive(Box::new(MismatchedArgumentCount {
                    called_id: variant.variant.into(),
                    expected_count: 0,
                    found_count: arguments.len(),
                    span: syntax_tree_span.clone(),
                }));
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

impl<'t, S: table::State, O: Observer<S, infer::Model>> Binder<'t, S, O> {
    fn bind_function_call(
        &mut self,
        arguments: &[(Span, ID<Register<infer::Model>>)],
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
        for ((argument_span, argument_register_id), parameter) in
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
                self.type_of_register(*argument_register_id)?,
                Expected::Known(parameter_ty),
                argument_span.clone(),
                handler,
            );

            acutal_arguments.push(*argument_register_id);
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

            let error_register_id = self.create_register_assignmnet(
                Assignment::Errored(parameter_ty),
                Some(syntax_tree_span.clone()),
            );

            acutal_arguments.push(error_register_id);
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

impl<'t, S: table::State, O: Observer<S, infer::Model>> Binder<'t, S, O> {
    #[allow(clippy::too_many_lines)]
    fn bind_postfix_call_with_resolution(
        &mut self,
        arguments: Vec<(Span, ID<Register<infer::Model>>)>,
        resolution_span: Span,
        resolution: resolution::Resolution<infer::Model>,
        syntax_tree_span: Span,
        config: Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        // can be enum variant and functin call
        match resolution {
            // bind as variant
            resolution::Resolution::Variant(variant) => {
                Ok(config.target.return_rvalue(
                    self.bind_variant_call(
                        arguments,
                        variant,
                        syntax_tree_span.clone(),
                        handler,
                    )?,
                    syntax_tree_span,
                    &self.create_handler_wrapper(handler),
                )?)
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

                Ok(config.target.return_rvalue(
                    self.bind_function_call(
                        &arguments,
                        callable_id,
                        instantiation,
                        syntax_tree_span.clone(),
                        handler,
                    )?,
                    syntax_tree_span,
                    &self.create_handler_wrapper(handler),
                )?)
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
                                .register(x, Constraint::All));
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

                Ok(config.target.return_rvalue(
                    self.bind_function_call(
                        &arguments,
                        callable_id,
                        inst,
                        syntax_tree_span.clone(),
                        handler,
                    )?,
                    syntax_tree_span,
                    &self.create_handler_wrapper(handler),
                )?)
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

impl<'t, S: table::State, O: Observer<S, infer::Model>>
    Bind<syntax_tree::expression::Postfix> for Binder<'t, S, O>
{
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
                            config,
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

            syntax_tree::expression::PostfixOperator::Cast(_) => todo!(),

            syntax_tree::expression::PostfixOperator::Access(_) => todo!(),
        }
    }
}

impl<'t, S: table::State, O: Observer<S, infer::Model>>
    Bind<syntax_tree::expression::Postfixable> for Binder<'t, S, O>
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

impl<'t, S: table::State, O: Observer<S, infer::Model>>
    Bind<syntax_tree::QualifiedIdentifier> for Binder<'t, S, O>
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

                let address = Address::Base(match name.load_address {
                    address::Stack::Alloca(alloca_id) => {
                        address::Memory::Alloca(alloca_id)
                    }
                    address::Stack::Parameter(parameter) => {
                        address::Memory::Parameter(parameter)
                    }
                });

                match config.target {
                    Target::Statement | Target::Value => {
                        let register_id = self.create_register_assignmnet(
                            Assignment::Load(Load {
                                address: Address::Base(
                                    match name.load_address {
                                        address::Stack::Alloca(alloca_id) => {
                                            address::Memory::Alloca(alloca_id)
                                        }
                                        address::Stack::Parameter(
                                            parameter,
                                        ) => address::Memory::Parameter(
                                            parameter,
                                        ),
                                    },
                                ),
                                kind: LoadKind::Copy,
                            }),
                            Some(syntax_tree.span()),
                        );

                        return Ok(match config.target {
                            Target::Statement => Expression::SideEffect,
                            Target::Value => Expression::Value(register_id),
                            Target::Address { .. } => unreachable!(),
                        });
                    }
                    Target::Address { expected_qualifier } => {
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
            resolution::Resolution::Module(_) => todo!(),
            resolution::Resolution::Variant(variant_res) => {
                let variant = self.table.get(variant_res.variant).unwrap();

                // expected a variant type
                if variant.associated_type.is_some() {
                    self.create_handler_wrapper(handler).receive(Box::new(
                        error::ExpectedAssociatedValue {
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

                    let associated_value = self.create_register_assignmnet(
                        Assignment::Errored(associated_type),
                        Some(syntax_tree.span()),
                    );

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
                    None,
                );

                Ok(config.target.return_rvalue(
                    register_id,
                    syntax_tree.span(),
                    &self.create_handler_wrapper(handler),
                )?)
            }
            resolution::Resolution::Generic(_) => todo!(),
            resolution::Resolution::MemberGeneric(_) => todo!(),
        }
    }
}

impl<'t, S: table::State, O: Observer<S, infer::Model>>
    Bind<syntax_tree::expression::Struct> for Binder<'t, S, O>
{
    #[allow(clippy::too_many_lines)]
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::Struct,
        config: Config,
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
                error::ExpectedStruct { span: syntax_tree.span() },
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
            HashMap::<ID<Field>, (ID<Register<infer::Model>>, Span)>::new();

        for field_syn in syntax_tree
            .field_initializers()
            .as_ref()
            .into_iter()
            .flat_map(ConnectedList::elements)
        {
            // get the field ID by name
            let register_id =
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
                self.type_of_register(register_id)?,
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
                    entry.insert((register_id, field_syn.span()));
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

        Ok(config.target.return_rvalue(
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
            syntax_tree.span(),
            &self.create_handler_wrapper(handler),
        )?)
    }
}

impl<'t, S: table::State, O: Observer<S, infer::Model>>
    Bind<syntax_tree::expression::Unit> for Binder<'t, S, O>
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
            syntax_tree::expression::Unit::Parenthesized(_) => todo!(),
            syntax_tree::expression::Unit::Struct(syn) => {
                self.bind(syn, config, handler)
            }
            syntax_tree::expression::Unit::Array(_) => todo!(),
            syntax_tree::expression::Unit::Phantom(_) => todo!(),
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

    use crate::ir::register::{
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

impl<'t, S: table::State, O: Observer<S, infer::Model>> Binder<'t, S, O> {
    fn bind_assignment(
        &mut self,
        tree: &BinaryTree,
        config: Config,
        lhs_address: Address<Memory<infer::Model>>,
        rhs_register_id: ID<Register<infer::Model>>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        let lhs_ty = self.type_of_address(&lhs_address)?;
        let rhs_ty = self.type_of_register(rhs_register_id)?;

        let _ = self.type_check(
            rhs_ty,
            Expected::Known(lhs_ty),
            tree.right.span(),
            handler,
        );

        self.current_block_mut().insert_basic(Instruction::Store(Store {
            address: lhs_address.clone(),
            value: rhs_register_id,
            is_initializattion: false,
        }));

        Ok(match config.target {
            Target::Value => {
                let register_id = self.create_register_assignmnet(
                    Assignment::Load(Load {
                        address: lhs_address,
                        kind: LoadKind::Copy,
                    }),
                    Some(tree.span()),
                );

                Expression::Value(register_id)
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

        let (lhs_address, lhs_register) = 'out: {
            if is_compound {
                let lhs_address = match self.bind(
                    &syntax_tree.left,
                    Config {
                        target: Target::Address {
                            expected_qualifier: Qualifier::Unique,
                        },
                    },
                    handler,
                ) {
                    Ok(address) => address.into_address().unwrap(),

                    // make the lhs errored
                    Err(Error::Semantic(SemanticError(span))) => {
                        let inference =
                            self.create_type_inference(r#type::Constraint::All);

                        let errored_register = self.create_register_assignmnet(
                            Assignment::Errored(Type::Inference(inference)),
                            Some(span),
                        );

                        break 'out (None, errored_register);
                    }

                    Err(Error::Internal(internal_error)) => {
                        return Err(Error::Internal(internal_error))
                    }
                };

                let lhs_register = self.create_register_assignmnet(
                    Assignment::Load(Load {
                        address: lhs_address.0.clone(),
                        kind: LoadKind::Copy,
                    }),
                    Some(syntax_tree.left.span()),
                );

                (Some(lhs_address), lhs_register)
            } else {
                (None, self.bind_value_or_error(&syntax_tree.left, handler)?)
            }
        };

        let rhs_register =
            self.bind_value_or_error(&syntax_tree.right, handler)?;

        let lhs_register_ty = self.type_of_register(lhs_register)?;

        // left shift and right shift doesn't necessarily require the same type
        // for both operands
        if !matches!(
            op,
            BinaryOperator::Bitwise(
                BitwiseOperator::LeftShift | BitwiseOperator::RightShift
            )
        ) {
            let rhs_register_ty = self.type_of_register(rhs_register)?;

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
                            | Constraint::Floating => true,

                            Constraint::All => false,
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
                                | Constraint::Signed => true,

                                Constraint::Floating | Constraint::All => false,
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

                    let rhs_register_ty =
                        self.type_of_register(rhs_register)?;

                    let _ = self.type_check(
                        rhs_register_ty,
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
                lhs: lhs_register,
                rhs: rhs_register,
            }),
            Some(syntax_tree.span()),
        );

        if let (Some(lhs_address), true) = (lhs_address, is_compound) {
            self.bind_assignment(
                syntax_tree,
                config,
                lhs_address.0,
                binary_register,
                handler,
            )
        } else {
            Ok(config.target.return_rvalue(
                binary_register,
                syntax_tree.span(),
                &self.create_handler_wrapper(handler),
            )?)
        }
    }
}

impl<'x, 't, S: table::State, O: Observer<S, infer::Model>> Bind<BinaryTree<'x>>
    for Binder<'t, S, O>
{
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
                let lhs = self
                    .bind(
                        &syntax_tree.left,
                        Config {
                            target: Target::Address {
                                expected_qualifier: Qualifier::Unique,
                            },
                        },
                        handler,
                    )?
                    .into_address()
                    .unwrap();

                self.bind_assignment(syntax_tree, config, lhs.0, rhs, handler)
            }

            syntax_tree::expression::BinaryOperator::LogicalAnd(_)
            | syntax_tree::expression::BinaryOperator::LogicalOr(_) => {
                todo!("handle lazy logical")
            }

            _ => self.bind_normal_binary(syntax_tree, config, handler),
        }
    }
}

impl<'x, 't, S: table::State, O: Observer<S, infer::Model>> Bind<BinaryNode<'x>>
    for Binder<'t, S, O>
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

impl<'t, S: table::State, O: Observer<S, infer::Model>>
    Bind<syntax_tree::expression::Binary> for Binder<'t, S, O>
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

impl<'t, S: table::State, O: Observer<S, infer::Model>>
    Bind<syntax_tree::expression::Expression> for Binder<'t, S, O>
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
            syntax_tree::expression::Expression::Terminator(_) => todo!(),
            syntax_tree::expression::Expression::Brace(_) => todo!(),
        }
    }
}

impl<'t, S: table::State, O: Observer<S, infer::Model>> Binder<'t, S, O> {
    /// Binds the given syntax tree as a value. In case of an error, an error
    /// register is returned.
    ///
    /// # Errors
    ///
    /// Returns [`InternalError`] that is returned by the [`Bind::bind()`]
    /// function.
    pub fn bind_value_or_error<T>(
        &mut self,
        syntax_tree: &T,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<ID<Register<infer::Model>>, InternalError>
    where
        Self: Bind<T>,
    {
        match self.bind(syntax_tree, Config { target: Target::Value }, handler)
        {
            Ok(value) => Ok(value.into_value().unwrap()),
            Err(Error::Semantic(semantic_error)) => {
                let inference =
                    self.create_type_inference(r#type::Constraint::All);

                Ok(self.create_register_assignmnet(
                    Assignment::Errored(Type::Inference(inference)),
                    Some(semantic_error.0),
                ))
            }
            Err(Error::Internal(internal_error)) => Err(internal_error),
        }
    }
}

#[cfg(test)]
mod tests;
