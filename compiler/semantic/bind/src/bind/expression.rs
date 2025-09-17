#![allow(missing_docs)]

use pernixc_handler::Handler;
use pernixc_ir::{
    address::{Address, Memory, Reference},
    instruction::{Instruction, Store},
    value::{
        literal::{self, Literal},
        register::{Assignment, Borrow, Load},
        Value,
    },
};
use pernixc_lexical::tree::RelativeLocation;
use pernixc_source_file::SourceElement;
use pernixc_term::{
    lifetime::Lifetime,
    r#type::{self, Qualifier, Type},
};

use crate::{
    bind::{Bind, Expression, Guidance, LValue},
    binder::{
        type_check::Expected, Binder, BindingError, Error, UnrecoverableError,
    },
    diagnostic::{Diagnostic, ExpectedLValue},
    inference_context::constraint,
};

pub mod array;
pub mod block;
pub mod boolean;
pub mod character;
pub mod dereference;
pub mod function_call;
pub mod numeric;
pub mod panic;
pub mod parenthesized;
pub mod phantom;
pub mod postfix;
pub mod prefix;
pub mod qualified_identifier;
pub mod string;
pub mod r#struct;

impl Bind<&pernixc_syntax::expression::Expression>
    for crate::binder::Binder<'_>
{
    async fn bind(
        &mut self,
        syntax_tree: &pernixc_syntax::expression::Expression,
        config: &Guidance<'_>,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<Expression, Error> {
        match syntax_tree {
            pernixc_syntax::expression::Expression::Binary(binary) => {
                self.bind(binary, config, handler).await
            }
            pernixc_syntax::expression::Expression::Terminator(terminator) => {
                self.bind(terminator, config, handler).await
            }
        }
    }
}

impl Bind<&pernixc_syntax::expression::binary::Binary>
    for crate::binder::Binder<'_>
{
    async fn bind(
        &mut self,
        syntax_tree: &pernixc_syntax::expression::binary::Binary,
        config: &Guidance<'_>,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<Expression, Error> {
        // TODO: implements proper binary expression binding
        let Some(first) = syntax_tree.first() else {
            return Err(Error::Binding(BindingError(syntax_tree.span())));
        };

        self.bind(&first, config, handler).await
    }
}

impl Bind<&pernixc_syntax::expression::binary::Node>
    for crate::binder::Binder<'_>
{
    async fn bind(
        &mut self,
        syntax_tree: &pernixc_syntax::expression::binary::Node,
        config: &Guidance<'_>,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<Expression, Error> {
        match syntax_tree {
            pernixc_syntax::expression::binary::Node::Prefixable(
                prefixable,
            ) => self.bind(prefixable, config, handler).await,
            pernixc_syntax::expression::binary::Node::Block(block) => {
                self.bind(block, config, handler).await
            }
        }
    }
}

impl Bind<&pernixc_syntax::expression::prefix::Prefixable>
    for crate::binder::Binder<'_>
{
    async fn bind(
        &mut self,
        syntax_tree: &pernixc_syntax::expression::prefix::Prefixable,
        config: &Guidance<'_>,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<Expression, Error> {
        match syntax_tree {
            pernixc_syntax::expression::prefix::Prefixable::Postfix(
                postfix,
            ) => self.bind(postfix, config, handler).await,
            pernixc_syntax::expression::prefix::Prefixable::Prefix(prefix) => {
                self.bind(prefix, config, handler).await
            }
        }
    }
}

impl Bind<&pernixc_syntax::expression::unit::Unit>
    for crate::binder::Binder<'_>
{
    async fn bind(
        &mut self,
        syntax_tree: &pernixc_syntax::expression::unit::Unit,
        config: &Guidance<'_>,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<Expression, Error> {
        match syntax_tree {
            pernixc_syntax::expression::unit::Unit::Boolean(boolean) => {
                self.bind(boolean, config, handler).await
            }
            pernixc_syntax::expression::unit::Unit::Numeric(numeric) => {
                self.bind(numeric, config, handler).await
            }
            pernixc_syntax::expression::unit::Unit::Parenthesized(
                parenthesized,
            ) => self.bind(parenthesized, config, handler).await,
            pernixc_syntax::expression::unit::Unit::Struct(st) => {
                self.bind(st, config, handler).await
            }
            pernixc_syntax::expression::unit::Unit::QualifiedIdentifier(
                qualified_identifier,
            ) => self.bind(qualified_identifier, config, handler).await,
            pernixc_syntax::expression::unit::Unit::Array(array) => {
                self.bind(array, config, handler).await
            }
            pernixc_syntax::expression::unit::Unit::Phantom(phantom) => {
                self.bind(phantom, config, handler).await
            }
            pernixc_syntax::expression::unit::Unit::String(string) => {
                self.bind(string, config, handler).await
            }
            pernixc_syntax::expression::unit::Unit::Character(character) => {
                self.bind(character, config, handler).await
            }
            pernixc_syntax::expression::unit::Unit::Panic(panic) => {
                self.bind(panic, config, handler).await
            }
            pernixc_syntax::expression::unit::Unit::FunctionCall(
                function_call,
            ) => self.bind(function_call, config, handler).await,
        }
    }
}

impl Bind<&pernixc_syntax::expression::block::Block>
    for crate::binder::Binder<'_>
{
    async fn bind(
        &mut self,
        syntax_tree: &pernixc_syntax::expression::block::Block,
        _config: &Guidance<'_>,
        _handler: &dyn Handler<Diagnostic>,
    ) -> Result<Expression, Error> {
        match syntax_tree {
            pernixc_syntax::expression::block::Block::Scope(_scope) => todo!(),
            pernixc_syntax::expression::block::Block::IfElse(if_else) => {
                Ok(Expression::RValue(pernixc_ir::value::Value::Literal(
                    self.create_unreachable(if_else.span()),
                )))
            }
            pernixc_syntax::expression::block::Block::Loop(_) => todo!(),
            pernixc_syntax::expression::block::Block::Match(_) => todo!(),
            pernixc_syntax::expression::block::Block::While(_) => todo!(),
        }
    }
}

impl Bind<&pernixc_syntax::expression::terminator::Terminator>
    for crate::binder::Binder<'_>
{
    async fn bind(
        &mut self,
        syntax_tree: &pernixc_syntax::expression::terminator::Terminator,
        _config: &Guidance<'_>,
        _handler: &dyn Handler<Diagnostic>,
    ) -> Result<Expression, Error> {
        match syntax_tree {
            pernixc_syntax::expression::terminator::Terminator::Return(ret) => {
                Ok(Expression::RValue(pernixc_ir::value::Value::Literal(
                    self.create_unreachable(ret.span()),
                )))
            }
            pernixc_syntax::expression::terminator::Terminator::Continue(_) => {
                todo!()
            }
            pernixc_syntax::expression::terminator::Terminator::Express(
                _express,
            ) => todo!(),
            pernixc_syntax::expression::terminator::Terminator::Break(_) => {
                todo!()
            }
        }
    }
}

impl Binder<'_> {
    /// Binds the given syntax tree as a value and type checks it against
    /// the given type if provided.
    ///
    /// The function also performs coercion if necessary.
    ///
    /// # Errors
    ///
    /// Returns [`UnrecoverableError`] that is returned by the [`Bind::bind()`]
    /// function.
    pub async fn bind_value_or_error<T>(
        &mut self,
        syntax_tree: T,
        type_check: Option<&Type>,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<Value, UnrecoverableError>
    where
        Self: Bind<T>,
    {
        match self
            .bind(
                syntax_tree,
                &Guidance::builder().maybe_type_hint(type_check).build(),
                handler,
            )
            .await
        {
            Ok(Expression::LValue(value)) => {
                let Some(type_hint) = type_check else {
                    return Ok(Value::Register(
                        self.create_register_assignment(
                            Assignment::Load(Load { address: value.address }),
                            value.span,
                        ),
                    ));
                };

                let checkpoint = self.start_inference_context_checkpoint();

                let value_ty =
                    self.type_of_address(&value.address, handler).await?;

                let diagnostic = self
                    .type_check_as_diagnostic(
                        &value_ty,
                        Expected::Known(type_hint.clone()),
                        value.span,
                        handler,
                    )
                    .await?;

                // if there is no diagnostic, commit the inference changes
                let Some(diagnostic) = diagnostic else {
                    self.commit_inference_context_checkpoint(checkpoint);

                    return self
                        .try_reborrow_mutable_reference(
                            value, value_ty, handler,
                        )
                        .await;
                };

                // TODO: performs rollback and coercion if possible
                self.restore_inference_context_checkpoint(checkpoint);

                let value_span = value.span;

                self.try_coerce_mutable_reference(value, type_hint, handler)
                    .await?
                    .map_or_else(
                        || {
                            // failed to coerce, report the diagnostic
                            handler.receive(diagnostic);

                            Ok(Value::Literal(Literal::Error(literal::Error {
                                r#type: type_hint.clone(),
                                span: Some(value_span),
                            })))
                        },
                        Ok,
                    )
            }

            Ok(Expression::RValue(value)) => {
                let Some(type_hint) = type_check else {
                    return Ok(value);
                };

                // performs type checking and possibly coercion if a type hint
                // is provided
                let checkpoint = self.start_inference_context_checkpoint();

                let span = self.span_of_value(&value);
                let value_ty = self.type_of_value(&value, handler).await?;

                let diagnostic = self
                    .type_check_as_diagnostic(
                        &value_ty,
                        Expected::Known(type_hint.clone()),
                        span,
                        handler,
                    )
                    .await?;

                // if there is no diagnostic, commit the inference changes
                let Some(diagnostic) = diagnostic else {
                    self.commit_inference_context_checkpoint(checkpoint);
                    return Ok(value);
                };

                // TODO: performs rollback and coercion if possible
                self.restore_inference_context_checkpoint(checkpoint);

                handler.receive(diagnostic);

                Ok(Value::Literal(Literal::Error(literal::Error {
                    r#type: type_hint.clone(),
                    span: Some(span),
                })))
            }

            Err(Error::Binding(semantic_error)) => type_check.map_or_else(
                || {
                    let inference = self
                        .create_type_inference(constraint::Type::All(false));

                    Ok(Value::Literal(Literal::Error(literal::Error {
                        r#type: Type::Inference(inference),
                        span: Some(semantic_error.0),
                    })))
                },
                |type_hint| {
                    Ok(Value::Literal(Literal::Error(literal::Error {
                        r#type: type_hint.clone(),
                        span: Some(semantic_error.0),
                    })))
                },
            ),

            Err(Error::Unrecoverable(internal_error)) => Err(internal_error),
        }
    }

    /// Binds the given syntax tree as an address.
    ///
    /// If the expression cannot be bound as an address, a variable will be
    /// created an the value is stored in the variable; the address of the
    /// variable is returned.
    async fn bind_as_lvalue<'a, T>(
        &mut self,
        syntax_tree: &'a T,
        create_temporary: bool,
        type_check: Option<&Type>,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<LValue, Error>
    where
        T: SourceElement<Location = RelativeLocation>,
        Self: Bind<&'a T>,
    {
        match self
            .bind(
                syntax_tree,
                &Guidance::builder().maybe_type_hint(type_check).build(),
                handler,
            )
            .await?
        {
            Expression::RValue(value) => {
                if create_temporary {
                    let type_of_value =
                        self.type_of_value(&value, handler).await?;

                    let alloca_id =
                        self.create_alloca(type_of_value, syntax_tree.span());

                    // initialize
                    self.push_instruction(Instruction::Store(Store {
                        address: Address::Memory(Memory::Alloca(alloca_id)),
                        span: Some(syntax_tree.span()),
                        value,
                    }));

                    Ok(LValue {
                        address: Address::Memory(Memory::Alloca(alloca_id)),
                        span: syntax_tree.span(),
                        qualifier: Qualifier::Mutable,
                    })
                } else {
                    handler.receive(
                        ExpectedLValue { expression_span: syntax_tree.span() }
                            .into(),
                    );

                    Err(Error::Binding(BindingError(syntax_tree.span())))
                }
            }
            Expression::LValue(lvalue) => Ok(lvalue),
        }
    }

    async fn try_reborrow_mutable_reference(
        &mut self,
        value: LValue,
        value_ty: Type,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<Value, UnrecoverableError> {
        // ERGONOMIC: if the type is a mutable reference, we will
        // do mutable reborrow here, improving ergonomics
        let simplified_type =
            self.simplify_type(value_ty, value.span, handler).await?;

        // not a mutable reference, simply load
        if !matches!(
            &simplified_type.result,
            Type::Reference(r#type::Reference {
                qualifier: Qualifier::Mutable,
                ..
            })
        ) {
            return Ok(Value::Register(self.create_register_assignment(
                Assignment::Load(Load { address: value.address }),
                value.span,
            )));
        }

        // mutable reference, do mutable reborrow
        let register_id = self.create_register_assignment(
            Assignment::Borrow(Borrow {
                address: Address::Reference(Reference {
                    qualifier: Qualifier::Mutable,
                    reference_address: Box::new(value.address),
                }),
                qualifier: Qualifier::Mutable,
                lifetime: Lifetime::Erased,
            }),
            value.span,
        );

        Ok(Value::Register(register_id))
    }

    async fn try_coerce_mutable_reference(
        &mut self,
        value: LValue,
        expected_ty: &Type,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<Option<Value>, UnrecoverableError> {
        // try coerce as &mut T to &T
        let current_value_ty =
            self.type_of_address(&value.address, handler).await?;

        let current_expected_ty = self
            .simplify_type(expected_ty.clone(), value.span, handler)
            .await?;

        let (
            Type::Reference(r#type::Reference {
                qualifier: Qualifier::Mutable,
                pointee: value_pointee,
                ..
            }),
            Type::Reference(r#type::Reference {
                qualifier: Qualifier::Immutable,
                pointee: expected_pointee,
                ..
            }),
        ) = (&current_value_ty, &current_expected_ty.result)
        else {
            return Ok(None);
        };

        let checkpoint = self.start_inference_context_checkpoint();

        // try unify the pointee types
        let diagnostic = self
            .type_check_as_diagnostic(
                value_pointee,
                Expected::Known(*expected_pointee.clone()),
                value.span,
                handler,
            )
            .await?;

        // failed to unify the pointee types, rollback
        if diagnostic.is_some() {
            self.restore_inference_context_checkpoint(checkpoint);

            return Ok(None);
        }

        // successfully unified, commit the changes
        self.commit_inference_context_checkpoint(checkpoint);

        let register_id = self.create_register_assignment(
            Assignment::Borrow(Borrow {
                address: Address::Reference(Reference {
                    qualifier: Qualifier::Mutable,
                    reference_address: Box::new(value.address),
                }),
                qualifier: Qualifier::Immutable,
                lifetime: Lifetime::Erased,
            }),
            value.span,
        );

        Ok(Some(Value::Register(register_id)))
    }
}
