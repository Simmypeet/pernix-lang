#![allow(missing_docs)]

use pernixc_handler::Handler;
use pernixc_ir::{
    address::{Address, Memory},
    instruction::{Instruction, Store},
    value::{
        literal::{self, Literal},
        Value,
    },
};
use pernixc_lexical::tree::RelativeLocation;
use pernixc_source_file::SourceElement;
use pernixc_term::r#type::{Qualifier, Type};

use super::Target;
use crate::{
    bind::{Bind, Config, Expression, LValue},
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
pub mod function_call;
pub mod numeric;
pub mod panic;
pub mod parenthesized;
pub mod phantom;
pub mod postfix;
pub mod qualified_identifier;
pub mod string;
pub mod r#struct;

impl Bind<&pernixc_syntax::expression::Expression>
    for crate::binder::Binder<'_>
{
    async fn bind(
        &mut self,
        syntax_tree: &pernixc_syntax::expression::Expression,
        config: &Config<'_>,
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
        config: &Config<'_>,
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
        config: &Config<'_>,
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
        config: &Config<'_>,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<Expression, Error> {
        match syntax_tree {
            pernixc_syntax::expression::prefix::Prefixable::Postfix(
                postfix,
            ) => self.bind(postfix, config, handler).await,
            pernixc_syntax::expression::prefix::Prefixable::Prefix(_prefix) => {
                todo!()
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
        config: &Config<'_>,
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
        _config: &Config<'_>,
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
        _config: &Config<'_>,
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
    /// Binds the given syntax tree as a value. In case of an error, an error
    /// register is returned.
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
                &Config::new(Target::RValue(type_check)),
                handler,
            )
            .await
        {
            Ok(value) => {
                let Some(type_hint) = type_check else {
                    return Ok(value.into_r_value().unwrap());
                };

                // performs type checking and possibly coercion if a type hint
                // is provided
                let checkpoint = self.start_inference_context_checkpoint();

                let value = value.into_r_value().unwrap();

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
                &Config::new(Target::LValue(type_check)),
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
}
