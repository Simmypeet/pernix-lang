#![allow(missing_docs)]

use pernixc_handler::Handler;
use pernixc_ir::value::{
    literal::{self, Literal},
    Value,
};
use pernixc_source_file::SourceElement;
use pernixc_term::r#type::Type;

use crate::{
    bind::{Bind, Config, Expression},
    binder::{Binder, BindingError, Error, UnrecoverableError},
    diagnostic::Diagnostic,
    inference_context::constraint,
};

pub mod array;
pub mod block;
pub mod boolean;
pub mod character;
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
                _function_call,
            ) => todo!(),
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
        type_hint: Option<&Type>,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<Value, UnrecoverableError>
    where
        Self: Bind<T>,
    {
        match self
            .bind(
                syntax_tree,
                &Config::new(super::Target::RValue(type_hint)),
                handler,
            )
            .await
        {
            Ok(value) => Ok(value.into_r_value().unwrap()),
            Err(Error::Binding(semantic_error)) => {
                let inference =
                    self.create_type_inference(constraint::Type::All(false));

                Ok(Value::Literal(Literal::Error(literal::Error {
                    r#type: Type::Inference(inference),
                    span: Some(semantic_error.0),
                })))
            }
            Err(Error::Unrecoverable(internal_error)) => Err(internal_error),
        }
    }
}
