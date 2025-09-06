#![allow(missing_docs)]

use pernixc_handler::Storage;
use pernixc_source_file::SourceElement;

use crate::{
    bind::{Bind, Config, Expression},
    binder::{BindingError, Error},
    diagnostic::Diagnostic,
};

pub mod block;
pub mod numeric;
pub mod postfix;

impl Bind<&pernixc_syntax::expression::Expression>
    for crate::binder::Binder<'_>
{
    async fn bind(
        &mut self,
        syntax_tree: &pernixc_syntax::expression::Expression,
        config: Config,
        handler: &Storage<Diagnostic>,
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
        config: Config,
        handler: &Storage<Diagnostic>,
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
        config: Config,
        handler: &Storage<Diagnostic>,
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
        config: Config,
        handler: &Storage<Diagnostic>,
    ) -> Result<Expression, Error> {
        match syntax_tree {
            pernixc_syntax::expression::prefix::Prefixable::Postfix(
                postfix,
            ) => todo!(),
            pernixc_syntax::expression::prefix::Prefixable::Prefix(prefix) => {
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
        config: Config,
        handler: &Storage<Diagnostic>,
    ) -> Result<Expression, Error> {
        match syntax_tree {
            pernixc_syntax::expression::unit::Unit::Boolean(boolean) => todo!(),
            pernixc_syntax::expression::unit::Unit::Numeric(numeric) => {
                self.bind(numeric, config, handler).await
            }
            pernixc_syntax::expression::unit::Unit::Parenthesized(
                parenthesized,
            ) => todo!(),
            pernixc_syntax::expression::unit::Unit::Struct(_) => todo!(),
            pernixc_syntax::expression::unit::Unit::QualifiedIdentifier(
                qualified_identifier,
            ) => todo!(),
            pernixc_syntax::expression::unit::Unit::Array(array) => todo!(),
            pernixc_syntax::expression::unit::Unit::Phantom(phantom) => todo!(),
            pernixc_syntax::expression::unit::Unit::String(_) => todo!(),
            pernixc_syntax::expression::unit::Unit::Character(character) => {
                todo!()
            }
            pernixc_syntax::expression::unit::Unit::Panic(panic) => todo!(),
        }
    }
}

impl Bind<&pernixc_syntax::expression::block::Block>
    for crate::binder::Binder<'_>
{
    async fn bind(
        &mut self,
        syntax_tree: &pernixc_syntax::expression::block::Block,
        config: Config,
        handler: &Storage<Diagnostic>,
    ) -> Result<Expression, Error> {
        match syntax_tree {
            pernixc_syntax::expression::block::Block::Scope(scope) => todo!(),
            pernixc_syntax::expression::block::Block::IfElse(if_else) => {
                todo!()
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
        config: Config,
        handler: &Storage<Diagnostic>,
    ) -> Result<Expression, Error> {
        match syntax_tree {
            pernixc_syntax::expression::terminator::Terminator::Return(_) => {
                todo!()
            }
            pernixc_syntax::expression::terminator::Terminator::Continue(_) => {
                todo!()
            }
            pernixc_syntax::expression::terminator::Terminator::Express(
                express,
            ) => todo!(),
            pernixc_syntax::expression::terminator::Terminator::Break(_) => {
                todo!()
            }
        }
    }
}
