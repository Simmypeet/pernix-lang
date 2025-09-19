//! Contains the code that binds the syntax tree into IR using binder.

use enum_as_inner::EnumAsInner;
use pernixc_handler::Handler;
use pernixc_ir::{address::Address, value::Value};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_term::r#type::{Qualifier, Type};

use crate::{binder::Error, diagnostic::Diagnostic};

pub mod expression;
pub mod statement;

/// The optional guidance for binding the expression.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Guidance<'a> {
    /// The type hint for the expression.
    ///
    /// This is simply a guidance for the implementation of [`Bind`] and
    /// does not have to be strictly followed or type-checked.
    Expression(Option<&'a Type>),

    /// The expression will be bound at the statement level where its value
    /// will be definitely discarded.
    Statement,
}

/// The result of binding the expression as an l-value. (The value has an
/// address where it is stored.)
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LValue {
    /// The address of the l-value.
    pub address: Address,

    /// The span of the expression that produces this l-value.
    pub span: RelativeSpan,

    /// The qualifier of the l-value.
    pub qualifier: Qualifier,
}

/// The result of binding the expression syntax tree.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Expression {
    /// The expression is bound as an r-value.
    RValue(Value),

    /// The expression is bound as an l-value.
    ///
    /// If the expression can be bound as l-value, it will always be bound as
    /// l-value. However, it's always possible to convert an l-value to an
    /// r-value by simply loading the value from the address.
    LValue(LValue),
}

/// The trait for binding the expression syntax tree.
pub trait Bind<T> {
    /// Binds the given syntax tree to the [`Expression`].
    ///
    /// # Returns an [`Expression`]
    ///
    /// If the expression can be bound as an l-value, the implementation must
    /// always bind it as an l-value.
    ///
    /// # Errors
    ///
    /// If an error occurs during the binding process, an [`Error`] is returned
    /// with the span of the syntax tree that caused the error.
    fn bind<'s, 'c, 't, 'h>(
        &'s mut self,
        syntax_tree: T,
        guidance: &'c Guidance<'t>,
        handler: &'h dyn Handler<Diagnostic>,
    ) -> impl std::future::Future<Output = Result<Expression, Error>>
           + use<'s, 'c, 't, 'h, T, Self>;
}
