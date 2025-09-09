//! Contains the code that binds the syntax tree into IR using binder.

use enum_as_inner::EnumAsInner;
use pernixc_handler::Handler;
use pernixc_ir::{address::Address, value::Value};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_syntax::item::r#type::Type;
use pernixc_term::r#type::Qualifier;

use crate::{binder::Error, diagnostic::Diagnostic};

pub mod expression;
pub mod statement;

/// An enumeration describes the intended purpose of binding the expression.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Target<'a> {
    /// Binds the expression syntax tree for the r-value.
    ///
    /// All the expressions can be bound as a r-value.
    RValue(Option<&'a Type>),

    /// Binds the syntax tree for the underlying address (l-value).
    ///
    /// This is a *request* to bind the expression as an l-value not strictly
    /// required. If the expression cannot be bound as an l-value, the r-value
    /// is returned instead.
    LValue(Option<&'a Type>),

    /// The expression is being bound for a statement, therefore the produced
    /// value will be discarded right away.
    Statement,
}

/// The configuration object for binding the expression syntax tree.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_new::new,
)]
pub struct Config<'a> {
    /// The intended purpose of binding the expression.
    pub target: Target<'a>,
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
    fn bind<'s, 'c, 't, 'h>(
        &'s mut self,
        syntax_tree: T,
        config: &'c Config<'t>,
        handler: &'h dyn Handler<Diagnostic>,
    ) -> impl std::future::Future<Output = Result<Expression, Error>>
           + use<'s, 'c, 't, 'h, T, Self>;
}
