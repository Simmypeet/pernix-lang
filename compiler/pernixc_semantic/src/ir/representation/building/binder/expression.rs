//! Contains the code for binding the expression syntax tree.

use enum_as_inner::EnumAsInner;
use pernixc_base::{
    diagnostic::Handler,
    source_file::{SourceElement, Span},
};
use pernixc_syntax::syntax_tree;

use super::Binder;
use crate::{
    error::{
        ExpectedLValue, FloatingPointLiteralHasIntegralSuffix,
        InvalidNumericSuffix,
    },
    ir::{
        address::Address,
        representation::building::{
            infer::{self, InferenceVariable},
            Model,
        },
        value::{
            literal::{Boolean, Literal, Numeric},
            Value,
        },
    },
    semantic::term::r#type::{self, Type},
    symbol::table::{self, resolution::Observer},
};

/// Is an error occurred while binding the expression syntax tree.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[error("encountered a fatal semantic error")]
pub struct Error(pub Span);

/// An enumeration describes the intended purpose of binding the expression.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Target {
    /// Binds the expression syntax tree for the value.
    Value,

    /// Binds the syntax tree for the underlying address.
    ///
    /// This is used for obtaining the address of some l-value.
    Address { is_mutable: bool },

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
    Value(Value<Model>),

    /// The expression is bound as an l-value.
    Address { address: Address<Model>, address_type: Type<Model>, span: Span },

    /// The expression is bound as a statement.
    SideEffect(Type<Model>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Expected<T, C> {
    Concrete(T),
    Constraint(C),
}

impl<'t, 'h, C, S: table::State, O: Observer<S, super::Model>>
    Binder<'t, 'h, C, S, O>
{
    ////////////////////////////////////////////////////////////////////////////

    /// Binds the given [`syntax_tree::expression::Numeric`]
    pub fn bind_numeric(
        &mut self,
        syntax_tree: &syntax_tree::expression::Numeric,
        config: Config,
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
                        self.handler.receive(Box::new(InvalidNumericSuffix {
                            suffix_span: suffix.span(),
                        }));

                        return Err(Error(syntax_tree.span()));
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
                    self.handler.receive(Box::new(
                        FloatingPointLiteralHasIntegralSuffix {
                            numeric_literal_span: syntax_tree.span(),
                        },
                    ));
                    return Err(Error(syntax_tree.span()));
                }

                Type::Primitive(primitive_type)
            }
            None => {
                // infer the type
                let inference_variable = InferenceVariable::new();

                let constraint = if syntax_tree.decimal().is_some() {
                    infer::TypeConstraint::Floating
                } else {
                    infer::TypeConstraint::Number
                };

                assert!(self
                    .inference_context
                    .register::<Type<_>>(inference_variable, constraint));

                Type::Inference(inference_variable)
            }
        };

        match config.target {
            Target::Value => Ok(Expression::Value(Value::Literal(
                Literal::Numeric(Numeric {
                    span: Some(syntax_tree.span()),
                    numeric: syntax_tree.numeric().span.str().to_owned(),
                    decimal: syntax_tree
                        .decimal()
                        .as_ref()
                        .map(|x| x.numeric().span.str().to_owned()),
                    r#type: numeric_ty,
                }),
            ))),
            Target::Address { .. } => {
                self.handler.receive(Box::new(ExpectedLValue {
                    expression_span: syntax_tree.span(),
                }));
                Err(Error(syntax_tree.span()))
            }
            Target::Statement => Ok(Expression::SideEffect(numeric_ty)),
        }
    }

    ////////////////////////////////////////////////////////////////////////////

    /// Binds the given [`syntax_tree::expression::Boolean`]
    pub fn bind_boolean(
        &mut self,
        syntax_tree: &syntax_tree::expression::Boolean,
        config: Config,
    ) -> Result<Expression, Error> {
        let value = match syntax_tree {
            syntax_tree::expression::Boolean::True(_) => true,
            syntax_tree::expression::Boolean::False(_) => false,
        };

        match config.target {
            Target::Value => {
                Ok(Expression::Value(Value::Literal(Literal::Boolean(
                    Boolean { value, span: Some(syntax_tree.span()) },
                ))))
            }
            Target::Address { .. } => {
                self.handler.receive(Box::new(ExpectedLValue {
                    expression_span: syntax_tree.span(),
                }));
                Err(Error(syntax_tree.span()))
            }
            Target::Statement => Ok(Expression::SideEffect(Type::Primitive(
                r#type::Primitive::Bool,
            ))),
        }
    }

    ////////////////////////////////////////////////////////////////////////////

    /// Binds the given [`syntax_tree::expression::Prefix`]
    pub fn bind_prefix(
        &mut self,
        syntax_tree: &syntax_tree::expression::Prefix,
        config: Config,
    ) -> Result<Expression, Error> {
        todo!()
    }

    ////////////////////////////////////////////////////////////////////////////

    /// Binds the given [`syntax_tree::expression::Prefixable`]
    pub fn bind_prefixable(
        &mut self,
        syntax_tree: &syntax_tree::expression::Prefixable,
        config: Config,
    ) -> Result<Expression, Error> {
        match syntax_tree {
            syntax_tree::expression::Prefixable::Postfixable(syn) => {
                self.bind_postfixable(syn, config)
            }
            syntax_tree::expression::Prefixable::Prefix(syn) => {
                self.bind_prefix(syn, config)
            }
        }
    }

    ////////////////////////////////////////////////////////////////////////////

    /// Binds the given [`syntax_tree::expression::Postfix`]
    pub fn bind_postfix(
        &mut self,
        syntax_tree: &syntax_tree::expression::Postfix,
        config: Config,
    ) -> Result<Expression, Error> {
        todo!()
    }

    ////////////////////////////////////////////////////////////////////////////

    /// Binds the given [`syntax_tree::expression::Postfixable`]
    pub fn bind_postfixable(
        &mut self,
        syntax_tree: &syntax_tree::expression::Postfixable,
        config: Config,
    ) -> Result<Expression, Error> {
        match syntax_tree {
            syntax_tree::expression::Postfixable::Unit(syn) => {
                self.bind_unit(syn, config)
            }
            syntax_tree::expression::Postfixable::Postfix(syn) => {
                self.bind_postfix(syn, config)
            }
        }
    }

    ////////////////////////////////////////////////////////////////////////////

    /// Binds the given [`syntax_tree::expression::Unit`]
    pub fn bind_unit(
        &mut self,
        syntax_tree: &syntax_tree::expression::Unit,
        config: Config,
    ) -> Result<Expression, Error> {
        match syntax_tree {
            syntax_tree::expression::Unit::Boolean(syn) => {
                self.bind_boolean(syn, config)
            }
            syntax_tree::expression::Unit::Numeric(syn) => {
                self.bind_numeric(syn, config)
            }
            syntax_tree::expression::Unit::QualifiedIdentifier(_) => todo!(),
            syntax_tree::expression::Unit::Parenthesized(_) => todo!(),
            syntax_tree::expression::Unit::Struct(_) => todo!(),
            syntax_tree::expression::Unit::Array(_) => todo!(),
            syntax_tree::expression::Unit::Phantom(_) => todo!(),
        }
    }

    ////////////////////////////////////////////////////////////////////////////

    pub fn bind_binary(
        &mut self,
        syntax_tree: &syntax_tree::expression::Binary,
        config: Config,
    ) -> Result<Expression, Error> {
        todo!()
    }

    ////////////////////////////////////////////////////////////////////////////
}

#[cfg(test)]
mod tests;
