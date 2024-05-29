//! Contains the definition of [`Value`] and its variants.

use enum_as_inner::EnumAsInner;
use pernixc_base::source_file::Span;

use self::{literal::Literal, register::Register};
use super::representation::Representation;
use crate::{
    arena::ID,
    semantic::{model::Model, term::r#type::Type},
    symbol::table::{self, Table},
};

pub mod literal;
pub mod register;

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[error("the value doesn't belong to the given IR object")]
#[allow(missing_docs)]
pub struct InvalidValueError;

/// A trait for retrieving the type and span of the value.
pub trait Inspect<M: Model> {
    /// Returns the type of the value.
    ///
    /// # Errors
    ///
    /// See [`InvalidValueError`] for more information.
    fn type_of(
        &self,
        ir: &Representation<M>,
        table: &Table<impl table::State>,
    ) -> Result<Type<M>, InvalidValueError>;

    /// Returns the span of the value.
    ///
    /// # Errors
    ///
    /// See [`InvalidValueError`] for more information.
    fn get_span(
        &self,
        ir: &Representation<M>,
        table: &Table<impl table::State>,
    ) -> Result<Option<Span>, InvalidValueError>;
}

/// Represents a value in the IR.
///
/// # Register vs Literal
///
/// A value can be either a register or a literal. The simple distinction
/// between the two is that a register involves in having to compose one or
/// more values together, while a literal is a single-simple value.
///
/// For example, the expression `32` is a literal, while the expression
/// `a + 32` is a register.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum Value<M: Model> {
    Literal(Literal<M>),
    Register(ID<Register<M>>),
}

impl<M: Model> Inspect<M> for Value<M> {
    fn type_of(
        &self,
        ir: &Representation<M>,
        table: &Table<impl table::State>,
    ) -> Result<Type<M>, InvalidValueError> {
        match self {
            Value::Literal(literal) => literal.type_of(ir, table),
            Value::Register(register) => register.type_of(ir, table),
        }
    }

    fn get_span(
        &self,
        ir: &Representation<M>,
        table: &Table<impl table::State>,
    ) -> Result<Option<Span>, InvalidValueError> {
        match self {
            Value::Literal(literal) => literal.get_span(ir, table),
            Value::Register(register) => register.get_span(ir, table),
        }
    }
}
