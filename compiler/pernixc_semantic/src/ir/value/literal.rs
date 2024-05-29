//! Contains the definition of the [`Literal`] enum.

use enum_as_inner::EnumAsInner;
use pernixc_base::source_file::Span;

use super::{Inspect, InvalidValueError};
use crate::{
    ir::representation::Representation,
    semantic::{
        model::Model,
        term::{
            r#type::{Primitive, Type},
            Tuple,
        },
    },
};

/// Represents a numeric literal.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Numeric<M: Model> {
    /// The span where the numeric literal was defined.
    pub span: Option<Span>,

    /// The numeric string representation.
    pub numeric: String,

    /// The decimal string representation.
    pub decimal: Option<String>,

    /// The type of the numeric literal.
    pub r#type: Type<M>,
}

impl<M: Model> Inspect<M> for Numeric<M> {
    fn type_of(
        &self,
        _: &Representation<M>,
    ) -> Result<Type<M>, InvalidValueError> {
        Ok(self.r#type.clone())
    }

    fn get_span(
        &self,
        _: &Representation<M>,
    ) -> Result<Option<Span>, InvalidValueError> {
        Ok(self.span.clone())
    }
}

/// Represents a boolean literal.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Boolean {
    /// The value of the boolean literal.
    pub value: bool,

    /// The span where the boolean literal was defined.
    pub span: Option<Span>,
}

impl<M: Model> Inspect<M> for Boolean {
    fn type_of(
        &self,
        _: &Representation<M>,
    ) -> Result<Type<M>, InvalidValueError> {
        Ok(Type::Primitive(Primitive::Bool))
    }

    fn get_span(
        &self,
        _: &Representation<M>,
    ) -> Result<Option<Span>, InvalidValueError> {
        Ok(self.span.clone())
    }
}

/// Represents a tuple with no elements.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EmptyTuple {
    /// The span where the empty tuple was defined.
    pub span: Option<Span>,
}

impl<M: Model> Inspect<M> for EmptyTuple {
    fn type_of(
        &self,
        _: &Representation<M>,
    ) -> Result<Type<M>, InvalidValueError> {
        Ok(Type::Tuple(Tuple { elements: Vec::new() }))
    }

    fn get_span(
        &self,
        _: &Representation<M>,
    ) -> Result<Option<Span>, InvalidValueError> {
        Ok(self.span.clone())
    }
}

/// Contains the different kinds of literals.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum Literal<M: Model> {
    EmptyTuple(EmptyTuple),
    Numeric(Numeric<M>),
    Boolean(Boolean),
}

impl<M: Model> Inspect<M> for Literal<M> {
    fn type_of(
        &self,
        ir: &Representation<M>,
    ) -> Result<Type<M>, InvalidValueError> {
        match self {
            Self::EmptyTuple(value) => value.type_of(ir),
            Self::Numeric(value) => value.type_of(ir),
            Self::Boolean(value) => value.type_of(ir),
        }
    }

    fn get_span(
        &self,
        ir: &Representation<M>,
    ) -> Result<Option<Span>, InvalidValueError> {
        match self {
            Self::EmptyTuple(value) => value.get_span(ir),
            Self::Numeric(value) => value.get_span(ir),
            Self::Boolean(value) => value.get_span(ir),
        }
    }
}
