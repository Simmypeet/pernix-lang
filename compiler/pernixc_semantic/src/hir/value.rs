//! Contains the definition of the values.

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::{CopyGetters, Getters};
use pernixc_source::{SourceElement, Span};
use pernixc_syntax::syntax_tree::expression::NumericLiteral as NumericLiteralSyntaxTree;

use super::{
    AllocaID, Container, InvalidValueError, Reachability, RegisterID, TypeSystem, ValueInspect,
};
use crate::symbol::{
    ty::{PrimitiveType, Type},
    EnumVariantID, FieldID, ParameterID,
};

pub mod binding;

/// Represents a numeric literal constant value.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
pub struct NumericLiteral<T: TypeSystem> {
    /// The token that represents the numeric literal.
    #[get = "pub"]
    pub(super) numeric_literal_syntax_tree: NumericLiteralSyntaxTree,

    /// The type of the numeric literal.
    #[get_copy = "pub"]
    pub(super) ty: T,
}

impl<T: TypeSystem> ValueInspect<T, NumericLiteral<T>> for Container<T> {
    fn get_type(&self, value: &NumericLiteral<T>) -> Result<T, InvalidValueError> { Ok(value.ty) }

    fn get_span(&self, value: &NumericLiteral<T>) -> Result<Span, InvalidValueError> {
        Ok(value.numeric_literal_syntax_tree.span())
    }

    fn get_reachability(&self, _: &NumericLiteral<T>) -> Result<Reachability, InvalidValueError> {
        Ok(Reachability::Reachable)
    }
}

/// Represents a boolean literal constant value.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
pub struct BooleanLiteral {
    /// Specfies the location of the boolean literal.
    #[get = "pub"]
    pub(super) span: Span,

    /// Gets the value of the boolean.
    #[get_copy = "pub"]
    pub(super) value: bool,
}

impl<T: TypeSystem> ValueInspect<T, BooleanLiteral> for Container<T> {
    fn get_type(&self, _: &BooleanLiteral) -> Result<T, InvalidValueError> {
        Ok(T::from_type(Type::PrimitiveType(PrimitiveType::Bool)))
    }

    fn get_span(&self, value: &BooleanLiteral) -> Result<Span, InvalidValueError> {
        Ok(value.span.clone())
    }

    fn get_reachability(&self, _: &BooleanLiteral) -> Result<Reachability, InvalidValueError> {
        Ok(Reachability::Reachable)
    }
}

/// Represents an enum literal constant value.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
pub struct EnumLiteral {
    /// Specfies the location of the enum literal.
    pub(super) span: Span,

    /// Gets the ID of the enum variant that is used for the literal.
    #[get_copy = "pub"]
    pub(super) enum_variant_id: EnumVariantID,
}

impl<T: TypeSystem> ValueInspect<T, EnumLiteral> for Container<T> {
    fn get_type(&self, value: &EnumLiteral) -> Result<T, InvalidValueError> {
        Ok(T::from_type(Type::TypedID(
            self.table
                .get_enum_variant(value.enum_variant_id)
                .map_err(|_| InvalidValueError)?
                .parent_enum_id()
                .into(),
        )))
    }

    fn get_span(&self, value: &EnumLiteral) -> Result<Span, InvalidValueError> {
        Ok(value.span.clone())
    }

    fn get_reachability(&self, _: &EnumLiteral) -> Result<Reachability, InvalidValueError> {
        Ok(Reachability::Reachable)
    }
}

/// Represents a constant value that doesn't require a register assignment.
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum Constant<T: TypeSystem> {
    NumericLiteral(NumericLiteral<T>),
    BooleanLiteral(BooleanLiteral),
    EnumLiteral(EnumLiteral),
}

impl<T: TypeSystem> ValueInspect<T, Constant<T>> for Container<T> {
    fn get_type(&self, value: &Constant<T>) -> Result<T, InvalidValueError> {
        match value {
            Constant::NumericLiteral(n) => self.get_type(n),
            Constant::BooleanLiteral(b) => self.get_type(b),
            Constant::EnumLiteral(e) => self.get_type(e),
        }
    }

    fn get_span(&self, value: &Constant<T>) -> Result<Span, InvalidValueError> {
        match value {
            Constant::NumericLiteral(n) => self.get_span(n),
            Constant::BooleanLiteral(b) => self.get_span(b),
            Constant::EnumLiteral(e) => self.get_span(e),
        }
    }

    fn get_reachability(&self, value: &Constant<T>) -> Result<Reachability, InvalidValueError> {
        match value {
            Constant::NumericLiteral(n) => self.get_reachability(n),
            Constant::BooleanLiteral(b) => self.get_reachability(b),
            Constant::EnumLiteral(e) => self.get_reachability(e),
        }
    }
}

/// Represents a value that doesn't really exist, but is used to replace a value that is unable to
/// be constructed.
///
/// This is generally used for error handling by replacing a value that encountered an error with
/// a placeholder value.
#[derive(Debug, Clone, PartialEq, Eq, Hash, CopyGetters, Getters)]
pub struct Placeholder<T: TypeSystem> {
    /// The location of the expression that this placeholder substitutes.
    #[get = "pub"]
    pub(super) span: Span,

    /// The type of the placeholder.
    #[get_copy = "pub"]
    pub(super) ty: T,
}

impl<T: TypeSystem> ValueInspect<T, Placeholder<T>> for Container<T> {
    fn get_type(&self, value: &Placeholder<T>) -> Result<T, InvalidValueError> { Ok(value.ty) }

    fn get_span(&self, value: &Placeholder<T>) -> Result<Span, InvalidValueError> {
        Ok(value.span.clone())
    }

    fn get_reachability(&self, _value: &Placeholder<T>) -> Result<Reachability, InvalidValueError> {
        Ok(Reachability::Reachable)
    }
}

/// Is an enumeration of all the ways a value can be represented.
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From)]
pub enum Value<T: TypeSystem> {
    /// Obtained from loading a value from register assignment.
    Register(RegisterID),

    /// Obtained from a constant value.
    Constant(Constant<T>),

    /// Is used when encountering an error.
    Placeholder(Placeholder<T>),
}

impl<T: TypeSystem> ValueInspect<T, Value<T>> for Container<T> {
    fn get_type(&self, value: &Value<T>) -> Result<T, InvalidValueError> {
        match value {
            Value::Register(id) => self.get_type(id),
            Value::Constant(c) => self.get_type(c),
            Value::Placeholder(p) => self.get_type(p),
        }
    }

    fn get_span(&self, value: &Value<T>) -> Result<Span, InvalidValueError> {
        match value {
            Value::Register(id) => self.get_span(id),
            Value::Constant(c) => self.get_span(c),
            Value::Placeholder(p) => self.get_span(p),
        }
    }

    fn get_reachability(&self, value: &Value<T>) -> Result<Reachability, InvalidValueError> {
        match value {
            Value::Register(id) => self.get_reachability(id),
            Value::Constant(c) => self.get_reachability(c),
            Value::Placeholder(p) => self.get_reachability(p),
        }
    }
}

/// Represents an adress to a particular field member.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
pub struct FieldAddress {
    /// Specifies the address of the struct that contains the field.
    #[get = "pub"]
    base_address: Box<Address>,

    /// The ID of the field that is being addressed.
    #[get_copy = "pub"]
    field_id: FieldID,
}

/// Represents an address to a particular stack allocated memory.
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum Address {
    AllocaID(AllocaID),
    ParameterID(ParameterID),
    FieldAddress(FieldAddress),
}
