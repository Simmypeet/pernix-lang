//! Contains the definition of the values.

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::{CopyGetters, Getters};
use pernixc_source::Span;
use pernixc_syntax::syntax_tree::expression::NumericLiteral as NumericLiteralSyntaxTree;

use super::{AllocaID, RegisterID, TypeSystem};
use crate::symbol::{EnumVariantID, FieldID};

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

/// Represents a boolean literal constant value.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
pub struct BooleanLiteral {
    /// Specfies the location of the boolean literal.
    #[get = "pub"]
    span: Span,

    /// Gets the value of the boolean.
    #[get_copy = "pub"]
    value: bool,
}

/// Represents an enum literal constant value.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
pub struct EnumLiteral {
    /// Specfies the location of the enum literal.
    span: Span,

    /// Gets the ID of the enum variant that is used for the literal.
    #[get_copy = "pub"]
    enum_variant_id: EnumVariantID,
}

/// Represents a constant value that doesn't require a register assignment.
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum Constant<T: TypeSystem> {
    NumericLiteral(NumericLiteral<T>),
    BooleanLiteral(BooleanLiteral),
    EnumLiteral(EnumLiteral),
}

/// Is an enumeration of all the ways a value can be represented.
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From)]
pub enum Value<T: TypeSystem> {
    /// Obtained from loading a value from register assignment.
    Register(RegisterID),

    /// Obtained from a constant value.
    Constant(Constant<T>),
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
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[allow(missing_docs)]
pub enum Address {
    AllocaID(AllocaID),
    FieldAddress(FieldAddress),
}
