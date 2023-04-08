use derive_more::From;
use enum_as_inner::EnumAsInner;

use crate::{infer::InferenceID, symbol::ty::Type, SourceSpan};

/// Represents a [`ValueType`] that is specfically used when building the intermediate
/// representation.
///
/// This type is used to represent a type that can be either a concrete type or an inference type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
pub enum InferrableType {
    /// Is a type that is inferred by the [`crate::infer::InferenceContext`].
    Inference(InferenceID),

    /// Is a concrete type that is explicitly specified by the user.
    ConcreteType(Type),
}

/// Is a trait that represents a type that can be used as a `type` in the value representation.
pub trait ValueType {
    /// Returns a [`ValueType`] from a [`Type`].
    fn from_symbol_type(ty: Type) -> Self;
}

impl ValueType for InferrableType {
    fn from_symbol_type(ty: Type) -> Self { Self::ConcreteType(ty) }
}

impl ValueType for Type {
    fn from_symbol_type(ty: Type) -> Self { ty }
}

/// Represents a value in the high-level intermediate representation.
///
/// The value accepts a generic parameter `T` that represents the `type` of the value. When building
/// the intermediate representation, it uses [`InferrableType`] as the generic parameter `T` to
/// represent a type that can be either a concrete type or an inference type. After the intermediate
/// representation is built, it uses [`Type`] as the generic parameter `T` to represent a concrete
/// type.
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From)]
pub enum Value<T: ValueType> {
    NumericLiteral(NumericLiteral<T>),
}

/// Represents a bound representation of [`pernixc_lexical::token::NumericLiteral`]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NumericLiteral<T: ValueType> {
    /// Specifies where the literal is located in the source code.
    pub source_span: SourceSpan,

    /// Is the value of the literal.
    pub ty: T,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ValueTypeBinding<T: ValueType> {
    pub ty: T,
    pub category: ValueCategory,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
pub enum ValueCategory {
    RValue,
    LValue,
}

pub struct LValue {
    pub is_mutable: bool,
    pub ty: Type,
}

pub type HirValue = Value<Type>;
pub type IntermediateValue = Value<InferrableType>;
