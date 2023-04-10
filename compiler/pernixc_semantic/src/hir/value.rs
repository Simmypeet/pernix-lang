//! Contains the definition of [`Value`] and its related types.

use derive_more::From;
use enum_as_inner::EnumAsInner;

use crate::{
    infer::{Constraint, InferenceID},
    symbol::{
        ty::{PrimitiveType, Type},
        EnumID, VariableID,
    },
    SourceSpan,
};

/// Represents a [`Type`] that is specfically used when building the intermediate representation.
///
/// This enum does not represent the final type of the value. It must be inferred by the
/// [`super::builder::Builder::get_inferrable_type()`] function to transform it into
/// [`InferrableType`] before it can be used in type checking.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
pub enum IntermediateType {
    /// Is a type that is inferred by the [`crate::infer::InferenceContext`].
    Inference(InferenceID),

    /// Is a concrete type that is explicitly specified by the user.
    Type(Type),
}

/// Is a type of the value during the intermediate representation building and can be used in type
/// checking.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
pub enum InferrableType {
    /// The type is still being inferred by the [`crate::infer::InferenceContext`].
    ///
    /// The variant contains the [`Constraint`] representing the constraint of the type that can be
    /// inferred at this point.
    Inferring(Constraint),

    /// The type is already inferred into a concrete type or has already been specified by the
    /// user.
    Type(Type),
}

/// Is a trait that represents a type that can be used as a `type` in the value representation.
#[allow(clippy::module_name_repetitions)]
pub trait ValueType {
    /// Returns a [`Type`] from a [`Type`].
    fn from_type(ty: Type) -> Self;
}

impl ValueType for IntermediateType {
    fn from_type(ty: Type) -> Self { Self::Type(ty) }
}

impl ValueType for Type {
    fn from_type(ty: Type) -> Self { ty }
}

/// Is the trait that all values must implement including [`Value`].
#[allow(clippy::module_name_repetitions)]
pub trait ValueTrait<T: ValueType> {
    /// Gets the [`TypeBinding`] of the value.
    fn type_binding(&self) -> TypeBinding<T>;

    /// Gets the [`SourceSpan`] of the value.
    fn source_span(&self) -> SourceSpan;
}

/// Represents a value in the high-level intermediate representation.
///
/// The value accepts a generic parameter `T` that represents the `type` of the value. When building
/// the intermediate representation, it uses [`InferrableType`] as the generic parameter `T` to
/// represent a type that can be either a concrete type or an inference type. After the intermediate
/// representation is built, it uses [`Type`] as the generic parameter `T` to represent a
/// concrete type.
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum Value<T: ValueType> {
    NumericLiteral(NumericLiteral<T>),
    BooleanLiteral(BooleanLiteral),
    Prefix(Prefix<T>),
    Binary(Binary<T>),
}

impl<T: ValueType + Clone> ValueTrait<T> for Value<T> {
    fn type_binding(&self) -> TypeBinding<T> {
        match self {
            Self::NumericLiteral(literal) => literal.type_binding(),
            Self::BooleanLiteral(literal) => literal.type_binding(),
            Self::Prefix(prefix) => prefix.type_binding(),
            Self::Binary(binary) => binary.type_binding(),
        }
    }

    fn source_span(&self) -> SourceSpan {
        match self {
            Self::NumericLiteral(literal) => literal.source_span(),
            Self::BooleanLiteral(literal) => ValueTrait::<T>::source_span(literal),
            Self::Prefix(prefix) => ValueTrait::<T>::source_span(prefix),
            Self::Binary(binary) => ValueTrait::<T>::source_span(binary),
        }
    }
}

/// Represents a bound representation of [`pernixc_lexical::token::NumericLiteral`]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NumericLiteral<T: ValueType> {
    /// Specifies where the literal is located in the source code.
    pub source_span: SourceSpan,

    /// Is the value of the literal.
    pub ty: T,
}

impl<T: ValueType + Clone> ValueTrait<T> for NumericLiteral<T> {
    fn type_binding(&self) -> TypeBinding<T> {
        TypeBinding {
            ty: self.ty.clone(),
            category: Category::RValue,
        }
    }

    fn source_span(&self) -> SourceSpan { self.source_span.clone() }
}

/// Represents a bound representation of
/// [`pernixc_syntax::syntax_tree::expression::BooleanLiteral`].
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BooleanLiteral {
    /// Specifies where the literal is located in the source code.
    pub source_span: SourceSpan,

    /// Is the boolean value of the literal.
    pub value: bool,
}

impl<T: ValueType> ValueTrait<T> for BooleanLiteral {
    fn type_binding(&self) -> TypeBinding<T> {
        TypeBinding {
            ty: T::from_type(PrimitiveType::Bool.into()),
            category: Category::RValue,
        }
    }

    fn source_span(&self) -> SourceSpan { self.source_span.clone() }
}

/// Is a bound representation of [`pernixc_syntax::syntax_tree::expression::Named`] that is used as
/// an enum literal.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumLiteral {
    /// Specifies where the literal is located in the source code.
    pub source_span: SourceSpan,

    /// Is the variant number in the enum.
    pub variant_number: usize,

    /// Specifies the enum that the literal is from.
    pub enum_id: EnumID,
}

/// Is a bound representation of [`pernixc_syntax::syntax_tree::expression::Named`] that is used as
/// a load.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Load<T: ValueType> {
    /// Specifies where the load is located in the source code.
    pub source_span: SourceSpan,

    /// Specifies the type of the load.
    pub type_binding: TypeBinding<T>,
}

/// Is a bound representation of [`pernixc_syntax::syntax_tree::expression::Named`].
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum Named<T: ValueType> {
    EnumLiteral(EnumLiteral),
    Load(Load<T>),
}

impl<T: ValueType + Clone> ValueTrait<T> for Named<T> {
    fn type_binding(&self) -> TypeBinding<T> {
        match self {
            Self::EnumLiteral(literal) => TypeBinding {
                ty: T::from_type(Type::TypedID(literal.enum_id.into())),
                category: Category::RValue,
            },
            Self::Load(load) => load.type_binding.clone(),
        }
    }

    fn source_span(&self) -> SourceSpan {
        match self {
            Self::EnumLiteral(literal) => literal.source_span.clone(),
            Self::Load(load) => load.source_span.clone(),
        }
    }
}

/// Represents a bound representation of
/// [`pernixc_syntax::syntax_tree::expression::PrefixOperator`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
pub enum PrefixOperator {
    /// Signed numeric/floating-point negation.
    Negate,

    /// Boolean negation.
    LogicalNot,
}

/// Is a bound representation of [`pernixc_syntax::syntax_tree::expression::Prefix`].
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Prefix<T: ValueType> {
    /// Specifies where the prefix expression is located in the source code.
    pub source_span: SourceSpan,

    /// Is the value of the prefix expression.
    pub operand: Box<Value<T>>,

    /// Is the operator of the prefix expression.
    pub prefix_operator: PrefixOperator,

    /// Is the type of the prefix expression.
    pub ty: T,
}

impl<T: ValueType + Clone> ValueTrait<T> for Prefix<T> {
    fn type_binding(&self) -> TypeBinding<T> {
        TypeBinding {
            ty: self.ty.clone(),
            category: Category::RValue,
        }
    }

    fn source_span(&self) -> SourceSpan { self.source_span.clone() }
}

/// Represents a bound representation of
/// [`pernixc_syntax::syntax_tree::expression::BinaryOperator`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    CompoundAdd,
    CompoundSubtract,
    CompoundMultiply,
    CompoundDivide,
    CompoundModulo,
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    LogicalAnd,
    LogicalOr,
    Assign,
}

/// Represents a bound representation of [`pernixc_syntax::syntax_tree::expression::Binary`].
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Binary<T: ValueType> {
    /// Specifies where the binary expression is located in the source code.
    pub source_span: SourceSpan,

    /// Is the left value of the binary expression.
    pub left_operand: Box<Value<T>>,

    /// Is the right value of the binary expression.
    pub right_operand: Box<Value<T>>,

    /// Is the operator of the binary expression.
    pub binary_operator: BinaryOperator,

    /// Is the type of the binary expression.
    pub type_binding: TypeBinding<T>,
}

impl<T: ValueType + Clone> ValueTrait<T> for Binary<T> {
    fn type_binding(&self) -> TypeBinding<T> { self.type_binding.clone() }

    fn source_span(&self) -> SourceSpan { self.source_span.clone() }
}

/// Represents a type binding of a value.
///
/// The type binding comprises of a [`Type`] and a [`Category`] of the value.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeBinding<T: ValueType> {
    /// The type of the value.
    pub ty: T,

    /// The category of the value.
    pub category: Category,
}

/// Is an enumeration that represents the category of a value.  
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
pub enum Category {
    /// The value doesn't have fixed storage location for it. It's a temporary value that isn't
    /// bound to any prominent location in the program
    RValue,

    /// The value has a fixed storage location for it. It's a value that is bound to a prominent
    LValue(LValue),
}

/// Represents a category of value that has its own storage location.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(clippy::module_name_repetitions)]
pub struct LValue {
    /// Specifies whether the value is mutable.
    pub is_mutable: bool,

    /// Is the address whether the value is stored.
    pub address: Address,
}

/// Represents an address of an [`LValue`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum Address {
    ArgumentAddress(ArgumentAddress),
    LocalVariableAddress(LocalVariableAddress),
    StructFieldAddress(StructFieldAddress),
}

/// Represents an address to a function argument.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ArgumentAddress {
    /// The [`VariableID`] used to identify the argument from the [`crate::symbol::Function`].
    pub variable_id: VariableID,
}

/// Represents an address to a local variable
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LocalVariableAddress {
    /// The variable index of the local variable in [`crate::hir::HIR::local_variables()`].
    pub variable_index: usize,
}

/// Represents an address to a member access lvalue.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StructFieldAddress {
    /// The address of the struct value.
    pub struct_address: Box<Address>,
}
