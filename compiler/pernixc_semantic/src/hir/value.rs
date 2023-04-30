//! Contains the definition of the values.

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::{CopyGetters, Getters};
use pernixc_source::{SourceElement, Span};
use pernixc_syntax::syntax_tree::expression::NumericLiteral as NumericLiteralSyntaxTree;

use super::{
    builder::{Builder, TypeID},
    AllocaID, Hir, InvalidValueError, Reachability, RegisterID, TypeBinding, TypeSystem,
    ValueContext,
};
use crate::{
    infer::InferableType,
    symbol::{
        ty::{PrimitiveType, Type},
        EnumVariantID, FieldID,
    },
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

impl ValueContext<Type, NumericLiteral<Type>> for Hir {
    fn get_type_binding(
        &self,
        value: &NumericLiteral<Type>,
    ) -> Result<TypeBinding<Type>, InvalidValueError> {
        Ok(TypeBinding {
            ty: value.ty,
            reachability: Reachability::Reachable,
        })
    }

    fn get_span(&self, value: &NumericLiteral<Type>) -> Result<Span, InvalidValueError> {
        Ok(value.numeric_literal_syntax_tree.span())
    }
}

impl ValueContext<InferableType, NumericLiteral<TypeID>> for Builder {
    fn get_type_binding(
        &self,
        value: &NumericLiteral<TypeID>,
    ) -> Result<TypeBinding<InferableType>, InvalidValueError> {
        Ok(TypeBinding {
            ty: match value.ty {
                TypeID::InferenceID(inference_id) => self
                    .inference_context()
                    .get_inferable_type(inference_id)
                    .map_err(|_| InvalidValueError)?,
                TypeID::Type(ty) => InferableType::Type(ty),
            },
            reachability: Reachability::Reachable,
        })
    }

    fn get_span(&self, value: &NumericLiteral<TypeID>) -> Result<Span, InvalidValueError> {
        Ok(value.numeric_literal_syntax_tree.span())
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

impl ValueContext<Type, BooleanLiteral> for Hir {
    fn get_type_binding(
        &self,
        value: &BooleanLiteral,
    ) -> Result<TypeBinding<Type>, InvalidValueError> {
        Ok(TypeBinding {
            ty: Type::PrimitiveType(PrimitiveType::Bool),
            reachability: Reachability::Reachable,
        })
    }

    fn get_span(&self, value: &BooleanLiteral) -> Result<Span, InvalidValueError> { Ok(value.span) }
}

impl ValueContext<InferableType, BooleanLiteral> for Builder {
    fn get_type_binding(
        &self,
        value: &BooleanLiteral,
    ) -> Result<TypeBinding<InferableType>, InvalidValueError> {
        Ok(TypeBinding {
            ty: InferableType::Type(Type::PrimitiveType(PrimitiveType::Bool)),
            reachability: Reachability::Reachable,
        })
    }

    fn get_span(&self, value: &BooleanLiteral) -> Result<Span, InvalidValueError> { Ok(value.span) }
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

impl ValueContext<Type, EnumLiteral> for Hir {
    fn get_type_binding(
        &self,
        value: &EnumLiteral,
    ) -> Result<TypeBinding<Type>, InvalidValueError> {
        let enum_variant = self
            .table
            .get_enum_variant(value.enum_variant_id)
            .map_err(|_| InvalidValueError)?;

        Ok(TypeBinding {
            ty: Type::TypableID(enum_variant.parent_enum_id().into()),
            reachability: Reachability::Reachable,
        })
    }

    fn get_span(&self, value: &EnumLiteral) -> Result<Span, InvalidValueError> {
        Ok(value.span.clone())
    }
}

impl ValueContext<InferableType, EnumLiteral> for Builder {
    fn get_type_binding(
        &self,
        value: &EnumLiteral,
    ) -> Result<TypeBinding<InferableType>, InvalidValueError> {
        let enum_variant = self
            .table()
            .get_enum_variant(value.enum_variant_id)
            .map_err(|_| InvalidValueError)?;

        Ok(TypeBinding {
            ty: InferableType::Type(Type::TypableID(enum_variant.parent_enum_id().into())),
            reachability: Reachability::Reachable,
        })
    }

    fn get_span(&self, value: &EnumLiteral) -> Result<Span, InvalidValueError> {
        Ok(value.span.clone())
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

impl ValueContext<Type, Constant<Type>> for Hir {
    fn get_type_binding(
        &self,
        value: &Constant<Type>,
    ) -> Result<TypeBinding<Type>, InvalidValueError> {
        match value {
            Constant::NumericLiteral(val) => self.get_type_binding(val),
            Constant::BooleanLiteral(val) => self.get_type_binding(val),
            Constant::EnumLiteral(val) => self.get_type_binding(val),
        }
    }

    fn get_span(&self, value: &Constant<Type>) -> Result<Span, InvalidValueError> {
        match value {
            Constant::NumericLiteral(val) => self.get_span(val),
            Constant::BooleanLiteral(val) => self.get_span(val),
            Constant::EnumLiteral(val) => self.get_span(val),
        }
    }
}

/// Represents a value that doesn't really exist, but is used to replace a value that is unable to
/// be constructed.
///
/// This is generally used for error handling by replacing a value that encountered an error with
/// a placeholder value.
#[derive(Debug, Clone, PartialEq, Eq, Hash, CopyGetters, Getters)]
pub struct PlaceHolder<T: TypeSystem> {
    /// The location of the expression that this placeholder substitutes.
    #[get = "pub"]
    pub(super) span: Span,

    /// The type of the placeholder.
    #[get_copy = "pub"]
    pub(super) ty: T,
}

impl ValueContext<Type, PlaceHolder<Type>> for Hir {
    fn get_type_binding(
        &self,
        value: &PlaceHolder<Type>,
    ) -> Result<TypeBinding<Type>, InvalidValueError> {
        Ok(TypeBinding {
            ty: value.ty,
            reachability: Reachability::Reachable,
        })
    }

    fn get_span(&self, value: &PlaceHolder<Type>) -> Result<Span, InvalidValueError> {
        Ok(value.span.clone())
    }
}

impl ValueContext<InferableType, PlaceHolder<TypeID>> for Builder {
    fn get_type_binding(
        &self,
        value: &PlaceHolder<TypeID>,
    ) -> Result<TypeBinding<InferableType>, InvalidValueError> {
        Ok(TypeBinding {
            ty: match value.ty {
                TypeID::InferenceID(id) => self
                    .inference_context()
                    .get_inferable_type(id)
                    .map_err(|_| InvalidValueError)?,
                TypeID::Type(ty) => InferableType::Type(ty),
            },
            reachability: Reachability::Reachable,
        })
    }

    fn get_span(&self, value: &PlaceHolder<TypeID>) -> Result<Span, InvalidValueError> { todo!() }
}

/// Is an enumeration of all the ways a value can be represented.
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From)]
pub enum Value<T: TypeSystem> {
    /// Obtained from loading a value from register assignment.
    Register(RegisterID),

    /// Obtained from a constant value.
    Constant(Constant<T>),

    /// Is used when encountering an error.
    PlaceHolder(PlaceHolder<T>),
}

impl ValueContext<Type, Value<Type>> for Hir {
    fn get_type_binding(
        &self,
        value: &Value<Type>,
    ) -> Result<TypeBinding<Type>, InvalidValueError> {
        todo!()
    }

    fn get_span(&self, value: &Value<Type>) -> Result<Span, InvalidValueError> { todo!() }
}

impl ValueContext<InferableType, Value<TypeID>> for Builder {
    fn get_type_binding(
        &self,
        value: &Value<TypeID>,
    ) -> Result<TypeBinding<InferableType>, InvalidValueError> {
        todo!()
    }

    fn get_span(&self, value: &Value<TypeID>) -> Result<Span, InvalidValueError> { todo!() }
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
