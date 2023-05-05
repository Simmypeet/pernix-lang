//! Contains all the definitions of bound syntax trees.

use std::collections::HashMap;

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::{CopyGetters, Getters};
use pernixc_source::Span;
use pernixc_syntax::syntax_tree::expression::PrefixOperator;

use super::{Address, TypeSystem, Value};
use crate::{
    cfg::BasicBlockID,
    hir::{Container, InvalidValueError, ValueInspect},
    symbol::{
        ty::{PrimitiveType, Type},
        FieldID, OverloadID, StructID,
    },
};

/// Represents a bound syntax tree.
///
/// The bound syntax tree is attached with type information and additional semantics.
#[derive(Debug, Clone, PartialEq, Eq, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum Binding<T: TypeSystem> {
    FunctionCall(FunctionCall<T>),
    Prefix(Prefix<T>),
    Load(Load),
    StructLiteral(StructLiteral<T>),
    MemberAccess(MemberAccess<T>),
    Binary(Binary<T>),
    PhiNode(PhiNode<T>),
}

impl<T: TypeSystem> ValueInspect<T, Binding<T>> for Container<T> {
    fn get_type(&self, value: &Binding<T>) -> Result<T, InvalidValueError> {
        match value {
            Binding::FunctionCall(value) => self.get_type(value),
            Binding::Prefix(value) => self.get_type(value),
            Binding::Load(value) => self.get_type(value),
            Binding::StructLiteral(value) => self.get_type(value),
            Binding::MemberAccess(value) => self.get_type(value),
            Binding::Binary(value) => self.get_type(value),
            Binding::PhiNode(value) => self.get_type(value),
        }
    }

    fn get_span(&self, value: &Binding<T>) -> Result<pernixc_source::Span, InvalidValueError> {
        match value {
            Binding::FunctionCall(value) => self.get_span(value),
            Binding::Prefix(value) => self.get_span(value),
            Binding::Load(value) => self.get_span(value),
            Binding::StructLiteral(value) => self.get_span(value),
            Binding::MemberAccess(value) => self.get_span(value),
            Binding::Binary(value) => self.get_span(value),
            Binding::PhiNode(value) => self.get_span(value),
        }
    }
}

/// Represents a bound [`FunctionCall`](pernixc_syntax::syntax_tree::expression::FunctionCall)
/// syntax tree.
#[derive(Debug, Clone, PartialEq, Eq, Getters, Hash, CopyGetters)]
pub struct FunctionCall<T: TypeSystem> {
    /// Specifies the location of the function call.
    #[get = "pub"]
    pub(in crate::hir) span: Span,

    /// The ID of the function overload that is being called.
    #[get_copy = "pub"]
    pub(in crate::hir) overload_id: OverloadID,

    /// The arguments that are passed to the function.
    #[get = "pub"]
    pub(in crate::hir) arguments: Vec<Value<T>>,
}

impl<T: TypeSystem> ValueInspect<T, FunctionCall<T>> for Container<T> {
    fn get_type(&self, value: &FunctionCall<T>) -> Result<T, InvalidValueError> {
        let overload = self
            .table
            .get_overload(value.overload_id)
            .map_err(|_| InvalidValueError)?;

        Ok(T::from_type(overload.return_type()))
    }

    fn get_span(&self, value: &FunctionCall<T>) -> Result<Span, InvalidValueError> {
        Ok(value.span.clone())
    }
}

/// Represents a bound [`Prefix`](pernixc_syntax::syntax_tree::expression::Prefix) syntax tree.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct Prefix<T: TypeSystem> {
    /// Specifies the location of the prefix expression.
    #[get = "pub"]
    pub(in crate::hir) span: Span,

    /// The operator that is applied to the operand.
    #[get = "pub"]
    pub(in crate::hir) prefix_operator: PrefixOperator,

    /// The operand that is being operated on.
    #[get = "pub"]
    pub(in crate::hir) operand: Value<T>,
}

impl<T: TypeSystem> ValueInspect<T, Prefix<T>> for Container<T> {
    fn get_type(&self, value: &Prefix<T>) -> Result<T, InvalidValueError> {
        self.get_type(&value.operand)
    }

    fn get_span(&self, value: &Prefix<T>) -> Result<Span, InvalidValueError> {
        Ok(value.span.clone())
    }
}

/// Specifies how the [`Load`] loads the value from the address.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
pub enum LoadType {
    /// The value is moved from the address.
    Move,

    /// The value is copied from the address.
    Copy,
}

/// Represents a bound syntax tree that loads a value from an address.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
pub struct Load {
    /// The span of the [`Load`].
    #[get = "pub"]
    pub(in crate::hir) span: Span,

    /// Determines how the value is loaded from the address.
    #[get_copy = "pub"]
    pub(in crate::hir) load_type: LoadType,

    /// The address of the value.
    #[get = "pub"]
    pub(in crate::hir) address: Address,
}

impl<T: TypeSystem> ValueInspect<T, Load> for Container<T> {
    fn get_type(&self, value: &Load) -> Result<T, InvalidValueError> {
        match &value.address {
            Address::AllocaID(id) => Ok(self.allocas.get(*id).map_err(|_| InvalidValueError)?.ty()),
            Address::ParameterID(id) => Ok(T::from_type(
                self.table
                    .get_parameter(*id)
                    .map_err(|_| InvalidValueError)?
                    .type_binding()
                    .ty,
            )),
            Address::FieldAddress(address) => Ok(T::from_type(
                self.table
                    .get_field(address.field_id)
                    .map_err(|_| InvalidValueError)?
                    .ty(),
            )),
        }
    }

    fn get_span(&self, value: &Load) -> Result<Span, InvalidValueError> { Ok(value.span.clone()) }
}

/// Represents a bound
/// [`StructLiteral`](pernixc_syntax::syntax_tree::expression::StructLiteral) syntax tree.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
pub struct StructLiteral<T: TypeSystem> {
    /// The span of the struct literal.
    #[get = "pub "]
    pub(in crate::hir) span: Span,

    /// The struct ID that this literal instantiates.
    #[get_copy = "pub"]
    pub(in crate::hir) struct_id: StructID,

    /// Is a list of tuple pairs of field IDs and the value to initialize them with.
    #[get = "pub"]
    pub(in crate::hir) initializations: Vec<(FieldID, Value<T>)>,
}

impl<T: TypeSystem> ValueInspect<T, StructLiteral<T>> for Container<T> {
    fn get_type(&self, value: &StructLiteral<T>) -> Result<T, InvalidValueError> {
        Ok(T::from_type(Type::TypedID(value.struct_id.into())))
    }

    fn get_span(&self, value: &StructLiteral<T>) -> Result<Span, InvalidValueError> {
        Ok(value.span.clone())
    }
}

/// Represents a bound [`MemberAccess`](pernixc_syntax::syntax_tree::expression::MemberAccess)
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
pub struct MemberAccess<T: TypeSystem> {
    /// The span of the member access.
    #[get = "pub"]
    pub(in crate::hir) span: Span,

    /// The operand of the member access.
    #[get = "pub"]
    pub(in crate::hir) operand: Value<T>,

    /// The field ID that is being accessed.
    #[get_copy = "pub"]
    pub(in crate::hir) field_id: FieldID,
}

impl<T: TypeSystem> ValueInspect<T, MemberAccess<T>> for Container<T> {
    fn get_type(&self, value: &MemberAccess<T>) -> Result<T, InvalidValueError> {
        self.table
            .get_field(value.field_id)
            .map_err(|_| InvalidValueError)
            .map(|x| T::from_type(Type::TypedID(x.parent_struct_id().into())))
    }

    fn get_span(&self, value: &MemberAccess<T>) -> Result<Span, InvalidValueError> {
        Ok(value.span.clone())
    }
}

/// Is an enumeration of arithmetic binary operators that can be applied numeric values.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum ArithmeticOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
}

/// Is an enumeration of comparison binary operators that can be applied to numeric values and
/// yields a boolean value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum ComparisonOperator {
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}

/// Is an enumeration of equality binary operators that can be applied to values of any primitive
/// types and yields a boolean value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum EqualityOperator {
    Equal,
    NotEqual,
}

/// Represents a `phi` instruction in the SSA form.
///
/// Some expressions in the Pernix programming language that involves in altering the control flow
/// and branching, such as logical short-circuiting, are lowered into `phi` instructions.
#[derive(Debug, Clone, PartialEq, Eq, Getters, CopyGetters)]
pub struct PhiNode<T: TypeSystem> {
    /// Is the span of the expression that is lowered into this `phi` instruction.
    #[get = "pub"]
    pub(in crate::hir) span: Span,

    /// Maps the predecessor basic blocks to the values that are passed from them.
    #[get = "pub"]
    pub(in crate::hir) values_by_predecessor: HashMap<BasicBlockID, Value<T>>,

    /// Specifies the kind of expression that is lowered into this `phi` instruction.
    #[get_copy = "pub"]
    pub(in crate::hir) phi_node_source: PhiNodeSource,
}

impl<T: TypeSystem> ValueInspect<T, PhiNode<T>> for Container<T> {
    fn get_type(&self, value: &PhiNode<T>) -> Result<T, InvalidValueError> {
        // assume that all values are of the same type, take the first one
        self.get_type(value.values_by_predecessor.values().next().unwrap())
    }

    fn get_span(&self, value: &PhiNode<T>) -> Result<Span, InvalidValueError> {
        Ok(value.span.clone())
    }
}

/// Is an enumeration of expressions that can be lowered into `phi` instructions.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum PhiNodeSource {
    LogicalShortCircuit,
    Express,
    Break,
    IfEsle,
}

/// Is an enumeration of all kinds of binary operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum BinaryOperator {
    ArithmeticOperator(ArithmeticOperator),
    ComparisonOperator(ComparisonOperator),
    EqualityOperator(EqualityOperator),
}

/// Represents a bound [`Binary`](pernixc_syntax::syntax_tree::expression::Binary) syntax tree.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
pub struct Binary<T: TypeSystem> {
    /// The span of binary.
    #[get = "pub"]
    pub(in crate::hir) span: Span,

    /// The left-hand-side operand of the binary.
    #[get = "pub"]
    pub(in crate::hir) left_operand: Value<T>,

    /// The right-hand-side operand of the binary.
    #[get = "pub"]
    pub(in crate::hir) right_operand: Value<T>,

    /// The operator of the binary.
    #[get_copy = "pub"]
    pub(in crate::hir) binary_operator: BinaryOperator,
}

impl<T: TypeSystem> ValueInspect<T, Binary<T>> for Container<T> {
    fn get_type(&self, value: &Binary<T>) -> Result<T, InvalidValueError> {
        match value.binary_operator {
            BinaryOperator::ArithmeticOperator(..) => self.get_type(&value.left_operand),
            BinaryOperator::ComparisonOperator(..) | BinaryOperator::EqualityOperator(..) => {
                Ok(T::from_type(PrimitiveType::Bool.into()))
            }
        }
    }

    fn get_span(&self, value: &Binary<T>) -> Result<Span, InvalidValueError> {
        Ok(value.span.clone())
    }
}
