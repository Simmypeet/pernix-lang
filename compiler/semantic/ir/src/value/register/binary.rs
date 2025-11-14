//! Contains the definition of the [`Binary`] register.

use pernixc_arena::ID;
use pernixc_query::runtime::executor::CyclicError;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_term::r#type::Type;
use pernixc_type_system::{normalizer::Normalizer, Error, Succeeded};

use crate::{
    transform::Transformer,
    value::{register::Register, Environment, TypeOf, Value},
    Values,
};

/// Represents an arithmetic operator that works on numbers.
///
/// The both lhs and rhs operands are required to have the same type. The return
/// type is the same as the operands.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
)]
pub enum ArithmeticOperator {
    /// Supports for all numeric types.
    Add,

    /// Supports for all numeric types.
    Subtract,

    /// Supports for all numeric types.
    Multiply,

    /// Supports for all numeric types.
    Divide,

    /// Supports for all numeric ypes.
    Modulo,
}

/// Represents a relational operator that works on numbers and booleans.
///
/// The both lhs and rhs operands are required to have the same type. The return
/// type is always boolean.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
)]
pub enum RelationalOperator {
    /// Supports for all numeric and boolean types.
    LessThan,

    /// Supports for all numeric and boolean types.
    LessThanOrEqual,

    /// Supports for all numeric and boolean types.
    GreaterThan,

    /// Supports for all numeric and boolean types.
    GreaterThanOrEqual,

    /// Supports for all numeric and boolean types.
    Equal,

    /// Supports for all numeric and boolean types.
    NotEqual,
}

/// Represents a bitwise operator that works on integers and booleans.
///
/// Except for `ShiftLeft` and `ShiftRight`, the both lhs and rhs operands are
/// required to have the same type. The return type is the same as the operands.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
)]
pub enum BitwiseOperator {
    /// Supports for all integer and boolean types.
    And,

    /// Supports for all integer and boolean types.
    Or,

    /// Supports for all integer and boolean types.
    Xor,

    /// Supports for all integer types. The lhs and rhs operands aren't
    /// required to have the same type.
    LeftShift,

    /// Supports for all integer types. The lhs and rhs operands aren't
    /// required to have the same type.
    RightShift,
}

/// An enumeration of all binary operators.
///
/// The operator doesn't includes `and` and `or` operators as they are
/// defined in another kind of register.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
)]
#[allow(missing_docs)]
pub enum BinaryOperator {
    Arithmetic(ArithmeticOperator),
    Relational(RelationalOperator),
    Bitwise(BitwiseOperator),
}

/// Represents a binary expression.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
)]
pub struct Binary {
    /// The left-hand side operand.
    pub lhs: Value,

    /// The right-hand side operand.
    pub rhs: Value,

    /// The operator applied to the operands.
    pub operator: BinaryOperator,
}

impl Binary {
    /// Returns the list of registers that are used in the binary.
    #[must_use]
    pub fn get_used_registers(&self) -> Vec<ID<Register>> {
        self.lhs
            .as_register()
            .copied()
            .into_iter()
            .chain(self.rhs.as_register().copied())
            .collect()
    }
}

pub(super) async fn transform_binary<T: Transformer<Type>>(
    binary: &mut Binary,
    transformer: &mut T,
    _span: Option<pernixc_lexical::tree::RelativeSpan>,
) -> Result<(), CyclicError> {
    if let Some(literal) = binary.lhs.as_literal_mut() {
        literal.transform(transformer).await?;
    }

    if let Some(literal) = binary.rhs.as_literal_mut() {
        literal.transform(transformer).await?;
    }

    Ok(())
}

impl TypeOf<&Binary> for Values {
    async fn type_of<N: Normalizer>(
        &self,
        binary: &Binary,
        environment: &Environment<'_, N>,
    ) -> Result<Succeeded<Type>, Error> {
        // the return type always based on the lhs field
        if let BinaryOperator::Relational(_) = binary.operator {
            Ok(Succeeded::new(Type::Primitive(
                pernixc_term::r#type::Primitive::Bool,
            )))
        } else {
            Box::pin(self.type_of(&binary.lhs, environment)).await
        }
    }
}
