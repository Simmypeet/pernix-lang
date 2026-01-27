//! Contains the definition of the [`Prefix`] register.

use pernixc_arena::ID;
use qbice::{Decode, Encode};
use qbice::StableHash;
use pernixc_term::r#type::Type;
use pernixc_type_system::{Error, Succeeded, normalizer::Normalizer};

use crate::{
    Values,
    transform::Transformer,
    value::{Environment, TypeOf, Value, register::Register},
};

/// An enumeration of the different kinds of prefix operators.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
)]
pub enum PrefixOperator {
    /// The value must be the signed numbers type.
    Negate,

    /// The value must be the boolean type.
    LogicalNot,

    /// The value must be integers.
    BitwiseNot,
}

/// A value applied with a prefix operator.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
)]
pub struct Prefix {
    /// The operand of the prefix operator.
    pub operand: Value,

    /// The operator applied to the operand.
    pub operator: PrefixOperator,
}

impl Prefix {
    /// Returns the register that is used in the prefix.
    #[must_use]
    pub fn get_used_registers(&self) -> Vec<ID<Register>> {
        self.operand.as_register().copied().into_iter().collect()
    }
}

impl crate::visitor::Element for Prefix {
    fn accept(&self, visitor: &mut impl crate::visitor::Visitor) {
        visitor.visit_value(std::borrow::Cow::Borrowed(&self.operand));
    }
}

pub(super) async fn transform_prefix<T: Transformer<Type>>(
    prefix: &mut Prefix,
    transformer: &mut T,
    _span: pernixc_lexical::tree::RelativeSpan,
) {
    if let Some(operand) = prefix.operand.as_literal_mut() {
        operand.transform(transformer).await;
    }
}

impl TypeOf<&Prefix> for Values {
    async fn type_of<N: Normalizer>(
        &self,
        prefix: &Prefix,
        environment: &Environment<'_, N>,
    ) -> Result<Succeeded<Type>, Error> {
        let operand_type =
            Box::pin(self.type_of(&prefix.operand, environment)).await?;

        match prefix.operator {
            PrefixOperator::Negate
            | PrefixOperator::LogicalNot
            | PrefixOperator::BitwiseNot => Ok(operand_type),
        }
    }
}
