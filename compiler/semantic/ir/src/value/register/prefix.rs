//! Contains the definition of the [`Prefix`] register.

use pernixc_arena::ID;
use pernixc_term::r#type::Type;
use pernixc_type_system::{OverflowError, Succeeded, normalizer::Normalizer};
use qbice::{Decode, Encode, StableHash};

use crate::{
    Values,
    resolution_visitor::{Abort, MutableResolutionVisitor, ResolutionVisitor},
    value::{ValueEnvironment, TypeOf, Value, register::Register},
};

macro_rules! visit_prefix_operand {
    ($prefix:expr, $visitor:expr, $literal_accessor:ident, $accept_method:ident) => {{
        if let Some(operand) = $prefix.operand.$literal_accessor() {
            operand.$accept_method($visitor).await?;
        }
        Ok(())
    }};
}

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

pub(super) async fn transform_prefix<T: MutableResolutionVisitor>(
    prefix: &mut Prefix,
    visitor: &mut T,
    _span: pernixc_lexical::tree::RelativeSpan,
) -> Result<(), Abort> {
    visit_prefix_operand!(prefix, visitor, as_literal_mut, accept_mut)
}

pub(super) async fn inspect_prefix<T: ResolutionVisitor>(
    prefix: &Prefix,
    visitor: &mut T,
    _span: pernixc_lexical::tree::RelativeSpan,
) -> Result<(), Abort> {
    visit_prefix_operand!(prefix, visitor, as_literal, accept)
}

impl TypeOf<&Prefix> for Values {
    async fn type_of<N: Normalizer>(
        &self,
        prefix: &Prefix,
        environment: &ValueEnvironment<'_, N>,
    ) -> Result<Succeeded<Type>, OverflowError> {
        let operand_type =
            Box::pin(self.type_of(&prefix.operand, environment)).await?;

        match prefix.operator {
            PrefixOperator::Negate
            | PrefixOperator::LogicalNot
            | PrefixOperator::BitwiseNot => Ok(operand_type),
        }
    }
}
