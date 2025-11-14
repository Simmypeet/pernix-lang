//! Contains the definition of the [`Prefix`] register.

use pernixc_arena::ID;
use pernixc_query::runtime::executor::CyclicError;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_term::r#type::Type;
use pernixc_type_system::{normalizer::Normalizer, Error, Succeeded};

use crate::{
    transform::{self, Transformer},
    value::{register::Register, Environment, TypeOf, Value},
    Values,
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
    Serialize,
    Deserialize,
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
    Serialize,
    Deserialize,
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

impl transform::Element for Prefix {
    async fn transform<T: Transformer<Type>>(
        &mut self,
        transformer: &mut T,
        _engine: &pernixc_query::TrackedEngine,
    ) -> Result<(), CyclicError> {
        if let Some(operand) = self.operand.as_literal_mut() {
            operand.transform(transformer).await?;
        }

        Ok(())
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
