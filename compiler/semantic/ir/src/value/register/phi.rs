//! Contains the definition of the [`Phi`] register.

use pernixc_arena::ID;
use pernixc_hash::HashMap;
use pernixc_query::runtime::executor::CyclicError;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_term::r#type::Type;

use crate::{
    control_flow_graph::Block,
    transform::{self, Transformer, TypeTermSource},
    value::{register::Register, Value},
};

/// Represents a phi node in the SSA form.
///
/// A phi node is used to determine the value based on the flow of the
/// execution. This is typcially used in the control flow related expressions
/// such as `if` and `match`.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, StableHash)]
pub struct Phi {
    /// Maps the incoming block to the value.
    pub incoming_values: HashMap<ID<Block>, Value>,

    /// The type of the phi node.
    ///
    /// The type must be declared separately as the incoming values can have
    /// different lifetime values; thus, the type of the phi node can't be
    /// solely determined by one of the incoming values.
    pub r#type: Type,
}

impl Phi {
    /// Returns the list of registers that are used in the phi node.
    #[must_use]
    pub fn get_used_registers(&self) -> Vec<ID<Register>> {
        self.incoming_values
            .values()
            .filter_map(|x| x.as_register().copied())
            .collect()
    }
}

impl transform::Element for Phi {
    async fn transform<T: Transformer<Type>>(
        &mut self,
        transformer: &mut T,
        _engine: &pernixc_query::TrackedEngine,
    ) -> Result<(), CyclicError> {
        for value in self.incoming_values.values_mut() {
            if let Some(literal) = value.as_literal_mut() {
                literal.transform(transformer).await?;
            }
        }

        transformer.transform(&mut self.r#type, TypeTermSource::Phi, None).await
    }
}
