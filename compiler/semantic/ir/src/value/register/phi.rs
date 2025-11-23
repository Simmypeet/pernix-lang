//! Contains the definition of the [`Phi`] register.

use std::ops::Deref;

use pernixc_arena::ID;
use pernixc_hash::HashMap;
use pernixc_query::runtime::executor::CyclicError;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_term::r#type::Type;
use pernixc_type_system::{Error, Succeeded, normalizer::Normalizer};

use crate::{
    Values,
    control_flow_graph::Block,
    transform::{Transformer, TypeTermSource},
    value::{Environment, TypeOf, Value, register::Register},
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

impl crate::visitor::Element for Phi {
    fn accept(&self, visitor: &mut impl crate::visitor::Visitor) {
        for value in self.incoming_values.values() {
            visitor.visit_value(std::borrow::Cow::Borrowed(value));
        }
    }
}

pub(super) async fn transform_phi<T: Transformer<Type>>(
    phi: &mut Phi,
    transformer: &mut T,
    span: Option<pernixc_lexical::tree::RelativeSpan>,
) -> Result<(), CyclicError> {
    for value in phi.incoming_values.values_mut() {
        if let Some(literal) = value.as_literal_mut() {
            literal.transform(transformer).await?;
        }
    }

    transformer.transform(&mut phi.r#type, TypeTermSource::Phi, span).await
}

impl TypeOf<&Phi> for Values {
    async fn type_of<N: Normalizer>(
        &self,
        phi: &Phi,
        environment: &Environment<'_, N>,
    ) -> Result<Succeeded<Type>, Error> {
        Ok(environment
            .type_environment
            .simplify(phi.r#type.clone())
            .await?
            .deref()
            .clone())
    }
}
