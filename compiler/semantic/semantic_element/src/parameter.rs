//! Contains the [`Parameters`] definition.

use std::sync::Arc;

use pernixc_arena::{Arena, ID};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::Global;
use pernixc_term::r#type::Type;

/// Represents a parameter in a function signature. `PATTERN: TYPE`
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
pub struct Parameter {
    /// The type of the parameter.
    pub r#type: Type,

    /// The span of the parameter.
    pub span: Option<RelativeSpan>,
}

/// Stores all the parameters of a function.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    Default,
    StableHash,
    Serialize,
    Deserialize,
    pernixc_query::Value,
)]
#[id(Global<pernixc_symbol::ID>)]
#[value(Arc<Parameters>)]
#[extend(method(get_parameters))]
pub struct Parameters {
    /// The parameters of the function.
    pub parameters: Arena<Parameter>,

    /// The order of the parameters.
    pub parameter_order: Vec<ID<Parameter>>,
}

impl Parameters {
    /// Returns an iterator over the parameters in order it was declared.
    pub fn parameters_as_order(
        &self,
    ) -> impl Iterator<Item = (ID<Parameter>, &Parameter)> {
        self.parameter_order.iter().map(|id| (*id, &self.parameters[*id]))
    }
}
