//! Contains the [`Parameters`] definition.

use pernixc_arena::{Arena, ID};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_target::Global;
use pernixc_term::r#type::Type;
use qbice::{
    Decode, Encode, Identifiable, Query, StableHash, storage::intern::Interned,
};

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
    Encode,
    Decode,
    Identifiable,
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
    Encode,
    Decode,
    Identifiable,
)]
pub struct Parameters {
    /// The parameters of the function.
    pub parameters: Arena<Parameter>,

    /// The order of the parameters.
    pub parameter_order: Vec<ID<Parameter>>,
}

/// Query key for retrieving the parameters of a function.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
    Query,
)]
#[value(Interned<Parameters>)]
#[extend(name = get_parameters, by_val)]
pub struct Key {
    /// The global ID of the function symbol.
    pub symbol_id: Global<pernixc_symbol::ID>,
}

impl Parameters {
    /// Returns an iterator over the parameters in order it was declared.
    pub fn parameters_as_order(
        &self,
    ) -> impl Iterator<Item = (ID<Parameter>, &Parameter)> {
        self.parameter_order.iter().map(|id| (*id, &self.parameters[*id]))
    }

    /// Returns the declaration order of the parameter.
    #[must_use]
    pub fn get_parameter_declaration_order(
        &self,
        parameter_id: ID<Parameter>,
    ) -> usize {
        self.parameter_order.iter().position(|id| *id == parameter_id).unwrap()
    }
}
