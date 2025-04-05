//! Contains the definition of function signature

use pernixc_arena::{Arena, ID};
use pernixc_source_file::GlobalSpan;
use serde::{Deserialize, Serialize};

use crate::{
    component::Derived,
    term::{r#type::Type, Default},
};

/// Represents a parameter in a function signature. `PATTERN: TYPE`
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct Parameter {
    /// The type of the parameter.
    pub r#type: Type<Default>,

    /// The span of the parameter.
    #[serde(skip)]
    pub span: Option<GlobalSpan>,
}

/// A **presistent-derived** component representing the function signature.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct FunctionSignature {
    /// The parameters of the function.
    pub parameters: Arena<Parameter>,

    /// The order of the parameters.
    pub parameter_order: Vec<ID<Parameter>>,

    /// The return type of the function.
    pub return_type: Type<Default>,
}

impl Derived for FunctionSignature {
    fn component_name() -> &'static str { "function signature" }
}
