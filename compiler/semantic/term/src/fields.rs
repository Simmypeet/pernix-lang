//! Contains the definition of [`Field`] and [`Feilds`].

use std::collections::HashMap;

use pernixc_arena::Arena;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::Value;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_symbol::accessibility::Accessibility;
use pernixc_target::Global;

use crate::r#type::Type;

/// Represents a field declaration in the struct, denoted by `NAME: TYPE`
/// syntax.
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
pub struct Field {
    /// The accessibility of the field.
    pub accessibility: Accessibility<pernixc_symbol::ID>,

    /// The name of the field.
    pub name: String,

    /// The type of the field.
    pub r#type: Type,

    /// Location of where the field is declared.
    pub span: Option<RelativeSpan>,
}

/// Represents a list of fields defined in the struct.
#[derive(
    Debug, Clone, PartialEq, Eq, Serialize, Deserialize, StableHash, Value,
)]
#[id(Global<pernixc_symbol::ID>)]
pub struct Fields {
    /// The arena storing all the fields in the struct.
    pub fields: Arena<Field>,

    /// Maps the field name to its ID.
    pub field_ids_by_name: HashMap<String, pernixc_arena::ID<Field>>,

    /// The order in which the fields are declared.
    pub field_declaration_order: Vec<pernixc_arena::ID<Field>>,
}
