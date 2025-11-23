//! Contains the definition of [`Field`] and [`Fields`].

use std::sync::Arc;

use flexstr::SharedStr;
use pernixc_arena::{Arena, ID};
use pernixc_hash::HashMap;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::Value;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_symbol::accessibility::Accessibility;
use pernixc_target::Global;
use pernixc_term::r#type::Type;

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
    pub name: SharedStr,

    /// The type of the field.
    pub r#type: Type,

    /// Location of where the field is declared.
    pub span: Option<RelativeSpan>,
}

/// Represents a list of fields defined in the struct.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    Serialize,
    Deserialize,
    StableHash,
    Value,
    Default,
)]
#[id(Global<pernixc_symbol::ID>)]
#[extend(method(get_fields))]
#[value(Arc<Fields>)]
pub struct Fields {
    /// The arena storing all the fields in the struct.
    pub fields: Arena<Field>,

    /// Maps the field name to its ID.
    pub field_ids_by_name: HashMap<SharedStr, pernixc_arena::ID<Field>>,

    /// The order in which the fields are declared.
    pub field_declaration_order: Vec<pernixc_arena::ID<Field>>,
}

impl Fields {
    /// Returns an iterator over the fields in the order they were declared.
    pub fn fields_as_order(
        &self,
    ) -> impl Iterator<Item = (ID<Field>, &'_ Field)> + '_ {
        self.field_declaration_order.iter().map(move |field_id| {
            let field = self.fields.get(*field_id).unwrap();
            (*field_id, field)
        })
    }
}
