//! Contains the definition of [`Field`] and [`Feilds`].

use std::collections::HashMap;

use pernixc_arena::{Arena, ID};
use pernixc_semantic::component::{Accessibility, Derived};
use pernixc_source_file::Span;
use pernixc_term::{r#type::Type, Default};
use serde::{Deserialize, Serialize};

/// Represents a field declaration in the struct, denoted by `NAME: TYPE`
/// syntax.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct Field {
    /// The accessibility of the field.
    pub accessibility: Accessibility,

    /// The name of the field.
    pub name: String,

    /// The type of the field.
    pub r#type: Type<Default>,

    /// Location of where the field is declared.
    #[serde(skip)]
    pub span: Option<Span>,
}

/// A **persistent-derived** component storing the fields of a struct.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Fields {
    /// The arena storing all the fields in the struct.
    pub fields: Arena<Field>,

    /// Maps the field name to its ID.
    pub field_ids_by_name: HashMap<String, ID<Field>>,

    /// The order in which the fields are declared.
    pub field_declaration_order: Vec<ID<Field>>,
}

impl Derived for Fields {
    fn component_name() -> &'static str { "fields" }
}
