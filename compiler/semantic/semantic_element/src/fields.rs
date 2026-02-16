//! Contains the definition of [`Field`] and [`Fields`].

use pernixc_arena::{Arena, ID};
use pernixc_hash::HashMap;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_symbol::accessibility::Accessibility;
use pernixc_target::Global;
use pernixc_term::r#type::Type;
use qbice::{
    Decode, Encode, Identifiable, Query, StableHash, storage::intern::Interned,
};

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
    Encode,
    Decode,
    Identifiable,
)]
pub struct Field {
    /// The accessibility of the field.
    pub accessibility: Accessibility<pernixc_symbol::ID>,

    /// The name of the field.
    pub name: Interned<str>,

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
    StableHash,
    Encode,
    Decode,
    Identifiable,
    Default,
)]
pub struct Fields {
    /// The arena storing all the fields in the struct.
    pub fields: Arena<Field>,

    /// Maps the field name to its ID.
    pub field_ids_by_name: HashMap<Interned<str>, pernixc_arena::ID<Field>>,

    /// The order in which the fields are declared.
    pub field_declaration_order: Vec<pernixc_arena::ID<Field>>,
}

/// Query key for retrieving the fields of a struct.
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
#[value(Interned<Fields>)]
#[extend(name = get_fields, by_val)]
pub struct Key {
    /// The global ID of the struct symbol.
    pub symbol_id: Global<pernixc_symbol::ID>,
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
