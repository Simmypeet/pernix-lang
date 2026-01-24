//! Contains the query representing the `import` statement.

use pernixc_hash::HashMap;
use pernixc_lexical::tree::RelativeLocation;
use pernixc_source_file::Span;
use pernixc_target::Global;
use qbice::{
    Decode, Encode, Identifiable, Query, StableHash, storage::intern::Interned,
};

/// Represents a single symbol being imported into the current scope.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
    Identifiable,
)]
pub struct Import {
    /// The ID of the symbol being imported.
    pub id: Global<pernixc_symbol::ID>,

    /// The span to the `import ... (as ...)?` statement in the source code.
    pub span: Span<RelativeLocation>,
}

/// A query representing all the imports in a module.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
    Query,
)]
#[value(Interned<HashMap<Interned<str>, Import>>)]
#[extend(name = get_import_map, by_val)]
pub struct Key {
    /// The global ID of the module symbol.
    pub symbol_id: Global<pernixc_symbol::ID>,
}
