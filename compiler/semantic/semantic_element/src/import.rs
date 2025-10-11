//! Contains the query representing the `import` statement.

use std::sync::Arc;

use flexstr::SharedStr;
use pernixc_hash::HashMap;
use pernixc_lexical::tree::RelativeLocation;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::Span;
use pernixc_stable_hash::StableHash;
use pernixc_target::Global;

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
    Serialize,
    Deserialize,
    StableHash,
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
    Serialize,
    Deserialize,
    StableHash,
    pernixc_query::Key,
)]
#[value(Arc<HashMap<SharedStr, Import>>)]
#[scc_value(Arc::new(HashMap::default()))]
#[extend(method(get_import_map))]
pub struct Key(pub Global<pernixc_symbol::ID>);
