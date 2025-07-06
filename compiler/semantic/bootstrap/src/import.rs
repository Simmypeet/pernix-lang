//! Contains the definition of the [`Import`] struct

use flexstr::SharedStr;
use pernixc_hash::HashMap;
use pernixc_lexical::tree::RelativeLocation;
use pernixc_query::Value;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::Span;
use pernixc_stable_hash::StableHash;
use pernixc_target::Global;

use crate::symbol;

/// Represents a single `import ...` statement.
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
pub struct Using {
    /// The ID of the symbol being imported.
    pub id: Global<symbol::ID>,

    /// The span to the `import ... (as ...)?` statement in the source code.
    pub span: Span<RelativeLocation>,
}

/// Represents the collection of `from ... import ...` statements that are
/// declared in the module.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    Default,
    Serialize,
    Deserialize,
    Value,
    derive_more::Deref,
    derive_more::DerefMut,
    StableHash,
)]
#[id(Global<symbol::ID>)]
#[extend(method(get_imports), unwrap("should have no cyclic dependencies"))]
pub struct Import(pub HashMap<SharedStr, Using>);
