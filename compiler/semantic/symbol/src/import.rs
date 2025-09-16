//! Contains the definition of the [`Import`] struct

use std::sync::Arc;

use flexstr::SharedStr;
use pernixc_handler::{Handler, Storage};
use pernixc_hash::HashMap;
use pernixc_lexical::tree::RelativeLocation;
use pernixc_query::TrackedEngine;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::{SourceElement, Span};
use pernixc_stable_hash::StableHash;
use pernixc_syntax::{item::module::ImportItems, SimplePathRoot};
use pernixc_target::{get_linked_targets, get_target_map, Global};

use crate::{
    accessibility::symbol_accessible,
    get_target_root_module_id,
    import::diagnostic::{
        ConflictingUsing, Diagnostic, TargetRootInImportIsNotAllowedwithFrom,
    },
    kind::{get_kind, Kind},
    member::get_members,
    name::{self, get_name},
    span::get_span,
    ID,
};

pub mod diagnostic;

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
    pub id: Global<ID>,

    /// The span to the `import ... (as ...)?` statement in the source code.
    pub span: Span<RelativeLocation>,
}
