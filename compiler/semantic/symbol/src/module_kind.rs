//! Defines the [`ModuleKind`] query.

use pernixc_arena::ID;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::SourceFile;
use pernixc_stable_hash::StableHash;
use pernixc_target::Global;

/// Determines the kinds of module symbol.
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
    pernixc_query::Value,
)]
#[id(Global<crate::ID>)]
pub enum ModuleKind {
    /// The module is defined inlined in the same place it was declared.
    ///
    /// For example:
    ///
    /// ```pernix
    /// public module inlineModule:
    ///     pass
    ///     # content goes here
    /// ```
    Inline,

    /// The module is declared in one file, but its content is located at the
    /// given external source file.
    ///
    /// `None` in case of the external file is failed to load.
    ExteranlFile(Option<ID<SourceFile>>),
}
