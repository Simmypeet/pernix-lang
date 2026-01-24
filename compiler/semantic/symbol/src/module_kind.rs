//! Defines the [`ModuleKind`] query.

use pernixc_source_file::LocalSourceID;
use pernixc_target::Global;
use qbice::{Decode, Encode, Query, StableHash};

use crate::ID;

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
    Encode,
    Decode,
    StableHash,
)]
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
    ExteranlFile(Option<LocalSourceID>),
}

/// The key type used with [`TrackedEngine`] to access the kind of a module
/// symbol.
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
    Query,
    StableHash,
)]
#[value(ModuleKind)]
#[extend(name = get_module_kind, by_val)]
pub struct Key {
    /// The global ID of the module symbol to get the kind for.
    pub module_id: Global<ID>,
}
