//! Module syntax query system for the Pernix compiler.

use std::{
    collections::hash_map::Entry,
    hash::{Hash as _, Hasher as _},
    path::{Path, PathBuf},
    sync::Arc,
};

use enum_as_inner::EnumAsInner;
use flexstr::{FlexStr, SharedStr};
use fnv::FnvHasher;
use parking_lot::RwLock;
use pernixc_diagnostic::{Diagnostic, Related, Report, Severity};
use pernixc_handler::{Handler, Storage};
use pernixc_hash::HashMap;
use pernixc_lexical::tree::RelativeLocation;
use pernixc_query::TrackedEngine;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::{GlobalSourceID, LocalSourceID, SourceFile, Span};
use pernixc_stable_hash::StableHash;
use pernixc_syntax::Passable;
use pernixc_target::{get_invocation_arguments, TargetID};
use rayon::iter::{
    IntoParallelIterator as _, IntoParallelRefIterator, ParallelIterator,
};

use crate::{source_file::LoadSourceFileError, syntax_tree::ModuleContent};
