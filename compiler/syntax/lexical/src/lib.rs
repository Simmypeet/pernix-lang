//! This crate implements the lexical analysis phase of the compiler.

use std::path::Path;

use pernixc_target::TargetID;
use qbice::{Decode, Encode, Query, StableHash, storage::intern::Interned};

pub mod error;
pub mod kind;
pub mod token;
pub mod tree;

/// Query for parsing a token tree from the given source file path.
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
    Query,
)]
#[value(Result<
    (Interned<tree::Tree>, Interned<[error::Error]>),
    pernixc_source_file::Error
>)]
pub struct Key {
    /// The path to load the source file.
    pub path: Interned<Path>,

    /// The target ID that requested the source file parsing.
    pub target_id: TargetID,
}

/// A key for retrieving the diagnostics that occurred while parsing the token
/// tree from the source file.
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
    Query,
)]
#[value(Result<Interned<[error::Error]>, pernixc_source_file::Error>)]
pub struct DiagnosticKey(pub Key);
