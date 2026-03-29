//! Provides a clean interface for accessing core library intrinsic symbols.
//!
//! This crate exposes queries that allow clients to retrieve symbol IDs for
//! frequently used intrinsic symbols such as the `Copy` marker and `Drop` trait.
//! This eliminates the need for clients to manually resolve qualified identifier
//! sequences, which is fragile and ad-hoc.

use pernixc_extend::extend;
use pernixc_symbol::SymbolID;
use pernixc_target::Global;
use qbice::{Decode, Encode, Query, StableHash};

/// The key type used with [`TrackedEngine`] to access the `Copy` marker symbol
/// ID.
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
#[value(Global<SymbolID>)]
#[extend(name = get_copy_marker_id, by_val)]
pub struct CopyMarkerKey;

/// The key type used with [`TrackedEngine`] to access the `Drop` trait symbol
/// ID.
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
#[value(Global<SymbolID>)]
#[extend(name = get_drop_trait_id, by_val)]
pub struct DropTraitKey;

/// The key type used with [`TrackedEngine`] to access the `drop` function
/// symbol ID (the associated function of the `Drop` trait).
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
#[value(Global<SymbolID>)]
#[extend(name = get_drop_function_id, by_val)]
pub struct DropFunctionKey;
