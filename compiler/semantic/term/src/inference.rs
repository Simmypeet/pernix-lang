//! Contains the definition of [`Inference`] type.

use std::marker::PhantomData;

use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;

/// A new type wrapper for representing inference tyep variable when building
/// the IR.
///
/// Since the language only allows type inference in the function body, this
/// inference instance should only exist in the function body.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Default,
    StableHash,
    Serialize,
    Deserialize,
)]
pub struct Inference<T>(pub PhantomData<Box<T>>);
