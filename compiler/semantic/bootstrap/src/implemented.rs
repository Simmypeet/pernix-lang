//! Contains the definition of the [`Implemented`] type.

use std::collections::HashSet;

use pernixc_query::Value;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::Global;

use crate::symbol;

/// A set of all the `implements` symbols that implements this symbol.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    Default,
    Serialize,
    Deserialize,
    derive_more::Deref,
    derive_more::DerefMut,
    Value,
    StableHash,
)]
#[id(Global<symbol::ID>)]
#[ext(method(get_implemented), unwrap("should have no cyclic dependencies"))]
pub struct Implemented(pub HashSet<Global<symbol::ID>>);
