//! Contains the definition of [`Implemented`] and its implementation for
//! components.

use std::{collections::HashSet, hash::Hash};

use serde::{Deserialize, Serialize};

/// Used for storing the information of which ID is being implemented by the
/// current implementation.
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
)]
pub struct Implemented<ID: Eq + Hash>(pub HashSet<ID>);
