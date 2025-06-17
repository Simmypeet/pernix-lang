//! Contains the definition of the [`Implements`] type.

use derive_more::{Deref, DerefMut};
use pernixc_query::Value;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_target::Global;

use crate::symbol;

/// Representing the symbol that is being implemented by the `implements`
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
    Default,
    Serialize,
    Deserialize,
    Deref,
    DerefMut,
    Value,
)]
#[id(Global<symbol::ID>)]
#[ext(method(get_implements), unwrap("should have no cyclic dependencies"))]
pub struct Implements(pub Global<symbol::ID>);
