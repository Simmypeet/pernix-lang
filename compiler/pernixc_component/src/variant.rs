//! Contains the definition of [`Variant`]

use derive_more::{Deref, DerefMut};
use pernixc_semantic::component::Derived;
use pernixc_term::{r#type::Type, Default};
use serde::{Deserialize, Serialize};

/// A **presistent-derived** component attached to all the enum-variants
/// symbols.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Deref,
    DerefMut,
    Serialize,
    Deserialize,
)]
pub struct Variant {
    /// If `Some` the variant has an associated value, like `Some(T)`.
    pub associated_type: Option<Type<Default>>,
}

impl Derived for Variant {
    fn component_name() -> &'static str { "variant" }
}
