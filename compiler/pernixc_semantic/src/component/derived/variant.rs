//! Contains the definition of [`Variant`]

use derive_more::{Deref, DerefMut};
use serde::{Deserialize, Serialize};

use crate::{
    component::Derived,
    term::{r#type::Type, Default},
};

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
