//! Contains the definition of [`TypeAlias`].

use derive_more::{Deref, DerefMut};
use pernixc_semantic::component::Derived;
use pernixc_term::{r#type::Type, Default};
use serde::{Deserialize, Serialize};

/// A **presistent-derived** component representing the type alias values for
/// various `type IDENT = TYPE` symbols.
#[derive(
    Debug, Clone, PartialEq, Eq, Deref, DerefMut, Serialize, Deserialize,
)]
pub struct TypeAlias(pub Type<Default>);

impl Derived for TypeAlias {
    fn component_name() -> &'static str { "type alias" }
}
