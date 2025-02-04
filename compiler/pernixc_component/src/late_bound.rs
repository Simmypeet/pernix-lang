//! Contains the definition of [`LateBound`].

use std::collections::HashSet;

use pernixc_arena::ID;
use pernixc_table::component::Derived;
use pernixc_term::generic_parameter::LifetimeParameter;
use serde::{Deserialize, Serialize};

/// A **presistent-derived** component storing the late bound lifetimes.
/// This component only exists in the function symbols.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    Default,
    derive_more::Deref,
    derive_more::DerefMut,
    Serialize,
    Deserialize,
)]
pub struct LateBound(pub HashSet<ID<LifetimeParameter>>);

impl Derived for LateBound {
    fn component_name() -> &'static str { "late bound lifetimes" }
}
