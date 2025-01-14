//! Contains the definition of [`VarianceMap`] component.

use std::collections::HashMap;

use pernixc_arena::ID;
use pernixc_table::component::Derived;
use pernixc_term::{
    generic_parameter::{LifetimeParameter, TypeParameter},
    variance::Variance,
};
use serde::{Deserialize, Serialize};

/// A **presistent-derived** component storing the variance of the generic
/// parameters. This is used for calculating the subtyping relationship between
/// two types.
///
/// The compnent should be attached to the structs and enums.
#[derive(Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct VarianceMap {
    /// Maps the lifetime parameter ID to its variance.
    pub variances_by_lifetime_ids: HashMap<ID<LifetimeParameter>, Variance>,

    /// Maps the type parameter ID to its variance.
    pub variances_by_type_ids: HashMap<ID<TypeParameter>, Variance>,
}

impl Derived for VarianceMap {
    fn component_name() -> &'static str { "variance map" }
}
