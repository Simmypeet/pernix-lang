use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use super::generic_parameters::{LifetimeParameter, TypeParameter};
use crate::{arena::ID, type_system::variance::Variance};

/// A **presistent-derived** component storing the variance of the generic
/// parameters. This is used for calculating the subtyping relationship between
/// two types.
///
/// The compnent should be attached to the structs and enums.
#[derive(Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct GenericParameterVariances {
    /// Maps the lifetime parameter ID to its variance.
    pub variances_by_lifetime_ids: HashMap<ID<LifetimeParameter>, Variance>,

    /// Maps the type parameter ID to its variance.
    pub variances_by_type_ids: HashMap<ID<TypeParameter>, Variance>,
}
