//! Contains the definition of [`GenericParameterVariances`].

use std::collections::HashMap;

use pernixc_arena::ID;
use pernixc_base::handler::Handler;
use pernixc_table::{
    component::Derived, diagnostic::Diagnostic, GlobalID, Table,
};
use serde::{Deserialize, Serialize};

use super::generic_parameters::{LifetimeParameter, TypeParameter};
use crate::type_system::variance::Variance;

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

impl Derived for GenericParameterVariances {
    fn compute(
        _: GlobalID,
        _: &Table,
        _: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Option<Self> {
        todo!()
    }

    fn component_name() -> &'static str { "generic parameter variances" }
}
