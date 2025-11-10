//! Contains the definition of the [`Load`] register.

use pernixc_query::{runtime::executor::CyclicError, TrackedEngine};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_term::r#type::Type;
use pernixc_type_system::{normalizer::Normalizer, Error, Succeeded};

use crate::{
    address::Address,
    transform::{self, Transformer},
    value::{Environment, TypeOf},
    Values,
};

/// Represents a load/read from an address in memory. (The type must be Copy)
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
)]
pub struct Load {
    /// The address where the value is stored and will be read from.
    pub address: Address,
}

impl transform::Element for Load {
    async fn transform<T: Transformer<pernixc_term::r#type::Type>>(
        &mut self,
        transformer: &mut T,
        _: &TrackedEngine,
    ) -> Result<(), CyclicError> {
        self.address.transform(transformer).await
    }
}

pub(super) async fn type_of_load_assignment(
    values: &Values,
    load: &Load,
    environment: &Environment<'_, impl Normalizer>,
) -> Result<Succeeded<Type>, Error> {
    values.type_of(&load.address, environment).await
}
