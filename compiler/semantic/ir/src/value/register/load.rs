//! Contains the definition of the [`Load`] register.

use getset::{Getters, MutGetters};
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

/// Indicates how a load is being used. This is used for improving diagnostics
/// related to use-after-move and similar errors.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
pub enum Purpose {
    /// A general purpose load.
    General,

    /// A load for moving the captured value into a capture structure.
    Capture,
}

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
    Getters,
    MutGetters,
)]
pub struct Load {
    /// The address where the value is stored and will be read from.
    #[get = "pub"]
    #[get_mut = "pub(crate)"]
    address: Address,

    /// The purpose of this load.
    #[get = "pub"]
    purpose: Purpose,
}

impl Load {
    /// Creates a new general purpose load from the given address.
    #[must_use]
    pub const fn new(address: Address) -> Self {
        Self { address, purpose: Purpose::General }
    }

    /// Creates a new load from the given address with the given purpose.
    #[must_use]
    pub const fn with_purpose(address: Address, purpose: Purpose) -> Self {
        Self { address, purpose }
    }
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
