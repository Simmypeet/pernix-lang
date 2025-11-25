//! Contains the definition of the [`Load`] register.

use getset::{CopyGetters, Getters, MutGetters};
use pernixc_query::runtime::executor::CyclicError;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_term::r#type::Type;
use pernixc_type_system::{Error, Succeeded, normalizer::Normalizer};

use crate::{
    Values,
    address::Address,
    transform::Transformer,
    value::{Environment, TypeOf},
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
    CopyGetters,
    MutGetters,
)]
pub struct Load {
    /// The address where the value is stored and will be read from.
    #[get = "pub"]
    #[get_mut = "pub(crate)"]
    address: Address,

    /// The purpose of this load.
    #[get_copy = "pub"]
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

impl crate::visitor::Element for Load {
    fn accept(&self, visitor: &mut impl crate::visitor::Visitor) {
        visitor.visit_address(std::borrow::Cow::Borrowed(&self.address));
    }
}

pub(super) async fn transform_load<
    T: Transformer<pernixc_term::r#type::Type>,
>(
    load: &mut Load,
    transformer: &mut T,
    _span: pernixc_lexical::tree::RelativeSpan,
) -> Result<(), CyclicError> {
    load.address.transform(transformer).await
}

impl TypeOf<&Load> for Values {
    async fn type_of<N: Normalizer>(
        &self,
        load: &Load,
        environment: &Environment<'_, N>,
    ) -> Result<Succeeded<Type>, Error> {
        self.type_of(&load.address, environment).await
    }
}
