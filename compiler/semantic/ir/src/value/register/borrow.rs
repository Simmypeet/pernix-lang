//! Contains the definition of the [`Borrow`] register.

use pernixc_query::{runtime::executor::CyclicError, TrackedEngine};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_term::{
    lifetime::Lifetime,
    r#type::{Qualifier, Type},
};
use pernixc_type_system::{normalizer::Normalizer, Error, Succeeded};

use crate::{
    address::Address,
    transform::{self, LifetimeTermSource, Transformer},
    value::{Environment, TypeOf},
    Values,
};

/// Obtains a reference at the given address.
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
pub struct Borrow {
    /// The address to the value.
    pub address: Address,

    /// The qualfier of the reference.
    pub qualifier: Qualifier,

    /// The lifetime introduces by the reference of operation.
    pub lifetime: Lifetime,
}

impl transform::Element for Borrow {
    async fn transform<T: Transformer<Type> + Transformer<Lifetime>>(
        &mut self,
        transformer: &mut T,
        _engine: &TrackedEngine,
    ) -> Result<(), CyclicError> {
        self.address.transform(transformer).await?;

        transformer
            .transform(&mut self.lifetime, LifetimeTermSource::Borrow, None)
            .await
    }
}

impl TypeOf<&Borrow> for Values {
    async fn type_of<N: Normalizer>(
        &self,
        reference_of: &Borrow,
        environment: &Environment<'_, N>,
    ) -> Result<Succeeded<Type>, Error> {
        self.type_of(&reference_of.address, environment).await.map(|x| {
            x.map(|x| {
                Type::Reference(pernixc_term::r#type::Reference {
                    qualifier: reference_of.qualifier,
                    lifetime: reference_of.lifetime.clone(),
                    pointee: Box::new(x),
                })
            })
        })
    }
}
