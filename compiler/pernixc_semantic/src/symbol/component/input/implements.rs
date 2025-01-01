//! Contains the definition of [`Implements`] and its implementation for
//! components.

use std::collections::HashMap;

use paste::paste;
use serde::{Deserialize, Serialize};

use crate::{
    arena::ID,
    symbol::{
        AdtID, AdtImplementation, Global, Marker, MarkerImplementationID,
        NegativeMarkerImplementation, NegativeTraitImplementation,
        PositiveMarkerImplementation, PositiveTraitImplementation, Trait,
        TraitImplementationID,
    },
};

/// Used for storing the information of which ID is being implemented by the
/// current implementation.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Default,
    Serialize,
    Deserialize,
    derive_more::Deref,
    derive_more::DerefMut,
)]
pub struct Implements<ID>(pub ID);

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub(super) struct Map {
    pub(super) trait_implementations:
        HashMap<Global<TraitImplementationID>, Implements<ID<Trait>>>,
    pub(super) marker_implementations:
        HashMap<Global<MarkerImplementationID>, Implements<ID<Marker>>>,
    pub(super) adt_implementations:
        HashMap<Global<ID<AdtImplementation>>, Implements<AdtID>>,
}

macro_rules! impl_implemented {
    ($(($name:ident, $id:ty, $impl_id:ty, $map_name:ident)),*) => {
        paste! {
            $(
                impl super::Input<Implements<$impl_id>> for $name {
                    type Requirement = super::Required;
                    type ID = $id;

                    fn get_map(
                        representation: &super::Map,
                    ) -> &HashMap<Global<Self::ID>, Implements<$impl_id>> {
                        &representation.implemented.$map_name
                    }

                    fn get_map_mut(
                        representation: &mut super::Map,
                    ) -> &mut HashMap<Global<Self::ID>, Implements<$impl_id>> {
                        &mut representation.implemented.$map_name
                    }
                }
            )*
        }
    };
}

impl_implemented!(
    (
        PositiveTraitImplementation,
        TraitImplementationID,
        ID<Trait>,
        trait_implementations
    ),
    (
        NegativeTraitImplementation,
        TraitImplementationID,
        ID<Trait>,
        trait_implementations
    ),
    (
        PositiveMarkerImplementation,
        MarkerImplementationID,
        ID<Marker>,
        marker_implementations
    ),
    (
        NegativeMarkerImplementation,
        MarkerImplementationID,
        ID<Marker>,
        marker_implementations
    ),
    (AdtImplementation, ID<AdtImplementation>, AdtID, adt_implementations)
);
