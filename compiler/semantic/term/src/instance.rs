//! Defines the [`Instance`] term

use std::fmt::Write;

use pernixc_qbice::TrackedEngine;
use pernixc_symbol::name::get_name;
use pernixc_target::Global;
use qbice::{Decode, Encode, StableHash};

use crate::{
    generic_arguments::{GenericArguments, Symbol},
    generic_parameters::{InstanceParameterID, get_generic_parameters},
};

#[cfg(any(test, feature = "arbitrary"))]
pub mod arbitrary;

/// The `TraitRef` refers to a trait that an instance implements.
///
/// This is analogous to "expression" has a specific "type". In this case, the
/// "instance" implements a specific "trait".
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
)]
pub struct TraitRef(Symbol);

/// Refers to a terrm that is associated with an instance.
///
/// This could be any term that is associated with an instance, such as "type"
/// or even "instance" associated with an instance.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
)]
pub struct InstanceAssociated {
    instance: Box<Instance>,
    trait_associated_symbol_id: Global<pernixc_symbol::ID>,
    trait_associated_symbol_generic_arguments: GenericArguments,
}

/// An instance of a trait implementation.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
)]
pub enum Instance {
    /// Directly refers to an `instance` symbol being defined on module level.
    Symbol(Symbol),

    /// Refers to an instance parameter denoted by `instance I: Trait` syntax.
    Parameter(InstanceParameterID),

    /// Refers to an instance that is associated with another instance.
    ///
    /// # Example
    ///
    /// ```pnx
    /// public trait Fizz:
    ///     public instance Inner
    ///
    /// public struct Buzz[instance I: Fizz]:
    ///     public myField: Foo[I::Inner]
    /// ```
    ///
    /// In the above example, `I::Inner` is an instance associated with the
    /// instance parameter `I`.
    InstanceAssociated(InstanceAssociated),
}

impl crate::display::Display for InstanceParameterID {
    async fn fmt(
        &self,
        engine: &TrackedEngine,
        formatter: &mut crate::display::Formatter<'_, '_>,
    ) -> std::fmt::Result {
        let generic_parameters =
            engine.get_generic_parameters(self.parent_id).await;

        write!(
            formatter,
            "{}",
            &**generic_parameters.get_instance_parameter(self.id).name()
        )
    }
}

impl crate::display::Display for TraitRef {
    async fn fmt(
        &self,
        engine: &TrackedEngine,
        formatter: &mut crate::display::Formatter<'_, '_>,
    ) -> std::fmt::Result {
        crate::display::Display::fmt(&self.0, engine, formatter).await
    }
}

impl crate::display::Display for InstanceAssociated {
    async fn fmt(
        &self,
        engine: &TrackedEngine,
        formatter: &mut crate::display::Formatter<'_, '_>,
    ) -> std::fmt::Result {
        Box::pin(crate::display::Display::fmt(
            &*self.instance,
            engine,
            formatter,
        ))
        .await?;

        write!(formatter, "::")?;

        let name = engine.get_name(self.trait_associated_symbol_id).await;
        write!(formatter, "{}", &*name)?;

        Box::pin(crate::display::Display::fmt(
            &self.trait_associated_symbol_generic_arguments,
            engine,
            formatter,
        ))
        .await
    }
}

impl crate::display::Display for Instance {
    async fn fmt(
        &self,
        engine: &TrackedEngine,
        formatter: &mut crate::display::Formatter<'_, '_>,
    ) -> std::fmt::Result {
        match self {
            Self::Symbol(symbol) => {
                Box::pin(crate::display::Display::fmt(
                    symbol, engine, formatter,
                ))
                .await
            }
            Self::Parameter(param_id) => {
                crate::display::Display::fmt(param_id, engine, formatter).await
            }
            Self::InstanceAssociated(instance_associated) => {
                Box::pin(crate::display::Display::fmt(
                    instance_associated,
                    engine,
                    formatter,
                ))
                .await
            }
        }
    }
}
