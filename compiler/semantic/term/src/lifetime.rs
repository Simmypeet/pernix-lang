//! Contains the definition of [`Lifetime`] term.

use std::fmt::Write;

use enum_as_inner::EnumAsInner;
use flexstr::SharedStr;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::TrackedEngine;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_symbol::MemberID;
use pernixc_target::Global;

use crate::{
    constant::Constant,
    error::Error,
    generic_parameters::{get_generic_parameters, LifetimeParameterID},
    inference,
    matching::{Match, Matching, Substructural},
    r#type::Type,
    sub_term::{Location, SubTerm},
    Never,
};

#[cfg(any(test, feature = "arbitrary"))]
pub mod arbitrary;

/// Represents a forall lifetime declared with `for['a]` syntax.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
    derive_new::new,
)]
pub struct NamedForall {
    /// The span where the named for all lifetime.
    ///
    /// The relative span can be uniquely identified the named for-all
    /// lifetime.
    pub span: RelativeSpan,

    //// The name of the named for all lifetime.
    pub shared_str: SharedStr,
}

/// From which semantic element the forall lifetime was generated.
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
pub enum FromSemanticElement {
    /// The forall lifetime was generated from a `do Effect` annotation. Where
    /// the elided lifetimes will be replaced with forall lifetimes.
    DoEffect,
}

/// Represents a lifetime that has been generated implicitly by the compiler
/// in the case where the lifetime is elided.
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
pub struct GeneratedForall {
    /// The ID of the symbol from which the forall lifetime was generated.
    pub from_id: Global<pernixc_symbol::ID>,

    /// From which semantic element the forall lifetime was generated.
    pub from_semantic_element: FromSemanticElement,

    /// A unique counter to distinguish multiple generated forall lifetimes
    /// from the same source.
    pub unique_counter: usize,
}

/// Represents a forall lifetime; a lifetime that represents all available
/// lifetimes.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
    EnumAsInner,
)]
#[allow(missing_docs)]
pub enum Forall {
    Named(NamedForall),
    Generated(GeneratedForall),
}

/// Represents a lifetime that has been generated implicitly by the compiler
/// in the case where the lifetime is elided such as `number: &int32`.
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
pub struct ElidedLifetime {
    /// The order in which the elided lifetime was encountered in the function
    /// signature.
    pub order: usize,
}

/// The ID to a elided lifetime.
pub type ElidedLifetimeID = MemberID<pernixc_arena::ID<ElidedLifetime>>;

/// Represents a lifetime term.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
    derive_more::From,
    EnumAsInner,
)]
#[allow(missing_docs)]
pub enum Lifetime {
    Inference(inference::Variable<Self>),
    Parameter(LifetimeParameterID),
    Elided(ElidedLifetimeID),
    Forall(Forall),
    Static,
    Erased,
    Error(Error),
}

impl From<Never> for Lifetime {
    fn from(never: Never) -> Self { match never {} }
}

impl Location<Lifetime, Lifetime> for Never {
    fn assign_sub_term(self, _: &mut Lifetime, _: Lifetime) { match self {} }

    fn get_sub_term(self, _: &Lifetime) -> Option<Lifetime> { match self {} }

    fn get_sub_term_ref(self, _: &Lifetime) -> Option<&Lifetime> {
        match self {}
    }

    fn get_sub_term_mut(self, _: &mut Lifetime) -> Option<&mut Lifetime> {
        match self {}
    }
}

impl Location<Lifetime, Type> for Never {
    fn assign_sub_term(self, _: &mut Lifetime, _: Type) { match self {} }

    fn get_sub_term(self, _: &Lifetime) -> Option<Type> { match self {} }

    fn get_sub_term_ref(self, _: &Lifetime) -> Option<&Type> { match self {} }

    fn get_sub_term_mut(self, _: &mut Lifetime) -> Option<&mut Type> {
        match self {}
    }
}

impl Location<Lifetime, Constant> for Never {
    fn assign_sub_term(self, _: &mut Lifetime, _: Constant) { match self {} }

    fn get_sub_term(self, _: &Lifetime) -> Option<Constant> { match self {} }

    fn get_sub_term_ref(self, _: &Lifetime) -> Option<&Constant> {
        match self {}
    }

    fn get_sub_term_mut(self, _: &mut Lifetime) -> Option<&mut Constant> {
        match self {}
    }
}

impl SubTerm for Lifetime {
    type SubTypeLocation = Never;
    type SubConstantLocation = Never;
    type SubLifetimeLocation = Never;
    type ThisSubTermLocation = Never;
}

impl Match for Lifetime {
    fn substructural_match(
        &self,
        _: &Self,
    ) -> Option<
        Substructural<
            Self::SubLifetimeLocation,
            Self::SubTypeLocation,
            Self::SubConstantLocation,
        >,
    > {
        None
    }

    fn get_substructural(
        substructural: &Substructural<
            Self::SubLifetimeLocation,
            Self::SubTypeLocation,
            Self::SubConstantLocation,
        >,
    ) -> &Vec<Matching<Self, Self::ThisSubTermLocation>> {
        &substructural.lifetimes
    }

    fn get_substructural_mut(
        substructural: &mut Substructural<
            Self::SubLifetimeLocation,
            Self::SubTypeLocation,
            Self::SubConstantLocation,
        >,
    ) -> &mut Vec<Matching<Self, Self::ThisSubTermLocation>> {
        &mut substructural.lifetimes
    }
}

impl crate::display::Display for LifetimeParameterID {
    async fn fmt(
        &self,
        engine: &TrackedEngine,
        formatter: &mut crate::display::Formatter<'_, '_>,
    ) -> std::fmt::Result {
        let generic_parameters =
            engine.get_generic_parameters(self.parent_id).await.unwrap();

        write!(formatter, "'{}", generic_parameters.lifetimes()[self.id].name)
    }
}

impl crate::display::Display for Lifetime {
    async fn fmt(
        &self,
        engine: &TrackedEngine,
        formatter: &mut crate::display::Formatter<'_, '_>,
    ) -> std::fmt::Result {
        match self {
            Self::Parameter(member_id) => {
                member_id.fmt(engine, formatter).await
            }

            Self::Forall(forall) => {
                let forall_number = formatter.forall_lifetime_names(forall);
                write!(formatter, "'∀{forall_number}",)
            }

            Self::Static => write!(formatter, "'static"),

            Self::Inference(inference) => {
                let Some(rendering) = formatter
                    .configuration()
                    .lifetime_inferences()
                    .and_then(|x| x.get(inference))
                else {
                    return Ok(());
                };

                match rendering {
                    crate::display::InferenceRendering::Recurse(lt) => {
                        Box::pin(lt.fmt(engine, formatter)).await
                    }
                    crate::display::InferenceRendering::Rendered(flex_str) => {
                        write!(formatter, "{flex_str}")
                    }
                }
            }

            Self::Elided(_) | Self::Error(_) | Self::Erased => Ok(()),
        }
    }
}
