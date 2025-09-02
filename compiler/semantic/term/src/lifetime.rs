//! Contains the definition of [`Lifetime`] term.

use std::fmt::Write;

use enum_as_inner::EnumAsInner;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::TrackedEngine;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_symbol::MemberID;

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
    Copy,
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
}

/// Represents a forall lifetime; a lifetime that represents all available
/// lifetimes.
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
    EnumAsInner,
)]
#[allow(missing_docs)]
pub enum Forall {
    Named(NamedForall),
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
    Copy,
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

impl Lifetime {
    /// Determines whether the lifetime will be displayed using
    /// [`crate::display::Display`].
    #[must_use]
    pub const fn will_be_displayed(&self) -> bool {
        !matches!(self, Self::Inference(_) | Self::Erased | Self::Error(_))
    }
}

impl crate::display::Display for Lifetime {
    async fn fmt(
        &self,
        engine: &TrackedEngine,
        formatter: &mut crate::display::Formatter<'_>,
    ) -> std::fmt::Result {
        match self {
            Self::Parameter(member_id) => {
                let generic_parameters = engine
                    .get_generic_parameters(member_id.parent_id)
                    .await
                    .unwrap();

                write!(
                    formatter,
                    "'{}",
                    generic_parameters.lifetimes()[member_id.id].name
                )
            }

            Self::Forall(forall) => {
                let forall_number = formatter.forall_lifetime_names(forall);
                write!(formatter, "'∀{forall_number}",)
            }

            Self::Static => write!(formatter, "'static"),

            Self::Elided(_)
            | Self::Inference(_)
            | Self::Error(_)
            | Self::Erased => Ok(()),
        }
    }
}
