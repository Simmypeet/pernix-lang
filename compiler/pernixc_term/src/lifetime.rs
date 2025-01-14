//! Contains the definition of the [`Lifetime`] term.

use core::fmt;

use enum_as_inner::EnumAsInner;
use pernixc_arena::Key;
use pernixc_source_file::Span;
use pernixc_table::{DisplayObject, GlobalID, Table};
use serde::{Deserialize, Serialize};

use crate::{
    constant::Constant,
    generic_parameter::{GenericParameters, LifetimeParameterID},
    matching::{self, Match, Matching},
    r#type::Type,
    sub_term::{AssignSubTermError, Location, SubTerm, TermLocation},
    Error, Model, ModelOf, Never,
};

mod arbitrary;

/// Represents a for-all quantified lifetime, denoted by `for['a]` syntax, used
/// in higher-ranked predicates.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Forall {
    /// The global ID where the forall lifetime was declared.
    pub global_id: GlobalID,

    /// The unique ID of the forall lifetime within the global ID scope.
    pub id: usize,

    /// The span where the forall lifetime was declared.
    ///
    /// This field doesn't influence the equality, ordering, and hashing od the
    /// this struct.
    #[serde(skip)]
    pub span: Option<Span>,
}

impl PartialEq for Forall {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id && self.global_id == other.global_id
    }
}

impl Eq for Forall {}

impl PartialOrd for Forall {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Forall {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.id.cmp(&other.id).then(self.global_id.cmp(&other.global_id))
    }
}

impl std::hash::Hash for Forall {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
        self.global_id.hash(state);
    }
}

/// Represents a lifetime annotation term.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumAsInner,
    Serialize,
    Deserialize,
    derive_more::From,
)]
#[allow(missing_docs)]
pub enum Lifetime<M: Model> {
    Static,
    #[from]
    Parameter(LifetimeParameterID),
    Inference(M::LifetimeInference),
    #[from]
    Forall(Forall),
    #[from]
    Error(Error),
}

impl<M: Model> pernixc_table::Display for Lifetime<M>
where
    M::LifetimeInference: pernixc_table::Display,
{
    fn fmt(
        &self,
        table: &Table,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        match self {
            Self::Static => write!(f, "'static"),
            Self::Parameter(parameter) => {
                match &table
                    .query::<GenericParameters>(parameter.parent)
                    .map_err(|_| fmt::Error)?
                    .lifetimes()
                    .get(parameter.id)
                    .ok_or(fmt::Error)?
                    .name
                {
                    Some(name) => write!(f, "'{name}"),
                    None => write!(f, "'{}", parameter.id.into_index()),
                }
            }
            Self::Inference(inference) => {
                write!(f, "'{}", DisplayObject { display: inference, table })
            }
            Self::Forall(forall_lifetime) => match &forall_lifetime.span {
                Some(span) => write!(f, "'∀{}", span.str()),
                None => write!(f, "'∀?"),
            },
            Self::Error(_) => write!(f, "'{{error}}"),
        }
    }
}

impl<M: Model> ModelOf for Lifetime<M> {
    type Model = M;
    type Rebind<U: Model> = Lifetime<U>;

    fn from_other_model<U: Model>(term: Self::Rebind<U>) -> Self
    where
        M::LifetimeInference: From<U::LifetimeInference>,
        M::TypeInference: From<U::TypeInference>,
        M::ConstantInference: From<U::ConstantInference>,
    {
        match term {
            Lifetime::Error(Error) => Self::Error(Error),
            Lifetime::Static => Self::Static,
            Lifetime::Parameter(parameter) => Self::Parameter(parameter),
            Lifetime::Inference(inference) => {
                Self::Inference(M::LifetimeInference::from(inference))
            }
            Lifetime::Forall(forall) => Self::Forall(forall),
        }
    }

    fn try_from_other_model<U: Model, E>(
        term: Self::Rebind<U>,
    ) -> Result<Self, E>
    where
        M::LifetimeInference: TryFrom<U::LifetimeInference, Error = E>,
        M::TypeInference: TryFrom<U::TypeInference, Error = E>,
        M::ConstantInference: TryFrom<U::ConstantInference, Error = E>,
    {
        match term {
            Lifetime::Error(Error) => Ok(Self::Error(Error)),
            Lifetime::Static => Ok(Self::Static),
            Lifetime::Parameter(parameter) => Ok(Self::Parameter(parameter)),
            Lifetime::Inference(inference) => {
                Ok(Self::Inference(M::LifetimeInference::try_from(inference)?))
            }
            Lifetime::Forall(forall) => Ok(Self::Forall(forall)),
        }
    }
}

impl<M: Model> Location<Lifetime<M>, Lifetime<M>> for Never {
    fn assign_sub_term(
        self,
        _: &mut Lifetime<M>,
        _: Lifetime<M>,
    ) -> Result<(), AssignSubTermError> {
        match self {}
    }

    fn get_sub_term(self, _: &Lifetime<M>) -> Option<Lifetime<M>> {
        match self {}
    }

    fn get_sub_term_ref(self, _: &Lifetime<M>) -> Option<&Lifetime<M>> {
        match self {}
    }

    fn get_sub_term_mut(self, _: &mut Lifetime<M>) -> Option<&mut Lifetime<M>> {
        match self {}
    }
}

impl From<Never> for TermLocation {
    fn from(value: Never) -> Self { match value {} }
}

impl<M: Model> Location<Lifetime<M>, Type<M>> for Never {
    fn assign_sub_term(
        self,
        _: &mut Lifetime<M>,
        _: Type<M>,
    ) -> Result<(), AssignSubTermError> {
        match self {}
    }

    fn get_sub_term(self, _: &Lifetime<M>) -> Option<Type<M>> { match self {} }

    fn get_sub_term_ref(self, _: &Lifetime<M>) -> Option<&Type<M>> {
        match self {}
    }

    fn get_sub_term_mut(self, _: &mut Lifetime<M>) -> Option<&mut Type<M>> {
        match self {}
    }
}

impl<M: Model> Location<Lifetime<M>, Constant<M>> for Never {
    fn assign_sub_term(
        self,
        _: &mut Lifetime<M>,
        _: Constant<M>,
    ) -> Result<(), AssignSubTermError> {
        match self {}
    }

    fn get_sub_term(self, _: &Lifetime<M>) -> Option<Constant<M>> {
        match self {}
    }

    fn get_sub_term_ref(self, _: &Lifetime<M>) -> Option<&Constant<M>> {
        match self {}
    }

    fn get_sub_term_mut(self, _: &mut Lifetime<M>) -> Option<&mut Constant<M>> {
        match self {}
    }
}

impl<M: Model> SubTerm for Lifetime<M> {
    type SubTypeLocation = Never;
    type SubConstantLocation = Never;
    type SubLifetimeLocation = Never;
    type ThisSubTermLocation = Never;
}

impl<M: Model> Match for Lifetime<M> {
    fn substructural_match(
        &self,
        _: &Self,
    ) -> Option<
        matching::Substructural<
            M,
            Self::SubLifetimeLocation,
            Self::SubTypeLocation,
            Self::SubConstantLocation,
        >,
    > {
        None
    }

    fn get_substructural(
        substructural: &matching::Substructural<
            M,
            Self::SubLifetimeLocation,
            Self::SubTypeLocation,
            Self::SubConstantLocation,
        >,
    ) -> &Vec<Matching<Self, Self::ThisSubTermLocation>> {
        &substructural.lifetimes
    }

    fn get_substructural_mut(
        substructural: &mut matching::Substructural<
            M,
            Self::SubLifetimeLocation,
            Self::SubTypeLocation,
            Self::SubConstantLocation,
        >,
    ) -> &mut Vec<Matching<Self, Self::ThisSubTermLocation>> {
        &mut substructural.lifetimes
    }
}

impl<M: Model> From<Never> for Lifetime<M> {
    fn from(value: Never) -> Self { match value {} }
}
