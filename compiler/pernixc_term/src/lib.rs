//! Contains the definition of basic terms for the type system.

use std::{
    fmt::{self, Debug},
    hash::Hash,
};

pub mod constant;
pub mod generic_arguments;
pub mod generic_parameter;
pub mod instantiation;
pub mod lifetime;
pub mod matching;
pub mod predicate;
pub mod sub_term;
pub mod r#type;
pub mod type_alias;
pub mod visitor;
pub mod where_clause;

mod arbitrary;

use constant::Constant;
use enum_as_inner::EnumAsInner;
use generic_arguments::GenericArguments;
use lifetime::Lifetime;
use pernixc_table::{
    component::{Name, Parent},
    DisplayObject, GlobalID, Table,
};
use r#type::Type;
use serde::{Deserialize, Serialize};

/// A type that can't never be instantiated.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
pub enum Never {}

/// The model that the terms will be based on.
///
/// The model is used for defining the inferences that can be made in the terms.
pub trait Model:
    Debug
    + Clone
    + Copy
    + PartialEq
    + Eq
    + PartialOrd
    + Ord
    + Hash
    + std::default::Default
    + 'static
    + Send
    + Sync
{
    /// The type to use for lifetime inference.
    type LifetimeInference: Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + Hash
        + 'static
        + Send
        + Sync
        + Serialize
        + for<'x> Deserialize<'x>
        + From<Never>;

    /// The type to use for type inference.
    type TypeInference: Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + Hash
        + 'static
        + Send
        + Sync
        + Serialize
        + for<'x> Deserialize<'x>
        + From<Never>;

    /// The type to use for constant inference.
    type ConstantInference: Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + Hash
        + 'static
        + Send
        + Sync
        + Serialize
        + for<'x> Deserialize<'x>
        + From<Never>;

    /// Converts a type from the default model to the current model.
    fn from_default_type(ty: Type<Default>) -> Type<Self>;

    /// Converts a lifetime from the default model to the current model.
    fn from_default_lifetime(lifetime: Lifetime<Default>) -> Lifetime<Self>;

    /// Converts a constant from the default model to the current model.
    fn from_default_constant(constant: Constant<Default>) -> Constant<Self>;
}

/// The trait used for specifying the model of a term.
pub trait ModelOf {
    /// In which model does the term belong to.
    type Model: Model;

    /// Rebinds the current type by changing the model.
    type Rebind<M: Model>: ModelOf<Model = M>;

    /// Converts the term from model `U` to model `M`.
    fn from_other_model<U: Model>(term: Self::Rebind<U>) -> Self
    where
        <Self::Model as Model>::LifetimeInference: From<U::LifetimeInference>,
        <Self::Model as Model>::TypeInference: From<U::TypeInference>,
        <Self::Model as Model>::ConstantInference: From<U::ConstantInference>;

    /// Tries to convert the term from model `U` to model `M`.
    ///
    /// # Errors
    ///
    /// Returns an error returned by the `TryFrom` implementation of the model.
    fn try_from_other_model<U: Model, E>(
        term: Self::Rebind<U>,
    ) -> Result<Self, E>
    where
        <Self::Model as Model>::LifetimeInference:
            TryFrom<U::LifetimeInference, Error = E>,
        <Self::Model as Model>::TypeInference:
            TryFrom<U::TypeInference, Error = E>,
        <Self::Model as Model>::ConstantInference:
            TryFrom<U::ConstantInference, Error = E>,

        Self: Sized;
}

/// The default model where all inferences are [`Never`].
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
)]
pub struct Default;

impl Model for Default {
    type LifetimeInference = Never;
    type TypeInference = Never;
    type ConstantInference = Never;

    fn from_default_type(ty: Type<Default>) -> Type<Self> { ty }
    fn from_default_lifetime(lifetime: Lifetime<Default>) -> Lifetime<Self> {
        lifetime
    }
    fn from_default_constant(constant: Constant<Default>) -> Constant<Self> {
        constant
    }
}

/// Represents a term where its value is stored as a member of a particular
/// symbol
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct MemberSymbol<M: Model> {
    /// The ID of the symbol that contains the value of the term.
    pub id: GlobalID,

    /// The generic arguments supplied to the member.
    pub member_generic_arguments: GenericArguments<M>,

    /// The generic arguments supplied to the parent scope.
    pub parent_generic_arguments: GenericArguments<M>,
}

impl<M: Model> ModelOf for MemberSymbol<M> {
    type Model = M;
    type Rebind<U: Model> = MemberSymbol<U>;

    fn from_other_model<U: Model>(member_symbol: MemberSymbol<U>) -> Self
    where
        M::LifetimeInference: From<U::LifetimeInference>,
        M::TypeInference: From<U::TypeInference>,
        M::ConstantInference: From<U::ConstantInference>,
    {
        Self {
            id: member_symbol.id,
            member_generic_arguments: GenericArguments::from_other_model(
                member_symbol.member_generic_arguments,
            ),
            parent_generic_arguments: GenericArguments::from_other_model(
                member_symbol.parent_generic_arguments,
            ),
        }
    }

    fn try_from_other_model<U: Model, E>(
        member_symbol: MemberSymbol<U>,
    ) -> Result<Self, E>
    where
        M::LifetimeInference: TryFrom<U::LifetimeInference, Error = E>,
        M::TypeInference: TryFrom<U::TypeInference, Error = E>,
        M::ConstantInference: TryFrom<U::ConstantInference, Error = E>,
    {
        Ok(Self {
            id: member_symbol.id,
            member_generic_arguments: GenericArguments::try_from_other_model(
                member_symbol.member_generic_arguments,
            )?,
            parent_generic_arguments: GenericArguments::try_from_other_model(
                member_symbol.parent_generic_arguments,
            )?,
        })
    }
}

impl<M: Model> pernixc_table::Display for MemberSymbol<M>
where
    GenericArguments<M>: pernixc_table::Display,
{
    fn fmt(
        &self,
        table: &Table,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        let parent_id = table.get::<Parent>(self.id).ok_or(fmt::Error)?.0;
        let parent_qualified_identifier = table
            .get_qualified_name(GlobalID::new(self.id.target_id, parent_id))
            .ok_or(fmt::Error)?;

        write!(f, "{parent_qualified_identifier}{}", DisplayObject {
            table,
            display: &self.parent_generic_arguments
        })?;

        write!(
            f,
            "::{}{}",
            table.get::<Name>(self.id).ok_or(fmt::Error)?.0,
            DisplayObject { table, display: &self.member_generic_arguments }
        )
    }
}

/// Represents a term where its value is stored as a symbol (i.e., `type` or
/// `const` declaration).
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct Symbol<M: Model> {
    /// The ID of the symbol that contains the value of the term.
    pub id: GlobalID,

    /// The generic arguments supplied to the symbol.
    pub generic_arguments: GenericArguments<M>,
}

impl<M: Model> ModelOf for Symbol<M> {
    type Model = M;
    type Rebind<U: Model> = Symbol<U>;

    fn from_other_model<U: Model>(symbol: Symbol<U>) -> Self
    where
        M::LifetimeInference: From<U::LifetimeInference>,
        M::TypeInference: From<U::TypeInference>,
        M::ConstantInference: From<U::ConstantInference>,
    {
        Self {
            id: symbol.id,
            generic_arguments: GenericArguments::from_other_model(
                symbol.generic_arguments,
            ),
        }
    }

    fn try_from_other_model<U: Model, E>(symbol: Symbol<U>) -> Result<Self, E>
    where
        M::LifetimeInference: TryFrom<U::LifetimeInference, Error = E>,
        M::TypeInference: TryFrom<U::TypeInference, Error = E>,
        M::ConstantInference: TryFrom<U::ConstantInference, Error = E>,
    {
        Ok(Self {
            id: symbol.id,
            generic_arguments: GenericArguments::try_from_other_model(
                symbol.generic_arguments,
            )?,
        })
    }
}

impl<M: Model> pernixc_table::Display for Symbol<M>
where
    GenericArguments<M>: pernixc_table::Display,
{
    fn fmt(
        &self,
        table: &Table,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        let qualified_name =
            table.get_qualified_name(self.id).ok_or(fmt::Error)?;

        write!(f, "{qualified_name}{}", DisplayObject {
            table,
            display: &self.generic_arguments
        })
    }
}

/// Represents a single element of a tuple.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct TupleElement<Term> {
    /// The term stored in this element.
    pub term: Term,

    /// Whether the term is unpacked.
    pub is_unpacked: bool,
}

/// Represents a tuple of terms.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Default,
    Serialize,
    Deserialize,
)]
pub struct Tuple<Term> {
    /// The elements of the tuple.
    pub elements: Vec<TupleElement<Term>>,
}

impl<T: ModelOf> Tuple<T> {
    /// Converts a tuple from model `U` to model `M`.
    #[must_use]
    pub fn from_other_model<U: Model>(tuple: Tuple<T::Rebind<U>>) -> Self
    where
        <T::Model as Model>::LifetimeInference: From<U::LifetimeInference>,
        <T::Model as Model>::TypeInference: From<U::TypeInference>,
        <T::Model as Model>::ConstantInference: From<U::ConstantInference>,
    {
        Self {
            elements: tuple
                .elements
                .into_iter()
                .map(|x| TupleElement {
                    term: T::from_other_model(x.term),
                    is_unpacked: x.is_unpacked,
                })
                .collect(),
        }
    }

    /// Tries to convert a tuple from model `U` to model `M`.
    ///
    /// # Errors
    ///
    /// Returns an error returned by the `TryFrom` implementation of the model.
    pub fn try_from_other_model<U: Model, E>(
        tuple: Tuple<T::Rebind<U>>,
    ) -> Result<Self, E>
    where
        <T::Model as Model>::LifetimeInference:
            TryFrom<U::LifetimeInference, Error = E>,
        <T::Model as Model>::TypeInference:
            TryFrom<U::TypeInference, Error = E>,
        <T::Model as Model>::ConstantInference:
            TryFrom<U::ConstantInference, Error = E>,
    {
        Ok(Self {
            elements: tuple
                .elements
                .into_iter()
                .map(|x| {
                    T::try_from_other_model(x.term).map(|term| TupleElement {
                        term,
                        is_unpacked: x.is_unpacked,
                    })
                })
                .collect::<Result<_, _>>()?,
        })
    }
}

impl<T: pernixc_table::Display + Clone> pernixc_table::Display for Tuple<T> {
    fn fmt(
        &self,
        table: &Table,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(f, "(")?;

        let mut peekable = self.elements.iter().peekable();

        while let Some(element) = peekable.next() {
            let is_last = peekable.peek().is_none();
            let is_unpacked = element.is_unpacked;

            if is_unpacked {
                write!(f, "...")?;
            }

            write!(f, "{}", DisplayObject { table, display: &element.term })?;

            if !is_last {
                write!(f, ", ")?;
            }
        }

        write!(f, ")")
    }
}

/// Represents an errornuos term. Used for representing errors in the type
/// system.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
pub struct Error;

/// An enumeration of references to all three kinds of terms: [`Lifetime`],
/// [`Type`], and [`Constant`].
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumAsInner,
    derive_more::From,
)]
#[allow(missing_docs)]
pub enum Kind<'a, M: Model> {
    Lifetime(&'a Lifetime<M>),
    Type(&'a Type<M>),
    Constant(&'a Constant<M>),
}

/// An enumeration of mutable references to all three kinds of terms:
/// [`Lifetime`], [`Type`], and [`Constant`].
#[derive(
    Debug, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, derive_more::From,
)]
#[allow(missing_docs)]
pub enum KindMut<'a, M: Model> {
    Lifetime(&'a mut Lifetime<M>),
    Type(&'a mut Type<M>),
    Constant(&'a mut Constant<M>),
}
