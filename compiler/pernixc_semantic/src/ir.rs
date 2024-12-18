//! Contains the definition of the intermediate representation of the program.

use std::{fmt::Debug, hash::Hash};

use enum_as_inner::EnumAsInner;
use pernixc_base::source_file::Span;

use self::representation::Representation;
use crate::{
    symbol::table::{self, Table},
    type_system::{
        fresh::Fresh,
        model,
        term::{
            constant::Constant,
            lifetime::Lifetime,
            r#type::{self, Type},
            Never, Term,
        },
        OverflowError,
    },
};

pub mod address;
pub mod alloca;
pub mod control_flow_graph;
pub mod instruction;
pub mod pattern;
pub mod representation;
pub mod scope;
pub mod value;

/// The model to used to generate the IR.
pub trait State {
    /// The model to use for the type system.
    type Model: model::Model;
}

/// The type system model used in the IR.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Model;

/// The inference variable used for lifetimes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Erased;

impl<T: table::State> table::Display<T> for Erased {
    fn fmt(
        &self,
        _: &Table<T>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(f, "_")
    }
}

impl From<Never> for Erased {
    fn from(value: Never) -> Self { match value {} }
}

impl Fresh for Erased {
    fn fresh() -> Self { Self }
}

impl model::Model for Model {
    type LifetimeInference = Erased;
    type TypeInference = Never;
    type ConstantInference = Never;

    fn from_default_type(ty: Type<model::Default>) -> Type<Self> {
        Type::from_other_model(ty)
    }

    fn from_default_lifetime(
        lifetime: Lifetime<model::Default>,
    ) -> Lifetime<Self> {
        Lifetime::from_other_model(lifetime)
    }

    fn from_default_constant(
        constant: Constant<model::Default>,
    ) -> Constant<Self> {
        Constant::from_other_model(constant)
    }
}

/// A tag type representing a successfully generated IR.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Success(() /* Prevent arbitrary instantiation */);

impl State for Success {
    type Model = Model;
}

/// A tag type representing an IR that is suboptimal (contains an error).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Suboptimal;

impl State for Suboptimal {
    type Model = Model;
}

/// An intermediate representation of the program.
#[derive(Debug, Clone, PartialEq, Eq, Default, derive_more::Deref)]
pub struct IR<T: State> {
    #[deref]
    pub(crate) representation: Representation<T::Model>,

    state: T,
}

/// A trait for transforming terms from one model to another.
///
/// For example, this can be used when finishing the inference process to
/// transform the terms from the inference model to the final concrete model.
pub trait Transform<T: Term> {
    /// The target model to transform the terms to.
    type Target: model::Model;

    /// The error that might occur when transforming the terms.
    type Error;

    /// Inspects the term. This is called before transforming the term.
    ///
    /// The method should report the error seen in the term. For exmple, if the
    /// term contains an un-inferrable type. This is called at most onece per
    /// expression/entity.
    #[allow(clippy::missing_errors_doc)]
    fn inspect(&mut self, term: &T, span: Span) -> Result<(), Self::Error>;

    /// Transforms the term.
    #[allow(clippy::missing_errors_doc)]
    fn transform(
        &mut self,
        term: T,
        span: Span,
    ) -> Result<T::Rebind<Self::Target>, Self::Error>;

    /// Inspects and transforms the term.
    #[allow(clippy::missing_errors_doc)]
    fn inspect_and_transform(
        &mut self,
        term: T,
        span: Span,
    ) -> Result<T::Rebind<Self::Target>, Self::Error> {
        self.inspect(&term, span.clone())?;
        self.transform(term, span)
    }
}

/// The model that uses the [`r#type::Constraint`] as the inference type for
/// type term and [`NoConstraint`]  as the inference type for constant and
/// lifetime terms.
///
/// This is primarily used for reporting the type inference errors, which can
/// display the current state of the inference context.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct ConstraintModel;

impl From<Never> for NoConstraint {
    fn from(never: Never) -> Self { match never {} }
}

impl<T: table::State> table::Display<T> for NoConstraint {
    fn fmt(
        &self,
        _: &Table<T>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(f, "_")
    }
}

impl<T: table::State> table::Display<T> for r#type::Constraint {
    #[allow(clippy::enum_glob_use)]
    fn fmt(
        &self,
        _: &Table<T>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        use r#type::Constraint::*;

        match self {
            All(_) => write!(f, "{{any}}"),
            Number => write!(f, "{{number}}"),
            Signed => write!(f, "{{signed}}"),
            Floating => write!(f, "{{floating}}"),
            Integer => write!(f, "{{integer}}"),
            SignedInteger => write!(f, "{{signedInteger}}"),
            UnsignedInteger => write!(f, "{{unsignedInteger}}"),
        }
    }
}

/// The struct used for representing the absence of any constraint in inference
/// variables.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct NoConstraint;

impl model::Model for ConstraintModel {
    type LifetimeInference = NoConstraint;
    type TypeInference = r#type::Constraint;
    type ConstantInference = NoConstraint;

    fn from_default_type(ty: Type<model::Default>) -> Type<Self> {
        Type::from_other_model(ty)
    }

    fn from_default_lifetime(
        lifetime: Lifetime<model::Default>,
    ) -> Lifetime<Self> {
        Lifetime::from_other_model(lifetime)
    }

    fn from_default_constant(
        constant: Constant<model::Default>,
    ) -> Constant<Self> {
        Constant::from_other_model(constant)
    }
}

/// An error returned calling [`representation::Values::type_of_register`] and
/// [`representation::Values::type_of_address`].
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    thiserror::Error,
    EnumAsInner,
)]
#[allow(missing_docs)]
pub enum TypeOfError {
    #[error(
        "this is caused by many resons such as, missing IDs, incompatible \
         types, etc. generally, it's an invalid inputs for the lookup"
    )]
    Discrepancy,

    #[error(transparent)]
    Overflow(#[from] OverflowError),
}
