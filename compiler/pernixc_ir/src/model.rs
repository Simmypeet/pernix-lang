//! Contains the definition of model used by the IR.

use pernixc_abort::Abort;
use pernixc_semantic::Table;
use pernixc_source_file::Span;
use pernixc_semantic::term::{
    constant::Constant, lifetime::Lifetime, r#type::Type, Default, ModelOf,
    Never,
};
use serde::{Deserialize, Serialize};

/// The set of types that can be inferred. Used in type inference.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    derive_more::Display,
    Serialize,
    Deserialize,
)]
pub enum Constraint {
    /// The type can be inferred into any type.
    ///
    /// The boolean valuie indicates whether the type can be inferred as a unit
    /// type as a default value.
    #[display(fmt = "{{any}}")]
    All(bool),

    /// The type can be any number type. (signed/unsigned/floating)
    #[display(fmt = "{{number}}")]
    Number,

    /// The type can be integer number type. (signed/unsigned)
    #[display(fmt = "{{integer}}")]
    Integer,

    /// The type can be signed number type. (signed integer)
    #[display(fmt = "{{signedInteger}}")]
    SignedInteger,

    /// The type can be signed number type. (signed integer/floating)
    #[display(fmt = "{{signed}}")]
    Signed,

    /// The type can be unsigned number type. (unsigned integer)
    #[display(fmt = "{{unsignedInteger}}")]
    UnsignedInteger,

    /// The type can be only floating number type. (float32/float64)
    #[display(fmt = "{{floating}}")]
    Floating,
}

impl From<Never> for Constraint {
    fn from(value: Never) -> Self { match value {} }
}

/// The type system model used in the IR.
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
pub struct Model;

/// The inference variable used for lifetimes.
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
pub struct Erased;

impl pernixc_semantic::Display for Erased {
    fn fmt(
        &self,
        _: &Table,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(f, "_")
    }
}

impl From<Never> for Erased {
    fn from(value: Never) -> Self { match value {} }
}

impl pernixc_term::Model for Model {
    type LifetimeInference = Erased;
    type TypeInference = Never;
    type ConstantInference = Never;

    fn from_default_type(ty: Type<Default>) -> Type<Self> {
        Type::from_other_model(ty)
    }

    fn from_default_lifetime(lifetime: Lifetime<Default>) -> Lifetime<Self> {
        Lifetime::from_other_model(lifetime)
    }

    fn from_default_constant(constant: Constant<Default>) -> Constant<Self> {
        Constant::from_other_model(constant)
    }
}

/// The model that uses the [`Constraint`] as the inference type for
/// type term and [`NoConstraint`]  as the inference type for constant and
/// lifetime terms.
///
/// This is primarily used for reporting the type inference errors, which can
/// display the current state of the inference context.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Constrained;

impl From<Never> for NoConstraint {
    fn from(never: Never) -> Self { match never {} }
}

impl pernixc_semantic::Display for NoConstraint {
    fn fmt(
        &self,
        _: &Table,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(f, "_")
    }
}

impl pernixc_semantic::Display for Constraint {
    #[allow(clippy::enum_glob_use)]
    fn fmt(
        &self,
        _: &Table,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        match self {
            Self::All(_) => write!(f, "{{any}}"),
            Self::Number => write!(f, "{{number}}"),
            Self::Signed => write!(f, "{{signed}}"),
            Self::Floating => write!(f, "{{floating}}"),
            Self::Integer => write!(f, "{{integer}}"),
            Self::SignedInteger => write!(f, "{{signedInteger}}"),
            Self::UnsignedInteger => write!(f, "{{unsignedInteger}}"),
        }
    }
}

/// The struct used for representing the absence of any constraint in inference
/// variables.
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
pub struct NoConstraint;

impl pernixc_term::Model for Constrained {
    type LifetimeInference = NoConstraint;
    type TypeInference = Constraint;
    type ConstantInference = NoConstraint;

    fn from_default_type(ty: Type<Default>) -> Type<Self> {
        Type::from_other_model(ty)
    }

    fn from_default_lifetime(lifetime: Lifetime<Default>) -> Lifetime<Self> {
        Lifetime::from_other_model(lifetime)
    }

    fn from_default_constant(constant: Constant<Default>) -> Constant<Self> {
        Constant::from_other_model(constant)
    }
}

/// A trait for transforming terms from one model to another.
///
/// For example, this can be used when finishing the inference process to
/// transform the terms from the inference model to the final concrete model.
pub trait Transform<T: ModelOf> {
    /// The target model to transform the terms to.
    type Target: pernixc_term::Model;

    /// The error that might occur when transforming the terms.
    type Error: From<Abort>;

    /// Inspects the term. This is called before transforming the term.
    ///
    /// The method should report the error seen in the term. For exmple, if the
    /// term contains an un-inferrable type. This is called at most onece per
    /// expression/entity.
    #[allow(clippy::missing_errors_doc)]
    fn inspect(
        &mut self,
        term: &T,
        span: Option<&Span>,
    ) -> Result<(), Self::Error>;

    /// Transforms the term.
    #[allow(clippy::missing_errors_doc)]
    fn transform(
        &mut self,
        term: T,
        span: Option<&Span>,
    ) -> Result<T::Rebind<Self::Target>, Self::Error>;

    /// Inspects and transforms the term.
    #[allow(clippy::missing_errors_doc)]
    fn inspect_and_transform(
        &mut self,
        term: T,
        span: Option<&Span>,
    ) -> Result<T::Rebind<Self::Target>, Self::Error> {
        self.inspect(&term, span)?;
        self.transform(term, span)
    }
}
