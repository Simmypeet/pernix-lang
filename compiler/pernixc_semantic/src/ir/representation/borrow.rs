//! Contains the model used in borrow checking and its structs/enums
//!
//! This model doesn't contain the logic for borrow checking, but only the
//! data structures used in the borrow checking.

use std::collections::HashSet;

use enum_as_inner::EnumAsInner;

use crate::{
    arena::ID,
    ir::value::register::Register,
    symbol::LifetimeParameterID,
    type_system::{
        model,
        term::{
            constant::Constant,
            lifetime::Lifetime,
            r#type::{Qualifier, Type},
            Never, Term,
        },
    },
};

/// Represents an expression `&x` or `&mutable x` which creates a reference to a
/// value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Borrow {
    /// The register to the `ReferenceOf` assignment.
    pub reference_of_register_id: ID<Register<Model>>,

    /// The qualifier of the loan.
    pub qualifier: Qualifier,
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner,
)]
#[allow(missing_docs)]
pub enum Loan {
    Borrow(Borrow),
    LifetimeParameter(LifetimeParameterID),
    Static,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[allow(missing_docs)]
pub struct Origin {
    pub loans: HashSet<Loan>,
}

/// The model used in borrow checking phase.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Model;

impl From<Never> for ID<Origin> {
    fn from(value: Never) -> Self { match value {} }
}

impl model::Model for Model {
    type LifetimeInference = ID<Origin>;
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
