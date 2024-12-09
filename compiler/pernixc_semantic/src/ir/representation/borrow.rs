//! Contains the model used in borrow checking and its structs/enums
//!
//! This model doesn't contain the logic for borrow checking, but only the
//! data structures used in the borrow checking.

use std::{collections::HashSet, fmt::Display};

use enum_as_inner::EnumAsInner;

use crate::{
    arena::{Key, ID},
    ir::value::register::Register,
    symbol::{table, LifetimeParameterID},
    type_system::{
        model,
        term::{
            constant::Constant, lifetime::Lifetime, r#type::Type, Never, Term,
        },
    },
};

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner,
)]
#[allow(missing_docs)]
pub enum Loan {
    Borrow(ID<Register<Model>>),
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

impl Display for ID<Origin> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "'{}", self.into_index())
    }
}

impl<S: table::State> table::Display<S> for ID<Origin> {
    fn fmt(
        &self,
        _: &table::Table<S>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(f, "'{}", self.into_index())
    }
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
