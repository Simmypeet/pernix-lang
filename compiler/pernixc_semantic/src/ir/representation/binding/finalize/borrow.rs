//! This module implements the borrow checker.
//!
//! ## Credits
//!
//! The model is based on the [`Polonius`](https://github.com/rust-lang/polonius).
//! Thanks to Niko Matsakis descrbing the algorithm in his blog post
//! [`Polonius revisited, part 1`](https://smallcultfollowing.com/babysteps/blog/2023/09/22/polonius-part-1/).
//! and [`Polonius revisited, part 2`](https://smallcultfollowing.com/babysteps/blog/2023/09/29/polonius-part-2/).

use crate::{
    arena::ID,
    ir::value::register::Register,
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
pub struct Loan {
    /// The register to the `ReferenceOf` assignment.
    pub reference_of_register_id: ID<Register<Model>>,

    /// The qualifier of the loan.
    pub qualifier: Qualifier,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Origin {}

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
