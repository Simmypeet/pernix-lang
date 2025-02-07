//! Contains various definition of predicates.

mod compatible;
mod constant_type;
mod marker;
mod outlives;
mod r#trait;
mod tuple;

use enum_as_inner::EnumAsInner;
use pernixc_table::Table;
use serde::{Deserialize, Serialize};

use crate::{
    sub_term::TermLocation,
    visitor::{self, accept_recursive, Recursive},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct ContainsErrorVisitor {
    contains_error: bool,
}

impl<M: Model> Recursive<'_, Lifetime<M>> for ContainsErrorVisitor {
    fn visit(
        &mut self,
        term: &Lifetime<M>,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        if term.is_error() {
            self.contains_error = true;
            false
        } else {
            true
        }
    }
}

impl<M: Model> Recursive<'_, Type<M>> for ContainsErrorVisitor {
    fn visit(
        &mut self,
        term: &Type<M>,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        if term.is_error() {
            self.contains_error = true;
            false
        } else {
            true
        }
    }
}

impl<M: Model> Recursive<'_, Constant<M>> for ContainsErrorVisitor {
    fn visit(
        &mut self,
        term: &Constant<M>,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        if term.is_error() {
            self.contains_error = true;
            false
        } else {
            true
        }
    }
}

fn contains_error<T: visitor::Element>(term: &T) -> bool {
    let mut visitor = ContainsErrorVisitor { contains_error: false };

    accept_recursive(term, &mut visitor);

    visitor.contains_error
}

pub use compatible::Compatible;
pub use constant_type::ConstantType;
pub use marker::{Negative as NegativeMarker, Positive as PositiveMarker};
pub use outlives::Outlives;
pub use r#trait::{Negative as NegativeTrait, Positive as PositiveTrait};
pub use tuple::Tuple;

use crate::{
    constant::Constant,
    generic_arguments::GenericArguments,
    instantiation::{self, Instantiation},
    lifetime::Lifetime,
    r#type::{TraitMember, Type},
    MemberSymbol, Model, ModelOf,
};

/// A predicate that can appear in the where clause.
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
pub enum Predicate<M: Model> {
    TraitTypeCompatible(Compatible<TraitMember<M>, Type<M>>),
    ConstantType(ConstantType<M>),
    LifetimeOutlives(Outlives<Lifetime<M>>),
    TypeOutlives(Outlives<Type<M>>),
    TupleType(Tuple<Type<M>>),
    PositiveTrait(PositiveTrait<M>),
    NegativeTrait(NegativeTrait<M>),
    PositiveMarker(PositiveMarker<M>),
    NegativeMarker(NegativeMarker<M>),
}

impl<M: Model> ModelOf for Predicate<M> {
    type Model = M;
    type Rebind<U: Model> = Predicate<U>;

    fn from_other_model<U: Model>(predicate: Predicate<U>) -> Self
    where
        M::LifetimeInference: From<U::LifetimeInference>,
        M::TypeInference: From<U::TypeInference>,
        M::ConstantInference: From<U::ConstantInference>,
    {
        match predicate {
            Predicate::TraitTypeCompatible(equality) => {
                Self::TraitTypeCompatible(Compatible::new(
                    TraitMember(MemberSymbol {
                        id: equality.lhs.0.id,
                        member_generic_arguments:
                            GenericArguments::from_other_model(
                                equality.lhs.0.member_generic_arguments,
                            ),
                        parent_generic_arguments:
                            GenericArguments::from_other_model(
                                equality.lhs.0.parent_generic_arguments,
                            ),
                    }),
                    Type::from_other_model(equality.rhs),
                ))
            }
            Predicate::ConstantType(constant_type) => Self::ConstantType(
                ConstantType::from_other_model(constant_type),
            ),
            Predicate::LifetimeOutlives(outlives) => {
                Self::LifetimeOutlives(Outlives::new(
                    Lifetime::from_other_model(outlives.operand),
                    Lifetime::from_other_model(outlives.bound),
                ))
            }
            Predicate::TypeOutlives(outlives) => {
                Self::TypeOutlives(Outlives::new(
                    Type::from_other_model(outlives.operand),
                    Lifetime::from_other_model(outlives.bound),
                ))
            }
            Predicate::TupleType(tuple) => {
                Self::TupleType(Tuple(Type::from_other_model(tuple.0)))
            }
            Predicate::PositiveTrait(positive) => {
                Self::PositiveTrait(PositiveTrait::from_other_model(positive))
            }
            Predicate::NegativeTrait(negative) => {
                Self::NegativeTrait(NegativeTrait::from_other_model(negative))
            }
            Predicate::PositiveMarker(positive) => {
                Self::PositiveMarker(PositiveMarker::from_other_model(positive))
            }
            Predicate::NegativeMarker(negative) => {
                Self::NegativeMarker(NegativeMarker::from_other_model(negative))
            }
        }
    }

    fn try_from_other_model<U: Model, E>(
        predicate: Predicate<U>,
    ) -> Result<Self, E>
    where
        M::LifetimeInference: TryFrom<U::LifetimeInference, Error = E>,
        M::TypeInference: TryFrom<U::TypeInference, Error = E>,
        M::ConstantInference: TryFrom<U::ConstantInference, Error = E>,
    {
        Ok(match predicate {
            Predicate::TraitTypeCompatible(equality) => {
                Self::TraitTypeCompatible(Compatible::new(
                    TraitMember(MemberSymbol {
                        id: equality.lhs.0.id,
                        member_generic_arguments:
                            GenericArguments::try_from_other_model(
                                equality.lhs.0.member_generic_arguments,
                            )?,
                        parent_generic_arguments:
                            GenericArguments::try_from_other_model(
                                equality.lhs.0.parent_generic_arguments,
                            )?,
                    }),
                    Type::try_from_other_model(equality.rhs)?,
                ))
            }
            Predicate::ConstantType(constant_type) => Self::ConstantType(
                ConstantType::try_from_other_model(constant_type)?,
            ),
            Predicate::LifetimeOutlives(outlives) => {
                Self::LifetimeOutlives(Outlives::new(
                    Lifetime::try_from_other_model(outlives.operand)?,
                    Lifetime::try_from_other_model(outlives.bound)?,
                ))
            }
            Predicate::TypeOutlives(outlives) => {
                Self::TypeOutlives(Outlives::new(
                    Type::try_from_other_model(outlives.operand)?,
                    Lifetime::try_from_other_model(outlives.bound)?,
                ))
            }
            Predicate::TupleType(tuple) => {
                Self::TupleType(Tuple(Type::try_from_other_model(tuple.0)?))
            }
            Predicate::PositiveTrait(positive) => Self::PositiveTrait(
                PositiveTrait::try_from_other_model(positive)?,
            ),
            Predicate::NegativeTrait(negative) => Self::NegativeTrait(
                NegativeTrait::try_from_other_model(negative)?,
            ),
            Predicate::PositiveMarker(positive) => Self::PositiveMarker(
                PositiveMarker::try_from_other_model(positive)?,
            ),
            Predicate::NegativeMarker(negative) => Self::NegativeMarker(
                NegativeMarker::try_from_other_model(negative)?,
            ),
        })
    }
}

impl<M: Model> pernixc_table::Display for Predicate<M>
where
    Compatible<TraitMember<M>, Type<M>>: pernixc_table::Display,
    ConstantType<M>: pernixc_table::Display,
    Outlives<Lifetime<M>>: pernixc_table::Display,
    Outlives<Type<M>>: pernixc_table::Display,
    Tuple<Type<M>>: pernixc_table::Display,
    PositiveTrait<M>: pernixc_table::Display,
    NegativeTrait<M>: pernixc_table::Display,
    PositiveMarker<M>: pernixc_table::Display,
    NegativeMarker<M>: pernixc_table::Display,
{
    fn fmt(
        &self,
        table: &Table,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        match self {
            Self::TraitTypeCompatible(equality) => equality.fmt(table, f),
            Self::ConstantType(constant_type) => constant_type.fmt(table, f),
            Self::LifetimeOutlives(outlives) => outlives.fmt(table, f),
            Self::TypeOutlives(outlives) => outlives.fmt(table, f),
            Self::TupleType(tuple) => tuple.fmt(table, f),
            Self::PositiveTrait(tr) => tr.fmt(table, f),
            Self::NegativeTrait(tr) => tr.fmt(table, f),
            Self::PositiveMarker(marker) => marker.fmt(table, f),
            Self::NegativeMarker(marker) => marker.fmt(table, f),
        }
    }
}

impl<M: Model> Predicate<M> {
    /// Checks if the predicate has an errornous term.
    #[must_use]
    pub fn contains_error(&self) -> bool {
        match self {
            Self::TraitTypeCompatible(equality) => equality.contains_error(),
            Self::ConstantType(constant_type) => constant_type.contains_error(),
            Self::LifetimeOutlives(outlives) => outlives.contains_error(),
            Self::TypeOutlives(outlives) => outlives.contains_error(),
            Self::TupleType(tuple) => tuple.contains_error(),
            Self::PositiveTrait(tr) => tr.contains_error(),
            Self::NegativeTrait(tr) => tr.contains_error(),
            Self::PositiveMarker(marker) => marker.contains_error(),
            Self::NegativeMarker(marker) => marker.contains_error(),
        }
    }

    /// Applies the instantiate to the predicate.
    pub fn instantiate(&mut self, substitution: &Instantiation<M>) {
        match self {
            Self::TraitTypeCompatible(equality) => {
                equality.instantiate(substitution);
            }
            Self::ConstantType(constant_type) => {
                constant_type.instantiate(substitution);
            }
            Self::LifetimeOutlives(outlives) => {
                outlives.instantiate(substitution);
            }
            Self::TypeOutlives(outlives) => outlives.instantiate(substitution),
            Self::TupleType(tuple) => tuple.instantiate(substitution),
            Self::PositiveTrait(tr) => tr.instantiate(substitution),
            Self::NegativeTrait(tr) => tr.instantiate(substitution),
            Self::PositiveMarker(marker) => marker.instantiate(substitution),
            Self::NegativeMarker(marker) => marker.instantiate(substitution),
        }
    }

    /// Converts a predicate with the model `U` into the model `M`.
    pub fn from_other_model<U: Model>(predicate: Predicate<U>) -> Self
    where
        M::LifetimeInference: From<U::LifetimeInference>,
        M::TypeInference: From<U::TypeInference>,
        M::ConstantInference: From<U::ConstantInference>,
    {
        match predicate {
            Predicate::TraitTypeCompatible(equality) => {
                Self::TraitTypeCompatible(Compatible::new(
                    TraitMember(MemberSymbol {
                        id: equality.lhs.0.id,
                        member_generic_arguments:
                            GenericArguments::from_other_model(
                                equality.lhs.0.member_generic_arguments,
                            ),
                        parent_generic_arguments:
                            GenericArguments::from_other_model(
                                equality.lhs.0.parent_generic_arguments,
                            ),
                    }),
                    Type::from_other_model(equality.rhs),
                ))
            }
            Predicate::ConstantType(constant_type) => Self::ConstantType(
                ConstantType::from_other_model(constant_type),
            ),
            Predicate::LifetimeOutlives(outlives) => {
                Self::LifetimeOutlives(Outlives::new(
                    Lifetime::from_other_model(outlives.operand),
                    Lifetime::from_other_model(outlives.bound),
                ))
            }
            Predicate::TypeOutlives(outlives) => {
                Self::TypeOutlives(Outlives::new(
                    Type::from_other_model(outlives.operand),
                    Lifetime::from_other_model(outlives.bound),
                ))
            }
            Predicate::TupleType(tuple) => {
                Self::TupleType(Tuple(Type::from_other_model(tuple.0)))
            }
            Predicate::PositiveTrait(positive) => {
                Self::PositiveTrait(PositiveTrait::from_other_model(positive))
            }
            Predicate::NegativeTrait(negative) => {
                Self::NegativeTrait(NegativeTrait::from_other_model(negative))
            }
            Predicate::PositiveMarker(positive) => {
                Self::PositiveMarker(PositiveMarker::from_other_model(positive))
            }
            Predicate::NegativeMarker(negative) => {
                Self::NegativeMarker(NegativeMarker::from_other_model(negative))
            }
        }
    }

    /// Tries to convert a predicate with the model `U` into the model
    /// `M`.
    ///
    /// # Errors
    ///
    /// Returns an error returned by the `TryFrom` implementation of the model.
    pub fn try_from_other_model<U: Model, E>(
        predicate: Predicate<U>,
    ) -> Result<Self, E>
    where
        M::LifetimeInference: TryFrom<U::LifetimeInference, Error = E>,
        M::TypeInference: TryFrom<U::TypeInference, Error = E>,
        M::ConstantInference: TryFrom<U::ConstantInference, Error = E>,
    {
        Ok(match predicate {
            Predicate::TraitTypeCompatible(equality) => {
                Self::TraitTypeCompatible(Compatible::new(
                    TraitMember(MemberSymbol {
                        id: equality.lhs.0.id,
                        member_generic_arguments:
                            GenericArguments::try_from_other_model(
                                equality.lhs.0.member_generic_arguments,
                            )?,
                        parent_generic_arguments:
                            GenericArguments::try_from_other_model(
                                equality.lhs.0.parent_generic_arguments,
                            )?,
                    }),
                    Type::try_from_other_model(equality.rhs)?,
                ))
            }
            Predicate::ConstantType(constant_type) => Self::ConstantType(
                ConstantType::try_from_other_model(constant_type)?,
            ),
            Predicate::LifetimeOutlives(outlives) => {
                Self::LifetimeOutlives(Outlives::new(
                    Lifetime::try_from_other_model(outlives.operand)?,
                    Lifetime::try_from_other_model(outlives.bound)?,
                ))
            }
            Predicate::TypeOutlives(outlives) => {
                Self::TypeOutlives(Outlives::new(
                    Type::try_from_other_model(outlives.operand)?,
                    Lifetime::try_from_other_model(outlives.bound)?,
                ))
            }
            Predicate::TupleType(tuple) => {
                Self::TupleType(Tuple(Type::try_from_other_model(tuple.0)?))
            }
            Predicate::PositiveTrait(positive) => Self::PositiveTrait(
                PositiveTrait::try_from_other_model(positive)?,
            ),
            Predicate::NegativeTrait(negative) => Self::NegativeTrait(
                NegativeTrait::try_from_other_model(negative)?,
            ),
            Predicate::PositiveMarker(positive) => Self::PositiveMarker(
                PositiveMarker::try_from_other_model(positive)?,
            ),
            Predicate::NegativeMarker(negative) => Self::NegativeMarker(
                NegativeMarker::try_from_other_model(negative)?,
            ),
        })
    }
}

impl<M: Model> Compatible<TraitMember<M>, Type<M>> {
    /// Checks if the predicate has a `forall` lifetime.
    pub fn contains_error(&self) -> bool {
        let trait_member = Type::TraitMember(self.lhs.clone());

        contains_error(&trait_member) || contains_error(&self.rhs)
    }

    /// Applies the instantiation to both [`Equality::lhs`] and
    /// [`Equality::rhs`].
    pub fn instantiate(&mut self, instantiation: &Instantiation<M>) {
        self.lhs.0.member_generic_arguments.instantiate(instantiation);
        self.lhs.0.parent_generic_arguments.instantiate(instantiation);
        instantiation::instantiate(&mut self.rhs, instantiation);
    }
}
