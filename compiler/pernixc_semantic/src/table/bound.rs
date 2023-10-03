use super::{
    transformation::{self, SubstitutionError, Transformed},
    Table,
};
use crate::{
    constant,
    symbol::{
        AssociatedBounds, GenericItemRef, HigherRankedableLifetime, LifetimeBoundOperand,
        LocalSubstitution, Substitution, TraitRef, WhereClause,
    },
    ty::{self, ElidedLifetime},
};

/// Represents a lifetime bound checking.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct Lifetime {
    pub operand: LifetimeBoundOperand,
    pub bound: ty::Lifetime,
}

/// Reprsents a trait associated type bound.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct TraitAssociatedType {
    pub trait_associated_type: ty::TraitAssociated,
    pub bound: ty::Type,
}

/// Represents a trait associated constant bound.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct TraitAssociatedConstant {
    pub trait_associated_constant: constant::TraitAssociated,
    pub constant: constant::Constant,
}

/// Represents a trait bound.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct Trait {
    pub trait_ref: TraitRef,
    pub type_substituions: Vec<ty::Type>,
    pub lifetime_substituions: Vec<HigherRankedableLifetime>,
    pub constant_substituions: Vec<constant::Constant>,
    pub is_const: bool,
}

/// Represents a bound checking.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Bound {
    Lifetime(Lifetime),
    Trait(Trait),
    TraitAssociatedConstant(TraitAssociatedConstant),
    TraitAssociatedType(TraitAssociatedType),
}

/// After transformation applied, the trait associated type was resolved to a concrete type and
/// failed to match the bound.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FailedTraitAssociatedType {
    /// The trait associated type that was resolved.
    pub resolved_trait_associated_type: ty::Type,

    /// The bound that failed to match.
    pub bound: ty::Type,
}

/// After transformation applied, the trait associated constant was resolved to a concrete constant
/// and failed to match the bound.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FailedTraitAssociatedConstant {
    /// The trait associated constant that was resolved.
    pub resolved_trait_associated_constant: constant::Constant,

    /// The bound that failed to match.
    pub bound: constant::Constant,
}

/// Elided lifetime were found as a bound in the lifetime bound checking, which will always fail.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FailedLifetime {
    /// The lifetime bound that contains elided lifetime.
    pub operand: LifetimeBoundOperand,

    /// The elided lifetime that was found.
    pub elided_lifetime: ty::ElidedLifetime,
}

/// Represents an early failed bound checking.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Failed {
    TraitAssociatedType(FailedTraitAssociatedType),
    TraitAssociatedConstant(FailedTraitAssociatedConstant),
    Lifetime(FailedLifetime),
    ElidedLifetimeOperand(ElidedLifetime),
    ElidedLifetimeOnType(ty::Type),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error)]
#[allow(missing_docs)]
pub enum Error {
    #[error("found early failed bounds")]
    Failed(Vec<Failed>),

    #[error("an error occurred while substituting the generic item to resolve for bound checking")]
    SubstitutionError(#[from] SubstitutionError),

    #[error("invalid input was found passed to the function")]
    InvalidInput,
}

impl Table {
    /// Gets the list of bound checking needed for the given generic item reference and
    /// substitution.
    ///
    /// # Errors
    ///
    /// Returns list of [`Failed`] bounds if found any early failed bound checking.
    #[allow(clippy::too_many_lines)]
    pub fn get_bounds(
        &self,
        generic_item_ref: GenericItemRef,
        local_substitution: &LocalSubstitution,
        active_associated_bounds: &AssociatedBounds,
    ) -> Result<Vec<Bound>, Error> {
        let mut bounds = Vec::new();
        let mut faileds = Vec::new();
        let substitution = Substitution::from_local(local_substitution, generic_item_ref);

        let generic_item = self
            .get_generic_item(generic_item_ref)
            .ok_or(Error::InvalidInput)?;
        let where_clause = generic_item.where_clause();

        for (operand, bound) in &where_clause.associated_bounds.associated_type_bounds {
            let Transformed {
                transformed: transformed_operand,
                resolved_implements: resolved_implements_operand,
            } = self.transform_type(
                ty::Type::TraitAssociated(operand.clone()),
                &substitution,
                active_associated_bounds,
            )?;

            let Transformed {
                transformed: transformed_bound,
                resolved_implements: resolved_implements_bound,
            } = self.transform_type(bound.clone(), &substitution, active_associated_bounds)?;

            // recursively collect additional bounds introduced by implements
            for resolved_implements in resolved_implements_operand
                .into_iter()
                .chain(resolved_implements_bound.into_iter())
            {
                let additional_bounds = self.get_bounds(
                    GenericItemRef::Implements(resolved_implements.implements_ref),
                    &resolved_implements.deduced_substituion,
                    active_associated_bounds,
                )?;

                for additional_bound in additional_bounds {
                    bounds.push(additional_bound);
                }
            }

            match transformed_operand {
                ty::Type::TraitAssociated(trait_associated) => {
                    bounds.push(Bound::TraitAssociatedType(TraitAssociatedType {
                        trait_associated_type: trait_associated,
                        bound: transformed_bound,
                    }));
                }
                transformed_operand => {
                    if transformed_operand != transformed_bound {
                        faileds.push(Failed::TraitAssociatedType(FailedTraitAssociatedType {
                            resolved_trait_associated_type: transformed_operand,
                            bound: transformed_bound,
                        }));
                    }
                }
            }
        }

        for (operand, bound) in &where_clause.associated_bounds.associated_constant_bounds {
            let Transformed {
                transformed: transformed_operand,
                resolved_implements: resolved_implements_operand,
            } = self.transform_constant(
                constant::Constant::TraitAssociated(operand.clone()),
                &substitution,
                active_associated_bounds,
            )?;

            let Transformed {
                transformed: transformed_bound,
                resolved_implements: resolved_implements_bound,
            } = self.transform_constant(bound.clone(), &substitution, active_associated_bounds)?;

            // recursively collect additional bounds introduced by implements
            for resolved_implements in resolved_implements_operand
                .into_iter()
                .chain(resolved_implements_bound.into_iter())
            {
                let additional_bounds = self.get_bounds(
                    GenericItemRef::Implements(resolved_implements.implements_ref),
                    &resolved_implements.deduced_substituion,
                    active_associated_bounds,
                )?;

                for additional_bound in additional_bounds {
                    bounds.push(additional_bound);
                }
            }

            match transformed_operand {
                constant::Constant::TraitAssociated(trait_associated) => {
                    bounds.push(Bound::TraitAssociatedConstant(TraitAssociatedConstant {
                        trait_associated_constant: trait_associated,
                        constant: transformed_bound,
                    }));
                }
                transformed_operand => {
                    if transformed_operand != transformed_bound {
                        faileds.push(Failed::TraitAssociatedConstant(
                            FailedTraitAssociatedConstant {
                                resolved_trait_associated_constant: transformed_operand,
                                bound: transformed_bound,
                            },
                        ));
                    }
                }
            }
        }

        todo!();

        if faileds.is_empty() {
            Ok(bounds)
        } else {
            Err(Error::Failed(faileds))
        }
    }
}
