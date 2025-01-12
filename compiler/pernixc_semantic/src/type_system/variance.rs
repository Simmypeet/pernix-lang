//! Contains the definition of [`Variance`]

use enum_as_inner::EnumAsInner;
use pernixc_table::{
    component::SymbolKind,
    query::{CyclicDependency, Error as QueryError},
    GlobalID,
};
use serde::{Deserialize, Serialize};

use super::{
    environment::Environment,
    model::Model,
    normalizer::Normalizer,
    sub_term::{Location, SubLifetimeLocation, SubTypeLocation, TermLocation},
    term::r#type::{self, Type},
};
use crate::component::{
    generic_parameters::GenericParameters, variance::GenericParameterVariances,
};

/// An enumeration of either an invariant or covariant variance.
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
pub enum Variance {
    /// The term is covariant and can be changed to a subtype.
    ///
    /// This is the most common variance; that is, the lifetime can be changed
    /// to a smaller lifetime e.g. `'static` to `'a`.
    Covariant,

    /// The term is contravariant and can be changed to a supertype.
    ///
    /// This is the opposite of convariant; that is, the lifetime can be
    /// changed to a larger lifetime e.g. `'a` to `'static`. This is
    /// generally used in the parameter of a function i.e. the function
    /// parameter used to accept a particular lifetime `'a` can be changed
    /// to accept a larger lifetime `'static`.
    Contravariant,

    /// The term is invariant and cannot be changed.
    #[default]
    Invariant,
}

impl Variance {
    /// Combines the two variances in terms of the variance of the parent and
    /// the variance of the child.
    ///
    /// The [`self`] is the variance of the parent and the [`child`] is the
    /// variance of the inner type.
    ///
    /// For example, to find the variance of `T` in `&'a T`, the variance of
    /// `&'a` is [`self`] and the variance of `T` is [`child`].
    #[must_use]
    pub const fn xfrom(self, child: Self) -> Self {
        match (self, child) {
            (Self::Invariant, _) | (_, Self::Invariant) => Self::Invariant,
            (amb, Self::Covariant) => amb,
            (Self::Covariant, Self::Contravariant) => Self::Contravariant,
            (Self::Contravariant, Self::Contravariant) => Self::Covariant,
        }
    }

    /// Combines the two variances in terms of the variance from different
    /// locations to find the most general variance.
    #[must_use]
    pub const fn combine(self, other: Self) -> Self {
        match (self, other) {
            (Self::Invariant, _) | (_, Self::Invariant) => Self::Invariant,

            (Self::Covariant, Self::Contravariant)
            | (Self::Contravariant, Self::Covariant) => Self::Invariant,

            (Self::Covariant, Self::Covariant) => Self::Covariant,
            (Self::Contravariant, Self::Contravariant) => Self::Contravariant,
        }
    }
}

/// An enumeration of errors that can occur when calling
/// [`Type::get_variance_of()`]
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumAsInner,
    thiserror::Error,
)]
#[allow(missing_docs)]
pub enum GetVarianceError {
    #[error("the location points to an invalid location in the term.")]
    InvalidLocation,

    #[error("the term is a constant, the constant doesn't have a variance.")]
    Constant,

    #[error(transparent)]
    CyclicDependency(#[from] CyclicDependency),

    #[error(
        "the symbol id is not found in the table or its kind is not one of \
         the expected kinds."
    )]
    InvalidSymbolID(GlobalID),
}

impl<'a, M: Model, N: Normalizer<M>> Environment<'a, M, N> {
    /// Retrieves the variance of the term at the given location.
    ///
    /// This function early returns the `parent_variance` if the variance is
    /// [`Variance::Invariant`] or if the location is empty.
    #[allow(clippy::too_many_lines)]
    pub fn get_variance_of(
        &self,
        ty: &Type<M>,
        parent_variance: Variance,
        mut locations: impl Iterator<Item = TermLocation>,
    ) -> Result<Variance, GetVarianceError> {
        let Some(location) = locations.next() else {
            return Ok(parent_variance);
        };

        // early return if the parent variance is invariant
        if parent_variance == Variance::Invariant {
            return Ok(Variance::Invariant);
        }

        match location {
            TermLocation::Lifetime(location) => {
                let SubLifetimeLocation::FromType(location) = location;

                match (location, ty) {
                    // lifetime in the adt
                    (
                        r#type::SubLifetimeLocation::Symbol(location),
                        Type::Symbol(symbol),
                    ) => {
                        // there's no sub-term in the lifetime
                        if locations.next().is_some() {
                            return Err(GetVarianceError::InvalidLocation);
                        }

                        let kind = self
                            .table()
                            .get::<SymbolKind>(symbol.id)
                            .as_deref()
                            .copied()
                            .ok_or(GetVarianceError::InvalidSymbolID(
                                symbol.id,
                            ))?;

                        if kind.is_adt() {
                            let id = self
                                .table()
                                .query::<GenericParameters>(symbol.id)
                                .map_err(QueryError::unwrap_cyclic_dependency)?
                                .lifetime_order()
                                .get(location.0)
                                .copied()
                                .ok_or(GetVarianceError::InvalidLocation)?;

                            Ok(parent_variance.xfrom(
                                self.table
                                    .query::<GenericParameterVariances>(
                                        symbol.id,
                                    )
                                    .map_err(
                                        QueryError::unwrap_cyclic_dependency,
                                    )?
                                    .variances_by_lifetime_ids
                                    .get(&id)
                                    .copied()
                                    .unwrap(),
                            ))
                        } else if kind == SymbolKind::Function {
                            Ok(parent_variance.xfrom(Variance::Invariant))
                        } else {
                            return Err(GetVarianceError::InvalidSymbolID(
                                symbol.id,
                            ));
                        }
                    }

                    // lifetime in the member function and trait member
                    (
                        r#type::SubLifetimeLocation::MemberSymbol(location),
                        Type::MemberSymbol(member_symbol),
                    ) => {
                        // there's no sub-term in the lifetime
                        if locations.next().is_some() {
                            return Err(GetVarianceError::InvalidLocation);
                        }

                        let invalid = if location.from_parent {
                            location.index
                                >= member_symbol
                                    .parent_generic_arguments
                                    .lifetimes
                                    .len()
                        } else {
                            location.index
                                >= member_symbol
                                    .member_generic_arguments
                                    .lifetimes
                                    .len()
                        };

                        if invalid {
                            return Err(GetVarianceError::InvalidLocation);
                        }

                        Ok(parent_variance.xfrom(Variance::Invariant))
                    }

                    // lifetime in the reference
                    (
                        r#type::SubLifetimeLocation::Reference,
                        Type::Reference(_),
                    ) => {
                        // there's no sub-term in the lifetime
                        if locations.next().is_some() {
                            return Err(GetVarianceError::InvalidLocation);
                        }

                        Ok(parent_variance.xfrom(Variance::Covariant))
                    }

                    _ => Err(GetVarianceError::InvalidLocation),
                }
            }

            TermLocation::Type(location) => {
                let SubTypeLocation::FromType(location) = location;

                match (location, ty) {
                    (
                        r#type::SubTypeLocation::Symbol(location),
                        Type::Symbol(symbol),
                    ) => {
                        let kind = self
                            .table()
                            .get::<SymbolKind>(symbol.id)
                            .as_deref()
                            .copied()
                            .ok_or(GetVarianceError::InvalidSymbolID(
                                symbol.id,
                            ))?;

                        if kind.is_adt() {
                            let id = self
                                .table()
                                .query::<GenericParameters>(symbol.id)
                                .map_err(QueryError::unwrap_cyclic_dependency)?
                                .type_order()
                                .get(location.0)
                                .copied()
                                .ok_or(GetVarianceError::InvalidLocation)?;

                            let next_variance = parent_variance.xfrom(
                                self.table
                                    .query::<GenericParameterVariances>(
                                        symbol.id,
                                    )
                                    .map_err(
                                        QueryError::unwrap_cyclic_dependency,
                                    )?
                                    .variances_by_type_ids
                                    .get(&id)
                                    .copied()
                                    .unwrap(),
                            );

                            let inner_term = symbol
                                .generic_arguments
                                .types
                                .get(location.0)
                                .ok_or(GetVarianceError::InvalidLocation)?;

                            self.get_variance_of(
                                inner_term,
                                next_variance,
                                locations,
                            )
                        } else if kind == SymbolKind::Function {
                            Ok(parent_variance.xfrom(Variance::Invariant))
                        } else {
                            return Err(GetVarianceError::InvalidSymbolID(
                                symbol.id,
                            ));
                        }
                    }

                    (
                        r#type::SubTypeLocation::Reference,
                        Type::Reference(reference),
                    ) => {
                        let current_variance =
                            parent_variance.xfrom(match reference.qualifier {
                                r#type::Qualifier::Immutable => {
                                    Variance::Covariant
                                }

                                r#type::Qualifier::Mutable => {
                                    Variance::Invariant
                                }
                            });

                        self.get_variance_of(
                            &reference.pointee,
                            current_variance,
                            locations,
                        )
                    }

                    (
                        r#type::SubTypeLocation::Pointer,
                        Type::Pointer(pointer),
                    ) => {
                        let current_variance =
                            parent_variance.xfrom(if pointer.mutable {
                                Variance::Invariant
                            } else {
                                Variance::Covariant
                            });

                        self.get_variance_of(
                            &pointer.pointee,
                            current_variance,
                            locations,
                        )
                    }

                    (r#type::SubTypeLocation::Array, Type::Array(array)) => {
                        let current_variance =
                            parent_variance.xfrom(Variance::Covariant);

                        self.get_variance_of(
                            &array.r#type,
                            current_variance,
                            locations,
                        )
                    }

                    (
                        r#type::SubTypeLocation::Phantom,
                        Type::Phantom(phantom),
                    ) => {
                        let current_variance =
                            parent_variance.xfrom(Variance::Covariant);

                        self.get_variance_of(
                            &phantom.0,
                            current_variance,
                            locations,
                        )
                    }

                    (
                        location @ r#type::SubTypeLocation::Tuple(_),
                        tuple @ Type::Tuple(_),
                    ) => {
                        let sub_term = location
                            .get_sub_term(tuple)
                            .ok_or(GetVarianceError::InvalidLocation)?;

                        let current_variance =
                            parent_variance.xfrom(Variance::Covariant);

                        self.get_variance_of(
                            &sub_term,
                            current_variance,
                            locations,
                        )
                    }

                    (
                        r#type::SubTypeLocation::MemberSymbol(location),
                        Type::MemberSymbol(symbol),
                    ) => {
                        let inner_term = if location.from_parent {
                            symbol
                                .parent_generic_arguments
                                .types
                                .get(location.index)
                        } else {
                            symbol
                                .member_generic_arguments
                                .types
                                .get(location.index)
                        }
                        .ok_or(GetVarianceError::InvalidLocation)?;

                        let current_variance =
                            parent_variance.xfrom(Variance::Invariant);

                        self.get_variance_of(
                            inner_term,
                            current_variance,
                            locations,
                        )
                    }

                    _ => Err(GetVarianceError::InvalidLocation),
                }
            }

            TermLocation::Constant(_) => Err(GetVarianceError::Constant),
        }
    }
}

impl<M: Model> Type<M> {}
