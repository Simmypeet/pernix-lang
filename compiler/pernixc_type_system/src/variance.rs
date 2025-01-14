//! Contains the function [`Environment::get_variance_of()`]

use enum_as_inner::EnumAsInner;
use pernixc_component::variance_map::VarianceMap;
use pernixc_table::{component::SymbolKind, GlobalID};
use pernixc_term::{
    generic_parameter::GenericParameters,
    r#type::{self, Type},
    sub_term::{Location, SubLifetimeLocation, SubTypeLocation, TermLocation},
    variance::{self, Variance},
    Model,
};

use crate::{environment::Environment, normalizer::Normalizer};

/// An enumeration of errors that can occur when calling
/// [`Type::get_variance_of()`]
#[derive(Debug, EnumAsInner, thiserror::Error)]
#[allow(missing_docs)]
pub enum GetVarianceError {
    #[error("the location points to an invalid location in the term.")]
    InvalidLocation,

    #[error("the term is a constant, the constant doesn't have a variance.")]
    Constant,

    #[error(transparent)]
    TableQuery(#[from] pernixc_table::query::Error),

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
    ///
    /// # Errors
    ///
    /// See [`GetVarianceError`] for more information.
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
                                .query::<GenericParameters>(symbol.id)?
                                .lifetime_order()
                                .get(location.0)
                                .copied()
                                .ok_or(GetVarianceError::InvalidLocation)?;

                            Ok(parent_variance.xfrom(
                                self.table()
                                    .query::<VarianceMap>(symbol.id)?
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
                                .query::<GenericParameters>(symbol.id)?
                                .type_order()
                                .get(location.0)
                                .copied()
                                .ok_or(GetVarianceError::InvalidLocation)?;

                            let next_variance = parent_variance.xfrom(
                                self.table()
                                    .query::<VarianceMap>(symbol.id)?
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
