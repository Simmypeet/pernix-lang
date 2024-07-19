//! Contains the definition of [`Variance`]

use super::{
    model::Model,
    sub_term::{Location, SubLifetimeLocation, SubTypeLocation, TermLocation},
    term::r#type::{self, Type},
};
use crate::symbol::{
    table::{State, Table},
    AdtID,
};

/// An enumeration of either an invariant or covariant variance.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum GetVarianceError {
    /// The location points to an invalid location in the term.
    InvalidLocation,

    /// The term is a constant, the constant doesn't have a variance.
    Constant,

    /// The term is an alias of another term. Therefore, the variance cannot be
    /// determined.
    ///
    /// Should normalize the term to determine the variance.
    Undeterminable,

    /// Found an invalid ADT ID.
    InvalidAdtID(AdtID),

    /// The variance information is not available for the ADT.
    NoVarianceInfo(AdtID),
}

impl<M: Model> Type<M> {
    /// Retrieves the variance of the term at the given location.
    #[allow(clippy::too_many_lines)]
    pub fn get_variance_of(
        &self,
        table: &Table<impl State>,
        parent_variance: Variance,
        mut locations: impl Iterator<Item = TermLocation>,
    ) -> Result<Variance, GetVarianceError> {
        let Some(location) = locations.next() else {
            return Ok(parent_variance);
        };

        match location {
            TermLocation::Lifetime(location) => {
                let SubLifetimeLocation::FromType(location) = location;

                match (location, self) {
                    // lifetime in the adt
                    (
                        r#type::SubLifetimeLocation::Symbol(location),
                        Self::Symbol(symbol),
                    ) => {
                        let adt_id = match symbol.id {
                            r#type::SymbolID::Struct(id) => AdtID::Struct(id),
                            r#type::SymbolID::Enum(id) => AdtID::Enum(id),
                            r#type::SymbolID::Type(_) => {
                                return Err(GetVarianceError::Undeterminable)
                            }
                        };

                        let adt = table
                            .get_adt(adt_id)
                            .ok_or(GetVarianceError::InvalidAdtID(adt_id))?;

                        // there's no sub-term in the lifetime
                        if locations.next().is_some() {
                            return Err(GetVarianceError::InvalidLocation);
                        }

                        // gets the id based on the position
                        let id = adt
                            .generic_declaration()
                            .parameters
                            .lifetime_order()
                            .get(location.0)
                            .ok_or(GetVarianceError::InvalidLocation)?;

                        Ok(parent_variance.xfrom(
                            *adt.generic_parameter_variances()
                                .variances_by_lifetime_ids
                                .get(id)
                                .ok_or(GetVarianceError::NoVarianceInfo(
                                    adt_id,
                                ))?,
                        ))
                    }

                    // lifetime in the reference
                    (
                        r#type::SubLifetimeLocation::Reference,
                        Self::Reference(_),
                    ) => {
                        // there's no sub-term in the lifetime
                        if locations.next().is_some() {
                            return Err(GetVarianceError::InvalidLocation);
                        }

                        Ok(parent_variance.xfrom(Variance::Covariant))
                    }

                    // lifetime in the trait member
                    (
                        r#type::SubLifetimeLocation::TraitMember(location),
                        Self::TraitMember(term),
                    ) => {
                        // there's no sub-term in the lifetime
                        if locations.next().is_some() {
                            return Err(GetVarianceError::InvalidLocation);
                        }

                        let invalid = if location.0.from_parent {
                            location.0.index
                                >= term.parent_generic_arguments.lifetimes.len()
                        } else {
                            location.0.index
                                >= term.member_generic_arguments.lifetimes.len()
                        };

                        if invalid {
                            return Err(GetVarianceError::InvalidLocation);
                        }

                        Ok(parent_variance.xfrom(Variance::Invariant))
                    }

                    _ => Err(GetVarianceError::InvalidLocation),
                }
            }

            TermLocation::Type(location) => {
                let SubTypeLocation::FromType(location) = location;

                match (location, self) {
                    (
                        r#type::SubTypeLocation::Symbol(location),
                        Self::Symbol(symbol),
                    ) => {
                        let adt_id = match symbol.id {
                            r#type::SymbolID::Struct(id) => AdtID::Struct(id),
                            r#type::SymbolID::Enum(id) => AdtID::Enum(id),
                            r#type::SymbolID::Type(_) => {
                                return Err(GetVarianceError::Undeterminable)
                            }
                        };

                        let adt = table
                            .get_adt(adt_id)
                            .ok_or(GetVarianceError::InvalidAdtID(adt_id))?;

                        // gets the id based on the position
                        let id = adt
                            .generic_declaration()
                            .parameters
                            .type_order()
                            .get(location.0)
                            .ok_or(GetVarianceError::InvalidLocation)?;

                        let current_variance = parent_variance.xfrom(
                            adt.generic_parameter_variances()
                                .variances_by_type_ids
                                .get(id)
                                .copied()
                                .ok_or(GetVarianceError::NoVarianceInfo(
                                    adt_id,
                                ))?,
                        );

                        let inner_term = symbol
                            .generic_arguments
                            .types
                            .get(location.0)
                            .ok_or(GetVarianceError::InvalidLocation)?;

                        inner_term.get_variance_of(
                            table,
                            current_variance,
                            locations,
                        )
                    }

                    (
                        r#type::SubTypeLocation::Reference,
                        Self::Reference(reference),
                    ) => {
                        let current_variance =
                            parent_variance.xfrom(match reference.qualifier {
                                r#type::Qualifier::Immutable => {
                                    Variance::Covariant
                                }

                                r#type::Qualifier::Mutable
                                | r#type::Qualifier::Unique => {
                                    Variance::Invariant
                                }
                            });

                        reference.pointee.get_variance_of(
                            table,
                            current_variance,
                            locations,
                        )
                    }

                    (
                        r#type::SubTypeLocation::Pointer,
                        Self::Pointer(pointer),
                    ) => {
                        let current_variance =
                            parent_variance.xfrom(match pointer.qualifier {
                                r#type::Qualifier::Immutable => {
                                    Variance::Covariant
                                }

                                r#type::Qualifier::Mutable
                                | r#type::Qualifier::Unique => {
                                    Variance::Invariant
                                }
                            });

                        pointer.pointee.get_variance_of(
                            table,
                            current_variance,
                            locations,
                        )
                    }

                    (r#type::SubTypeLocation::Array, Self::Array(array)) => {
                        let current_variance =
                            parent_variance.xfrom(Variance::Covariant);

                        array.r#type.get_variance_of(
                            table,
                            current_variance,
                            locations,
                        )
                    }

                    (r#type::SubTypeLocation::Local, Self::Local(local)) => {
                        let current_variance =
                            parent_variance.xfrom(Variance::Covariant);

                        local.0.get_variance_of(
                            table,
                            current_variance,
                            locations,
                        )
                    }

                    (
                        r#type::SubTypeLocation::Phantom,
                        Self::Phantom(phantom),
                    ) => {
                        let current_variance =
                            parent_variance.xfrom(Variance::Covariant);

                        phantom.0.get_variance_of(
                            table,
                            current_variance,
                            locations,
                        )
                    }

                    (
                        location @ r#type::SubTypeLocation::Tuple(_),
                        tuple @ Self::Tuple(_),
                    ) => {
                        let sub_term = location
                            .get_sub_term(tuple)
                            .ok_or(GetVarianceError::InvalidLocation)?;

                        let current_variance =
                            parent_variance.xfrom(Variance::Covariant);

                        sub_term.get_variance_of(
                            table,
                            current_variance,
                            locations,
                        )
                    }

                    (
                        r#type::SubTypeLocation::MemberSymbol(_),
                        Self::MemberSymbol(_),
                    ) => Err(GetVarianceError::Undeterminable),

                    (
                        r#type::SubTypeLocation::TraitMember(location),
                        Self::TraitMember(symbol),
                    ) => {
                        let inner_term = if location.0.from_parent {
                            symbol
                                .parent_generic_arguments
                                .types
                                .get(location.0.index)
                        } else {
                            symbol
                                .member_generic_arguments
                                .types
                                .get(location.0.index)
                        }
                        .ok_or(GetVarianceError::InvalidLocation)?;

                        let current_variance =
                            parent_variance.xfrom(Variance::Invariant);

                        inner_term.get_variance_of(
                            table,
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
