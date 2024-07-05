use std::{collections::HashSet, sync::Arc};

use super::{
    model::Model,
    normalizer::Normalizer,
    predicate::Outlives,
    query::Context,
    sub_term::{SubTypeLocation, TermLocation},
    term::{
        constant::Constant,
        lifetime::Lifetime,
        r#type::{self, Type},
        MemberSymbol, Symbol,
    },
    unification::{self, Matching, Unification},
    Compute, Environment, LifetimeConstraint, Output, OverflowError, Satisfied,
    Succeeded,
};
use crate::{
    semantic::sub_term::{Location, SubLifetimeLocation},
    symbol::{
        table::{State, Table},
        AdtID, Variance,
    },
};

// TODO: Maybe move the `get_variance_of` to a separate module

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum GetVarianceError {
    /// The location points to an invalid location in the term.
    InvalidLocation,

    /// The iterator of locations is exhausted.
    EmptyLocationIterator,

    /// The term is a constant, the constant doesn't have a variance.
    Constant,

    /// The term is an alias of another term. Therefore, the variance cannot be
    /// determined.
    ///
    /// Should normalize the term to determine the variance.
    Undeterminable,

    /// Found an invalid ADT ID.
    InvalidAdtID(AdtID),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct LifetimeMatching<M: Model> {
    from: Type<M>,
}

impl<M: Model> unification::Predicate<Lifetime<M>> for LifetimeMatching<M> {
    fn unifiable(
        &self,
        from: &Lifetime<M>,
        to: &Lifetime<M>,
        from_logs: &Vec<unification::Log<M>>,
        _: &Vec<unification::Log<M>>,
    ) -> Result<Output<Satisfied, M>, OverflowError> {
        if from == to {
            return Ok(Some(Succeeded::satisfied()));
        }

        let mut current_from = self.from.clone();

        for log in from_logs {
            match log {
                unification::Log::Substructural(location) => match location {
                    TermLocation::Lifetime(lifetime) => {
                        let SubLifetimeLocation::FromType(location) = lifetime;

                        // shouldn't be from the type alias
                        if matches!(
                            (&current_from, location),
                            (
                                Type::Symbol(Symbol {
                                    id: r#type::SymbolID::Type(_),
                                    ..
                                }),
                                r#type::SubLifetimeLocation::Symbol(_)
                            ) | (
                                Type::MemberSymbol(MemberSymbol { .. }),
                                r#type::SubLifetimeLocation::MemberSymbol(_)
                            )
                        ) {
                            return Ok(None);
                        }
                    }
                    TermLocation::Type(location) => {
                        let SubTypeLocation::FromType(location) = location;

                        // shouldn't be from the type alias
                        if matches!(
                            (&current_from, location),
                            (
                                Type::Symbol(Symbol {
                                    id: r#type::SymbolID::Type(_),
                                    ..
                                }),
                                r#type::SubTypeLocation::Symbol(_)
                            ) | (
                                Type::MemberSymbol(MemberSymbol { .. }),
                                r#type::SubTypeLocation::MemberSymbol(_)
                            )
                        ) {
                            return Ok(None);
                        }

                        current_from =
                            location.get_sub_term(&current_from).unwrap();
                    }
                    TermLocation::Constant(_) => {
                        unreachable!("lifetime shouldn't come from constant")
                    }
                },

                unification::Log::RewrittenLifetime(_) => {
                    unreachable!("lifetime shouldn't be rewritten")
                }

                unification::Log::RewrittenType(rewritten) => {
                    current_from = rewritten.clone();
                }

                unification::Log::RewrittenConstant(_) => {
                    unreachable!("Lifetime shouldn't be in the constant")
                }
            }
        }

        Ok(Some(Succeeded::satisfied()))
    }
}

impl<M: Model> unification::Predicate<Type<M>> for LifetimeMatching<M> {
    fn unifiable(
        &self,
        _: &Type<M>,
        _: &Type<M>,
        _: &Vec<unification::Log<M>>,
        _: &Vec<unification::Log<M>>,
    ) -> Result<Output<Satisfied, M>, OverflowError> {
        Ok(None)
    }
}

impl<M: Model> unification::Predicate<Constant<M>> for LifetimeMatching<M> {
    fn unifiable(
        &self,
        _: &Constant<M>,
        _: &Constant<M>,
        _: &Vec<unification::Log<M>>,
        _: &Vec<unification::Log<M>>,
    ) -> Result<Output<Satisfied, M>, OverflowError> {
        Ok(None)
    }
}

impl<M: Model> Type<M> {
    #[allow(clippy::too_many_lines)]
    fn get_variance_of(
        &self,
        table: &Table<impl State>,
        mut locations: impl Iterator<Item = TermLocation>,
    ) -> Result<Variance, GetVarianceError> {
        const fn combine_variance_result(
            variance: Variance,
            result: Result<Variance, GetVarianceError>,
        ) -> Result<Variance, GetVarianceError> {
            match result {
                Ok(inner_variance) => Ok(variance.chain(inner_variance)),
                Err(GetVarianceError::EmptyLocationIterator) => Ok(variance),
                Err(error) => Err(error),
            }
        }

        let Some(location) = locations.next() else {
            return Err(GetVarianceError::EmptyLocationIterator);
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

                        Ok(*adt
                            .generic_parameter_variances()
                            .variances_by_lifetime_ids
                            .get(id)
                            .unwrap())
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

                        Ok(Variance::Covariant)
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

                        let current_variance = adt
                            .generic_parameter_variances()
                            .variances_by_type_ids
                            .get(id)
                            .unwrap();

                        let inner_term = symbol
                            .generic_arguments
                            .types
                            .get(location.0)
                            .ok_or(GetVarianceError::InvalidLocation)?;

                        combine_variance_result(
                            *current_variance,
                            inner_term.get_variance_of(table, locations),
                        )
                    }

                    (
                        r#type::SubTypeLocation::Reference,
                        Self::Reference(reference),
                    ) => {
                        let current_variance = match reference.qualifier {
                            r#type::Qualifier::Immutable => Variance::Covariant,

                            r#type::Qualifier::Mutable
                            | r#type::Qualifier::Unique => Variance::Invariant,
                        };

                        let inner_variance =
                            reference.pointee.get_variance_of(table, locations);

                        combine_variance_result(
                            current_variance,
                            inner_variance,
                        )
                    }

                    (
                        r#type::SubTypeLocation::Pointer,
                        Self::Pointer(pointer),
                    ) => {
                        let current_variance = match pointer.qualifier {
                            r#type::Qualifier::Immutable => Variance::Covariant,

                            r#type::Qualifier::Mutable
                            | r#type::Qualifier::Unique => Variance::Invariant,
                        };

                        let inner_variance =
                            pointer.pointee.get_variance_of(table, locations);

                        combine_variance_result(
                            current_variance,
                            inner_variance,
                        )
                    }

                    (r#type::SubTypeLocation::Array, Self::Array(array)) => {
                        let inner_variance =
                            array.r#type.get_variance_of(table, locations);

                        combine_variance_result(
                            Variance::Covariant,
                            inner_variance,
                        )
                    }

                    (r#type::SubTypeLocation::Local, Self::Local(local)) => {
                        let inner_variance =
                            local.0.get_variance_of(table, locations);

                        combine_variance_result(
                            Variance::Covariant,
                            inner_variance,
                        )
                    }

                    (
                        r#type::SubTypeLocation::Phantom,
                        Self::Phantom(phantom),
                    ) => {
                        let inner_variance =
                            phantom.0.get_variance_of(table, locations);

                        combine_variance_result(
                            Variance::Covariant,
                            inner_variance,
                        )
                    }

                    (
                        location @ r#type::SubTypeLocation::Tuple(_),
                        tuple @ Self::Tuple(_),
                    ) => {
                        let tuple = location
                            .get_sub_term(tuple)
                            .ok_or(GetVarianceError::InvalidLocation)?;

                        let inner_variance =
                            tuple.get_variance_of(table, locations);

                        combine_variance_result(
                            Variance::Covariant,
                            inner_variance,
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

                        let inner_variance =
                            inner_term.get_variance_of(table, locations);

                        combine_variance_result(
                            Variance::Covariant,
                            inner_variance,
                        )
                    }

                    _ => Err(GetVarianceError::InvalidLocation),
                }
            }

            TermLocation::Constant(_) => Err(GetVarianceError::Constant),
        }
    }
}

pub(super) fn compatible_with_context<M: Model>(
    term: &Type<M>,
    target: &Type<M>,
    variance: Variance,
    environment: &Environment<M, impl State, impl Normalizer<M>>,
    context: &mut Context<M>,
) -> Result<Output<Satisfied, M>, OverflowError> {
    let Some(Succeeded { result: unification, mut constraints }) =
        Unification::new(
            term.clone(),
            target.clone(),
            Arc::new(LifetimeMatching { from: term.clone() }),
        )
        .query_with_context(environment, context)?
    else {
        return Ok(None);
    };

    if !append_outlives_constraints_from_unification(
        term.clone(),
        unification,
        variance,
        environment.table,
        &mut constraints,
    ) {
        return Ok(None);
    }

    Ok(Some(Succeeded::with_constraints(Satisfied, constraints)))
}

#[must_use]
fn append_outlives_constraints_from_unification<M: Model>(
    mut current_from: Type<M>,
    unifier: unification::Unifier<Type<M>>,
    current_variance: Variance,
    table: &Table<impl State>,
    outlives: &mut HashSet<LifetimeConstraint<M>>,
) -> bool {
    if let Some(rewritten_from) = unifier.rewritten_from {
        current_from = rewritten_from;
    }

    match unifier.matching {
        unification::Matching::Unifiable(_, _) => {
            unreachable!("the can never be unified")
        }
        unification::Matching::Substructural(substructural) => {
            // look for matched lifetimes
            for (location, unification) in substructural.lifetimes {
                match current_from.get_variance_of(
                    table,
                    std::iter::once(TermLocation::Lifetime(
                        SubLifetimeLocation::FromType(location),
                    )),
                ) {
                    Ok(variance) => match variance {
                        Variance::Bivariant => { /*no need to add constraint*/ }
                        Variance::Covariant => {
                            // 'from: 'to
                            if let Matching::Unifiable(from, to) =
                                unification.matching
                            {
                                outlives.insert(
                                    LifetimeConstraint::LifetimeOutlives(
                                        Outlives { operand: from, bound: to },
                                    ),
                                );
                            }
                        }
                        Variance::Contravariant => {
                            // 'to: 'from
                            if let Matching::Unifiable(from, to) =
                                unification.matching
                            {
                                outlives.insert(
                                    LifetimeConstraint::LifetimeOutlives(
                                        Outlives { operand: to, bound: from },
                                    ),
                                );
                            }
                        }
                        Variance::Invariant => {
                            // 'from: 'to
                            // 'to: 'from
                            if let Matching::Unifiable(from, to) =
                                unification.matching
                            {
                                outlives.insert(
                                    LifetimeConstraint::LifetimeMatching(
                                        from.clone(),
                                        to.clone(),
                                    ),
                                );
                            }
                        }
                    },

                    Err(_) => {
                        // the variance cannot be determined, flawed term input
                        return false;
                    }
                }
            }

            // look for matched types
            for (location, unification) in substructural.types {
                let new_variance = match current_from.get_variance_of(
                    table,
                    std::iter::once(TermLocation::Type(
                        SubTypeLocation::FromType(location),
                    )),
                ) {
                    Ok(variance) => current_variance.chain(variance),
                    Err(_) => return false,
                };

                let new_from = location.get_sub_term(&current_from).unwrap();

                if !append_outlives_constraints_from_unification(
                    new_from,
                    unification,
                    new_variance,
                    table,
                    outlives,
                ) {
                    return false;
                }
            }
        }
        unification::Matching::Equality => {}
    }

    true
}

/// Checks if the `term` is compatible with the `target` type.
///
/// This is similar to the equality check, but it allows the lifetimes to be
/// variant. The variance of the lifetimes is determined by the variance of the
/// type that contains the lifetime.
///
/// The result is `Satisfied` if the `term` is compatible with the `target` with
/// the list of outlives constraints.
///
/// # Parameters
///
/// - `term`: The term to be checked.
/// - `target`: The target type to be checked against.
/// - `variance`: The variance to used for determining the constraint of the
///   lifetimes. For the most cases, the default should be
///   [`Variance::Bivariant`]
pub fn compatible<M: Model>(
    term: &Type<M>,
    target: &Type<M>,
    variance: Variance,
    environment: &Environment<M, impl State, impl Normalizer<M>>,
) -> Result<Output<Satisfied, M>, OverflowError> {
    compatible_with_context(
        term,
        target,
        variance,
        environment,
        &mut Context::new(),
    )
}
