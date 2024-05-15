//! Contains the code related to resolving the correct [`Variance`] for each
//! generic parameters defined in the ADTs.

use pernixc_base::diagnostic::Handler;

use crate::{
    error::Error,
    semantic::{
        equality,
        model::Default,
        normalizer::NoOp,
        session::{self, ExceedLimitError, Limit, Session},
        sub_term::{self, Location, TermLocation},
        term::{
            constant::Constant,
            lifetime::Lifetime,
            r#type::{
                Qualifier, SubLifetimeLocation, SubTypeLocation, SymbolID, Type,
            },
            Term,
        },
        visitor, Environment, Premise,
    },
    symbol::{
        AdtID, GenericID, GenericParameterVariances, GenericParameters,
        LifetimeParameterID, TypeParameterID, Variance,
    },
    table::{State, Table},
};

struct TermCollector<
    'a,
    'l,
    'r,
    Term,
    T: State,
    R: Session<Lifetime<Default>>
        + Session<Type<Default>>
        + Session<Constant<Default>>,
> {
    target: &'a Term,
    locations: Result<Vec<Vec<TermLocation>>, ExceedLimitError>,

    environment: &'a Environment<'a, Default, T, NoOp>,
    limit: &'l mut Limit<'r, R>,
}

macro_rules! implements_visitor {
    ($first_term:ty, $second_term:ty) => {
        impl<
                'a,
                'l,
                'r,
                'v,
                T: State,
                R: Session<Lifetime<Default>>
                    + Session<Type<Default>>
                    + Session<Constant<Default>>,
            > visitor::Recursive<'v, $first_term>
            for TermCollector<'a, 'l, 'r, $second_term, T, R>
        {
            fn visit(
                &mut self,
                _: &'v $first_term,
                _: impl Iterator<Item = TermLocation>,
            ) -> bool {
                self.locations.is_ok()
            }
        }
    };
}

implements_visitor!(Lifetime<Default>, Type<Default>);
implements_visitor!(Lifetime<Default>, Constant<Default>);
implements_visitor!(Type<Default>, Lifetime<Default>);
implements_visitor!(Type<Default>, Constant<Default>);
implements_visitor!(Constant<Default>, Lifetime<Default>);
implements_visitor!(Constant<Default>, Type<Default>);

impl<
        'a,
        'l,
        'r,
        'v,
        U: Term<Model = Default>,
        T: State,
        R: Session<U>
            + Session<Lifetime<Default>>
            + Session<Type<Default>>
            + Session<Constant<Default>>,
    > visitor::Recursive<'v, U> for TermCollector<'a, 'l, 'r, U, T, R>
{
    fn visit(
        &mut self,
        term: &U,
        locations: impl Iterator<Item = TermLocation>,
    ) -> bool {
        let Ok(locations_list) = &mut self.locations else {
            return false;
        };

        match equality::equals(term, self.target, self.environment, self.limit)
        {
            Ok(ok) => {
                if ok {
                    locations_list.push(locations.collect());
                }

                true
            }

            Err(error) => {
                self.locations = Err(error);
                false
            }
        }
    }
}

impl<S: State> Table<S> {
    #[allow(clippy::too_many_lines, clippy::too_many_arguments)]
    fn get_variance_for_locations<
        U: Term,
        T: State,
        R: Session<Lifetime<Default>>
            + Session<Type<Default>>
            + Session<Constant<Default>>,
    >(
        &self,
        respect_to_type: &Type<Default>,
        mut locations: Vec<TermLocation>,
    ) -> Result<Option<Variance>, ExceedLimitError>
    where
        for<'a, 'l, 'r, 'v> TermCollector<'a, 'l, 'r, U, T, R>:
            visitor::Recursive<'v, Lifetime<Default>>
                + visitor::Recursive<'v, Type<Default>>
                + visitor::Recursive<'v, Constant<Default>>,
    {
        let this_location = if locations.is_empty() {
            return Ok(Some(Variance::Bivariant));
        } else {
            locations.remove(0)
        };

        match this_location {
            TermLocation::Lifetime(
                sub_term::SubLifetimeLocation::FromType(location),
            ) => match (location, respect_to_type) {
                // lifetime in the symbol kind
                (
                    SubLifetimeLocation::Symbol(location),
                    Type::Symbol(symbol),
                ) => {
                    match symbol.id {
                        id @ (SymbolID::Struct(_) | SymbolID::Enum(_)) => {
                            let adt_id = match id {
                                SymbolID::Struct(id) => AdtID::Struct(id),
                                SymbolID::Enum(id) => AdtID::Enum(id),
                                SymbolID::Type(_) => unreachable!(),
                            };

                            assert!(locations.is_empty());

                            let adt = self.get_adt(adt_id).unwrap();
                            // gets the id based on the position
                            let id = adt
                                .generic_declaration()
                                .parameters
                                .lifetime_order()[location.0];

                            Ok(adt
                                .generic_parameter_variances()
                                .variances_by_lifetime_ids
                                .get(&id)
                                .copied())
                        }

                        // results None, we need to normalize the type
                        SymbolID::Type(_) => Ok(None),
                    }
                }

                // lifetime in the reference
                (SubLifetimeLocation::Reference, Type::Reference(_)) => {
                    assert!(locations.is_empty());

                    Ok(Some(Variance::Covariant))
                }

                (location, ty) => unreachable!(
                    "mismatched location and type: {:?}, {:?}",
                    location, ty
                ),
            },

            TermLocation::Type(sub_term::SubTypeLocation::FromType(
                location,
            )) => {
                match (location, respect_to_type) {
                    (
                        SubTypeLocation::Symbol(location),
                        Type::Symbol(symbol),
                    ) => {
                        match symbol.id {
                            id @ (SymbolID::Struct(_) | SymbolID::Enum(_)) => {
                                let adt_id = match id {
                                    SymbolID::Struct(id) => AdtID::Struct(id),
                                    SymbolID::Enum(id) => AdtID::Enum(id),
                                    SymbolID::Type(_) => unreachable!(),
                                };

                                let adt = self.get_adt(adt_id).unwrap();
                                // gets the id based on the position
                                let id = adt
                                    .generic_declaration()
                                    .parameters
                                    .type_order()[location.0];

                                let inner_variance = self
                                    .get_variance_for_locations(
                                        &symbol.generic_arguments.types
                                            [location.0],
                                        locations,
                                    )?;

                                Ok(
                                    match (
                                        inner_variance,
                                        adt.generic_parameter_variances()
                                            .variances_by_type_ids
                                            .get(&id)
                                            .copied(),
                                    ) {
                                        (Some(first), Some(second)) => {
                                            Some(first.chain(second))
                                        }

                                        (Some(variance), None)
                                        | (None, Some(variance)) => {
                                            Some(variance)
                                        }

                                        (None, None) => None,
                                    },
                                )
                            }

                            SymbolID::Type(_) => Ok(None),
                        }
                    }

                    (SubTypeLocation::Pointer, Type::Pointer(pointer)) => {
                        if pointer.qualifier == Qualifier::Mutable
                            || pointer.qualifier == Qualifier::Unique
                        {
                            Ok(Some(Variance::Invariant))
                        } else {
                            Ok(Some(
                                self.get_variance_for_locations(
                                    &pointer.pointee,
                                    locations,
                                )?
                                .map_or(
                                    Variance::Covariant,
                                    |variance| {
                                        variance.chain(Variance::Covariant)
                                    },
                                ),
                            ))
                        }
                    }

                    (
                        SubTypeLocation::Reference,
                        Type::Reference(reference),
                    ) => {
                        if reference.qualifier == Qualifier::Mutable
                            || reference.qualifier == Qualifier::Unique
                        {
                            Ok(Some(Variance::Invariant))
                        } else {
                            Ok(Some(
                                self.get_variance_for_locations(
                                    &reference.pointee,
                                    locations,
                                )?
                                .map_or(
                                    Variance::Covariant,
                                    |variance| {
                                        variance.chain(Variance::Covariant)
                                    },
                                ),
                            ))
                        }
                    }

                    (SubTypeLocation::Array, Type::Array(array)) => Ok(Some(
                        self.get_variance_for_locations(
                            &array.r#type,
                            locations,
                        )?
                        .map_or(Variance::Covariant, |variance| {
                            variance.chain(Variance::Covariant)
                        }),
                    )),

                    (
                        location @ SubTypeLocation::Tuple(_),
                        tuple @ Type::Tuple(_),
                    ) => {
                        let tuple = location.get_sub_term(tuple).unwrap();

                        Ok(Some(
                            self.get_variance_for_locations(&tuple, locations)?
                                .map_or(Variance::Covariant, |variance| {
                                    variance.chain(Variance::Covariant)
                                }),
                        ))
                    }

                    (SubTypeLocation::Local, Type::Local(local)) => Ok(Some(
                        self.get_variance_for_locations(&local.0, locations)?
                            .map_or(Variance::Covariant, |variance| {
                                variance.chain(Variance::Covariant)
                            }),
                    )),

                    (SubTypeLocation::Phantom, Type::Phantom(phantom)) => {
                        Ok(Some(
                            self.get_variance_for_locations(
                                &phantom.0, locations,
                            )?
                            .map_or(Variance::Covariant, |variance| {
                                variance.chain(Variance::Covariant)
                            }),
                        ))
                    }

                    (
                        SubTypeLocation::MemberSymbol(_),
                        Type::MemberSymbol(_),
                    ) => Ok(None),

                    (SubTypeLocation::TraitMember(_), Type::TraitMember(_)) => {
                        Ok(Some(Variance::Invariant))
                    }

                    _ => unreachable!(),
                }
            }

            TermLocation::Constant(
                sub_term::SubConstantLocation::FromConstant(_),
            ) => Ok(Some(Variance::Invariant)),

            _ => unreachable!(),
        }
    }
}

/// Gets the variance constraint of a particular term with respect to the
/// given type.
///
/// # Errors
///
/// See [`ExceedLimitError`] for more information.
#[allow(private_bounds)]
pub(super) fn get_variance_for<
    U: Term,
    T: State,
    R: Session<Lifetime<Default>>
        + Session<Type<Default>>
        + Session<Constant<Default>>,
>(
    term: &U,
    respect_to_type: &Type<Default>,
    environment: &Environment<Default, T, NoOp>,
    session: &mut Limit<R>,
) -> Result<Option<Variance>, ExceedLimitError>
where
    for<'a, 'l, 'r, 'v> TermCollector<'a, 'l, 'r, U, T, R>: visitor::Recursive<'v, Lifetime<Default>>
        + visitor::Recursive<'v, Type<Default>>
        + visitor::Recursive<'v, Constant<Default>>,
{
    get_variance_for_internal(term, respect_to_type, true, environment, session)
}

#[allow(clippy::too_many_arguments)]
fn get_variance_for_internal<
    U: Term,
    T: State,
    R: Session<Lifetime<Default>>
        + Session<Type<Default>>
        + Session<Constant<Default>>,
>(
    term: &U,
    respect_to_type: &Type<Default>,
    is_root: bool,
    environment: &Environment<Default, T, NoOp>,
    limit: &mut Limit<R>,
) -> Result<Option<Variance>, ExceedLimitError>
where
    for<'a, 'l, 'r, 'v> TermCollector<'a, 'l, 'r, U, T, R>: visitor::Recursive<'v, Lifetime<Default>>
        + visitor::Recursive<'v, Type<Default>>
        + visitor::Recursive<'v, Constant<Default>>,
{
    let locations =
        get_all_term_locations(term, respect_to_type, environment, limit)?;

    let mut variance: Option<Variance> = None;

    for locations in locations {
        if locations.is_empty() && is_root {
            match &mut variance {
                Some(variance) => {
                    *variance = variance.chain(Variance::Covariant);
                }
                None => variance = Some(Variance::Covariant),
            }
            continue;
        }

        let new_variance = environment
            .table
            .get_variance_for_locations(respect_to_type, locations)?;

        variance = match (variance, new_variance) {
            (None, Some(variance)) | (Some(variance), None) => Some(variance),
            (Some(first), Some(second)) => Some(first.chain(second)),
            (None, None) => None,
        };
    }

    Ok(variance)
}

impl<T: State> Table<T> {
    #[allow(clippy::needless_pass_by_value, clippy::too_many_arguments)]
    pub(super) fn build_variance<'a>(
        &self,
        generic_parameters: &GenericParameters,
        generic_parameter_variances: &mut GenericParameterVariances,
        active_premise: &Premise<Default>,
        generic_id: GenericID,
        type_usages: impl Iterator<Item = &'a Type<Default>> + Clone,
        partial_variance: bool,
        _: &dyn Handler<Box<dyn Error>>,
    ) {
        let mut session = session::Default::default();

        for (id, _) in generic_parameters.lifetime_parameters_as_order() {
            let lifetime_term = Lifetime::Parameter(LifetimeParameterID {
                parent: generic_id,
                id,
            });

            for ty in type_usages.clone() {
                match get_variance_for(
                    &lifetime_term,
                    ty,
                    &Environment {
                        premise: active_premise,
                        table: self,
                        normalizer: &NoOp,
                    },
                    &mut Limit::new(&mut session),
                ) {
                    Ok(variance) => {
                        match (
                            generic_parameter_variances
                                .variances_by_lifetime_ids
                                .get_mut(&id),
                            variance,
                        ) {
                            (None, None) => {
                                if !partial_variance {
                                    assert!(generic_parameter_variances
                                        .variances_by_lifetime_ids
                                        .insert(id, Variance::Bivariant)
                                        .is_none());
                                }
                            }
                            (None, Some(variance)) => {
                                assert!(generic_parameter_variances
                                    .variances_by_lifetime_ids
                                    .insert(id, variance)
                                    .is_none());
                            }
                            (Some(current), None) => {
                                if !partial_variance {
                                    *current =
                                        current.chain(Variance::Bivariant);
                                }
                            }
                            (Some(current), Some(variance)) => {
                                *current = current.chain(variance);
                            }
                        }
                    }
                    Err(_) => todo!("report undecdiable variance"),
                }
            }
        }

        for (id, _) in generic_parameters.type_parameters_as_order() {
            let type_term =
                Type::Parameter(TypeParameterID { parent: generic_id, id });

            for ty in type_usages.clone() {
                match get_variance_for(
                    &type_term,
                    ty,
                    &Environment {
                        premise: active_premise,
                        table: self,
                        normalizer: &NoOp,
                    },
                    &mut Limit::new(&mut session),
                ) {
                    Ok(variance) => {
                        match (
                            generic_parameter_variances
                                .variances_by_type_ids
                                .get_mut(&id),
                            variance,
                        ) {
                            (None, None) => {
                                if !partial_variance {
                                    assert!(generic_parameter_variances
                                        .variances_by_type_ids
                                        .insert(id, Variance::Bivariant)
                                        .is_none());
                                }
                            }
                            (None, Some(variance)) => {
                                assert!(generic_parameter_variances
                                    .variances_by_type_ids
                                    .insert(id, variance)
                                    .is_none());
                            }
                            (Some(current), None) => {
                                if !partial_variance {
                                    *current =
                                        current.chain(Variance::Bivariant);
                                }
                            }
                            (Some(current), Some(variance)) => {
                                *current = current.chain(variance);
                            }
                        }
                    }
                    Err(_) => todo!("report undecdiable variance"),
                }
            }
        }
    }
}

fn get_all_term_locations<
    Term: visitor::Element,
    T: State,
    R: Session<Lifetime<Default>>
        + Session<Type<Default>>
        + Session<Constant<Default>>,
>(
    target_term: &Term,
    respect_to_type: &Type<Default>,
    environment: &Environment<Default, T, NoOp>,
    limit: &mut Limit<R>,
) -> Result<Vec<Vec<TermLocation>>, ExceedLimitError>
where
    for<'a, 'l, 'r, 'v> TermCollector<'a, 'l, 'r, Term, T, R>:
        visitor::Recursive<'v, Lifetime<Default>>
            + visitor::Recursive<'v, Type<Default>>
            + visitor::Recursive<'v, Constant<Default>>,
{
    let mut collector = TermCollector {
        target: target_term,
        locations: Ok(Vec::new()),

        limit,
        environment,
    };

    visitor::accept_recursive(respect_to_type, &mut collector);

    collector.locations
}

#[cfg(test)]
mod tests;
