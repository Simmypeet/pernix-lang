//! Contains the code related to resolving the correct [`Variance`] for each
//! generic parameters defined in the ADTs.

use pernixc_base::diagnostic::Handler;

use crate::{
    error::Error,
    semantic::{
        equality,
        session::{self, ExceedLimitError, Limit, Session},
        sub_term::Location,
        term::{
            constant::Constant,
            lifetime::Lifetime,
            r#type::{
                Qualifier, SubLifetimeLocation, SubTypeLocation, SymbolID, Type,
            },
            Term,
        },
        visitor::{self, SubTermLocation},
        Environment, Premise,
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
    R: Session<Lifetime> + Session<Type> + Session<Constant>,
> {
    target: &'a Term,
    locations: Result<Vec<Vec<SubTermLocation>>, ExceedLimitError>,

    environment: &'a Environment<'a, T>,
    limit: &'l mut Limit<'r, R>,
}

macro_rules! implements_visitor {
    ($first_term:ident, $second_term:ident) => {
        impl<
                'a,
                'l,
                'r,
                T: State,
                R: Session<Lifetime> + Session<Type> + Session<Constant>,
            > visitor::Recursive<$first_term>
            for TermCollector<'a, 'l, 'r, $second_term, T, R>
        {
            fn visit(
                &mut self,
                _: &$first_term,
                _: impl Iterator<Item = SubTermLocation>,
            ) -> bool {
                self.locations.is_ok()
            }
        }
    };
}

implements_visitor!(Lifetime, Type);
implements_visitor!(Lifetime, Constant);
implements_visitor!(Type, Lifetime);
implements_visitor!(Type, Constant);
implements_visitor!(Constant, Lifetime);
implements_visitor!(Constant, Type);

impl<
        'a,
        'l,
        'r,
        U: Term,
        T: State,
        R: Session<U> + Session<Lifetime> + Session<Type> + Session<Constant>,
    > visitor::Recursive<U> for TermCollector<'a, 'l, 'r, U, T, R>
{
    fn visit(
        &mut self,
        term: &U,
        locations: impl Iterator<Item = SubTermLocation>,
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
        R: Session<Lifetime> + Session<Type> + Session<Constant>,
    >(
        &self,
        respect_to_type: &Type,
        mut locations: Vec<SubTermLocation>,
    ) -> Result<Variance, ExceedLimitError>
    where
        for<'a, 'l, 'r> TermCollector<'a, 'l, 'r, U, T, R>: visitor::Recursive<Lifetime>
            + visitor::Recursive<Type>
            + visitor::Recursive<Constant>,
    {
        let this_location = if locations.is_empty() {
            return Ok(Variance::Bivariant);
        } else {
            locations.remove(0)
        };

        match this_location {
            SubTermLocation::Lifetime(
                visitor::SubLifetimeLocation::FromType(location),
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
                                .copied()
                                .unwrap())
                        }

                        // results None, we need to normalize the type
                        SymbolID::Type(_) => Ok(Variance::Bivariant),
                    }
                }

                // lifetime in the reference
                (SubLifetimeLocation::Reference, Type::Reference(_)) => {
                    assert!(locations.is_empty());

                    Ok(Variance::Covariant)
                }

                (location, ty) => unreachable!(
                    "mismatched location and type: {:?}, {:?}",
                    location, ty
                ),
            },

            SubTermLocation::Type(visitor::SubTypeLocation::FromType(
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

                                Ok(adt
                                    .generic_parameter_variances()
                                    .variances_by_type_ids
                                    .get(&id)
                                    .copied()
                                    .unwrap()
                                    .chain(inner_variance))
                            }

                            SymbolID::Type(_) => Ok(Variance::Bivariant),
                        }
                    }

                    (SubTypeLocation::Pointer, Type::Pointer(pointer)) => {
                        if pointer.qualifier == Qualifier::Mutable
                            || pointer.qualifier == Qualifier::Unique
                        {
                            Ok(Variance::Invariant)
                        } else {
                            Ok(self
                                .get_variance_for_locations(
                                    &pointer.pointee,
                                    locations,
                                )?
                                .chain(Variance::Covariant))
                        }
                    }

                    (
                        SubTypeLocation::Reference,
                        Type::Reference(reference),
                    ) => {
                        if reference.qualifier == Qualifier::Mutable
                            || reference.qualifier == Qualifier::Unique
                        {
                            Ok(Variance::Invariant)
                        } else {
                            Ok(self
                                .get_variance_for_locations(
                                    &reference.pointee,
                                    locations,
                                )?
                                .chain(Variance::Covariant))
                        }
                    }

                    (SubTypeLocation::Array, Type::Array(array)) => Ok(self
                        .get_variance_for_locations(&array.r#type, locations)?
                        .chain(Variance::Covariant)),

                    (
                        location @ SubTypeLocation::Tuple(_),
                        tuple @ Type::Tuple(_),
                    ) => {
                        let tuple = location.get_sub_term(tuple).unwrap();

                        Ok(self
                            .get_variance_for_locations(&tuple, locations)?
                            .chain(Variance::Covariant))
                    }

                    (SubTypeLocation::Local, Type::Local(local)) => Ok(self
                        .get_variance_for_locations(&local.0, locations)?
                        .chain(Variance::Covariant)),

                    (SubTypeLocation::Phantom, Type::Phantom(phantom)) => {
                        Ok(self
                            .get_variance_for_locations(&phantom.0, locations)?
                            .chain(Variance::Covariant))
                    }

                    (
                        SubTypeLocation::MemberSymbol(_),
                        Type::MemberSymbol(_),
                    ) => Ok(Variance::Bivariant),

                    (SubTypeLocation::TraitMember(_), Type::TraitMember(_)) => {
                        Ok(Variance::Invariant)
                    }

                    _ => unreachable!(),
                }
            }

            SubTermLocation::Constant(
                visitor::SubConstantLocation::FromConstant(_),
            ) => Ok(Variance::Invariant),

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
    R: Session<Lifetime> + Session<Type> + Session<Constant>,
>(
    term: &U,
    respect_to_type: &Type,
    environment: &Environment<T>,
    session: &mut Limit<R>,
) -> Result<Variance, ExceedLimitError>
where
    for<'a, 'l, 'r> TermCollector<'a, 'l, 'r, U, T, R>: visitor::Recursive<Lifetime>
        + visitor::Recursive<Type>
        + visitor::Recursive<Constant>,
{
    get_variance_for_internal(term, respect_to_type, true, environment, session)
        .map(|x| x.unwrap_or(Variance::Bivariant))
}

#[allow(clippy::too_many_arguments)]
fn get_variance_for_internal<
    U: Term,
    T: State,
    R: Session<Lifetime> + Session<Type> + Session<Constant>,
>(
    term: &U,
    respect_to_type: &Type,
    is_root: bool,
    environment: &Environment<T>,
    limit: &mut Limit<R>,
) -> Result<Option<Variance>, ExceedLimitError>
where
    for<'a, 'l, 'r> TermCollector<'a, 'l, 'r, U, T, R>: visitor::Recursive<Lifetime>
        + visitor::Recursive<Type>
        + visitor::Recursive<Constant>,
{
    let locations =
        get_all_term_locations(term, respect_to_type, environment, limit)?;

    let mut variance: Variance = Variance::Bivariant;
    for locations in locations {
        if locations.is_empty() && is_root {
            variance = variance.chain(Variance::Covariant);
            continue;
        }

        variance = variance.chain(
            environment
                .table
                .get_variance_for_locations(respect_to_type, locations)?,
        );
    }

    Ok(Some(variance))
}

impl<T: State> Table<T> {
    #[allow(clippy::needless_pass_by_value)]
    pub(super) fn build_variance<'a>(
        &self,
        generic_parameters: &GenericParameters,
        generic_parameter_variances: &mut GenericParameterVariances,
        active_premise: &Premise,
        generic_id: GenericID,
        type_usages: impl Iterator<Item = &'a Type> + Clone,
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
                    &Environment { premise: active_premise, table: self },
                    &mut Limit::new(&mut session),
                ) {
                    Ok(variance) => {
                        let current_variance = generic_parameter_variances
                            .variances_by_lifetime_ids
                            .get_mut(&id)
                            .unwrap();

                        *current_variance = current_variance.chain(variance);
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
                    &Environment { premise: active_premise, table: self },
                    &mut Limit::new(&mut session),
                ) {
                    Ok(variance) => {
                        let current_variance = generic_parameter_variances
                            .variances_by_type_ids
                            .get_mut(&id)
                            .unwrap();

                        *current_variance = current_variance.chain(variance);
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
    R: Session<Lifetime> + Session<Type> + Session<Constant>,
>(
    target_term: &Term,
    respect_to_type: &Type,
    environment: &Environment<T>,
    limit: &mut Limit<R>,
) -> Result<Vec<Vec<SubTermLocation>>, ExceedLimitError>
where
    for<'a, 'l, 'r> TermCollector<'a, 'l, 'r, Term, T, R>: visitor::Recursive<Lifetime>
        + visitor::Recursive<Type>
        + visitor::Recursive<Constant>,
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
