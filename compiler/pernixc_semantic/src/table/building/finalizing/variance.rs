use std::collections::HashSet;

use pernixc_base::diagnostic::Handler;

use crate::{
    error::Error,
    semantic::{
        self, equality,
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
        Premise, Semantic,
    },
    symbol::{
        AdtID, GenericID, GenericParameterVariances, GenericParameters,
        LifetimeParameterID, TypeParameterID, Variance,
    },
    table::{State, Table},
};

struct TermCollector<
    'a,
    't,
    'p,
    's,
    'l,
    'r,
    Term,
    T: State,
    S: Semantic<Lifetime> + Semantic<Type> + Semantic<Constant>,
    R: Session<Lifetime> + Session<Type> + Session<Constant>,
> {
    target: &'a Term,
    locations: Result<Vec<Vec<SubTermLocation>>, ExceedLimitError>,

    table: &'t Table<T>,
    premise: &'p Premise,
    semantic: &'s mut S,
    session: &'l mut Limit<'r, R>,
}

macro_rules! implements_visitor {
    ($first_term:ident, $second_term:ident) => {
        impl<
                'a,
                't,
                'p,
                's,
                'l,
                'r,
                T: State,
                S: Semantic<Lifetime> + Semantic<Type> + Semantic<Constant>,
                R: Session<Lifetime> + Session<Type> + Session<Constant>,
            > visitor::Recursive<$first_term>
            for TermCollector<'a, 't, 'p, 's, 'l, 'r, $second_term, T, S, R>
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
        't,
        'p,
        's,
        'l,
        'r,
        U: Term,
        T: State,
        S: Semantic<U> + Semantic<Lifetime> + Semantic<Type> + Semantic<Constant>,
        R: Session<U> + Session<Lifetime> + Session<Type> + Session<Constant>,
    > visitor::Recursive<U>
    for TermCollector<'a, 't, 'p, 's, 'l, 'r, U, T, S, R>
{
    fn visit(
        &mut self,
        term: &U,
        locations: impl Iterator<Item = SubTermLocation>,
    ) -> bool {
        let Ok(locations_list) = &mut self.locations else {
            return false;
        };

        match equality::equals(
            term,
            self.target,
            self.premise,
            self.table,
            self.semantic,
            self.session,
        ) {
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

fn get_all_term_locations<
    Term: visitor::Element,
    T: State,
    S: Semantic<Lifetime> + Semantic<Type> + Semantic<Constant>,
    R: Session<Lifetime> + Session<Type> + Session<Constant>,
>(
    target_term: &Term,
    respect_to_type: &Type,
    table: &Table<T>,
    premise: &Premise,
    semantic: &mut S,
    session: &mut Limit<R>,
) -> Result<Vec<Vec<SubTermLocation>>, ExceedLimitError>
where
    for<'a, 't, 'p, 's, 'l, 'r> TermCollector<'a, 't, 'p, 's, 'l, 'r, Term, T, S, R>:
        visitor::Recursive<Lifetime>
            + visitor::Recursive<Type>
            + visitor::Recursive<Constant>,
{
    let mut collector = TermCollector {
        target: target_term,
        locations: Ok(Vec::new()),

        table,
        premise,
        semantic,
        session,
    };

    visitor::accept_recursive(respect_to_type, &mut collector);

    collector.locations
}

impl<T: State> Table<T> {
    #[allow(clippy::too_many_lines, clippy::too_many_arguments)]
    fn get_variance_for_locations<
        U: Term,
        S: Semantic<Lifetime> + Semantic<Type> + Semantic<Constant>,
        R: Session<Lifetime> + Session<Type> + Session<Constant>,
    >(
        &self,
        term: &U,
        respect_to_type: &Type,
        mut locations: Vec<SubTermLocation>,
        premise: &Premise,
        semantic: &mut S,
        session: &mut Limit<R>,
        visited_terms: &mut HashSet<Type>,
        is_root: bool,
    ) -> Result<Variance, ExceedLimitError>
    where
        for<'a, 't, 'p, 's, 'l, 'r> TermCollector<'a, 't, 'p, 's, 'l, 'r, U, T, S, R>:
            visitor::Recursive<Lifetime>
                + visitor::Recursive<Type>
                + visitor::Recursive<Constant>,
    {
        let this_location = if locations.is_empty() {
            return Ok(Variance::Bivariant);
        } else {
            locations.remove(0)
        };

        let mut result = match this_location {
            SubTermLocation::Lifetime(
                visitor::SubLifetimeLocation::FromType(location),
            ) => match (location, respect_to_type) {
                // lifetime in the symbol kind
                (
                    SubLifetimeLocation::Symbol(location),
                    Type::Symbol(symbol),
                ) => match symbol.id {
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

                        Some(
                            adt.generic_parameter_variances()
                                .variances_by_lifetime_ids
                                .get(&id)
                                .copied()
                                .unwrap(),
                        )
                    }

                    // results None, we need to normalize the type
                    SymbolID::Type(_) => None,
                },

                // lifetime in the reference
                (SubLifetimeLocation::Reference, Type::Reference(_)) => {
                    assert!(locations.is_empty());

                    Some(Variance::Covariant)
                }

                (location, ty) => unreachable!(
                    "mismatched location and type: {:?}, {:?}",
                    location, ty
                ),
            },

            SubTermLocation::Type(visitor::SubTypeLocation::FromType(
                location,
            )) => match (location, respect_to_type) {
                (SubTypeLocation::Symbol(location), Type::Symbol(symbol)) => {
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
                                    term,
                                    &symbol.generic_arguments.types[location.0],
                                    locations,
                                    premise,
                                    semantic,
                                    session,
                                    visited_terms,
                                    false,
                                )?;

                            dbg!(
                                inner_variance,
                                adt.generic_parameter_variances(),
                                id
                            );

                            Some(
                                adt.generic_parameter_variances()
                                    .variances_by_type_ids
                                    .get(&id)
                                    .copied()
                                    .unwrap()
                                    .chain(inner_variance),
                            )
                        }

                        // results None, we need to normalize the type
                        SymbolID::Type(_) => None,
                    }
                }

                (SubTypeLocation::Pointer, Type::Pointer(pointer)) => {
                    if pointer.qualifier == Qualifier::Mutable
                        || pointer.qualifier == Qualifier::Restrict
                    {
                        Some(Variance::Invariant)
                    } else {
                        Some(
                            self.get_variance_for_locations(
                                term,
                                &pointer.pointee,
                                locations,
                                premise,
                                semantic,
                                session,
                                visited_terms,
                                false,
                            )?
                            .chain(Variance::Covariant),
                        )
                    }
                }

                (SubTypeLocation::Reference, Type::Reference(reference)) => {
                    if reference.qualifier == Qualifier::Mutable
                        || reference.qualifier == Qualifier::Restrict
                    {
                        Some(Variance::Invariant)
                    } else {
                        Some(
                            self.get_variance_for_locations(
                                term,
                                &reference.pointee,
                                locations,
                                premise,
                                semantic,
                                session,
                                visited_terms,
                                false,
                            )?
                            .chain(Variance::Covariant),
                        )
                    }
                }

                (SubTypeLocation::Array, Type::Array(array)) => Some(
                    self.get_variance_for_locations(
                        term,
                        &array.r#type,
                        locations,
                        premise,
                        semantic,
                        session,
                        visited_terms,
                        false,
                    )?
                    .chain(Variance::Covariant),
                ),

                (
                    location @ SubTypeLocation::Tuple(_),
                    tuple @ Type::Tuple(_),
                ) => {
                    let tuple = location.get_sub_term(tuple).unwrap();

                    Some(
                        self.get_variance_for_locations(
                            term,
                            &tuple,
                            locations,
                            premise,
                            semantic,
                            session,
                            visited_terms,
                            false,
                        )?
                        .chain(Variance::Covariant),
                    )
                }

                (SubTypeLocation::Local, Type::Local(local)) => Some(
                    self.get_variance_for_locations(
                        term,
                        &local.0,
                        locations,
                        premise,
                        semantic,
                        session,
                        visited_terms,
                        false,
                    )?
                    .chain(Variance::Covariant),
                ),

                (SubTypeLocation::Phantom, Type::Phantom(phantom)) => Some(
                    self.get_variance_for_locations(
                        term,
                        &phantom.0,
                        locations,
                        premise,
                        semantic,
                        session,
                        visited_terms,
                        false,
                    )?
                    .chain(Variance::Covariant),
                ),

                (SubTypeLocation::MemberSymbol(_), Type::MemberSymbol(_)) => {
                    None
                }

                (SubTypeLocation::TraitMember(_), Type::TraitMember(_)) => {
                    Some(Variance::Invariant)
                }

                _ => unreachable!(),
            },

            SubTermLocation::Constant(
                visitor::SubConstantLocation::FromConstant(_),
            ) => Some(Variance::Invariant),

            _ => unreachable!(),
        };

        // if none or invariant, try to normalize the type
        result = match result {
            result @ (Some(Variance::Invariant) | None) => {
                if let Some(normalized_type) = semantic.normalize(
                    respect_to_type,
                    premise,
                    self,
                    session,
                )? {
                    self.get_variance_for_internal(
                        term,
                        &normalized_type,
                        premise,
                        semantic,
                        session,
                        visited_terms,
                        is_root,
                    )?
                } else {
                    result
                }
            }

            Some(result) => {
                return Ok(result);
            }
        };

        // if none or invariant, try to look for equivalences
        match result {
            result @ (Some(Variance::Invariant) | None) => {
                for (key, values) in &premise.equalities_mapping.types {
                    if !equality::equals(
                        key,
                        respect_to_type,
                        premise,
                        self,
                        semantic,
                        session,
                    )? {
                        continue;
                    }

                    match self.get_variance_for_internal(
                        term,
                        respect_to_type,
                        premise,
                        semantic,
                        session,
                        visited_terms,
                        is_root,
                    )? {
                        Some(
                            result @ (Variance::Contravariant
                            | Variance::Bivariant
                            | Variance::Covariant),
                        ) => {
                            return Ok(result);
                        }
                        None | Some(Variance::Invariant) => {}
                    }

                    for value in values {
                        match self.get_variance_for_internal(
                            term,
                            value,
                            premise,
                            semantic,
                            session,
                            visited_terms,
                            is_root,
                        )? {
                            Some(
                                result @ (Variance::Contravariant
                                | Variance::Bivariant
                                | Variance::Covariant),
                            ) => {
                                return Ok(result);
                            }
                            None | Some(Variance::Invariant) => {}
                        }
                    }
                }

                Ok(result.unwrap_or(Variance::Bivariant))
            }

            Some(result) => Ok(result),
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
        S: Semantic<Lifetime> + Semantic<Type> + Semantic<Constant>,
        R: Session<Lifetime> + Session<Type> + Session<Constant>,
    >(
        &self,
        term: &U,
        respect_to_type: &Type,
        premise: &Premise,
        semantic: &mut S,
        session: &mut Limit<R>,
    ) -> Result<Variance, ExceedLimitError>
    where
        for<'a, 't, 'p, 's, 'l, 'r> TermCollector<'a, 't, 'p, 's, 'l, 'r, U, T, S, R>:
            visitor::Recursive<Lifetime>
                + visitor::Recursive<Type>
                + visitor::Recursive<Constant>,
    {
        self.get_variance_for_internal(
            term,
            respect_to_type,
            premise,
            semantic,
            session,
            &mut HashSet::new(),
            true,
        )
        .map(|x| x.unwrap_or(Variance::Bivariant))
    }

    #[allow(clippy::too_many_arguments)]
    fn get_variance_for_internal<
        U: Term,
        S: Semantic<Lifetime> + Semantic<Type> + Semantic<Constant>,
        R: Session<Lifetime> + Session<Type> + Session<Constant>,
    >(
        &self,
        term: &U,
        respect_to_type: &Type,
        premise: &Premise,
        semantic: &mut S,
        session: &mut Limit<R>,
        visited_terms: &mut HashSet<Type>,
        is_root: bool,
    ) -> Result<Option<Variance>, ExceedLimitError>
    where
        for<'a, 't, 'p, 's, 'l, 'r> TermCollector<'a, 't, 'p, 's, 'l, 'r, U, T, S, R>:
            visitor::Recursive<Lifetime>
                + visitor::Recursive<Type>
                + visitor::Recursive<Constant>,
    {
        if !visited_terms.insert(respect_to_type.clone()) {
            return Ok(None);
        }

        let locations = get_all_term_locations(
            term,
            respect_to_type,
            self,
            premise,
            semantic,
            session,
        )?;
        dbg!(&locations, respect_to_type, term, is_root);

        let mut variance: Variance = Variance::Bivariant;
        for locations in locations {
            if locations.is_empty() && is_root {
                variance = variance.chain(Variance::Covariant);
                continue;
            }

            variance = variance.chain(self.get_variance_for_locations(
                term,
                respect_to_type,
                locations,
                premise,
                semantic,
                session,
                visited_terms,
                is_root,
            )?);
        }

        assert!(visited_terms.remove(respect_to_type));
        Ok(Some(variance))
    }

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
        let mut semantic = semantic::Default;
        let mut session = session::Default::default();

        for (id, _) in generic_parameters.lifetime_parameters_as_order() {
            let lifetime_term = Lifetime::Parameter(LifetimeParameterID {
                parent: generic_id,
                id,
            });

            for ty in type_usages.clone() {
                match self.get_variance_for(
                    &lifetime_term,
                    ty,
                    active_premise,
                    &mut semantic,
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
                match self.get_variance_for(
                    &type_term,
                    ty,
                    active_premise,
                    &mut semantic,
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
