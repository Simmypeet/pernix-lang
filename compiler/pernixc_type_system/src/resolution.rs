//! Contains the logic for resolving implementation for traits and markers.

use std::collections::BTreeSet;

use pernixc_component::{
    implementation::Implementation as ImplementationComponent,
    where_clause::WhereClause,
};
use pernixc_table::{
    component::{Implemented, Implements, SymbolKind, TraitImplementation},
    query::CyclicDependency,
    GlobalID,
};
use pernixc_term::{
    generic_arguments::GenericArguments, instantiation::Instantiation,
    predicate::Predicate, r#type::Type, variance::Variance, Default, Model,
};
use thiserror::Error;

use crate::{
    deduction::{self, Deduction},
    environment::Environment,
    normalizer::Normalizer,
    order::Order,
    AbruptError, LifetimeConstraint, ResultExt, Succeeded,
};

/// A result of a implementation resolution query.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Implementation<M: Model> {
    /// The deduced substitution for the generic arguments of the trait
    /// implementation.
    pub instantiation: Instantiation<M>,

    /// The ID of the resolved implementation.
    pub id: GlobalID,

    /// If `true`, the implementation is not general enough to accomodate the
    /// forall lifetime requirements.
    pub is_not_general_enough: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Error)]
#[allow(missing_docs)]
pub enum Error {
    #[error("the `implemented_id` is invalid to the table")]
    InvalidID,
    #[error(
        "the generic arguments contained a term that can be rewritten in \
         multiple ways and caused an ambiguity in resolution"
    )]
    Ambiguous,
    #[error("no matching implementation was found")]
    NotFound,
    #[error(transparent)]
    Abrupt(#[from] AbruptError),
    #[error("the given `implemented_id` is not a trait or marker")]
    NotTraitOrMarker,
}

impl From<CyclicDependency> for Error {
    fn from(value: CyclicDependency) -> Self {
        Self::Abrupt(AbruptError::CyclicDependency(value))
    }
}

impl<M: Model, N: Normalizer<M>> Environment<'_, M, N> {
    /// Resolves the implementation for the given `implemented_id` and
    /// `generic_arguments`.
    ///
    /// # Errors
    ///
    /// See [`Error`] for more information.
    #[allow(clippy::too_many_lines)]
    pub fn resolve_implementation(
        &self,
        implemented_id: GlobalID,
        generic_arguments: &GenericArguments<M>,
    ) -> Result<Succeeded<Implementation<M>, M>, Error> {
        let symbol_kind = *self
            .table()
            .get::<SymbolKind>(implemented_id)
            .ok_or(Error::InvalidID)?;

        // check that it must be trait or marker
        if !matches!(symbol_kind, SymbolKind::Trait | SymbolKind::Marker) {
            return Err(Error::NotTraitOrMarker);
        }

        // we might be in the implementation site already
        if let Some(result) = is_in_active_implementation(
            implemented_id,
            symbol_kind,
            generic_arguments,
            self,
        )? {
            return Ok(result);
        }

        let definite = self.generic_arguments_definite(generic_arguments)?;

        let default_environment =
            Environment::<M, _>::with_default(self.table());

        // the current candidate
        #[allow(clippy::type_complexity)]
        let mut candidate: Option<(
            GlobalID,
            Deduction<M>,
            BTreeSet<LifetimeConstraint<M>>,
            GenericArguments<M>,
        )> = None;

        let implementations = self
            .table()
            .get::<Implemented>(implemented_id)
            .ok_or(Error::InvalidID)?
            .iter()
            .copied()
            .collect::<Vec<_>>();

        for current_impl_id in implementations {
            let Some(implementation_generic_arguments) = self
                .table()
                .query::<ImplementationComponent>(current_impl_id)
                .extract_cyclic_dependency()?
                .map(|x| {
                    GenericArguments::from_default_model(
                        x.generic_arguments.clone(),
                    )
                })
            else {
                continue;
            };

            // build the unification
            let Succeeded {
                result: deduction,
                constraints: lifetime_constraints,
            } = match self
                .deduce(&implementation_generic_arguments, generic_arguments)
            {
                Ok(unification) => unification,

                Err(deduction::Error::Abrupt(error)) => {
                    return Err(error.into())
                }

                Err(
                    deduction::Error::MismatchedGenericArgumentCount(_)
                    | deduction::Error::UnificationFailure(_),
                ) => continue,
            };
            let is_final = match symbol_kind {
                SymbolKind::Trait => {
                    let Some(is_final) = self
                        .table()
                        .get::<TraitImplementation>(current_impl_id)
                        .map(|x| x.is_final)
                    else {
                        continue;
                    };

                    is_final
                }

                // every marker's implementaions are final
                SymbolKind::Marker => true,

                _ => unreachable!(),
            };

            if !is_final {
                // the implementation is not final, therefore, it requires
                // generic arguments to be definite
                if definite.is_none() {
                    continue;
                }

                // all predicates must satisfy to continue
                let Some(where_clause) = self
                    .table()
                    .query::<WhereClause>(current_impl_id)
                    .extract_cyclic_dependency()?
                else {
                    continue;
                };

                if !predicate_satisfies(
                    where_clause.predicates.iter().map(|x| &x.predicate),
                    &deduction.instantiation,
                    self,
                )? {
                    continue;
                }
            }

            // compare with the current candidate
            match &mut candidate {
                Some((
                    candidate_id,
                    candidate_instantiation,
                    candidate_lifetime_constraints,
                    candidate_generic_arguments,
                )) => {
                    // check which one is more specific
                    match default_environment.order(
                        &implementation_generic_arguments,
                        candidate_generic_arguments,
                    )? {
                        Order::Ambiguous | Order::Incompatible => {
                            return Err(Error::Ambiguous)
                        }
                        Order::MoreGeneral => {}
                        Order::MoreSpecific => {
                            *candidate_id = current_impl_id;
                            *candidate_instantiation = deduction;
                            *candidate_lifetime_constraints =
                                lifetime_constraints;
                            *candidate_generic_arguments =
                                implementation_generic_arguments;
                        }
                    }
                }

                candidate @ None => {
                    *candidate = Some((
                        current_impl_id,
                        deduction,
                        lifetime_constraints,
                        implementation_generic_arguments,
                    ));
                }
            }
        }

        match candidate {
            Some((
                implementation_id,
                deduction,
                mut lifetime_constraints,
                _,
            )) => Ok(Succeeded {
                result: Implementation {
                    instantiation: deduction.instantiation,
                    id: implementation_id,
                    is_not_general_enough: deduction.is_not_general_enough,
                },
                constraints: {
                    lifetime_constraints.extend(
                        definite.into_iter().flat_map(|x| x.constraints),
                    );
                    lifetime_constraints
                },
            }),
            None => Err(Error::NotFound),
        }
    }
}

#[allow(clippy::type_complexity)]
fn is_in_active_implementation<M: Model>(
    implemented_id: GlobalID,
    implemented_kind: SymbolKind, /* either trait or marker */
    generic_arguments: &GenericArguments<M>,
    environment: &Environment<M, impl Normalizer<M>>,
) -> Result<Option<Succeeded<Implementation<M>, M>>, Error> {
    let Some(query_site) = environment.premise().query_site else {
        return Ok(None);
    };

    for current_id in environment.table().scope_walker(query_site) {
        let current_id = GlobalID::new(query_site.target_id, current_id);
        let Some(current_kind) =
            environment.table().get::<SymbolKind>(current_id).map(|x| *x)
        else {
            continue;
        };

        // must be the implementation kind
        match implemented_kind {
            SymbolKind::Trait => {
                if !matches!(
                    current_kind,
                    SymbolKind::PositiveTraitImplementation
                        | SymbolKind::NegativeTraitImplementation
                ) {
                    continue;
                }
            }

            SymbolKind::Marker => {
                if !matches!(
                    current_kind,
                    SymbolKind::PositiveMarkerImplementation
                        | SymbolKind::NegativeMarkerImplementation
                ) {
                    continue;
                }
            }

            _ => unreachable!(),
        }

        // must be an implementation
        if environment
            .table()
            .get::<Implements>(current_id)
            .map_or(true, |x| x.0 != implemented_id)
        {
            continue;
        }

        let Some(implementation_arguments) = environment
            .table()
            .query::<ImplementationComponent>(current_id)
            .extract_cyclic_dependency()?
            .map(|x| {
                GenericArguments::from_default_model(
                    x.generic_arguments.clone(),
                )
            })
        else {
            continue;
        };

        let Some(_) = environment.generic_arguments_compatible(
            generic_arguments,
            &implementation_arguments,
            Variance::Invariant,
        )?
        else {
            continue;
        };

        match environment.deduce(generic_arguments, &implementation_arguments) {
            Ok(result) => {
                return Ok(Some(Succeeded {
                    result: Implementation {
                        instantiation: result.result.instantiation,
                        id: current_id,
                        is_not_general_enough: result
                            .result
                            .is_not_general_enough,
                    },
                    constraints: result.constraints,
                }))
            }

            Err(deduction::Error::Abrupt(err)) => {
                return Err(Error::Abrupt(err))
            }

            _ => continue,
        }
    }

    Ok(None)
}

fn predicate_satisfies<'a, M: Model>(
    predicates: impl IntoIterator<Item = &'a Predicate<Default>>,
    substitution: &Instantiation<M>,
    environment: &Environment<M, impl Normalizer<M>>,
) -> Result<bool, AbruptError> {
    // check if satisfies all the predicate
    for mut predicate in
        predicates.into_iter().map(|x| Predicate::from_other_model(x.clone()))
    {
        predicate.instantiate(substitution);

        if !match predicate {
            Predicate::TraitTypeCompatible(equality) => environment
                .compatible(
                    &Type::TraitMember(equality.lhs),
                    &equality.rhs,
                    Variance::Covariant,
                )?
                .is_some(),

            Predicate::ConstantType(constant_type) => {
                environment.query(&constant_type)?.is_some()
            }

            Predicate::TupleType(tuple_type) => {
                environment.query(&tuple_type)?.is_some()
            }

            Predicate::PositiveTrait(tr) => environment.query(&tr)?.is_some(),
            Predicate::NegativeTrait(tr) => environment.query(&tr)?.is_none(),
            Predicate::PositiveMarker(tr) => environment.query(&tr)?.is_some(),
            Predicate::NegativeMarker(tr) => environment.query(&tr)?.is_none(),

            Predicate::TypeOutlives(_) | Predicate::LifetimeOutlives(_) => true,
        } {
            return Ok(false);
        }
    }

    Ok(true)
}
