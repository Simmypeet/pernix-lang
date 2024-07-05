use super::{contains_forall_lifetime, Satisfiability};
use crate::{
    semantic::{
        equality, get_equivalences_with_context,
        instantiation::{self, Instantiation},
        model::Model,
        normalizer::Normalizer,
        query::{self, Context, Query},
        term::Term,
        visitor, Compute, Environment, Output, OverflowError, Satisfied,
        Succeeded,
    },
    symbol::table::{self, DisplayObject, State, Table},
};

#[derive(Debug)]
struct Visitor<'a, 'c, T: State, N: Normalizer<M>, M: Model> {
    constant_type: Result<Output<Satisfied, M>, OverflowError>,
    environment: &'a Environment<'a, M, T, N>,
    context: &'c mut Context<M>,
}

impl<'a, 'c, 'v, U: Term, T: State, N: Normalizer<U::Model>>
    visitor::Visitor<'v, U> for Visitor<'a, 'c, T, N, U::Model>
{
    fn visit(&mut self, term: &U, _: U::Location) -> bool {
        match ConstantType(term.clone()).query_with_context_full(
            self.environment,
            self.context,
            (),
            QuerySource::Normal,
        ) {
            result @ (Err(_) | Ok(None)) => {
                self.constant_type = result;
                false
            }

            Ok(Some(result)) => match &mut self.constant_type {
                Ok(Some(current)) => {
                    current.constraints.extend(result.constraints);
                    true
                }

                _ => false,
            },
        }
    }
}

/// Describes the source of the query for constant type predicate.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub enum QuerySource {
    /// Used to reason the satisfiability by querying it by an another
    /// equivalent type.
    FromEquivalence,

    /// Normal
    #[default]
    Normal,
}

/// Represents a type can be used as a type of a compile-time constant value.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConstantType<T>(pub T);

impl<T: Term> Compute for ConstantType<T> {
    type Error = OverflowError;
    type Parameter = ();

    #[allow(private_bounds, private_interfaces)]
    fn implementation(
        &self,
        environment: &Environment<
            Self::Model,
            impl State,
            impl Normalizer<Self::Model>,
        >,
        context: &mut Context<Self::Model>,
        (): Self::Parameter,
        _: Self::InProgress,
    ) -> Result<Option<Self::Result>, Self::Error> {
        let satisfiability = self.0.definite_satisfiability();

        // trivially satisfiable
        if satisfiability == Satisfiability::Satisfied {
            return Ok(Some(Succeeded::satisfied()));
        }

        // if the term is congruent, then we need to check the sub-terms
        if satisfiability == Satisfiability::Congruent {
            let mut visitor = Visitor {
                constant_type: Ok(Some(Succeeded::satisfied())),
                environment,
                context,
            };

            let _ = self.0.accept_one_level(&mut visitor);

            if let Some(mut result) = visitor.constant_type? {
                // look for the fields of the term as well (if it's an ADT)
                let mut found_error = false;
                match self.0.get_adt_fields(environment.table) {
                    Some(fields) => {
                        for field in fields {
                            let Some(new_result) = ConstantType(field.clone())
                                .query_with_context_full(
                                    environment,
                                    context,
                                    (),
                                    QuerySource::Normal,
                                )?
                            else {
                                found_error = true;
                                break;
                            };

                            result.constraints.extend(new_result.constraints);
                        }
                    }
                    None => {}
                }

                if !found_error {
                    return Ok(Some(result));
                }
            }
        }

        // satisfiable with premises
        for premise_term in environment
            .premise
            .predicates
            .iter()
            .filter_map(|x| T::as_constant_type_predicate(x))
        {
            if let Some(result) =
                equality::Equality::new(self.0.clone(), premise_term.clone())
                    .query_with_context(environment, context)?
            {
                return Ok(Some(result));
            }
        }

        // satisfiable with equivalence
        for Succeeded { result: eq, constraints } in
            get_equivalences_with_context(&self.0, environment, context)?
        {
            if let Some(mut result) = ConstantType(eq.clone())
                .query_with_context_full(
                    environment,
                    context,
                    (),
                    QuerySource::FromEquivalence,
                )?
            {
                result.constraints.extend(constraints);
                return Ok(Some(result));
            }
        }

        Ok(None)
    }

    #[allow(private_bounds, private_interfaces)]
    fn on_cyclic(
        &self,
        _: &Environment<Self::Model, impl State, impl Normalizer<Self::Model>>,
        _: &mut Context<Self::Model>,
        _: Self::Parameter,
        _: Self::InProgress,
        _: Self::InProgress,
        call_stacks: &[query::QueryCall<Self::Model>],
    ) -> Result<Option<Self::Result>, Self::Error> {
        for call in call_stacks.iter().skip(1) {
            let Some(query) = <Self as Query>::from_call(call) else {
                continue;
            };

            if query.in_progress == QuerySource::Normal {
                return Ok(Some(Succeeded::satisfied()));
            }
        }

        Ok(None)
    }
}

impl<S: State, T: Term> table::Display<S> for ConstantType<T>
where
    T: table::Display<S>,
{
    fn fmt(
        &self,
        table: &Table<S>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(f, "const type {}", DisplayObject { display: &self.0, table })
    }
}

impl<T: Term> ConstantType<T> {
    /// Checks if the type contains a `forall` lifetime.
    #[must_use]
    pub fn contains_forall_lifetime(&self) -> bool {
        contains_forall_lifetime(&self.0)
    }

    /// Applies the instantiation to the type.
    pub fn instantiate(&mut self, instantiation: &Instantiation<T::Model>) {
        instantiation::instantiate(&mut self.0, instantiation);
    }
}
