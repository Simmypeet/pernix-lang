use std::sync::Arc;

use pernixc_table::{DisplayObject, Table};
use serde::{Deserialize, Serialize};

use super::contains_error;
use crate::type_system::{
    self,
    compatible::Compatibility,
    equivalence::get_equivalences,
    instantiation::{self, Instantiation},
    normalizer::Normalizer,
    query::Query,
    term::Term,
    variance::Variance,
    Environment, Satisfied, Succeeded,
};

/// The predicate meaning that the term is a tuple and is unpackable.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct Tuple<T>(pub T);

impl<T: pernixc_table::Display> pernixc_table::Display for Tuple<T> {
    fn fmt(
        &self,
        table: &Table,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(f, "tuple {}", DisplayObject { display: &self.0, table })
    }
}

impl<T: Term> Tuple<T> {
    /// Checks if the term contains a `forall` lifetime.
    #[must_use]
    pub fn contains_error(&self) -> bool { contains_error(&self.0) }

    /// Applies a instantiation to the [`Tuple`] term.
    pub fn instantiate(&mut self, instantiation: &Instantiation<T::Model>) {
        instantiation::instantiate(&mut self.0, instantiation);
    }
}

impl<T: Term> Query for Tuple<T> {
    type Model = T::Model;
    type Parameter = ();
    type InProgress = ();
    type Result = Succeeded<Satisfied, T::Model>;
    type Error = type_system::AbruptError;

    fn query(
        &self,
        environment: &Environment<Self::Model, impl Normalizer<Self::Model>>,
        (): Self::Parameter,
        (): Self::InProgress,
    ) -> Result<Option<Arc<Self::Result>>, Self::Error> {
        // trivially satisfied
        if self.0.as_tuple().is_some() {
            return Ok(Some(Arc::new(Succeeded::satisfied())));
        }

        // check from predicates
        for predicate in environment
            .premise
            .predicates
            .iter()
            .filter_map(|x| T::as_tuple_predicate(x))
        {
            if let Some(Succeeded {
                result: Compatibility { forall_lifetime_errors, .. },
                constraints,
            }) = self.0.compatible(
                &predicate.0,
                Variance::Covariant,
                environment,
            )? {
                if !forall_lifetime_errors.is_empty() {
                    continue;
                }

                return Ok(Some(Arc::new(Succeeded::satisfied_with(
                    constraints,
                ))));
            }
        }

        // get the equivalences
        for Succeeded { result: eq, mut constraints } in
            get_equivalences(&self.0, environment)?
        {
            if let Some(result) = environment.query(&Self(eq))? {
                constraints.extend(result.constraints.iter().cloned());
                return Ok(Some(result));
            }
        }

        Ok(None)
    }
}
