use super::contains_forall_lifetime;
use crate::{
    symbol::table::{self, DisplayObject, State, Table},
    type_system::{
        compatible::Compatibility,
        equivalence::get_equivalences_with_context,
        instantiation::{self, Instantiation},
        normalizer::Normalizer,
        term::Term,
        variance::Variance,
        Compute, Environment, OverflowError, Succeeded,
    },
};

/// The predicate meaning that the term is a tuple and is unpackable.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Tuple<T>(pub T);

impl<S: State, T: table::Display<S>> table::Display<S> for Tuple<T> {
    fn fmt(
        &self,
        table: &Table<S>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(f, "tuple {}", DisplayObject { display: &self.0, table })
    }
}

impl<T: Term> Tuple<T> {
    /// Checks if the term contains a `forall` lifetime.
    #[must_use]
    pub fn contains_forall_lifetime(&self) -> bool {
        contains_forall_lifetime(&self.0)
    }

    /// Applies a instantiation to the [`Tuple`] term.
    pub fn instantiate(&mut self, instantiation: &Instantiation<T::Model>) {
        instantiation::instantiate(&mut self.0, instantiation);
    }
}

impl<T: Term> Compute for Tuple<T> {
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
        context: &mut crate::type_system::query::Context<Self::Model>,
        (): Self::Parameter,
        (): Self::InProgress,
    ) -> Result<Option<Self::Result>, Self::Error> {
        // trivially satisfied
        if self.0.as_tuple().is_some() {
            return Ok(Some(Succeeded::satisfied()));
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
            }) = self.0.compatible_with_context(
                &predicate.0,
                Variance::Covariant,
                environment,
                context,
            )? {
                if !forall_lifetime_errors.is_empty() {
                    continue;
                }

                return Ok(Some(Succeeded::satisfied_with(constraints)));
            }
        }

        // get the equivalences
        for Succeeded { result: eq, constraints } in
            get_equivalences_with_context(&self.0, environment, context)?
        {
            if let Some(mut result) =
                Tuple(eq).query_with_context(environment, context)?
            {
                result.constraints.extend(constraints);
                return Ok(Some(result));
            }
        }

        Ok(None)
    }
}
