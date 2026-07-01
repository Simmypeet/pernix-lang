mod associated;
mod candidates;
mod deduction;
mod recursive;
mod selection;

use std::{future::Future, sync::Arc};

pub use candidates::{
    GlobalInstanceCandidatesKey, LexicalInstanceCandidate,
    LexicalInstanceCandidates, LexicalInstanceCandidatesKey,
};
use pernixc_symbol::GlobalSymbolID;
use pernixc_type::{
    generic_parameters::GenericParameterID, symbol::TraitRef2, r#type::Type2,
};
use qbice::storage::intern::Interned;

use crate::{
    constraints::Constraints,
    solver::{Agree, OverflowError, Provisional, Solve, Solver},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum InstanceSource {
    InstanceParameter(GenericParameterID),
    AssociatedInstance(GlobalSymbolID),
    GlobalInstance(GlobalSymbolID),
    InstanceScope(GlobalSymbolID),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolvedInstance {
    instance: Interned<Type2>,
    source: InstanceSource,
}

impl ResolvedInstance {
    #[must_use]
    pub const fn new(
        instance: Interned<Type2>,
        source: InstanceSource,
    ) -> Self {
        Self { instance, source }
    }

    #[must_use]
    pub const fn instance(&self) -> &Interned<Type2> { &self.instance }

    #[must_use]
    pub const fn source(&self) -> InstanceSource { self.source }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NormalFormFailure {
    NotClosed,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResolveError {
    NotFound,
    Cyclic,
    Ambiguous(Arc<[InstanceSource]>),
    Recursive(Arc<RecursiveError>),
    NormalForm(NormalFormFailure),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RecursiveError {
    resolving_symbol: GlobalSymbolID,
    errors: Arc<[(GenericParameterID, ResolveError, TraitRef2)]>,
}

impl RecursiveError {
    #[must_use]
    pub const fn resolving_symbol(&self) -> GlobalSymbolID {
        self.resolving_symbol
    }

    #[must_use]
    pub fn errors(&self) -> &[(GenericParameterID, ResolveError, TraitRef2)] {
        &self.errors
    }
}

impl Agree for Result<(ResolvedInstance, Constraints), ResolveError> {
    fn agree(&self, other: &Self) -> bool {
        match (self, other) {
            (Ok(left), Ok(right)) => left == right,
            (Err(left), Err(right)) => {
                left == right || (contains_cycle(left) && contains_cycle(right))
            }
            (Ok(_), Err(_)) | (Err(_), Ok(_)) => false,
        }
    }
}

fn contains_cycle(error: &ResolveError) -> bool {
    match error {
        ResolveError::Cyclic => true,
        ResolveError::Recursive(error) => {
            error.errors.iter().any(|(_, error, _)| contains_cycle(error))
        }
        ResolveError::NotFound
        | ResolveError::Ambiguous(_)
        | ResolveError::NormalForm(_) => false,
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct ResolveInstance(TraitRef2);

impl Solve for ResolveInstance {
    type Result = Result<(ResolvedInstance, Constraints), ResolveError>;

    fn solve(
        &self,
        solver: &mut Solver<'_>,
    ) -> impl Future<Output = Result<Self::Result, OverflowError>> + Send {
        let trait_ref = self.0.clone();
        async move { Box::pin(solver.resolve_instance_uncached(&trait_ref)).await }
    }

    fn provisional_result(&self) -> Provisional<Self::Result> {
        Provisional::Continue(Err(ResolveError::Cyclic))
    }
}

impl Solver<'_> {
    pub async fn resolve_instance(
        &mut self,
        trait_ref: &TraitRef2,
    ) -> Result<
        Result<(ResolvedInstance, Constraints), ResolveError>,
        OverflowError,
    > {
        self.solve(&ResolveInstance(trait_ref.clone())).await
    }

    async fn resolve_instance_uncached(
        &mut self,
        trait_ref: &TraitRef2,
    ) -> Result<
        Result<(ResolvedInstance, Constraints), ResolveError>,
        OverflowError,
    > {
        let Some((normalized, propagated_constraints)) =
            self.normal_form_trait_ref(trait_ref).await?
        else {
            return Ok(Err(ResolveError::NormalForm(
                NormalFormFailure::NotClosed,
            )));
        };

        if normalized.binder().is_empty() {
            return Ok(self
                .resolve_normalized(&normalized, &[], None)
                .await?
                .map(|(resolved, constraints)| {
                    (resolved, propagated_constraints.union_into(constraints))
                }));
        }

        let resolved = self
            .new_universe(async |solver| {
                let skolems = solver
                    .create_skolem_instantiations(normalized.binder().kinds());
                let instantiated_arguments = normalized.binder().instantiate(
                    normalized.generic_arguments(),
                    &skolems,
                    solver.engine(),
                );
                let instantiated = TraitRef2::new(
                    normalized.trait_id(),
                    instantiated_arguments,
                    solver.empty_binder(),
                );

                solver
                    .resolve_normalized(
                        &instantiated,
                        &skolems,
                        Some(normalized.binder().clone()),
                    )
                    .await
            })
            .await?;

        Ok(resolved.map(|(resolved, constraints)| {
            (resolved, propagated_constraints.union_into(constraints))
        }))
    }

    /// Normalizes the [`TraitRef2`]  into its normal form using the
    /// `normal_form` query.
    async fn normal_form_trait_ref(
        &mut self,
        trait_ref: &TraitRef2,
    ) -> Result<Option<(TraitRef2, Constraints)>, OverflowError> {
        let mut arguments =
            Vec::with_capacity(trait_ref.generic_arguments().len());
        let mut constraints = Constraints::new();

        for argument in trait_ref.generic_arguments().iter() {
            let Some((normal, argument_constraints)) =
                self.normal_form(argument.clone()).await?
            else {
                return Ok(None);
            };
            constraints = constraints.union_into(argument_constraints);
            arguments.push(normal);
        }

        Ok(Some((
            TraitRef2::new(
                trait_ref.trait_id(),
                self.engine().intern_unsized(arguments),
                trait_ref.binder().clone(),
            ),
            constraints,
        )))
    }

    pub(crate) fn empty_binder(&self) -> pernixc_type::r#type::bound::Binder {
        pernixc_type::r#type::bound::Binder::new(
            self.engine().intern_unsized(Vec::new()),
        )
    }
}

#[cfg(test)]
mod test;
