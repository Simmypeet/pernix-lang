use std::{future::Future, sync::Arc};

use pernixc_symbol::GlobalSymbolID;
use pernixc_type::{
    generic_parameters::GenericParameterID, symbol::TraitRef2, r#type::Type2,
};
use qbice::storage::intern::Interned;

use crate::{
    constraints::Constraints,
    solver::{Agree, OverflowError, Provisional, Solve, Solver},
};

/// A request to resolve an instance implementing a trait reference.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ResolveInstance {
    trait_ref: TraitRef2,
}

impl ResolveInstance {
    /// Creates an instance-resolution request.
    #[must_use]
    pub const fn new(trait_ref: TraitRef2) -> Self { Self { trait_ref } }

    /// Returns the trait reference for which an instance is requested.
    #[must_use]
    pub const fn trait_ref(&self) -> &TraitRef2 { &self.trait_ref }
}

/// The result of an instance-resolution request.
pub type ResolveInstanceResult =
    Result<(ResolvedInstance, Constraints), ResolveError>;

impl Agree for ResolveInstanceResult {
    fn agree(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Ok((left_instance, left_constraints)),
                Ok((right_instance, right_constraints)),
            ) => {
                left_instance == right_instance
                    && left_constraints.agree(right_constraints)
            }
            (Err(_), Err(_)) => true,
            (Ok(_), Err(_)) | (Err(_), Ok(_)) => false,
        }
    }
}

impl Solve for ResolveInstance {
    type Result = ResolveInstanceResult;

    async fn solve(
        &self,
        solver: &mut Solver<'_>,
    ) -> Result<Self::Result, OverflowError> {
        solver.resolve_instance_impl(self).await
    }

    fn provisional_result(&self) -> Provisional<Self::Result> {
        Provisional::Continue(Err(ResolveError::Cyclic))
    }
}

impl Solver<'_> {
    /// Resolves an instance implementing `trait_ref` in the current premise.
    pub async fn resolve_instance(
        &mut self,
        trait_ref: TraitRef2,
    ) -> Result<ResolveInstanceResult, OverflowError> {
        self.solve(&ResolveInstance::new(trait_ref)).await
    }

    #[allow(clippy::manual_async_fn)]
    fn resolve_instance_impl<'a>(
        &'a mut self,
        request: &'a ResolveInstance,
    ) -> impl Future<Output = Result<ResolveInstanceResult, OverflowError>> + Send + 'a
    {
        async move {
            let _ = (self, request);
            todo!("instance-resolution logic is not implemented yet")
        }
    }
}

/// The source from which a resolved instance was selected.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum InstanceSource {
    /// An instance generic parameter visible at the query site.
    FromInstanceParameterID(GenericParameterID),

    /// An associated instance of the enclosing trait or instance.
    FromAssociatedInstance(GlobalSymbolID),

    /// A globally declared instance.
    FromGlobalInstance(GlobalSymbolID),

    /// The instance declaration enclosing the query site.
    FromInstanceScope(GlobalSymbolID),
}

/// An instance selected by instance resolution.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ResolvedInstance {
    instance: Interned<Type2>,
    source: InstanceSource,
}

impl ResolvedInstance {
    /// Creates a resolved instance.
    #[must_use]
    pub const fn new(
        instance: Interned<Type2>,
        source: InstanceSource,
    ) -> Self {
        Self { instance, source }
    }

    /// Returns the resolved instance type.
    #[must_use]
    pub const fn instance(&self) -> &Interned<Type2> { &self.instance }

    /// Consumes this value and returns the resolved instance type.
    #[must_use]
    pub fn into_instance(self) -> Interned<Type2> { self.instance }

    /// Returns where the instance was selected from.
    #[must_use]
    pub const fn source(&self) -> InstanceSource { self.source }
}

/// An error produced while resolving an instance.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResolveError {
    /// No matching instance could be found.
    NotFound,

    /// Resolving the instance recursively required the same request.
    Cyclic,

    /// More than one candidate is applicable.
    Ambiguous(Arc<[InstanceSource]>),

    /// Resolving a candidate's instance parameters failed.
    Recursive(Arc<RecursiveError>),

    /// A requested trait argument could not be reduced to a closed normal
    /// form, or the normalization constraints could not be satisfied.
    NormalFormFailure,
}

/// Failures encountered while resolving a candidate's instance parameters.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RecursiveError {
    resolving_symbol: GlobalSymbolID,
    errors: Vec<(GenericParameterID, ResolveError, TraitRef2)>,
}

impl RecursiveError {
    /// Creates a recursive instance-resolution error.
    #[must_use]
    pub const fn new(
        resolving_symbol: GlobalSymbolID,
        errors: Vec<(GenericParameterID, ResolveError, TraitRef2)>,
    ) -> Self {
        Self { resolving_symbol, errors }
    }

    /// Returns the candidate whose parameters were being resolved.
    #[must_use]
    pub const fn resolving_symbol(&self) -> GlobalSymbolID {
        self.resolving_symbol
    }

    /// Returns the failed instance-parameter resolutions.
    #[must_use]
    pub fn errors(&self) -> &[(GenericParameterID, ResolveError, TraitRef2)] {
        &self.errors
    }
}
