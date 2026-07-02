use std::{future::Future, sync::Arc};

use pernixc_lexical::tree::RelativeSpan;
use pernixc_symbol::GlobalSymbolID;
use pernixc_type::{
    generic_parameters::GenericParameterID, predicate::Predicate2,
    symbol::TraitRef2, r#type::Type2,
};
use qbice::storage::intern::Interned;

use crate::{
    constraints::Constraints,
    solver::{Agree, OverflowError, Provisional, Solve, Solver},
};

mod deduce_instance_symbol;
mod match_trait_ref;
mod normal_form;
mod skolemize;
mod unify_trait_ref;

pub use deduce_instance_symbol::DeducedInstanceSymbol;

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
    soft_errors: Arc<[ResolveSoftError]>,
}

impl ResolvedInstance {
    /// Creates a resolved instance.
    #[must_use]
    pub const fn new(
        instance: Interned<Type2>,
        source: InstanceSource,
        soft_errors: Arc<[ResolveSoftError]>,
    ) -> Self {
        Self { instance, source, soft_errors }
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

    /// Returns the non-fatal errors encountered while resolving this instance.
    #[must_use]
    pub fn soft_errors(&self) -> &[ResolveSoftError] { &self.soft_errors }
}

/// A non-fatal error encountered while resolving an instance.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ResolveSoftError {
    /// A predicate from a selected instance's `where` clause could not be
    /// satisfied.
    UnsatisfiedPredicate(UnsatisfiedPredicate),
}

impl ResolveSoftError {
    fn prepend_instance_resolution_frame(
        self,
        frame: InstanceResolutionFrame,
    ) -> Self {
        match self {
            Self::UnsatisfiedPredicate(mut unsatisfied) => {
                let mut stack = Vec::with_capacity(
                    unsatisfied.instance_resolution_stack.len() + 1,
                );
                stack.push(frame);
                stack.extend(
                    unsatisfied.instance_resolution_stack.iter().cloned(),
                );
                unsatisfied.instance_resolution_stack = Arc::from(stack);

                Self::UnsatisfiedPredicate(unsatisfied)
            }
        }
    }
}

/// One level in a nested instance-resolution trace.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct InstanceResolutionFrame {
    instance_symbol: GlobalSymbolID,
    trait_ref: TraitRef2,
}

impl InstanceResolutionFrame {
    /// Creates a resolution frame for an instance candidate.
    #[must_use]
    pub const fn new(
        instance_symbol: GlobalSymbolID,
        trait_ref: TraitRef2,
    ) -> Self {
        Self { instance_symbol, trait_ref }
    }

    /// Returns the instance symbol selected at this resolution level.
    #[must_use]
    pub const fn instance_symbol(&self) -> GlobalSymbolID {
        self.instance_symbol
    }

    /// Returns the trait reference being solved at this resolution level.
    #[must_use]
    pub const fn trait_ref(&self) -> &TraitRef2 { &self.trait_ref }
}

/// A predicate from an instance's `where` clause that could not be satisfied.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct UnsatisfiedPredicate {
    predicate: Predicate2,
    predicate_declaration_span: Option<RelativeSpan>,
    instance_resolution_stack: Arc<[InstanceResolutionFrame]>,
}

impl UnsatisfiedPredicate {
    /// Creates an unsatisfied predicate.
    #[must_use]
    pub const fn new(
        predicate: Predicate2,
        predicate_declaration_span: Option<RelativeSpan>,
        instance_resolution_stack: Arc<[InstanceResolutionFrame]>,
    ) -> Self {
        Self {
            predicate,
            predicate_declaration_span,
            instance_resolution_stack,
        }
    }

    /// Returns the instantiated predicate that could not be satisfied.
    #[must_use]
    pub const fn predicate(&self) -> &Predicate2 { &self.predicate }

    /// Returns where the predicate was declared, when source information is
    /// available.
    #[must_use]
    pub const fn predicate_declaration_span(&self) -> Option<&RelativeSpan> {
        self.predicate_declaration_span.as_ref()
    }

    /// Returns the instance resolutions traversed before reaching this
    /// predicate, ordered from the outermost resolution to the instance
    /// declaring it.
    #[must_use]
    pub fn instance_resolution_stack(&self) -> &[InstanceResolutionFrame] {
        &self.instance_resolution_stack
    }
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

    /// The higher-ranked leak check failed because the selected instance is
    /// not general enough for the requested trait reference.
    HigherRankedLeakCheckFailure,
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
