use pernixc_symbol::kind::{Kind, get_kind};
use pernixc_type::{
    generic_parameters::get_generic_parameters,
    predicate::Subtype,
    substitution::Substitution,
    r#type::{
        Type,
        constructor::{Application, Constructor, Mutability, Symbolic},
    },
    variance::{Variance, get_variances},
};
use qbice::storage::intern::Interned;

use crate::{
    constraints::Constraints,
    solver::{OverflowError, Solver},
    subtype::Step,
};

mod hrtb;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ResolveStrategy {
    /// Exhaustively resolves the set of subtypes immediately until no more
    /// residual subtypes remain. If successful, returns `Ok(Some(Step))` with
    /// zero residual subtypes.
    ResolveImmediately,
    DeferResolution,
}

impl Solver<'_> {
    pub(super) async fn handle_application(
        &mut self,
        lesser: &Interned<Type>,
        greater: &Interned<Type>,
        lesser_ap: &Application,
        greater_ap: &Application,
        variance: Variance,
    ) -> Result<Option<Step>, OverflowError> {
        let Some(iter) = lesser_ap.destructure(greater_ap, self.engine())
        else {
            return Box::pin(self.try_reduce(lesser, greater, variance)).await;
        };

        let arguments = iter.collect::<Vec<_>>();

        let has_binder =
            lesser_ap.binder().is_some_and(|binder| !binder.is_empty())
                || greater_ap.binder().is_some_and(|binder| !binder.is_empty());

        // if the application doesn't have any higher-ranked binders, we can
        // simply breaking down the application into its arguments and add them
        // as subtyping subgoals.
        if has_binder {
            Box::pin(self.handle_hrtb_application(
                lesser_ap, greater_ap, &arguments, variance,
            ))
            .await
        } else {
            // Instance-associated applications may reduce to the same type
            // even when their corresponding arguments are not subtypes. For
            // example, consider a trait `Col` with an associated type `Elm`
            // and the relation:
            //
            // `Col.Elm[Set[Int32]] <: Col.Elm[Vec[Int32]]`
            //
            // Both associated types may reduce to `Int32`, so the relation can
            // hold even though `Set[Int32] <: Vec[Int32]` does not.
            //
            // If the applications were destructured and that argument
            // subproblem were deferred, the enclosing associated applications
            // would be discarded. The solver would then only retain the
            // unsolvable `Set[Int32] <: Vec[Int32]` subproblem and lose the
            // opportunity to reduce the original associated types.
            // Therefore every argument subproblem must solve immediately; if
            // one is stuck, destructuring fails and the caller retains the
            // original relation for a later reduction attempt.
            let resolve_strategy = match lesser_ap.constructor() {
                Constructor::InstanceAssociated(_) => {
                    ResolveStrategy::ResolveImmediately
                }
                Constructor::Primitive(_)
                | Constructor::Lifetime(_)
                | Constructor::Reference(_)
                | Constructor::Symbolic(_)
                | Constructor::Tuple(_)
                | Constructor::FunctionPointer(_)
                | Constructor::AnonymousTraitInstance(_) => {
                    ResolveStrategy::DeferResolution
                }
            };

            Box::pin(self.handle_application_arguments(
                lesser_ap,
                arguments.into_iter(),
                variance,
                resolve_strategy,
            ))
            .await
        }
    }

    /// Handles the matched application arguments by calculating the correct
    /// variance for each argument.
    async fn handle_application_arguments(
        &mut self,
        lesser_ap: &Application,
        mut arguments: impl Iterator<Item = (Interned<Type>, Interned<Type>)>,
        variance: Variance,
        resolve_strategy: ResolveStrategy,
    ) -> Result<Option<Step>, OverflowError> {
        match lesser_ap.constructor() {
            Constructor::Primitive(_)
            | Constructor::AnonymousTraitInstance(_) => {
                assert!(lesser_ap.arguments().is_empty());
                assert!(arguments.next().is_none());

                Ok(Some((
                    Substitution::new(),
                    Vec::new(),
                    Constraints::default(),
                )))
            }

            Constructor::Lifetime(_) => {
                unreachable!("should've been caught by the lifetime case")
            }

            Constructor::Reference(reference) => {
                let (lt_l, lt_g) =
                    arguments.next().expect("expect lifetime component");
                let (ty_l, ty_g) =
                    arguments.next().expect("expect type component");

                assert!(arguments.next().is_none());

                Box::pin(
                    self.handle_set_of_subtypes(
                        [
                            (lt_l, lt_g, variance.xfrom(Variance::Covariant)),
                            (
                                ty_l,
                                ty_g,
                                variance.xfrom(
                                    if reference.mutability()
                                        == Mutability::Mutable
                                    {
                                        Variance::Invariant
                                    } else {
                                        Variance::Covariant
                                    },
                                ),
                            ),
                        ]
                        .into_iter(),
                        resolve_strategy,
                    ),
                )
                .await
            }

            Constructor::Symbolic(symbolic) => {
                Box::pin(self.handle_symbolic_arguments(
                    *symbolic,
                    arguments,
                    variance,
                    resolve_strategy,
                ))
                .await
            }

            Constructor::Tuple(_) => {
                Box::pin(self.handle_set_of_subtypes(
                    arguments.map(|(lesser, greater)| {
                        (lesser, greater, variance.xfrom(Variance::Covariant))
                    }),
                    resolve_strategy,
                ))
                .await
            }

            Constructor::FunctionPointer(_) => {
                let argument_count = lesser_ap.arguments().len();
                assert!(argument_count > 0);

                Box::pin(self.handle_set_of_subtypes(
                    arguments.enumerate().map(|(index, (lesser, greater))| {
                        let argument_variance = if index + 1 == argument_count {
                            Variance::Covariant
                        } else {
                            Variance::Contravariant
                        };

                        (lesser, greater, variance.xfrom(argument_variance))
                    }),
                    resolve_strategy,
                ))
                .await
            }

            Constructor::InstanceAssociated(_) => {
                Box::pin(self.handle_set_of_subtypes(
                    arguments.map(|(lesser, greater)| {
                        (lesser, greater, variance.xfrom(Variance::Invariant))
                    }),
                    resolve_strategy,
                ))
                .await
            }
        }
    }

    async fn handle_symbolic_arguments(
        &mut self,
        symbolic: Symbolic,
        arguments: impl Iterator<Item = (Interned<Type>, Interned<Type>)>,
        variance: Variance,
        resolve_strategy: ResolveStrategy,
    ) -> Result<Option<Step>, OverflowError> {
        let kind = self.engine().get_kind(symbolic.symbol_id()).await;

        match kind {
            Kind::Struct | Kind::Enum => {
                let generic_parameters = self
                    .engine()
                    .get_generic_parameters(symbolic.symbol_id())
                    .await;
                let variances =
                    self.engine().get_variances(symbolic.symbol_id()).await;

                Box::pin(self.handle_set_of_subtypes(
                    arguments.zip(generic_parameters.iter()).map(
                        |((lesser, greater), (id, _))| {
                            (
                                lesser,
                                greater,
                                variance.xfrom(variances.get_variacne_of(id)),
                            )
                        },
                    ),
                    resolve_strategy,
                ))
                .await
            }

            Kind::Instance => {
                Box::pin(self.handle_set_of_subtypes(
                    arguments.map(|(lesser, greater)| {
                        (lesser, greater, variance.xfrom(Variance::Invariant))
                    }),
                    resolve_strategy,
                ))
                .await
            }

            Kind::Module
            | Kind::Function
            | Kind::Type
            | Kind::Constant
            | Kind::Trait
            | Kind::ExternFunction
            | Kind::Variant
            | Kind::TraitAssociatedType
            | Kind::TraitAssociatedFunction
            | Kind::TraitAssociatedConstant
            | Kind::TraitAssociatedInstance
            | Kind::Effect
            | Kind::EffectOperation
            | Kind::Marker
            | Kind::PositiveImplementation
            | Kind::NegativeImplementation
            | Kind::ImplementationAssociatedType
            | Kind::ImplementationAssociatedFunction
            | Kind::ImplementationAssociatedConstant
            | Kind::InstanceAssociatedType
            | Kind::InstanceAssociatedFunction
            | Kind::InstanceAssociatedConstant
            | Kind::InstanceAssociatedInstance => {
                panic!("expected ADT or instance symbol, found {kind:?}")
            }
        }
    }

    /// Either resolves the set of subtypes immediately, or returns them as
    /// subgoals to be resolved later, depending on the `resolve_immediately`
    /// flag.
    async fn handle_set_of_subtypes(
        &mut self,
        pairs: impl Iterator<Item = (Interned<Type>, Interned<Type>, Variance)>,
        resolve_strategy: ResolveStrategy,
    ) -> Result<Option<Step>, OverflowError> {
        let subtypes = pairs.map(|(l, r, v)| Subtype::new(l, r, v)).collect();

        match resolve_strategy {
            ResolveStrategy::DeferResolution => Ok(Some((
                Substitution::new(),
                subtypes,
                Constraints::default(),
            ))),
            ResolveStrategy::ResolveImmediately => {
                let (substitution, residual_subtypes, constraints) =
                    Box::pin(self.resolve_subtypes(subtypes)).await?;

                if residual_subtypes.is_empty() {
                    Ok(Some((substitution, Vec::new(), constraints)))
                } else {
                    Ok(None)
                }
            }
        }
    }
}
