use pernixc_symbol::kind::{Kind, get_kind};
use pernixc_type::{
    generic_parameters::get_generic_parameters,
    predicate::Subtype,
    substitution::Substitution,
    r#type::{
        Type,
        bound::Instantiate,
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

        self.new_universe(async |solver| {
            let lesser_inst = lesser_ap
                .binder()
                .map(|x| solver.create_inference_instantiations(x.kinds()));
            let greater_inst = greater_ap
                .binder()
                .map(|x| solver.create_skolem_instantiations(x.kinds()));

            let engine = solver.engine();

            Box::pin(solver.handle_application_arguments(
                lesser_ap,
                iter.map(|(l, g)| {
                    (
                        lesser_inst.as_ref().map_or_else(
                            || l.clone(),
                            |insts| l.instantiate(insts, engine),
                        ),
                        greater_inst.as_ref().map_or_else(
                            || g.clone(),
                            |insts| g.instantiate(insts, engine),
                        ),
                    )
                }),
                lesser_inst.as_deref(),
                greater_inst.as_deref(),
                variance,
            ))
            .await
        })
        .await
    }

    async fn handle_application_arguments(
        &mut self,
        lesser_ap: &Application,
        mut arguments: impl Iterator<Item = (Interned<Type>, Interned<Type>)>,
        lesser_inst: Option<&[Interned<Type>]>,
        greater_inst: Option<&[Interned<Type>]>,
        variance: Variance,
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
                    self.handle_subtype_of_arguments(
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
                        lesser_inst,
                        greater_inst,
                    ),
                )
                .await
            }

            Constructor::Symbolic(symbolic) => {
                Box::pin(self.handle_symbolic_arguments(
                    *symbolic,
                    arguments,
                    lesser_inst,
                    greater_inst,
                    variance,
                ))
                .await
            }

            Constructor::Tuple(_) => {
                Box::pin(self.handle_subtype_of_arguments(
                    arguments.map(|(lesser, greater)| {
                        (lesser, greater, variance.xfrom(Variance::Covariant))
                    }),
                    lesser_inst,
                    greater_inst,
                ))
                .await
            }

            Constructor::FunctionPointer(_) => {
                let argument_count = lesser_ap.arguments().len();
                assert!(argument_count > 0);

                Box::pin(self.handle_subtype_of_arguments(
                    arguments.enumerate().map(|(index, (lesser, greater))| {
                        let argument_variance = if index + 1 == argument_count {
                            Variance::Covariant
                        } else {
                            Variance::Contravariant
                        };

                        (lesser, greater, variance.xfrom(argument_variance))
                    }),
                    lesser_inst,
                    greater_inst,
                ))
                .await
            }

            Constructor::InstanceAssociated(_) => {
                Box::pin(self.handle_subtype_of_arguments(
                    arguments.map(|(lesser, greater)| {
                        (lesser, greater, variance.xfrom(Variance::Invariant))
                    }),
                    lesser_inst,
                    greater_inst,
                ))
                .await
            }
        }
    }

    async fn handle_symbolic_arguments(
        &mut self,
        symbolic: Symbolic,
        arguments: impl Iterator<Item = (Interned<Type>, Interned<Type>)>,
        lesser_inst: Option<&[Interned<Type>]>,
        greater_inst: Option<&[Interned<Type>]>,
        variance: Variance,
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

                Box::pin(self.handle_subtype_of_arguments(
                    arguments.zip(generic_parameters.iter()).map(
                        |((lesser, greater), (id, _))| {
                            (
                                lesser,
                                greater,
                                variance.xfrom(variances.get_variacne_of(id)),
                            )
                        },
                    ),
                    lesser_inst,
                    greater_inst,
                ))
                .await
            }

            Kind::Instance => {
                Box::pin(self.handle_subtype_of_arguments(
                    arguments.map(|(lesser, greater)| {
                        (lesser, greater, variance.xfrom(Variance::Invariant))
                    }),
                    lesser_inst,
                    greater_inst,
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

    async fn handle_subtype_of_arguments(
        &mut self,
        pairs: impl Iterator<Item = (Interned<Type>, Interned<Type>, Variance)>,
        lesser_inst: Option<&[Interned<Type>]>,
        greater_inst: Option<&[Interned<Type>]>,
    ) -> Result<Option<Step>, OverflowError> {
        let subtypes = pairs.map(|(l, r, v)| Subtype::new(l, r, v)).collect();

        if lesser_inst.is_none_or(<[_]>::is_empty)
            && greater_inst.is_none_or(<[_]>::is_empty)
        {
            return Ok(Some((
                Substitution::new(),
                subtypes,
                Constraints::default(),
            )));
        }

        let (substitution, residual_subtypes, constraints) =
            Box::pin(self.resolve_subtypes(subtypes)).await?;

        if residual_subtypes.is_empty() {
            Ok(Some((substitution, Vec::new(), constraints)))
        } else {
            Ok(None)
        }
    }
}
