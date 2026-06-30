use linkme::distributed_slice;
use pernixc_qbice::{Config, PERNIX_PROGRAM, TrackedEngine};
use pernixc_symbol::GlobalSymbolID;
use pernixc_type::{
    generic_parameters::GenericParameterID,
    substitution::{Substitutable, Substitution},
    symbol::{TraitRef2, get_trait_ref_of_instance_symbol2},
    r#type::Type2,
};
use qbice::{
    Decode, Encode, Query, StableHash, executor, program::Registration,
    storage::intern::Interned,
};

use crate::{
    premise::Premise,
    solver::{OverflowError, Solver},
};

/// The ordering between two trait references.
///
/// The result is read relative to the left operand. For example,
/// [`Order::MoreGeneral`] means the left trait reference is more general than
/// the right trait reference.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
)]
pub enum Order {
    Incompatible,
    MoreGeneral,
    MoreSpecific,
    Ambiguous,
}

impl Solver<'_> {
    /// Orders two trait references by checking whether each direction can match
    /// the other.
    pub async fn order_trait_refs(
        &mut self,
        left: &TraitRef2,
        right: &TraitRef2,
    ) -> Result<Order, OverflowError> {
        if left.trait_id() != right.trait_id()
            || left.generic_arguments().len() != right.generic_arguments().len()
        {
            return Ok(Order::Incompatible);
        }

        let left_matches_right = self
            .match_trait_refs(left, right, InstantiationSide::Sided)
            .await?;
        let right_matches_left = self
            .match_trait_refs(right, left, InstantiationSide::Sided)
            .await?;

        if left_matches_right && right_matches_left {
            return Ok(Order::Ambiguous);
        }

        if left_matches_right {
            return Ok(Order::MoreGeneral);
        }

        if right_matches_left {
            return Ok(Order::MoreSpecific);
        }

        if self.match_trait_refs(left, right, InstantiationSide::Both).await? {
            Ok(Order::Ambiguous)
        } else {
            Ok(Order::Incompatible)
        }
    }

    async fn match_trait_refs(
        &mut self,
        head: &TraitRef2,
        subject: &TraitRef2,
        side: InstantiationSide,
    ) -> Result<bool, OverflowError> {
        self.new_universe(async |solver| {
            let head_substitution = solver
                .instantiate_trait_ref_generics(head, InstantiationMode::Infer)
                .await;
            let subject_substitution = solver
                .instantiate_trait_ref_generics(subject, match side {
                    InstantiationSide::Sided => InstantiationMode::Skolem,
                    InstantiationSide::Both => InstantiationMode::Infer,
                })
                .await;
            let mut match_substitution = Substitution::new();

            for (head_argument, subject_argument) in head
                .generic_arguments()
                .iter()
                .zip(subject.generic_arguments().iter())
            {
                let head_argument = head_argument
                    .apply_or_clone(&head_substitution, solver.engine())
                    .apply_or_clone(&match_substitution, solver.engine());
                let subject_argument = subject_argument
                    .apply_or_clone(&subject_substitution, solver.engine())
                    .apply_or_clone(&match_substitution, solver.engine());

                let Some((mut new_substitution, _)) =
                    solver.unify(head_argument, subject_argument).await?
                else {
                    return Ok(false);
                };

                new_substitution.compose(match_substitution, solver.engine());
                match_substitution = new_substitution;
            }

            Ok(true)
        })
        .await
    }

    async fn instantiate_trait_ref_generics(
        &mut self,
        trait_ref: &TraitRef2,
        mode: InstantiationMode,
    ) -> Substitution {
        let mut generic_parameters = Vec::new();

        for argument in trait_ref.generic_arguments().iter() {
            collect_generic_parameters(argument, &mut generic_parameters);
        }

        let mut substitution = Substitution::new();

        for generic_parameter in generic_parameters {
            let generic_parameter_ty =
                Type2::new_generic_parameter(generic_parameter, self.engine());
            let kind = self.kind_of(&generic_parameter_ty).await;
            let replacement = match mode {
                InstantiationMode::Infer => {
                    let variable = self.fresh_inference_variable(kind);
                    self.intern(Type2::InferenceVariable(variable))
                }
                InstantiationMode::Skolem => {
                    let variable = self.fresh_skolem_variable(kind);
                    self.intern(Type2::SkolemizedVariable(variable))
                }
            };

            substitution.insert_generic(generic_parameter, replacement);
        }

        substitution
    }
}

#[derive(Debug, Clone, Copy)]
enum InstantiationSide {
    Sided,
    Both,
}

#[derive(Debug, Clone, Copy)]
enum InstantiationMode {
    Infer,
    Skolem,
}

fn collect_generic_parameters(
    ty: &Interned<Type2>,
    generic_parameters: &mut Vec<GenericParameterID>,
) {
    match ty.as_ref() {
        Type2::GenericParameter(id) => {
            if !generic_parameters.contains(id) {
                generic_parameters.push(*id);
            }
        }

        Type2::Application(application) => {
            for argument in application.arguments().iter() {
                collect_generic_parameters(argument, generic_parameters);
            }
        }

        Type2::InferenceVariable(_)
        | Type2::BoundVariable(_)
        | Type2::SkolemizedVariable(_) => {}
    }
}

/// A query for retrieving the order (level of specificity) between two
/// `instance`'s trait ref arguments.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
    Query,
    derive_new::new,
)]
#[value(Result<Option<Order>, OverflowError>)]
#[extend(name = get_instance_order, by_val)]
pub struct InstanceOrderKey {
    /// The `this` in the [`Environment::order`]
    pub this: GlobalSymbolID,
    /// The `other` in the [`Environment::order`]
    pub other: GlobalSymbolID,
}

/// The executor for the [`InstanceOrderExecutor`] query.
#[executor(config = Config)]
pub async fn instance_order_executor(
    InstanceOrderKey { this, other }: &InstanceOrderKey,
    tracked_engine: &TrackedEngine,
) -> Result<Option<Order>, OverflowError> {
    let (Some(lhs_generic_arguments), Some(rhs_generic_arguments)) = (
        tracked_engine.get_trait_ref_of_instance_symbol2(*this).await,
        tracked_engine.get_trait_ref_of_instance_symbol2(*other).await,
    ) else {
        return Ok(None);
    };

    if lhs_generic_arguments.trait_id() != rhs_generic_arguments.trait_id() {
        return Ok(None);
    }

    let default_premise = Premise::default();
    let mut default_solver = Solver::new(&default_premise, tracked_engine);

    match default_solver
        .order_trait_refs(&lhs_generic_arguments, &rhs_generic_arguments)
        .await
    {
        Ok(order) => Ok(Some(order)),
        Err(overflow) => Err(overflow),
    }
}

#[distributed_slice(PERNIX_PROGRAM)]
static INSTANCE_ORDER_EXECUTOR: Registration<Config> =
    Registration::new::<InstanceOrderKey, InstanceOrderExecutor>();

#[cfg(test)]
mod test;
