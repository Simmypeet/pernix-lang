use pernixc_type::{
    r#type::{
        Type2, constructor::ApplicationView, inference::InferenceVariable,
        kind::TyKind,
    },
    variance::Variance2,
};
use qbice::storage::intern::Interned;

use super::ResolveStrategy;
use crate::{
    solver::{OverflowError, Solver},
    type_relation::{RelationFlags, Step, TypeRelation},
};

#[cfg(test)]
mod test;

#[derive(Debug)]
struct EffectRow {
    slots: Vec<(Interned<str>, Interned<Type2>)>,
    tail: Interned<Type2>,
}

type RelationArguments = (Interned<Type2>, Interned<Type2>, Variance2);
type EffectSlot = (Interned<str>, Interned<Type2>);

impl Solver<'_> {
    /// Decomposes two effect rows into invariant signature and tail relations.
    ///
    /// Returns `None` when opaque or rigid tails prevent a sound decomposition.
    pub(super) async fn handle_effect_rows(
        &mut self,
        relation: &TypeRelation,
    ) -> Result<Option<Step>, OverflowError> {
        let lesser = Self::flatten_effect_row(relation.lesser().clone());
        let greater = Self::flatten_effect_row(relation.greater().clone());
        let flags = relation.flags().with_variance(Variance2::Invariant);

        let (mut signature_relations, unmatched_lesser, unmatched_greater) =
            Self::pair_effect_row_slots(&lesser, &greater);

        let Some(tail_relations) = self.reconcile_effect_row_tails(
            lesser,
            greater,
            unmatched_lesser,
            unmatched_greater,
            flags,
        ) else {
            return Ok(None);
        };

        signature_relations.extend(tail_relations);

        self.handle_set_of_relations(
            signature_relations.into_iter(),
            flags,
            ResolveStrategy::DeferResolution,
        )
        .await
    }

    /// Pairs slots by label while preserving occurrence order for duplicate
    /// labels, returning invariant signature relations and both residual lists.
    fn pair_effect_row_slots(
        lesser: &EffectRow,
        greater: &EffectRow,
    ) -> (Vec<RelationArguments>, Vec<EffectSlot>, Vec<EffectSlot>) {
        let mut matched_greater = vec![false; greater.slots.len()];
        let mut signature_relations = Vec::new();
        let mut unmatched_lesser = Vec::new();

        for (lesser_label, lesser_signature) in &lesser.slots {
            let matching_index = greater
                .slots
                .iter()
                .enumerate()
                .find(|(index, (greater_label, _))| {
                    !matched_greater[*index] && lesser_label == greater_label
                })
                .map(|(index, _)| index);

            if let Some(index) = matching_index {
                matched_greater[index] = true;
                signature_relations.push((
                    lesser_signature.clone(),
                    greater.slots[index].1.clone(),
                    Variance2::Invariant,
                ));
            } else {
                unmatched_lesser
                    .push((lesser_label.clone(), lesser_signature.clone()));
            }
        }

        let unmatched_greater = greater
            .slots
            .iter()
            .zip(matched_greater)
            .filter(|(_, matched)| !matched)
            .map(|((label, signature), _)| (label.clone(), signature.clone()))
            .collect();

        (signature_relations, unmatched_lesser, unmatched_greater)
    }

    /// Reconciles unmatched row prefixes with their tails.
    ///
    /// A one-sided residual must be absorbed by the opposite inference tail.
    /// Two-sided residuals require two distinct bindable inference tails and a
    /// shared fresh tail. `None` preserves the original relation as residual
    /// work.
    fn reconcile_effect_row_tails(
        &mut self,
        lesser: EffectRow,
        greater: EffectRow,
        unmatched_lesser: Vec<EffectSlot>,
        unmatched_greater: Vec<EffectSlot>,
        flags: RelationFlags,
    ) -> Option<Vec<RelationArguments>> {
        match (unmatched_lesser.is_empty(), unmatched_greater.is_empty()) {
            (true, true) => {
                Some(vec![(lesser.tail, greater.tail, Variance2::Invariant)])
            }
            (false, true) => self.relate_lesser_residual_to_greater_tail(
                unmatched_lesser,
                lesser.tail,
                greater.tail,
                flags.greater_rigid_inference(),
            ),
            (true, false) => self.relate_lesser_tail_to_greater_residual(
                lesser.tail,
                unmatched_greater,
                greater.tail,
                flags.lesser_rigid_inference(),
            ),
            (false, false) => self.relate_two_open_row_tails(
                unmatched_lesser,
                lesser.tail,
                unmatched_greater,
                greater.tail,
                flags,
            ),
        }
    }

    /// Builds a relation in which the greater inference tail absorbs the
    /// unmatched lesser prefix.
    fn relate_lesser_residual_to_greater_tail(
        &self,
        unmatched_lesser: Vec<EffectSlot>,
        lesser_tail: Interned<Type2>,
        greater_tail: Interned<Type2>,
        greater_tail_rigid: bool,
    ) -> Option<Vec<RelationArguments>> {
        if !Self::is_bindable_effect_row_tail(&greater_tail, greater_tail_rigid)
        {
            return None;
        }

        Some(vec![(
            self.rebuild_effect_row(unmatched_lesser, lesser_tail),
            greater_tail,
            Variance2::Invariant,
        )])
    }

    /// Builds a relation in which the lesser inference tail absorbs the
    /// unmatched greater prefix.
    fn relate_lesser_tail_to_greater_residual(
        &self,
        lesser_tail: Interned<Type2>,
        unmatched_greater: Vec<EffectSlot>,
        greater_tail: Interned<Type2>,
        lesser_tail_rigid: bool,
    ) -> Option<Vec<RelationArguments>> {
        if !Self::is_bindable_effect_row_tail(&lesser_tail, lesser_tail_rigid) {
            return None;
        }

        Some(vec![(
            lesser_tail,
            self.rebuild_effect_row(unmatched_greater, greater_tail),
            Variance2::Invariant,
        )])
    }

    /// Relates two distinct open tails to opposite residual prefixes over one
    /// shared fresh tail in the lowest universe nameable by both variables.
    fn relate_two_open_row_tails(
        &mut self,
        unmatched_lesser: Vec<EffectSlot>,
        lesser_tail: Interned<Type2>,
        unmatched_greater: Vec<EffectSlot>,
        greater_tail: Interned<Type2>,
        flags: RelationFlags,
    ) -> Option<Vec<RelationArguments>> {
        let lesser_variable = Self::bindable_effect_row_tail(
            &lesser_tail,
            flags.lesser_rigid_inference(),
        )?;
        let greater_variable = Self::bindable_effect_row_tail(
            &greater_tail,
            flags.greater_rigid_inference(),
        )?;

        if lesser_variable == greater_variable {
            return None;
        }

        let shared_universe = lesser_variable
            .universe_index()
            .min(greater_variable.universe_index());
        let shared_variable = self.fresh_inference_variable_in_universe(
            TyKind::EffectRow,
            shared_universe,
        );
        let shared_tail =
            Type2::new_inference_variable(shared_variable, self.engine());
        let lesser_residual =
            self.rebuild_effect_row(unmatched_lesser, shared_tail.clone());
        let greater_residual =
            self.rebuild_effect_row(unmatched_greater, shared_tail);

        Some(vec![
            (lesser_tail, greater_residual, Variance2::Invariant),
            (lesser_residual, greater_tail, Variance2::Invariant),
        ])
    }

    /// Flattens the known extension prefix of a row, leaving the first empty,
    /// inference, or opaque node as its tail.
    fn flatten_effect_row(mut row: Interned<Type2>) -> EffectRow {
        let mut slots = Vec::new();

        loop {
            let Type2::Application(application) = &*row else {
                break;
            };
            let ApplicationView::EffectRowExtend(extension) =
                application.view()
            else {
                break;
            };

            slots.push((
                extension.label().clone(),
                extension.effect_signature().clone(),
            ));
            row = extension.row_tail().clone();
        }

        EffectRow { slots, tail: row }
    }

    /// Rebuilds an effect row without changing the original slot order.
    fn rebuild_effect_row(
        &self,
        slots: Vec<(Interned<str>, Interned<Type2>)>,
        tail: Interned<Type2>,
    ) -> Interned<Type2> {
        slots.into_iter().rev().fold(tail, |tail, (label, signature)| {
            Type2::new_effect_row_extend(label, signature, tail, self.engine())
        })
    }

    /// Returns whether a tail is a non-rigid effect-row inference variable.
    fn is_bindable_effect_row_tail(tail: &Type2, rigid: bool) -> bool {
        Self::bindable_effect_row_tail(tail, rigid).is_some()
    }

    /// Extracts a tail's effect-row inference variable when relation rigidity
    /// permits binding it.
    fn bindable_effect_row_tail(
        tail: &Type2,
        rigid: bool,
    ) -> Option<InferenceVariable> {
        if rigid {
            return None;
        }

        let Type2::InferenceVariable(variable) = tail else {
            return None;
        };

        (variable.kind() == TyKind::EffectRow).then_some(*variable)
    }
}
