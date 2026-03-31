//! Implementation of instance coherence checking.
//!
//! This module validates that:
//! - All trait members are implemented by the corresponding instance
//! - No extraneous members from the trait are defined in the instance
//! - The kind of trait members matches the kind of instance members

use linkme::distributed_slice;
use pernixc_hash::{FxHashMap, FxHashSet};
use pernixc_qbice::{Config, PERNIX_PROGRAM, TrackedEngine};
use pernixc_semantic_element::trait_ref::get_trait_ref_of_instance_symbol;
use pernixc_symbol::{
    accessibility::{get_accessibility, is_accessible_from_globally},
    kind::{Kind, get_kind},
    member::get_members,
};
use pernixc_target::Global;
use qbice::{
    Decode, Encode, Query, StableHash, executor, program::Registration,
    storage::intern::Interned,
};

use crate::instance_coherence::diagnostic::{
    Diagnostic, ExtraneousMember, MismatchedMemberKind, MissingMember,
    TraitMemberInvisible,
};

pub mod diagnostic;

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
    Query,
)]
#[value(Interned<[Diagnostic]>)]
pub struct Key {
    pub symbol_id: Global<pernixc_symbol::SymbolID>,
}

#[executor(config = Config)]
async fn instance_coherence_executor(
    &Key { symbol_id: instance_id }: &Key,
    engine: &TrackedEngine,
) -> Interned<[Diagnostic]> {
    let mut diagnostics = Vec::new();

    // Get the trait this instance implements
    let trait_ref = engine.get_trait_ref_of_instance_symbol(instance_id).await;

    let Some(trait_id) = trait_ref.map(|x| x.trait_id()) else {
        // If there's no trait ref, we can't check coherence
        // (This is likely already an error reported elsewhere)
        return engine.intern_unsized([]);
    };

    // Get members of the trait and instance
    let trait_members = engine.get_members(trait_id).await;
    let instance_members = engine.get_members(instance_id).await;

    // Create a map of instance members by name for quick lookup
    let mut instance_members_map: FxHashMap<
        Interned<str>,
        Global<pernixc_symbol::SymbolID>,
    > = FxHashMap::default();

    for (name, &member_id) in &instance_members.member_ids_by_name {
        instance_members_map
            .insert(name.clone(), instance_id.target_id.make_global(member_id));
    }

    // Track which instance members we've seen
    let checked_instance_members = check_trait_members_coherence(
        engine,
        instance_id,
        trait_id,
        &trait_members,
        &instance_members_map,
        &mut diagnostics,
    )
    .await;

    // Check for extraneous members in the instance
    for (instance_member_name, &instance_member_id) in
        &instance_members.member_ids_by_name
    {
        if !checked_instance_members.contains(instance_member_name) {
            // This member is not in the trait
            diagnostics.push(Diagnostic::ExtraneousMember(ExtraneousMember {
                instance_member_id: instance_id
                    .target_id
                    .make_global(instance_member_id),
                member_name: instance_member_name.clone(),
                trait_id,
            }));
        }
    }

    engine.intern_unsized(diagnostics)
}

async fn check_trait_members_coherence(
    engine: &TrackedEngine,
    instance_id: Global<pernixc_symbol::SymbolID>,
    trait_id: Global<pernixc_symbol::SymbolID>,
    trait_members: &pernixc_symbol::member::Member,
    instance_members_map: &FxHashMap<
        Interned<str>,
        Global<pernixc_symbol::SymbolID>,
    >,
    diagnostics: &mut Vec<Diagnostic>,
) -> FxHashSet<Interned<str>> {
    let mut checked_instance_members = FxHashSet::default();

    // Check that all trait members are implemented
    for (trait_member_name, &trait_member_id) in
        &trait_members.member_ids_by_name
    {
        let trait_member_global_id =
            trait_id.target_id.make_global(trait_member_id);

        let is_visible = engine
            .is_accessible_from_globally(
                instance_id,
                engine
                    .get_accessibility(trait_member_global_id)
                    .await
                    .into_global(trait_member_global_id.target_id),
            )
            .await;

        if !is_visible {
            diagnostics.push(Diagnostic::TraitMemberInvisible(
                TraitMemberInvisible {
                    instance_id,
                    trait_member_id: trait_member_global_id,
                },
            ));
        }

        let trait_member_kind = engine.get_kind(trait_member_global_id).await;

        if let Some(&instance_member_id) =
            instance_members_map.get(trait_member_name)
        {
            // Member exists, check if the kind matches
            let instance_member_kind =
                engine.get_kind(instance_member_id).await;

            if !kinds_match(trait_member_kind, instance_member_kind) {
                // Get the expected instance kind
                let expected_instance_kind =
                    trait_kind_to_instance_kind(trait_member_kind);

                diagnostics.push(Diagnostic::MismatchedMemberKind(
                    MismatchedMemberKind {
                        instance_member_id,
                        trait_member_id: trait_member_global_id,
                        member_name: trait_member_name.clone(),
                        expected_kind: expected_instance_kind,
                        actual_kind: instance_member_kind,
                    },
                ));
            }

            checked_instance_members.insert(trait_member_name.clone());
        } else {
            // Member is missing
            diagnostics.push(Diagnostic::MissingMember(MissingMember {
                instance_id,
                trait_member_id: trait_member_global_id,
                member_name: trait_member_name.clone(),
            }));
        }
    }

    checked_instance_members
}

/// Checks if the kinds of trait and instance members match.
///
/// The matching rules:
/// - `TraitAssociatedType` matches `InstanceAssociatedType`
/// - `TraitAssociatedFunction` matches `InstanceAssociatedFunction`
/// - `TraitAssociatedConstant` matches `InstanceAssociatedConstant`
/// - `TraitAssociatedInstance` matches `InstanceAssociatedInstance`
const fn kinds_match(trait_kind: Kind, instance_kind: Kind) -> bool {
    matches!(
        (trait_kind, instance_kind),
        (Kind::TraitAssociatedType, Kind::InstanceAssociatedType)
            | (Kind::TraitAssociatedFunction, Kind::InstanceAssociatedFunction)
            | (Kind::TraitAssociatedConstant, Kind::InstanceAssociatedConstant)
            | (Kind::TraitAssociatedInstance, Kind::InstanceAssociatedInstance)
    )
}

/// Converts a trait member kind to the expected instance member kind.
const fn trait_kind_to_instance_kind(trait_kind: Kind) -> Kind {
    match trait_kind {
        Kind::TraitAssociatedType => Kind::InstanceAssociatedType,
        Kind::TraitAssociatedFunction => Kind::InstanceAssociatedFunction,
        Kind::TraitAssociatedConstant => Kind::InstanceAssociatedConstant,
        Kind::TraitAssociatedInstance => Kind::InstanceAssociatedInstance,
        _ => trait_kind, // Shouldn't happen, but return as-is
    }
}

#[distributed_slice(PERNIX_PROGRAM)]
static INSTANCE_COHERENCE_EXECUTOR: Registration<Config> =
    Registration::<Config>::new::<Key, InstanceCoherenceExecutor>();
