use linkme::distributed_slice;
use pernixc_qbice::{Config, PERNIX_PROGRAM, TrackedEngine};
use pernixc_symbol::{
    ID,
    accessibility::{
        Accessibility, Key, accessibility_hierarchy_relationship,
        get_accessibility,
    },
    get_target_root_module_id,
    kind::{Kind, get_kind},
    member::get_members,
    parent::{HierarchyRelationship, get_closest_module_id, get_parent},
    syntax::get_implements_member_access_modifier,
};
use pernixc_target::Global;
use qbice::{
    Decode, Encode, Query, StableHash, executor, program::Registration,
    storage::intern::Interned,
};

use crate::{
    accessibility::diagnostic::SymbolIsMoreAccessibleThanParent,
    table::get_table_of_symbol,
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
#[value(Option<Accessibility<ID>>)]
pub struct ProjectionKey {
    pub symbol_id: Global<pernixc_symbol::ID>,
}

#[executor(config = Config, style = qbice::ExecutionStyle::Projection)]
async fn projection_executor(
    key: &ProjectionKey,
    engine: &TrackedEngine,
) -> Option<Accessibility<ID>> {
    let table = engine.get_table_of_symbol(key.symbol_id).await?;
    table.accessibilities.get(&key.symbol_id.id).copied()
}

#[distributed_slice(PERNIX_PROGRAM)]
static PROJECTION_EXECUTOR: Registration<Config> =
    Registration::new::<ProjectionKey, ProjectionExecutor>();

#[executor(config = Config)]
async fn accessibility_executor(
    &Key { symbol_id: id }: &Key,
    engine: &TrackedEngine,
) -> Accessibility<ID> {
    match engine.get_kind(id).await {
        Kind::Module
        | Kind::Struct
        | Kind::Trait
        | Kind::Enum
        | Kind::Type
        | Kind::Constant
        | Kind::TraitType
        | Kind::TraitFunction
        | Kind::TraitConstant
        | Kind::Marker
        | Kind::ExternFunction
        | Kind::Effect
        | Kind::Function => {
            engine.query(&ProjectionKey { symbol_id: id }).await.unwrap()
        }

        Kind::EffectOperation | Kind::Variant => {
            engine
                .get_accessibility(Global::new(
                    id.target_id,
                    engine.get_parent(id).await.unwrap(),
                ))
                .await
        }

        Kind::PositiveImplementation | Kind::NegativeImplementation => {
            // normally, you wouldn't retrieve the accessibility of the
            // implementation we'll return default public
            Accessibility::Public
        }
        Kind::ImplementationType
        | Kind::ImplementationFunction
        | Kind::ImplementationConstant => {
            let access_modifier =
                engine.get_implements_member_access_modifier(id).await;

            match access_modifier {
                Some(pernixc_syntax::AccessModifier::Private(_)) => {
                    let module_id = engine.get_closest_module_id(id).await;
                    Accessibility::Scoped(module_id)
                }

                Some(pernixc_syntax::AccessModifier::Public(_)) => {
                    Accessibility::Public
                }

                Some(pernixc_syntax::AccessModifier::Internal(_)) => {
                    Accessibility::Scoped(
                        engine.get_target_root_module_id(id.target_id).await,
                    )
                }

                None => Accessibility::Public,
            }
        }
    }
}

#[distributed_slice(PERNIX_PROGRAM)]
static ACCESSIBILITY_EXECUTOR: Registration<Config> =
    Registration::new::<Key, AccessibilityExecutor>();

/// A query for checking if the trait has members that are more accessible than
/// itself.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    qbice::Encode,
    qbice::Decode,
    qbice::StableHash,
    qbice::Query,
)]
#[value(Option<Interned<[diagnostic::Diagnostic]>>)]
pub struct MemberIsMoreAaccessibleKey {
    pub trait_id: Global<ID>,
}

#[executor(config = Config)]
async fn member_is_more_accessible_executor(
    key: &MemberIsMoreAaccessibleKey,
    tracked_engine: &TrackedEngine,
) -> Option<Interned<[diagnostic::Diagnostic]>> {
    let trait_id = key.trait_id;
    let trait_accessibility: Accessibility<ID> =
        tracked_engine.get_accessibility(trait_id).await;

    let members = tracked_engine.get_members(trait_id).await;

    let mut diagnostic = Vec::new();

    for member in members
        .member_ids_by_name
        .values()
        .copied()
        .chain(members.unnameds.iter().copied())
    {
        let member_accessibiliy = tracked_engine
            .get_accessibility(Global::new(trait_id.target_id, member))
            .await;

        // if the member has accessibility that is more accessible
        // than the trait's accessibility, then we report a diagnostic
        if tracked_engine
            .accessibility_hierarchy_relationship(
                trait_id.target_id,
                member_accessibiliy,
                trait_accessibility,
            )
            .await
            == HierarchyRelationship::Parent
        {
            diagnostic.push(
                diagnostic::Diagnostic::SymbolIsMoreAccessibleThanParent(
                    SymbolIsMoreAccessibleThanParent {
                        symbol_id: member,
                        parent_id: trait_id.id,
                        target_id: trait_id.target_id,
                    },
                ),
            );
        }
    }

    if diagnostic.is_empty() {
        None
    } else {
        Some(tracked_engine.intern_unsized(diagnostic))
    }
}

#[distributed_slice(PERNIX_PROGRAM)]
static MEMBER_IS_MORE_ACCESSIBLE_EXECUTOR: Registration<Config> =
    Registration::new::<
        MemberIsMoreAaccessibleKey,
        MemberIsMoreAccessibleExecutor,
    >();
