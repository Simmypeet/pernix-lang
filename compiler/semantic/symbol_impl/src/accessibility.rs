use std::sync::Arc;

use pernixc_query::{runtime::executor::CyclicError, TrackedEngine};
use pernixc_symbol::{
    accessibility::{
        accessibility_hierarchy_relationship, get_accessibility, Accessibility,
        Key,
    },
    get_target_root_module_id,
    kind::{get_kind, Kind},
    member::get_members,
    parent::{get_closest_module_id, get_parent, HierarchyRelationship},
    syntax::get_implements_member_access_modifier,
    ID,
};
use pernixc_target::Global;

use crate::{
    accessibility::diagnostic::SymbolIsMoreAccessibleThanParent,
    table::get_table_of_symbol,
};

pub mod diagnostic;

#[pernixc_query::executor(key(Key), name(Executor))]
pub async fn executor(
    &Key(id): &Key,
    engine: &TrackedEngine,
) -> Result<Accessibility<ID>, CyclicError> {
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
        | Kind::Function => {
            let table = engine.get_table_of_symbol(id).await;
            Ok(table.accessibilities.get(&id.id).copied().unwrap())
        }

        Kind::Variant => Ok(engine
            .get_accessibility(Global::new(
                id.target_id,
                engine.get_parent(id).await.unwrap(),
            ))
            .await),

        Kind::PositiveImplementation | Kind::NegativeImplementation => {
            // normally, you wouldn't retrieve the accessibility of the
            // implementation we'll return default public
            Ok(Accessibility::Public)
        }
        Kind::ImplementationType
        | Kind::ImplementationFunction
        | Kind::ImplementationConstant => {
            let access_modifier =
                engine.get_implements_member_access_modifier(id).await;

            match access_modifier {
                Some(pernixc_syntax::AccessModifier::Private(_)) => {
                    let module_id = engine.get_closest_module_id(id).await;
                    Ok(Accessibility::Scoped(module_id))
                }

                Some(pernixc_syntax::AccessModifier::Public(_)) => {
                    Ok(Accessibility::Public)
                }

                Some(pernixc_syntax::AccessModifier::Internal(_)) => {
                    Ok(Accessibility::Scoped(
                        engine.get_target_root_module_id(id.target_id).await,
                    ))
                }

                None => Ok(Accessibility::Public),
            }
        }
    }
}

pernixc_register::register!(Key, Executor);

/// A query for checking if the trait has members that are more accessible than
/// itself.
#[pernixc_query::query(
    key(MemberIsMoreAaccessibleKey),
    id(Global<ID>),
    value(Option<Arc<[diagnostic::Diagnostic]>>),
    executor(MemberIsMoreAccessibleExecutor)
)]
pub async fn member_is_more_accessible_executor(
    trait_id: Global<ID>,
    tracked_engine: &TrackedEngine,
) -> Result<Option<Arc<[diagnostic::Diagnostic]>>, CyclicError> {
    let trait_accessibility: Accessibility<ID> =
        tracked_engine.get_accessibility(trait_id).await;

    let members: Arc<pernixc_symbol::member::Member> =
        tracked_engine.get_members(trait_id).await;

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
        Ok(None)
    } else {
        Ok(Some(Arc::from(diagnostic)))
    }
}

pernixc_register::register!(
    MemberIsMoreAaccessibleKey,
    MemberIsMoreAccessibleExecutor
);
