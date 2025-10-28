//! Checks for redefinition of ADT implementation members across different
//! implementation blocks.

use std::sync::Arc;

use pernixc_hash::HashMap;
use pernixc_query::{runtime::executor, TrackedEngine};
use pernixc_semantic_element::{
    implemented::get_implemented, implements_arguments::get_implements_argument,
};
use pernixc_symbol::{
    kind::{get_kind, Kind},
    member::get_members,
    span::get_span,
};
use pernixc_target::Global;
use pernixc_term::generic_arguments::GenericArguments;

use crate::adt_implementation_member_check::diagnostic::Diagnostic;

pub mod diagnostic;

/// The key for querying ADT implementation member redefinition diagnostics.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    pernixc_serialize::Serialize,
    pernixc_serialize::Deserialize,
    pernixc_stable_hash::StableHash,
    pernixc_query::Key,
)]
#[value(Arc<[Diagnostic]>)]
pub struct Key(pub Global<pernixc_symbol::ID>);

#[pernixc_query::executor(key(Key), name(Executor))]
pub async fn executor(
    key: &Key,
    engine: &TrackedEngine,
) -> Result<Arc<[Diagnostic]>, executor::CyclicError> {
    let adt_id = key.0;
    let kind = engine.get_kind(adt_id).await;

    // Only check structs, enums, traits, and markers
    if !matches!(kind, Kind::Struct | Kind::Enum | Kind::Trait | Kind::Marker) {
        return Ok(Arc::new([]));
    }

    // Get all implementations for this ADT
    let implementations = engine.get_implemented(adt_id).await?;

    if implementations.len() <= 1 {
        // No need to check if there's only one or zero implementations
        return Ok(Arc::new([]));
    }

    let mut diagnostics = Vec::new();

    // Group implementations by their generic arguments
    let mut implementations_by_args: HashMap<
        Option<Arc<GenericArguments>>,
        Vec<Global<pernixc_symbol::ID>>,
    > = HashMap::default();

    for &impl_id in implementations.iter() {
        let args = engine.get_implements_argument(impl_id).await?;
        implementations_by_args.entry(args).or_default().push(impl_id);
    }

    // For each group of implementations with the same generic arguments,
    // check for member redefinitions
    for (_args, impl_ids) in implementations_by_args.iter() {
        if impl_ids.len() <= 1 {
            // No need to check if there's only one implementation for this
            // specific instantiation
            continue;
        }

        let mut member_to_impl_map: HashMap<
            flexstr::SharedStr,
            (
                Global<pernixc_symbol::ID>,
                Option<pernixc_lexical::tree::RelativeSpan>,
            ),
        > = HashMap::default();

        // Iterate through each implementation with the same generic arguments
        for &impl_id in impl_ids.iter() {
            let members = engine.get_members(impl_id).await;

            // Check each named member in this implementation
            for (member_name, &member_id) in members.member_ids_by_name.iter() {
                let global_member_id =
                    Global::new(impl_id.target_id, member_id);

                // Check if this member name was already seen in another
                // implementation
                if let Some((previous_impl_id, previous_span)) =
                    member_to_impl_map.get(member_name)
                {
                    // Found a redefinition across implementations
                    let current_span = engine.get_span(global_member_id).await;

                    diagnostics.push(
                        Diagnostic::AdtImplementationMemberRedefinition(
                            diagnostic::AdtImplementationMemberRedefinition {
                                member_name: member_name.clone(),
                                adt_id,
                                first_implementation_id: *previous_impl_id,
                                first_span: *previous_span,
                                second_implementation_id: impl_id,
                                second_span: current_span,
                            },
                        ),
                    );
                } else {
                    // Record this member for future checks
                    let span = engine.get_span(global_member_id).await;
                    member_to_impl_map
                        .insert(member_name.clone(), (impl_id, span));
                }
            }
        }
    }

    Ok(diagnostics.into())
}

pernixc_register::register!(Key, Executor);
