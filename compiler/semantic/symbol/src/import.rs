//! Contains the definition of the [`Import`] struct

use std::sync::Arc;

use flexstr::SharedStr;
use pernixc_handler::{Handler, Storage};
use pernixc_hash::HashMap;
use pernixc_lexical::tree::RelativeLocation;
use pernixc_query::TrackedEngine;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::{SourceElement, Span};
use pernixc_stable_hash::StableHash;
use pernixc_syntax::{item::module::ImportItems, SimplePathRoot};
use pernixc_target::{get_linked_targets, get_target_map, Global};

use crate::{
    accessibility::symbol_accessible,
    get_target_root_module_id,
    import::diagnostic::{
        ConflictingUsing, Diagnostic, TargetRootInImportIsNotAllowedwithFrom,
    },
    kind::{get_kind, Kind},
    member::get_members,
    name::{
        self,
        diagnostic::{ExpectModule, SymbolIsNotAccessible, SymbolNotFound},
        get_name, resolve_simple_path,
    },
    span::get_span,
    ID,
};

pub mod diagnostic;

/// Represents a single `import ...` statement.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
)]
pub struct Using {
    /// The ID of the symbol being imported.
    pub id: Global<ID>,

    /// The span to the `import ... (as ...)?` statement in the source code.
    pub span: Span<RelativeLocation>,
}

#[pernixc_query::query(
    key(WithDiagnosticKey),
    value((
        Arc<HashMap<SharedStr, Using>>,
        Arc<[diagnostic::Diagnostic]>
    )),
    id(Global<ID>),
    executor(WithDiagnosticExecutor)
)]
#[allow(clippy::type_complexity)]
pub async fn import_executor(
    id: Global<ID>,
    engine: &pernixc_query::TrackedEngine,
) -> Result<
    (Arc<HashMap<SharedStr, Using>>, Arc<[diagnostic::Diagnostic]>),
    pernixc_query::runtime::executor::CyclicError,
> {
    let imports = engine.query(&crate::syntax::ImportKey(id)).await?;
    let storage = Storage::<diagnostic::Diagnostic>::new();

    let mut import_map = HashMap::default();

    for import in imports.as_ref() {
        let start_from = if let Some(from_simple_path) =
            import.from().and_then(|x| x.simple_path())
        {
            let Some(from_id) = engine
                .resolve_simple_path(&from_simple_path, id, true, &storage)
                .await
            else {
                continue;
            };

            // must be module
            if engine.get_kind(from_id).await != Kind::Module {
                storage.receive(diagnostic::Diagnostic::Naming(
                    name::diagnostic::Diagnostic::ExpectModule(ExpectModule {
                        module_path: from_simple_path.inner_tree().span(),
                        found_id: from_id,
                    }),
                ));
                continue;
            }

            Some(from_id)
        } else {
            None
        };

        let import_items = import.items().into_iter().flat_map(|x| {
            let test: Option<ImportItems> = match x {
            pernixc_syntax::item::module::ImportItemsKind::Regular(
                import_items,
            ) => Some(import_items),

            pernixc_syntax::item::module::ImportItemsKind::Parenthesized(i) => {
                i.import_items()
            }
        };

            test.into_iter()
        });

        for import_item in import_items {
            process_import_items(
                engine,
                id,
                import,
                &import_item,
                start_from,
                &mut import_map,
                &storage,
            );
        }
    }

    Ok((Arc::new(import_map), storage.into_vec().into()))
}

#[pernixc_query::query(
    key(DiagnosticKey),
    value(Arc<[Diagnostic]>),
    id(Global<ID>),
    executor(DiagnosticExecutor)
)]
pub async fn diagnostic_executor(
    id: Global<ID>,
    engine: &pernixc_query::TrackedEngine,
) -> Result<Arc<[Diagnostic]>, pernixc_query::runtime::executor::CyclicError> {
    let import_with_diagnostic = engine.query(&WithDiagnosticKey(id)).await?;

    Ok(import_with_diagnostic.1)
}

#[pernixc_query::query(
    key(Key),
    value(Arc<HashMap<SharedStr, Using>>),
    id(Global<ID>),
    executor(Executor),
    extend(method(get_imports), no_cyclic)
)]
pub async fn import_executor(
    id: Global<ID>,
    engine: &pernixc_query::TrackedEngine,
) -> Result<
    Arc<HashMap<SharedStr, Using>>,
    pernixc_query::runtime::executor::CyclicError,
> {
    let import_with_diagnostic = engine.query(&WithDiagnosticKey(id)).await?;

    Ok(import_with_diagnostic.0)
}

#[allow(clippy::too_many_lines)]
async fn process_import_items(
    engine: &TrackedEngine,
    defined_in_module_id: Global<ID>,
    import: &pernixc_syntax::item::module::Import,
    import_item: &ImportItems,
    start_from: Option<Global<ID>>,
    import_component: &mut HashMap<SharedStr, Using>,
    handler: &dyn Handler<diagnostic::Diagnostic>,
) {
    'item: for import_item in import_item.items() {
        // if start from , then all the import items can't have `target` as
        // it root path

        let (Some(root), Some(simple_path)) = (
            import_item.simple_path().and_then(|x| x.root()),
            import_item.simple_path(),
        ) else {
            continue;
        };

        if root.is_target() && import.from().is_some() {
            handler.receive(
                diagnostic::Diagnostic::TargetRootInImportIsNotAllowedwithFrom(
                    TargetRootInImportIsNotAllowedwithFrom {
                        target_root_span: root.span(),
                    },
                ),
            );
            continue;
        }

        let mut start = match start_from {
            Some(current) => {
                let identifier_root = root.as_identifier().unwrap();

                let Some(id) = engine
                    .get_members(current)
                    .await
                    .member_ids_by_name
                    .get(identifier_root.kind.0.as_str())
                    .copied()
                else {
                    handler.receive(
                        SymbolNotFound {
                            searched_item_id: Some(current),
                            resolution_span: root.span(),
                            name: identifier_root.kind.0.clone(),
                        }
                        .into(),
                    );
                    continue;
                };

                let result = Global::new(current.target_id, id);
                if !engine.symbol_accessible(defined_in_module_id, result).await
                {
                    handler.receive(
                        SymbolIsNotAccessible {
                            referring_site: defined_in_module_id,
                            referred: Global::new(current.target_id, id),
                            referred_span: root.span(),
                        }
                        .into(),
                    );
                }

                result
            }

            None => match root {
                SimplePathRoot::Target(_) => Global::new(
                    defined_in_module_id.target_id,
                    engine
                        .get_target_root_module_id(
                            defined_in_module_id.target_id,
                        )
                        .await,
                ),
                SimplePathRoot::Identifier(identifier) => {
                    let target_map = engine.get_target_map().await;
                    let linked_targets = engine
                        .get_linked_targets(defined_in_module_id.target_id)
                        .await;

                    let Some(id) = target_map
                        .get(identifier.kind.0.as_str())
                        .copied()
                        .filter(|x| {
                            x == &defined_in_module_id.target_id
                                || linked_targets.contains(x)
                        })
                    else {
                        handler.receive(
                            SymbolNotFound {
                                searched_item_id: None,
                                resolution_span: identifier.span,
                                name: identifier.kind.0.clone(),
                            }
                            .into(),
                        );
                        continue;
                    };

                    Global::new(id, engine.get_target_root_module_id(id).await)
                }
            },
        };

        for rest in simple_path.subsequences().filter_map(|x| x.identifier()) {
            let Some(next) = engine
                .get_members(start)
                .await
                .member_ids_by_name
                .get(rest.kind.0.as_str())
                .copied()
            else {
                handler.receive(
                    SymbolNotFound {
                        searched_item_id: Some(start),
                        resolution_span: rest.span,
                        name: rest.kind.0.clone(),
                    }
                    .into(),
                );
                continue 'item;
            };

            let next_id = Global::new(start.target_id, next);

            if !engine.symbol_accessible(defined_in_module_id, next_id).await {
                handler.receive(
                    SymbolIsNotAccessible {
                        referring_site: defined_in_module_id,
                        referred: next_id,
                        referred_span: rest.span,
                    }
                    .into(),
                );
            }

            start = next_id;
        }

        let name = match import_item
            .alias()
            .as_ref()
            .and_then(pernixc_syntax::item::module::Alias::identifier)
        {
            Some(alias) => alias.kind.0.clone(),
            None => engine.get_name(start).await,
        };

        // check if there's existing symbol right now
        let existing = match engine
            .get_members(defined_in_module_id)
            .await
            .member_ids_by_name
            .get(&name)
        {
            Some(x) => Some(
                engine
                    .get_span(Global::new(defined_in_module_id.target_id, *x))
                    .await,
            ),
            None => import_component.get(name.as_str()).map(|x| Some(x.span)),
        };

        if let Some(existing) = existing {
            handler.receive(diagnostic::Diagnostic::ConflictingUsing(
                ConflictingUsing {
                    using_span: import_item.alias().as_ref().map_or_else(
                        || import_item.span(),
                        SourceElement::span,
                    ),
                    name: name.clone(),
                    module_id: defined_in_module_id,
                    conflicting_span: existing,
                },
            ));
        } else {
            import_component.insert(name, Using {
                id: start,
                span: import_item
                    .alias()
                    .as_ref()
                    .map_or_else(|| import_item.span(), SourceElement::span),
            });
        }
    }
}
