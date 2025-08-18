//! Contains the definition of query related to naming symbols and provides
//! basic naming resolution functionality.

use flexstr::SharedStr;
use pernixc_extend::extend;
use pernixc_handler::Handler;
use pernixc_query::{runtime::executor::CyclicError, TrackedEngine};
use pernixc_syntax::{Identifier, SimplePath, SimplePathRoot};
use pernixc_target::{get_linked_targets, get_target_map, Global};

use crate::{
    accessibility::symbol_accessible,
    get_table_of_symbol, get_target_root_module_id,
    import::get_imports,
    kind::{get_kind, Kind},
    member::get_members,
    name::diagnostic::{Diagnostic, SymbolIsNotAccessible, SymbolNotFound},
    parent::{get_closest_module_id, get_parent},
    ID,
};

pub mod diagnostic;

#[pernixc_query::query(
    key(Key),
    id(Global<ID>),
    value(SharedStr),
    executor(Executor),
    extend(method(get_name), no_cyclic)
)]
#[allow(clippy::unnecessary_wraps)]
pub async fn executor(
    id: Global<ID>,
    engine: &TrackedEngine,
) -> Result<SharedStr, CyclicError> {
    let table = engine.get_table_of_symbol(id).await;

    Ok(table
        .names
        .get(&id.id)
        .cloned()
        .unwrap_or_else(|| panic!("invalid symbol ID: {:?}", id.id)))
}

/// Gets the qualified name of the symbol such as `module::function`.
#[extend]
pub async fn get_qualified_name(
    self: &TrackedEngine,
    mut id: Global<ID>,
) -> String {
    let mut qualified_name = String::new();

    loop {
        let current_name = self.get_name(id).await;

        if qualified_name.is_empty() {
            qualified_name.push_str(&current_name);
        } else {
            qualified_name.insert_str(0, "::");
            qualified_name.insert_str(0, &current_name);
        }

        if let Some(parent_id) = self.get_parent(id).await {
            id = Global::new(id.target_id, parent_id);
        } else {
            break;
        }
    }

    qualified_name
}

/// Gets the [`Global<symbol::ID>`] of the symbol with the given sequence of
/// qualified names.
#[extend]
pub async fn get_by_qualified_name<'a>(
    self: &TrackedEngine,
    qualified_names: impl IntoIterator<Item = &'a str>,
) -> Option<Global<ID>> {
    let mut current_id: Option<Global<ID>> = None;

    for name in qualified_names {
        match current_id {
            Some(searched_in_item_id) => {
                let has_member =
                    self.get_kind(searched_in_item_id).await.has_member();

                if !has_member {
                    return None;
                }

                current_id = Some(
                    self.get_members(searched_in_item_id)
                        .await
                        .member_ids_by_name
                        .get(name)
                        .copied()
                        .map(|x| {
                            Global::new(searched_in_item_id.target_id, x)
                        })?,
                );
            }
            None => {
                current_id = if let Some(target_id) =
                    self.get_target_map().await.get(name).copied()
                {
                    Some(Global::new(
                        target_id,
                        self.get_target_root_module_id(target_id).await,
                    ))
                } else {
                    None
                };
            }
        }
    }

    current_id
}

/// Resolves a [`SimplePath`] as a [`GlobalID`].
#[extend]
pub async fn resolve_simple_path(
    self: &TrackedEngine,
    simple_path: &SimplePath,
    referring_site: Global<ID>,
    start_from_root: bool,
    handler: &dyn Handler<Diagnostic>,
) -> Option<Global<ID>> {
    // simple path should always have root tough
    let root: Global<ID> = match simple_path.root()? {
        SimplePathRoot::Target(_) => Global::new(
            referring_site.target_id,
            self.get_target_root_module_id(referring_site.target_id).await,
        ),

        SimplePathRoot::Identifier(ident) => {
            if start_from_root {
                let target_map = self.get_target_map().await;
                let target =
                    self.get_linked_targets(referring_site.target_id).await;

                let Some(target_id) = target_map
                    .get(ident.kind.0.as_str())
                    .copied()
                    .filter(|x| {
                        x == &referring_site.target_id || { target.contains(x) }
                    })
                else {
                    handler.receive(Diagnostic::SymbolNotFound(
                        SymbolNotFound {
                            searched_item_id: None,
                            resolution_span: ident.span,
                            name: ident.kind.0,
                        },
                    ));

                    return None;
                };

                Global::new(
                    target_id,
                    self.get_target_root_module_id(target_id).await,
                )
            } else {
                let closet_module_id =
                    self.get_closest_module_id(referring_site).await;

                let global_closest_module_id =
                    Global::new(referring_site.target_id, closet_module_id);

                let id = match self
                    .get_members(global_closest_module_id)
                    .await
                    .member_ids_by_name
                    .get(ident.kind.0.as_str())
                    .map(|x| Global::new(referring_site.target_id, *x))
                {
                    Some(id) => Some(id),
                    None => self
                        .get_imports(global_closest_module_id)
                        .await
                        .get(ident.kind.0.as_str())
                        .map(|x| x.id),
                };

                let Some(id) = id else {
                    handler.receive(Diagnostic::SymbolNotFound(
                        SymbolNotFound {
                            searched_item_id: Some(global_closest_module_id),
                            resolution_span: ident.span,
                            name: ident.kind.0,
                        },
                    ));

                    return None;
                };

                id
            }
        }
    };

    self.resolve_sequence(
        simple_path.subsequences().flat_map(|x| x.identifier().into_iter()),
        referring_site,
        root,
        handler,
    )
    .await
}

/// Resolves a sequence of identifier starting of from the given `root`.
#[extend]
pub async fn resolve_sequence<'a>(
    self: &TrackedEngine,
    simple_path: impl Iterator<Item = Identifier>,
    referring_site: Global<ID>,
    root: Global<ID>,
    handler: &dyn Handler<Diagnostic>,
) -> Option<Global<ID>> {
    let mut lastest_resolution = root;

    for identifier in simple_path {
        let Some(new_id) = self
            .get_member_of(lastest_resolution, identifier.kind.0.as_str())
            .await
        else {
            handler.receive(Diagnostic::SymbolNotFound(SymbolNotFound {
                searched_item_id: Some(lastest_resolution),
                resolution_span: identifier.span,
                name: identifier.kind.0,
            }));

            return None;
        };

        // non-fatal error, no need to return early
        if !self.symbol_accessible(referring_site, new_id).await {
            handler.receive(Diagnostic::SymbolIsNotAccessible(
                SymbolIsNotAccessible {
                    referring_site,
                    referred: new_id,
                    referred_span: identifier.span,
                },
            ));
        }

        lastest_resolution = new_id;
    }

    Some(lastest_resolution)
}

/// Searches for a member in the given symbol scope and returns its ID if it
/// exists.
#[extend]
pub async fn get_member_of(
    self: &TrackedEngine,
    id: Global<ID>,
    member_name: &str,
) -> Option<Global<ID>> {
    let symbol_kind = self.get_kind(id).await;

    let result = if symbol_kind.has_member() {
        self.get_members(id).await.member_ids_by_name.get(member_name).copied()
    } else {
        None
    };

    if let Some(test) = result {
        return Some(Global::new(id.target_id, test));
    }

    // TODO: search the member in the adt implementations
    let this_kind = self.get_kind(id).await;

    match this_kind {
        Kind::Module => {
            let imports = self.get_imports(id).await;

            imports.get(member_name).map(|x| x.id)
        }

        _ => None,
    }
}
