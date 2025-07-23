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
pub fn executor(
    id: Global<ID>,
    engine: &TrackedEngine,
) -> Result<SharedStr, CyclicError> {
    let table = engine.get_table_of_symbol(id);

    Ok(table
        .names
        .get(&id.id)
        .cloned()
        .unwrap_or_else(|| panic!("invalid symbol ID: {:?}", id.id)))
}

/// Gets the qualified name of the symbol such as `module::function`.
#[extend]
pub fn get_qualified_name(
    self: &TrackedEngine<'_>,
    mut id: Global<ID>,
) -> String {
    let mut qualified_name = String::new();

    loop {
        let current_name = self.get_name(id);

        if qualified_name.is_empty() {
            qualified_name.push_str(&current_name);
        } else {
            qualified_name.insert_str(0, "::");
            qualified_name.insert_str(0, &current_name);
        }

        if let Some(parent_id) = self.get_parent(id) {
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
pub fn get_by_qualified_name<'a>(
    self: &TrackedEngine<'_>,
    qualified_names: impl IntoIterator<Item = &'a str>,
) -> Option<Global<ID>> {
    let mut current_id: Option<Global<ID>> = None;

    for name in qualified_names {
        match current_id {
            Some(searched_in_item_id) => {
                let has_member =
                    self.get_kind(searched_in_item_id).has_member();

                if !has_member {
                    return None;
                }

                current_id = Some(
                    self.get_members(searched_in_item_id)
                        .member_ids_by_name
                        .get(name)
                        .copied()
                        .map(|x| {
                            Global::new(searched_in_item_id.target_id, x)
                        })?,
                );
            }
            None => {
                current_id = self.get_target_map().get(name).map(|&x| {
                    Global::new(x, self.get_target_root_module_id(x))
                });
            }
        }
    }

    current_id
}

/// Resolves a [`SimplePath`] as a [`GlobalID`].
#[extend]
pub fn resolve_simple_path(
    self: &TrackedEngine<'_>,
    simple_path: &SimplePath,
    referring_site: Global<ID>,
    start_from_root: bool,
    use_imports: bool,
    handler: &dyn Handler<Diagnostic>,
) -> Option<Global<ID>> {
    // simple path should always have root tough
    let root: Global<ID> = match simple_path.root()? {
        SimplePathRoot::Target(_) => Global::new(
            referring_site.target_id,
            self.get_target_root_module_id(referring_site.target_id),
        ),

        SimplePathRoot::Identifier(ident) => {
            if start_from_root {
                let target_map = self.get_target_map();
                let Some(target_id) = target_map
                    .get(ident.kind.0.as_str())
                    .copied()
                    .filter(|x| {
                        x == &referring_site.target_id || {
                            let target = self
                                .get_linked_targets(referring_site.target_id);

                            target.contains(x)
                        }
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
                    self.get_target_root_module_id(target_id),
                )
            } else {
                let closet_module_id =
                    self.get_closest_module_id(referring_site);

                let global_closest_module_id =
                    Global::new(referring_site.target_id, closet_module_id);

                let Some(id) = self
                    .get_members(global_closest_module_id)
                    .member_ids_by_name
                    .get(ident.kind.0.as_str())
                    .map(|x| Global::new(referring_site.target_id, *x))
                    .or_else(|| {
                        self.get_imports(global_closest_module_id)
                            .get(ident.kind.0.as_str())
                            .map(|x| x.id)
                    })
                else {
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
        use_imports,
        handler,
    )
}

/// Resolves a sequence of identifier starting of from the given `root`.
#[extend]
pub fn resolve_sequence<'a>(
    self: &TrackedEngine<'_>,
    simple_path: impl Iterator<Item = Identifier>,
    referring_site: Global<ID>,
    root: Global<ID>,
    use_imports: bool,
    handler: &dyn Handler<Diagnostic>,
) -> Option<Global<ID>> {
    let mut lastest_resolution = root;
    for identifier in simple_path {
        let Some(new_id) = self.get_member_of(
            lastest_resolution,
            use_imports,
            identifier.kind.0.as_str(),
        ) else {
            handler.receive(Diagnostic::SymbolNotFound(SymbolNotFound {
                searched_item_id: Some(lastest_resolution),
                resolution_span: identifier.span,
                name: identifier.kind.0,
            }));

            return None;
        };

        // non-fatal error, no need to return early
        if !self.symbol_accessible(referring_site, new_id) {
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
pub fn get_member_of(
    self: &TrackedEngine<'_>,
    id: Global<ID>,
    use_imports: bool,
    member_name: &str,
) -> Option<Global<ID>> {
    let symbol_kind = self.get_kind(id);

    if let Some(Some(test)) = symbol_kind.has_member().then(|| {
        self.get_members(id).member_ids_by_name.get(member_name).copied()
    }) {
        return Some(Global::new(id.target_id, test));
    }

    match (symbol_kind == Kind::Module, symbol_kind.is_adt(), use_imports) {
        (true, false, true) => {
            self.get_imports(id).get(member_name).map(|x| x.id)
        }

        // serach for the member of implementations
        (false, true, _) => {
            /*
            let implements = self.get::<Implemented>(id);

            for implementation_id in implements.iter().copied() {
                if let Some(id) =
                    self.get::<Member>(implementation_id).get(member_name)
                {
                    return Some(GlobalID::new(
                        implementation_id.target_id,
                        *id,
                    ));
                }
            }

            None
            */
            None
        }

        _ => None,
    }
}
