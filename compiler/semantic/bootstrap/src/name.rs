//! Defines the [`Name`] type.
use extend::ext;
use flexstr::SharedStr;
use pernixc_handler::Handler;
use pernixc_query::{Engine, Value};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_syntax::{Identifier, SimplePath, SimplePathRoot};
use pernixc_target::Global;

use crate::{
    accessibility::Ext as _,
    diagnostic::{Diagnostic, SymbolIsNotAccessible, SymbolNotFound},
    import::Ext as _,
    kind::Ext as _,
    member::Ext as _,
    parent::Ext as _,
    symbol::{self, ID},
    target::{Ext as _, MapExt as _},
};

/// A simple name identifier given to a symbol.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Default,
    Serialize,
    Deserialize,
    derive_more::DerefMut,
    derive_more::Deref,
    Value,
    StableHash,
)]
#[id(Global<symbol::ID>)]
pub struct Name(pub SharedStr);

/// Extension trait related to retrieving the name of a symbol.
#[ext(name = Ext)]
pub impl Engine {
    /// Returns the name of the symbol with the given ID.
    fn get_name(&self, id: Global<symbol::ID>) -> Name {
        self.query(&Key(id)).expect("should have no cyclic dependencies")
    }

    /// Gets the qualified name of the symbol such as `module::function`.
    fn get_qualified_name(&self, mut id: Global<symbol::ID>) -> String {
        let mut qualified_name = String::new();

        loop {
            let current_name = self.get_name(id);

            if qualified_name.is_empty() {
                qualified_name.push_str(&current_name);
            } else {
                qualified_name.insert_str(0, "::");
                qualified_name.insert_str(0, &current_name);
            }

            if let Some(parent_id) = *self.get_parent(id) {
                id = Global::new(id.target_id, parent_id);
            } else {
                break;
            }
        }

        qualified_name
    }

    /// Gets the [`Global<symbol::ID>`] of the symbol with the given sequence of
    /// qualified names.
    fn get_by_qualified_name<'a>(
        &self,
        qualified_names: impl IntoIterator<Item = &'a str>,
    ) -> Option<Global<symbol::ID>> {
        let mut current_id: Option<Global<symbol::ID>> = None;

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
                    current_id = self
                        .get_target_map()
                        .get(name)
                        .map(|&x| Global::new(x, ID::ROOT_MODULE));
                }
            }
        }

        current_id
    }

    /// Resolves a [`SimplePath`] as a [`GlobalID`].
    fn resolve_simple_path(
        &self,
        simple_path: &SimplePath,
        referring_site: Global<symbol::ID>,
        start_from_root: bool,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Option<Global<symbol::ID>> {
        // simple path should always have root tough
        let root: Global<symbol::ID> = match simple_path.root()? {
            SimplePathRoot::Target(_) => {
                Global::new(referring_site.target_id, ID::ROOT_MODULE)
            }
            SimplePathRoot::Identifier(ident) => {
                if start_from_root {
                    let target_map = self.get_target_map();
                    let Some(target_id) = target_map
                        .get(ident.kind.0.as_str())
                        .copied()
                        .filter(|x| {
                            x == &referring_site.target_id || {
                                let target =
                                    self.get_target(referring_site.target_id);

                                target.linked_targets.contains(x)
                            }
                        })
                    else {
                        handler.receive(Box::new(SymbolNotFound {
                            searched_item_id: None,
                            resolution_span: ident.span,
                            name: ident.kind.0,
                        }));

                        return None;
                    };

                    Global::new(target_id, ID::ROOT_MODULE)
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
                        handler.receive(Box::new(SymbolNotFound {
                            searched_item_id: Some(global_closest_module_id),
                            resolution_span: ident.span,
                            name: ident.kind.0,
                        }));

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
    }

    /// Resolves a sequence of identifier starting of from the given `root`.
    fn resolve_sequence<'a>(
        &self,
        simple_path: impl Iterator<Item = Identifier>,
        referring_site: Global<symbol::ID>,
        root: Global<symbol::ID>,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Option<Global<symbol::ID>> {
        let mut lastest_resolution = root;
        for identifier in simple_path {
            let Some(new_id) = self
                .get_member_of(lastest_resolution, identifier.kind.0.as_str())
            else {
                handler.receive(Box::new(SymbolNotFound {
                    searched_item_id: Some(lastest_resolution),
                    resolution_span: identifier.span,
                    name: identifier.kind.0,
                }));

                return None;
            };

            // non-fatal error, no need to return early
            if !self.symbol_accessible(referring_site, new_id) {
                handler.receive(Box::new(SymbolIsNotAccessible {
                    referring_site,
                    referred: new_id,
                    referred_span: identifier.span,
                }));
            }

            lastest_resolution = new_id;
        }

        Some(lastest_resolution)
    }
}
