//! Contains the resolution logic for the table.

use diagnostic::{
    NoGenericArgumentsRequired, SymbolIsNotAccessible, SymbolNotFound,
    ThisNotFound,
};
use pernixc_handler::Handler;
use pernixc_lexical::token::Identifier;
use pernixc_source_file::SourceElement;
use pernixc_syntax::syntax_tree::{
    QualifiedIdentifier, QualifiedIdentifierRoot, SimplePath, SimplePathRoot,
};

use super::{GlobalID, Representation, ID};
use crate::{
    component::{Import, Member, SymbolKind},
    diagnostic::{Diagnostic, ExpectModule},
};

pub mod diagnostic;

impl Representation {
    /// Resolves a [`SimplePath`] as a [`GlobalID`].
    pub fn resolve_simple_path(
        &self,
        simple_path: &SimplePath,
        referring_site: GlobalID,
        start_from_root: bool,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Option<GlobalID> {
        let root: GlobalID = match &simple_path.root {
            SimplePathRoot::Target(_) => {
                GlobalID::new(referring_site.target_id, ID::ROOT_MODULE)
            }
            SimplePathRoot::Identifier(ident) => {
                if start_from_root {
                    let Some(id) = self
                        .targets_by_name
                        .get(ident.span.str())
                        .copied()
                        .filter(|x| {
                            self.targets_by_id
                                .get(&referring_site.target_id)
                                .is_some_and(|y| y.linked_targets.contains(x))
                                || x == &referring_site.target_id
                        })
                    else {
                        handler.receive(Box::new(SymbolNotFound {
                            searched_item_id: None,
                            resolution_span: ident.span.clone(),
                        }));

                        return None;
                    };

                    GlobalID::new(id, ID::ROOT_MODULE)
                } else {
                    let closet_module_id =
                        self.get_closet_module_id(referring_site);

                    let global_closest_module_id = GlobalID::new(
                        referring_site.target_id,
                        closet_module_id,
                    );

                    let Some(id) = self
                        .storage
                        .get::<Member>(global_closest_module_id)
                        .unwrap()
                        .get(ident.span.str())
                        .map(|x| GlobalID::new(referring_site.target_id, *x))
                        .or_else(|| {
                            self.storage
                                .get::<Import>(global_closest_module_id)
                                .unwrap()
                                .get(ident.span.str())
                                .map(|x| x.id)
                        })
                    else {
                        handler.receive(Box::new(SymbolNotFound {
                            searched_item_id: Some(global_closest_module_id),
                            resolution_span: ident.span.clone(),
                        }));

                        return None;
                    };

                    id
                }
            }
        };

        self.resolve_sequence(
            simple_path.rest.iter().map(|x| &x.1),
            referring_site,
            root,
            handler,
        )
    }

    /// Resolves a sequence of identifier starting of from the given `root`.
    pub fn resolve_sequence<'a>(
        &self,
        simple_path: impl Iterator<Item = &'a Identifier>,
        referring_site: GlobalID,
        root: GlobalID,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Option<GlobalID> {
        let mut lastest_resolution = root;
        for identifier in simple_path {
            let Some(new_id) =
                self.get_member_of(lastest_resolution, identifier.span.str())
            else {
                handler.receive(Box::new(SymbolNotFound {
                    searched_item_id: Some(lastest_resolution),
                    resolution_span: identifier.span.clone(),
                }));

                return None;
            };

            // non-fatal error, no need to return early
            if !self.symbol_accessible(referring_site, new_id) {
                handler.receive(Box::new(SymbolIsNotAccessible {
                    referring_site,
                    referred: new_id,
                    referred_span: identifier.span.clone(),
                }));
            }

            lastest_resolution = new_id;
        }

        Some(lastest_resolution)
    }

    /// Resolves a [`QualifiedIdentifier`] as a [`GlobalID`]. Where all the
    /// generic arguments prohibited except for the last identifier.
    pub fn resolve_implementation_path(
        &self,
        qualified_identifier: &QualifiedIdentifier,
        current_site: GlobalID,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Option<GlobalID> {
        let mut current_id: GlobalID = match &qualified_identifier.root {
            QualifiedIdentifierRoot::Target(_) => {
                GlobalID::new(current_site.target_id, ID::ROOT_MODULE)
            }
            QualifiedIdentifierRoot::This(keyword) => {
                handler
                    .receive(Box::new(ThisNotFound { span: keyword.span() }));
                return None;
            }
            QualifiedIdentifierRoot::GenericIdentifier(generic_identifier) => {
                let search_id = GlobalID::new(
                    current_site.target_id,
                    self.get_closet_module_id(current_site),
                );

                let Some(id) = self.get_member_of(
                    search_id,
                    generic_identifier.identifier.span.str(),
                ) else {
                    handler.receive(Box::new(SymbolNotFound {
                        searched_item_id: Some(search_id),
                        resolution_span: generic_identifier
                            .identifier
                            .span
                            .clone(),
                    }));
                    return None;
                };

                id
            }
        };

        if !qualified_identifier.rest.is_empty() {
            if *self.storage.get::<SymbolKind>(current_id).unwrap()
                != SymbolKind::Module
            {
                handler.receive(Box::new(ExpectModule {
                    module_path: qualified_identifier.root.span(),
                    found_id: current_id,
                }));
                return None;
            }

            if let Some(gen_args) = qualified_identifier
                .root
                .as_generic_identifier()
                .and_then(|x| x.generic_arguments.as_ref())
            {
                handler.receive(Box::new(NoGenericArgumentsRequired {
                    global_id: current_id,
                    generic_argument_span: gen_args.span(),
                }));
            }
        }

        for (index, (_, generic_identifier)) in
            qualified_identifier.rest.iter().enumerate()
        {
            let Some(next_id) = self.get_member_of(
                current_id,
                generic_identifier.identifier.span.str(),
            ) else {
                handler.receive(Box::new(SymbolNotFound {
                    searched_item_id: Some(current_id),
                    resolution_span: generic_identifier.identifier.span.clone(),
                }));
                return None;
            };

            // non-fatal error, no need to return early
            if !self.symbol_accessible(current_site, next_id) {
                handler.receive(Box::new(SymbolIsNotAccessible {
                    referring_site: current_site,
                    referred: next_id,
                    referred_span: generic_identifier.identifier.span.clone(),
                }));
            }

            if index == qualified_identifier.rest.len() - 1 {
                current_id = next_id;
            } else {
                if *self.storage.get::<SymbolKind>(next_id).unwrap()
                    != SymbolKind::Module
                {
                    handler.receive(Box::new(ExpectModule {
                        module_path: generic_identifier.identifier.span.clone(),
                        found_id: next_id,
                    }));
                    return None;
                }

                if let Some(gen_args) =
                    generic_identifier.generic_arguments.as_ref()
                {
                    handler.receive(Box::new(NoGenericArgumentsRequired {
                        global_id: next_id,
                        generic_argument_span: gen_args.span(),
                    }));
                }
            }
        }

        Some(current_id)
    }
}
