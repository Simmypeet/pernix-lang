use std::collections::{HashMap, HashSet};

use pernixc_lexical::token::Identifier;
use pernixc_source::SourceElement;
use pernixc_syntax::syntax_tree::QualifiedIdentifier;
use pernixc_system::{arena, diagnostic::Handler};

use super::{drafting::ImplementsSyntaxTreeWithModuleID, Error, FatalSemanticError, Table};
use crate::{
    error::{self, ResolutionAmbiguity, TargetNotFound},
    Generics, GlobalID, Implements, Scoped, ScopedID, Substitution, Trait, ID,
};

impl Table {
    /// Resolves the symbol from the current scope and using modules.
    pub(super) fn first_resolution(
        &self,
        identifier: &Identifier,
        referrer: ID,
        handler: &impl Handler<error::Error>,
    ) -> Result<Option<GlobalID>, Error> {
        let parent_module = self.get_current_module_id(referrer)?;

        let search_locations = self.modules[parent_module]
            .usings
            .iter()
            .copied()
            .map(ScopedID::from)
            .chain(std::iter::once(self.get_current_scoped_id(referrer)?));

        let mut candidates = HashSet::new();
        for location in search_locations {
            if let Some(id) = self
                .get_scoped(location)?
                .get_child_id_by_name(identifier.span.str())
            {
                if self.symbol_accessible(id, referrer).unwrap() {
                    candidates.insert(id);
                }
            }
        }

        match candidates.len() {
            1 => Ok(Some(candidates.into_iter().next().unwrap())),
            0 => Ok(None),
            _ => {
                handler.recieve(error::Error::ResolutionAmbiguity(ResolutionAmbiguity {
                    span: identifier.span.clone(),
                    candidates: candidates.into_iter().collect(),
                }));
                Err(super::Error::FatalSementic(FatalSemanticError))
            }
        }
    }

    /// Iterates down through the scope heirarchies and tries to find a symbol with the given name.
    pub(super) fn second_resolution(
        &self,
        identifier: &Identifier,
        mut referrer: ID,
        handler: &impl Handler<error::Error>,
    ) -> Result<GlobalID, Error> {
        // NOTE: Accessibility is not checked here because the symbol serching is done within the
        // same module ancestor tree.

        loop {
            // try to find the symbol in the current scope
            if let Ok(scoped_id) = referrer.try_into() {
                if let Some(id) = self
                    .get_scoped(scoped_id)?
                    .get_child_id_by_name(identifier.span.str())
                {
                    return Ok(id);
                }
            }

            if let Some(parent_id) = self.get_symbol(referrer)?.parent_symbol() {
                referrer = parent_id;
            } else {
                handler.recieve(error::Error::SymbolNotFound(error::SymbolNotFound {
                    span: identifier.span.clone(),
                    searched_scoped_id: referrer
                        .try_into()
                        .expect("It should have been some kind of `Scoped` by now"),
                }));
                return Err(Error::FatalSementic(FatalSemanticError));
            }
        }
    }

    pub(super) fn resolve_root(
        &self,
        identifier: &Identifier,
        referrer: ID,
        handler: &impl Handler<error::Error>,
    ) -> Result<GlobalID, Error> {
        let found_symbol_id =
            if let Some(id) = self.first_resolution(identifier, referrer, handler)? {
                id
            } else {
                self.second_resolution(identifier, referrer, handler)?
            };

        Ok(found_symbol_id)
    }

    #[allow(clippy::too_many_lines)]
    fn resolve_trait_path(
        &self,
        qualified_identifier: &QualifiedIdentifier,
        referrer: ID,
        handler: &impl Handler<error::Error>,
    ) -> Result<arena::ID<Trait>, Error> {
        match (
            qualified_identifier.leading_scope_separator.is_some(),
            qualified_identifier.rest.is_empty(),
        ) {
            // single identifier part with no leading scope separator
            (false, true) => {
                let found_symbol_id =
                    self.resolve_root(&qualified_identifier.first.identifier, referrer, handler)?;

                if let GlobalID::Trait(id) = found_symbol_id {
                    Ok(id)
                } else {
                    handler.recieve(error::Error::TraitExpected(error::TraitExpected {
                        span: qualified_identifier.first.identifier.span.clone(),
                        found: found_symbol_id,
                    }));
                    Err(Error::FatalSementic(FatalSemanticError))
                }
            }

            // it's impossible to have a trait at the root scope
            (true, true) => {
                handler.recieve(error::Error::InvalidTraitPath(error::InvalidTraitPath {
                    span: qualified_identifier.span()?,
                }));
                Err(Error::FatalSementic(FatalSemanticError))
            }

            // normal case
            _ => {
                let mut generic_identifiers = qualified_identifier.generic_identifiers().peekable();

                // resolve the root 💀
                let mut current_module_id = {
                    // first generic identifier
                    let generic_identifier = generic_identifiers
                        .next()
                        .expect("qualified identifier must at least have one identifier");

                    // check if the path is invalid
                    if generic_identifier.generic_arguments.is_some() {
                        handler.recieve(error::Error::InvalidTraitPath(error::InvalidTraitPath {
                            span: generic_identifier.span()?,
                        }));
                    }

                    if qualified_identifier.leading_scope_separator.is_some() {
                        if let Some(id) = self
                            .target_root_module_ids_by_name
                            .get(generic_identifier.identifier.span.str())
                            .copied()
                        {
                            id
                        } else {
                            handler.recieve(error::Error::TargetNotFound(TargetNotFound {
                                unknown_target_span: generic_identifier.identifier.span.clone(),
                            }));
                            return Err(Error::FatalSementic(FatalSemanticError));
                        }
                    } else {
                        let resolved_symbol_id =
                            self.resolve_root(&generic_identifier.identifier, referrer, handler)?;

                        if let GlobalID::Module(id) = resolved_symbol_id {
                            id
                        } else {
                            handler.recieve(error::Error::ModuleExpected(error::ModuleExpected {
                                span: generic_identifier.span()?,
                                found: resolved_symbol_id,
                            }));
                            return Err(Error::FatalSementic(FatalSemanticError));
                        }
                    }
                };

                let trait_id = loop {
                    if let Some(generic_identifier) = generic_identifiers.next() {
                        let is_last = generic_identifiers.peek().is_none();

                        // expect only module if the current generic identifier lookup is not the
                        // last one
                        if !is_last && generic_identifier.generic_arguments.is_some() {
                            handler.recieve(error::Error::InvalidTraitPath(
                                error::InvalidTraitPath {
                                    span: generic_identifier.span()?,
                                },
                            ));
                        }

                        // expect a symbol
                        let Some(id) = self.modules[current_module_id]
                            .get_child_id_by_name(generic_identifier.identifier.span.str())
                        else {
                            handler.recieve(error::Error::SymbolNotFound(error::SymbolNotFound {
                                searched_scoped_id: current_module_id.into(),
                                span: generic_identifier.span()?,
                            }));
                            return Err(Error::FatalSementic(FatalSemanticError));
                        };

                        match (is_last, id) {
                            (true, GlobalID::Trait(trait_id)) => break trait_id,
                            (false, GlobalID::Module(module_id)) => {
                                current_module_id = module_id;
                            }
                            (is_last, found) => {
                                handler.recieve(if is_last {
                                    error::Error::TraitExpected(error::TraitExpected {
                                        span: generic_identifier.span()?,
                                        found,
                                    })
                                } else {
                                    error::Error::ModuleExpected(error::ModuleExpected {
                                        span: generic_identifier.span()?,
                                        found,
                                    })
                                });
                                return Err(Error::FatalSementic(FatalSemanticError));
                            }
                        }
                    } else {
                        unreachable!()
                    }
                };

                Ok(trait_id)
            }
        }
    }

    /// Attaches implements blocks to their corresponding trait.
    pub(super) fn attach_implements(
        &mut self,
        implements_syntax_tree_with_module_ids: Vec<ImplementsSyntaxTreeWithModuleID>,
        handler: &impl Handler<error::Error>,
    ) {
        for implements_syntax_tree_with_module_id in implements_syntax_tree_with_module_ids {
            let trait_id = match self.resolve_trait_path(
                &implements_syntax_tree_with_module_id
                    .implements
                    .signature
                    .qualified_identifier,
                implements_syntax_tree_with_module_id.module_id.into(),
                handler,
            ) {
                Ok(trait_id) => trait_id,
                Err(err) => {
                    assert!(err.as_fatal_sementic().is_some());
                    continue;
                }
            };

            let implements_id = self.implements.push(Implements {
                generics: Generics::default(),
                trait_id,
                substitution: Substitution::default(), // will be filled later
                implements_types_by_associated_type: HashMap::new(), // will be filled later
                implements_functions_by_trait_function: HashMap::new(), // will be filled later
            });

            // assigns generic parameters
            if let Some(generic_parameters) = implements_syntax_tree_with_module_id
                .implements
                .signature
                .generic_parameters
                .as_ref()
            {
                let generic_parameters = self.create_generic_parameters(
                    implements_id.into(),
                    generic_parameters,
                    handler,
                );

                self.implements[implements_id].generics.generic_parameters = generic_parameters;
            }

            self.traits[trait_id].implements.push(implements_id);
        }
    }
}
