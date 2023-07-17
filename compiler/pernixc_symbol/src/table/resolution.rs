use std::collections::{HashMap, HashSet};

use pernixc_lexical::token::Identifier;
use pernixc_source::SourceElement;
use pernixc_syntax::syntax_tree::{self, QualifiedIdentifier};
use pernixc_system::{arena, diagnostic::Handler};

use super::{drafting::ImplementsSyntaxTreeWithModuleID, Table};
use crate::{
    error::{self, LifetimeNotFound, ResolutionAmbiguity, TargetNotFound},
    table, GenericableID, Generics, GlobalID, Implements, LifetimeArgument, LifetimeParameter,
    Scoped, ScopedID, Substitution, Trait, ID,
};

impl Table {
    /// Resolves the symbol from the current scope and using modules.
    pub(super) fn first_resolution(
        &self,
        identifier: &Identifier,
        referring_site: ID,
        handler: &impl Handler<error::Error>,
    ) -> Result<Option<GlobalID>, table::Error> {
        let parent_module = self.get_current_module_id(referring_site)?;

        let search_locations = self.modules[parent_module]
            .usings
            .iter()
            .copied()
            .map(ScopedID::from)
            .chain(std::iter::once(self.get_current_scoped_id(referring_site)?));

        let mut candidates = HashSet::new();
        for location in search_locations {
            if let Some(id) = self
                .get_scoped(location)?
                .get_child_id_by_name(identifier.span.str())
            {
                if self.symbol_accessible(id, referring_site).unwrap() {
                    candidates.insert(id);
                }
            }
        }

        match candidates.len() {
            1 => Ok(Some(candidates.into_iter().next().unwrap())),
            0 => Ok(None),
            _ => {
                handler.receive(error::Error::ResolutionAmbiguity(ResolutionAmbiguity {
                    span: identifier.span.clone(),
                    candidates: candidates.into_iter().collect(),
                }));
                Err(super::Error::FatalSemantic)
            }
        }
    }

    /// Iterates down through the scope hierarchies and tries to find a symbol with the given name.
    pub(super) fn second_resolution(
        &self,
        identifier: &Identifier,
        mut referring_site: ID,
        handler: &impl Handler<error::Error>,
    ) -> Result<GlobalID, table::Error> {
        // NOTE: Accessibility is not checked here because the symbol searching is done within the
        // same module ancestor tree.

        loop {
            // try to find the symbol in the current scope
            if let Ok(scoped_id) = referring_site.try_into() {
                if let Some(id) = self
                    .get_scoped(scoped_id)?
                    .get_child_id_by_name(identifier.span.str())
                {
                    return Ok(id);
                }
            }

            if let Some(parent_id) = self.get_symbol(referring_site)?.parent_symbol() {
                referring_site = parent_id;
            } else {
                handler.receive(error::Error::SymbolNotFound(error::SymbolNotFound {
                    span: identifier.span.clone(),
                    searched_scoped_id: referring_site
                        .try_into()
                        .expect("It should have been some kind of `Scoped` by now"),
                }));
                return Err(table::Error::FatalSemantic);
            }
        }
    }

    pub(super) fn resolve_root(
        &self,
        identifier: &Identifier,
        referring_site: ID,
        handler: &impl Handler<error::Error>,
    ) -> Result<GlobalID, table::Error> {
        let found_symbol_id =
            if let Some(id) = self.first_resolution(identifier, referring_site, handler)? {
                id
            } else {
                self.second_resolution(identifier, referring_site, handler)?
            };

        Ok(found_symbol_id)
    }

    #[allow(clippy::too_many_lines)]
    fn resolve_trait_path(
        &self,
        qualified_identifier: &QualifiedIdentifier,
        referrer: ID,
        handler: &impl Handler<error::Error>,
    ) -> Result<arena::ID<Trait>, table::Error> {
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
                    handler.receive(error::Error::TraitExpected(error::TraitExpected {
                        span: qualified_identifier.first.identifier.span.clone(),
                        found: found_symbol_id,
                    }));
                    Err(table::Error::FatalSemantic)
                }
            }

            // it's impossible to have a trait at the root scope
            (true, true) => {
                handler.receive(error::Error::InvalidTraitPath(error::InvalidTraitPath {
                    span: qualified_identifier.span()?,
                }));
                Err(table::Error::FatalSemantic)
            }

            // normal case
            _ => {
                let mut generic_identifiers = qualified_identifier.generic_identifiers().peekable();

                let mut current_module_id = {
                    // first generic identifier
                    let generic_identifier = generic_identifiers
                        .next()
                        .expect("qualified identifier must at least have one identifier");

                    // check if the path is invalid
                    if generic_identifier.generic_arguments.is_some() {
                        handler.receive(error::Error::InvalidTraitPath(error::InvalidTraitPath {
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
                            handler.receive(error::Error::TargetNotFound(TargetNotFound {
                                unknown_target_span: generic_identifier.identifier.span.clone(),
                            }));
                            return Err(table::Error::FatalSemantic);
                        }
                    } else {
                        let resolved_symbol_id =
                            self.resolve_root(&generic_identifier.identifier, referrer, handler)?;

                        if let GlobalID::Module(id) = resolved_symbol_id {
                            id
                        } else {
                            handler.receive(error::Error::ModuleExpected(error::ModuleExpected {
                                span: generic_identifier.span()?,
                                found: resolved_symbol_id,
                            }));
                            return Err(table::Error::FatalSemantic);
                        }
                    }
                };

                let trait_id = loop {
                    if let Some(generic_identifier) = generic_identifiers.next() {
                        let is_last = generic_identifiers.peek().is_none();

                        // expect only module if the current generic identifier lookup is not the
                        // last one
                        if !is_last && generic_identifier.generic_arguments.is_some() {
                            handler.receive(error::Error::InvalidTraitPath(
                                error::InvalidTraitPath {
                                    span: generic_identifier.span()?,
                                },
                            ));
                        }

                        // expect a symbol
                        let Some(id) = self.modules[current_module_id]
                            .get_child_id_by_name(generic_identifier.identifier.span.str())
                        else {
                            handler.receive(error::Error::SymbolNotFound(error::SymbolNotFound {
                                searched_scoped_id: current_module_id.into(),
                                span: generic_identifier.span()?,
                            }));
                            return Err(table::Error::FatalSemantic);
                        };

                        match (is_last, id) {
                            (true, GlobalID::Trait(trait_id)) => break trait_id,
                            (false, GlobalID::Module(module_id)) => {
                                current_module_id = module_id;
                            }
                            (is_last, found) => {
                                handler.receive(if is_last {
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
                                return Err(table::Error::FatalSemantic);
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

    pub(super) fn resolve_lifetime_argument(
        &self,
        current_id: ID,
        lifetime_argument: &syntax_tree::LifetimeArgument,
        handler: &impl Handler<error::Error>,
    ) -> Result<LifetimeArgument, table::Error> {
        match &lifetime_argument.identifier {
            syntax_tree::LifetimeArgumentIdentifier::Identifier(lifetime_parameter) => {
                Ok(LifetimeArgument::Parameter(
                    self.resolve_lifetime_parameter(current_id, lifetime_parameter, handler)?,
                ))
            }
            syntax_tree::LifetimeArgumentIdentifier::Static(_) => Ok(LifetimeArgument::Static),
        }
    }

    pub(super) fn resolve_lifetime_parameter(
        &self,
        current_id: ID,
        lifetime_parameter: &Identifier,
        handler: &impl Handler<error::Error>,
    ) -> Result<arena::ID<LifetimeParameter>, table::Error> {
        let scope_walker = self.scope_walker(current_id)?;

        for id in scope_walker {
            let Ok(genericable_id) = GenericableID::try_from(id) else {
                continue;
            };

            let genericable = self.get_genericable(genericable_id)?;
            if let Some(lifetime_parameter_id) = genericable
                .generic_parameters()
                .lifetime_parameter_ids_by_name
                .get(lifetime_parameter.span.str())
                .copied()
            {
                return Ok(lifetime_parameter_id);
            }
        }

        handler.receive(error::Error::LifetimeNotFound(LifetimeNotFound {
            unknown_lifetime_span: lifetime_parameter.span.clone(),
        }));
        Err(table::Error::FatalSemantic)
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
                    assert!(err.is_fatal_semantic());
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

                self.implements[implements_id].generics.parameters = generic_parameters;
            }

            self.traits[trait_id].implements.push(implements_id);
        }
    }
}
