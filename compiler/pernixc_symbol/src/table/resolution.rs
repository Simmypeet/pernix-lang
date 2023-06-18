use std::collections::HashSet;

use pernixc_lexical::token::Identifier;
use pernixc_source::SourceElement;
use pernixc_syntax::syntax_tree::QualifiedIdentifier;
use pernixc_system::{arena, diagnostic::Handler};

use super::{drafting::ImplementsSyntaxTreeWithModuleID, Error, FatalSemanticError, Table};
use crate::{
    error::{self, ResolutionAmbiguity},
    GlobalID, ScopedID, Trait, ID,
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
                }));
                return Err(Error::FatalSementic(FatalSemanticError));
            }
        }
    }

    pub(super) fn resolve_trait_path(
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
                // first pass fail
                let found_symbol_id = if let Some(id) = self.first_resolution(
                    &qualified_identifier.first.identifier,
                    referrer,
                    handler,
                )? {
                    id
                } else {
                    self.second_resolution(
                        &qualified_identifier.first.identifier,
                        referrer,
                        handler,
                    )?
                };

                if let GlobalID::Trait(id) = found_symbol_id {
                    Ok(id)
                } else {
                    handler.recieve(error::Error::TraitExpected(error::TraitExpected {
                        span: qualified_identifier.first.identifier.span.clone(),
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
                let mut current_id = {
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
                    } else {
                    };
                };

                todo!()
            }
        }
    }

    pub(super) fn attach_implements(
        &mut self,
        implements_syntax_tree_with_module_ids: Vec<ImplementsSyntaxTreeWithModuleID>,
    ) {
    }
}
