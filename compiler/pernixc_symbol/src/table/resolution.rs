use pernixc_lexical::token::Identifier;
use pernixc_source::SourceElement;
use pernixc_syntax::syntax_tree::QualifiedIdentifier;
use pernixc_system::{arena, diagnostic::Handler};

use super::{drafting::ImplementsSyntaxTreeWithModuleID, Error, FatalSemanticError, Table};
use crate::{
    error::{self, ResolutionAmbiguity, SymbolNotFound},
    GlobalID, Module, ScopedID, Trait, TraitMemberID, ID,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(super) enum SearchResult<T, E> {
    Continue,
    StopOk(T),
    StopErr(E),
}

impl Table {
    /// Resolves the symbol from the current scope and using modules.
    pub(super) fn first_resolution(
        &self,
        identifier: &Identifier,
        scoped_id: ScopedID,
        handler: &impl Handler<error::Error>,
    ) -> super::Result<Option<GlobalID>> {
        let parent_module = self
            .get_parent_module_id(scoped_id.into())
            .ok_or(arena::Error)?;

        let search_locations = self.modules[parent_module]
            .usings
            .iter()
            .copied()
            .map(ScopedID::from)
            .chain(std::iter::once(scoped_id));

        let mut candidates = Vec::new();
        for location in search_locations {
            if let Some(id) = self
                .get_scoped(location)
                .ok_or(arena::Error)?
                .get_child_id_by_name(identifier.span.str())
            {
                candidates.push(id);
            }
        }

        match candidates.len() {
            1 => Ok(Some(candidates[0])),
            0 => Ok(None),
            _ => {
                handler.recieve(error::Error::ResolutionAmbiguity(ResolutionAmbiguity {
                    span: identifier.span.clone(),
                    candidates,
                }));
                Err(super::Error::FatalSementic(FatalSemanticError))
            }
        }
    }

    /// Iterates down through the scope heirarchies and tries to find a symbol with the given name.
    pub(super) fn second_resolution(
        &self,
        identifier: &Identifier,
        mut scoped_id: ScopedID,
        handler: &impl Handler<error::Error>,
    ) -> Result<GlobalID, Error> {
        loop {
            if let Some(id) = self
                .get_scoped(scoped_id)
                .ok_or(arena::Error)?
                .get_child_id_by_name(identifier.span.str())
            {
                return Ok(id);
            }

            // no more scope to search
            let Some(parent_symbol) = self
                .get_scoped(scoped_id)
                .ok_or(arena::Error)?
                .parent_symbol()
            else {
                handler.recieve(error::Error::SymbolNotFound(SymbolNotFound {
                    span: identifier.span.clone()
                }));

                return Err(super::Error::FatalSementic(FatalSemanticError));
            };

            scoped_id = parent_symbol
                .try_into()
                .expect("parent symbol must be one of the scoped id");
        }
    }

    /// Gets the module ID that contains the given symbol ID.
    pub(super) fn get_parent_module_id(&self, mut id: ID) -> Option<arena::ID<Module>> {
        loop {
            // If the ID is a module ID, return it.
            if let ID::Module(module_id) = id {
                return Some(module_id);
            }

            let Some(parent_id) = self.get_symbol(id)?.parent_symbol() else {
                panic!("should have found parent module ID already!");
            };

            id = parent_id;
        }
    }

    pub(super) fn resolve_trait_path(
        &self,
        qualified_identifier: &QualifiedIdentifier,
        scoped_id: ScopedID,
        handler: &impl Handler<error::Error>,
    ) -> super::Result<arena::ID<Trait>> {
        match (
            qualified_identifier.leading_scope_separator.is_some(),
            qualified_identifier.rest.is_empty(),
        ) {
            (false, true) => {
                // first pass fail
                let found_symbol_id = if let Some(id) = self.first_resolution(
                    &qualified_identifier.first.identifier,
                    scoped_id,
                    handler,
                )? {
                    id
                } else {
                    self.second_resolution(
                        &qualified_identifier.first.identifier,
                        scoped_id,
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

            _ => todo!(),
        }
    }

    pub(super) fn attach_implements(
        &mut self,
        implements_syntax_tree_with_module_ids: Vec<ImplementsSyntaxTreeWithModuleID>,
    ) {
    }
}
