//! Contains the logic for resolving symbols and types.

use std::convert::Into;

use pernixc_base::diagnostic::Handler;
use pernixc_lexical::token::Identifier;
use pernixc_syntax::syntax_tree::item::ModulePath;

use super::{Index, State, Table};
use crate::{
    arena::ID,
    error::{self, ExpectModule, ResolutionAmbiguity, SymbolNotFound},
    semantic::term::GenericArguments,
    symbol::{
        self, AdtImplementationConstant, AdtImplementationFunction,
        AdtImplementationType, Constant, Enum, Function, GlobalID, Module,
        Struct, Trait, TraitConstant, TraitFunction,
        TraitImplementationConstant, TraitImplementationFunction,
        TraitImplementationType, TraitType, Type,
    },
};

/// An enumeration of the symbols that accepts generic parameters and are not a
/// member of another symbol.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum GenericID {
    Struct(ID<Struct>),
    Enum(ID<Enum>),
    Function(ID<Function>),
    Trait(ID<Trait>),
    Constant(ID<Constant>),
    Type(ID<Type>),

    /*
    Even though the the trait implementation symbols are members of the trait
    implementation, they can never be resolved directly using the full path
    of the trait implementation. They can only be resolved by referencing from
    the trait implementation scope, which doesn't require supplying the generic
    arguments of the trait implementation.
     */
    TraitImplementationFunction(ID<TraitImplementationFunction>),
    TraitImplementationType(ID<TraitImplementationType>),
    TraitImplementationConstant(ID<TraitImplementationConstant>),
}

impl From<GenericID> for GlobalID {
    fn from(value: GenericID) -> Self {
        match value {
            GenericID::Struct(id) => Self::Struct(id),
            GenericID::Enum(id) => Self::Enum(id),
            GenericID::Function(id) => Self::Function(id),
            GenericID::Trait(id) => Self::Trait(id),
            GenericID::Constant(id) => Self::Constant(id),
            GenericID::Type(id) => Self::Type(id),
            GenericID::TraitImplementationFunction(id) => {
                Self::TraitImplementationFunction(id)
            }
            GenericID::TraitImplementationType(id) => {
                Self::TraitImplementationType(id)
            }
            GenericID::TraitImplementationConstant(id) => {
                Self::TraitImplementationConstant(id)
            }
        }
    }
}

impl From<GenericID> for symbol::GenericID {
    fn from(value: GenericID) -> Self {
        match value {
            GenericID::Struct(id) => Self::Struct(id),
            GenericID::Enum(id) => Self::Enum(id),
            GenericID::Function(id) => Self::Function(id),
            GenericID::Trait(id) => Self::Trait(id),
            GenericID::Constant(id) => Self::Constant(id),
            GenericID::Type(id) => Self::Type(id),
            GenericID::TraitImplementationFunction(id) => {
                Self::TraitImplementationFunction(id)
            }
            GenericID::TraitImplementationType(id) => {
                Self::TraitImplementationType(id)
            }
            GenericID::TraitImplementationConstant(id) => {
                Self::TraitImplementationConstant(id)
            }
        }
    }
}

/// A resolution for a symbol that accepts generic parameters and is not a
/// member of another symbol.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Generic {
    /// The resolved symbol.
    pub id: GenericID,

    /// The generic arguments supplied to the symbol.
    pub generic_arguments: GenericArguments,
}

/// An enumeration of the symbols that accepts generic parameters for both
/// the symbol and the parent symbol that it is a member of.
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum MemberGenericID {
    TraitFunction(ID<TraitFunction>),
    TraitType(ID<TraitType>),
    TraitConstant(ID<TraitConstant>),
    AdtImplementationFunction(ID<AdtImplementationFunction>),
    AdtImplementationType(ID<AdtImplementationType>),
    AdtImplementationConstant(ID<AdtImplementationConstant>),
}

impl From<MemberGenericID> for GlobalID {
    fn from(value: MemberGenericID) -> Self {
        match value {
            MemberGenericID::TraitFunction(id) => Self::TraitFunction(id),
            MemberGenericID::TraitType(id) => Self::TraitType(id),
            MemberGenericID::TraitConstant(id) => Self::TraitConstant(id),
            MemberGenericID::AdtImplementationFunction(id) => {
                Self::AdtImplementationFunction(id)
            }
            MemberGenericID::AdtImplementationType(id) => {
                Self::AdtImplementationType(id)
            }
            MemberGenericID::AdtImplementationConstant(id) => {
                Self::AdtImplementationConstant(id)
            }
        }
    }
}

/// A resolution for a symbol that accepts generic parameters for both the
/// symbol and the parent symbol that it is a member of.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MemberGeneric {
    /// The resolved symbol.
    pub id: MemberGenericID,

    /// The generic arguments supplied to the parent symbol.
    pub parent_generic_arguments: GenericArguments,

    /// The generic arguments supplied to the symbol.
    pub generic_arguments: GenericArguments,
}

/// A resolution for an enum-variant.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Variant {
    /// The resolved variant.
    pub variant: ID<symbol::Variant>,

    /// The generic arguments supplied to the enum.
    pub generic_arguments: GenericArguments,
}

/// Represents a resolution for a symbol.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Resolution {
    Module(ID<Module>),
    Variant(Variant),
    Generic(Generic),
    MemberGeneric(MemberGeneric),
}

impl Resolution {
    /// Gets the [`GlobalID`] of the resolved symbol.
    #[must_use]
    pub fn global_id(&self) -> GlobalID {
        match self {
            Self::Module(id) => GlobalID::Module(*id),
            Self::Variant(variant) => GlobalID::Variant(variant.variant),
            Self::Generic(generic) => generic.id.into(),
            Self::MemberGeneric(member_generic) => member_generic.id.into(),
        }
    }
}

/// An error that can occur when resolving a symbol.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[allow(missing_docs)]
pub enum Error {
    #[error("The given `referring_site` id does not exist in the table")]
    InvalidReferringSiteID,

    #[error("Encountered a fatal semantic error that aborts the process")]
    SemanticError,
}

/// An error returned by [`Table::resolve_simple_path`].
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[allow(missing_docs)]
pub enum ResolveSimplePathError {
    #[error("the given 'simple_path' iterator was empty")]
    EmptyIterator,

    #[error(transparent)]
    ResolutionError(#[from] Error),
}

impl<T: State> Table<T> {
    /// Resolves an ID from the given simple path.
    ///
    /// # Errors
    ///
    /// See [`ResolveSimplePathError`] for more information
    pub fn resolve_simple_path<'a>(
        &self,
        mut simple_path: impl Iterator<Item = &'a Identifier>,
        referring_site: GlobalID,
        always_start_from_root: bool,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<GlobalID, ResolveSimplePathError> {
        let mut lastest_resolution = {
            let Some(first_identifier) = simple_path.next() else {
                return Err(ResolveSimplePathError::EmptyIterator);
            };

            if always_start_from_root {
                self.root_module_ids_by_name
                    .get(first_identifier.span.str())
                    .copied()
                    .map(Into::into)
                    .ok_or_else(|| {
                        handler.receive(Box::new(SymbolNotFound {
                            searched_global_id: None,
                            resolution_span: first_identifier.span.clone(),
                        }));

                        Error::SemanticError
                    })?
            } else {
                self.resolve_root_relative(
                    first_identifier,
                    referring_site,
                    handler,
                )?
            }
        };

        for identifier in simple_path {
            let new_id = self
                .get_global(lastest_resolution)
                .unwrap()
                .get_member(identifier.span.str())
                .ok_or_else(|| {
                    handler.receive(Box::new(SymbolNotFound {
                        searched_global_id: Some(lastest_resolution),
                        resolution_span: identifier.span.clone(),
                    }));
                    Error::SemanticError
                })?;

            // non-fatal error, no need to return early
            if !self
                .symbol_accessible(referring_site, new_id)
                .ok_or(Error::InvalidReferringSiteID)?
            {
                handler.receive(Box::new(error::SymbolIsNotAccessible {
                    referring_site,
                    referred: new_id,
                    referred_span: identifier.span.clone(),
                }));
            }

            lastest_resolution = new_id;
        }

        Ok(lastest_resolution)
    }

    fn resolve_root_from_using_and_self(
        &self,
        identifier: &Identifier,
        referring_site: GlobalID,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Option<GlobalID>, Error> {
        let closet_module_id = self
            .get_closet_module_id(referring_site)
            .ok_or(Error::InvalidReferringSiteID)?;
        let closet_module = self.get(closet_module_id).unwrap();

        let map = closet_module.usings.iter().copied().map(Into::into);
        let search_locations = map.chain(std::iter::once(referring_site));

        let mut candidates = search_locations
            .filter_map(|x| {
                self.get_global(x)
                    .expect("should've been all valid ID")
                    .get_member(identifier.span.str())
            })
            .filter(|x| {
                self.symbol_accessible(referring_site, *x)
                    .expect("should've been a valid ID")
            });

        // if there is only one candidate, return it.
        let Some(result) = candidates.next() else {
            return Ok(None);
        };

        // if there are more than one candidate, report an ambiguity error.
        candidates.next().map_or(Ok(Some(result)), |other| {
            handler.receive(Box::new(ResolutionAmbiguity {
                resolution_span: identifier.span.clone(),
                candidates: [result, other]
                    .into_iter()
                    .chain(candidates)
                    .collect(),
            }));
            Err(Error::SemanticError)
        })
    }

    fn resolve_root_down_the_tree(
        &self,
        identifier: &Identifier,
        mut referring_site: GlobalID,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<GlobalID, Error> {
        // NOTE: Accessibility is not checked here because the symbol searching
        // is done within the same module ancestor tree.

        loop {
            // try to find the symbol in the current scope
            let global_symbol = self
                .get_global(referring_site)
                .ok_or(Error::InvalidReferringSiteID)?;

            if let Some(id) = global_symbol.get_member(identifier.span.str()) {
                return Ok(id);
            }

            if let Some(parent_id) = global_symbol.parent_global_id() {
                referring_site = parent_id;
            } else {
                // try to search the symbol in the root scope
                if let Some(id) = self
                    .root_module_ids_by_name
                    .get(identifier.span.str())
                    .copied()
                {
                    return Ok(id.into());
                }

                handler.receive(Box::new(SymbolNotFound {
                    searched_global_id: None,
                    resolution_span: identifier.span.clone(),
                }));
                return Err(Error::SemanticError);
            }
        }
    }

    /// Resolves the symbol for the first identifier appearing in the qualified
    /// identifier.
    ///
    /// # Errors
    ///
    /// See [`Error`] for more information
    pub fn resolve_root_relative(
        &self,
        identifier: &Identifier,
        referring_site: GlobalID,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<GlobalID, Error> {
        if let Some(error) = self.resolve_root_from_using_and_self(
            identifier,
            referring_site,
            handler,
        )? {
            return Ok(error);
        }

        self.resolve_root_down_the_tree(identifier, referring_site, handler)
    }

    /// Resolves an ID from the given [`ModulePath`].
    ///
    /// # Errors
    ///
    /// See [`Error`] for more information
    pub fn resolve_module_path(
        &self,
        module_path: &ModulePath,
        referring_site: GlobalID,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<ID<Module>, Error> {
        let mut current_module_id: Option<ID<Module>> = None;

        // loop through each path and resolve the module
        for path in module_path.paths() {
            let next = match current_module_id {
                Some(id) => self
                    .get(id)
                    .unwrap()
                    .module_child_ids_by_name
                    .get(path.span.str())
                    .copied()
                    .map(|x| {
                        x.into_module().map_err(|found_id| {
                            handler.receive(Box::new(ExpectModule {
                                module_path: path.span.clone(),
                                found_id: found_id.into(),
                            }));
                            Error::SemanticError
                        })
                    })
                    .transpose()?,
                None => {
                    self.root_module_ids_by_name.get(path.span.str()).copied()
                }
            };

            let Some(next) = next else {
                handler.receive(Box::new(SymbolNotFound {
                    searched_global_id: current_module_id.map(Into::into),
                    resolution_span: path.span.clone(),
                }));
                return Err(Error::SemanticError);
            };

            // check if accessible
            if !self
                .symbol_accessible(referring_site, next.into())
                .ok_or(Error::InvalidReferringSiteID)?
            {
                handler.receive(Box::new(error::SymbolIsNotAccessible {
                    referring_site,
                    referred: next.into(),
                    referred_span: path.span.clone(),
                }));
                return Err(Error::SemanticError);
            }

            current_module_id = Some(next);
        }

        Ok(current_module_id.unwrap())
    }
}
