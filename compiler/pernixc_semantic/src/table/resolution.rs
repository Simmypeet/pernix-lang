//! Contains the logic for resolving symbols and types.

use pernixc_base::diagnostic::Handler;
use pernixc_syntax::syntax_tree::item::ModulePath;

use super::{Index, State, Table};
use crate::{
    arena::ID,
    error::{self, ModuleExpected, SymbolNotFound},
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

impl<T: State> Table<T> {
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
                            handler.receive(Box::new(ModuleExpected {
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
