//! Contains the logic for resolving symbols and types.

use std::{collections::HashMap, convert::Into, fmt::Debug};

use enum_as_inner::EnumAsInner;
use pernixc_base::{
    diagnostic::Handler,
    source_file::{SourceElement, Span},
};
use pernixc_lexical::token::Identifier;
use pernixc_syntax::syntax_tree::{
    self, ConnectedList, GenericIdentifier, LifetimeIdentifier,
    QualifiedIdentifier,
};

use super::{
    evaluate,
    representation::{self, Index},
    State, Table,
};
use crate::{
    arena::ID,
    error::{
        self, ExpectLifetime, ExpectType, LifetimeParameterNotFound,
        MisOrderedGenericArgument, MismatchedGenericArgumentCount,
        MoreThanOneUnpackedInTupleType, NoGenericArgumentsRequired,
        ResolutionAmbiguity, SymbolNotFound,
    },
    symbol::{
        self, AdtImplementationConstant, AdtImplementationFunction,
        AdtImplementationType, Constant, ConstantParameter, Enum, Function,
        GenericKind, GlobalID, LifetimeParameter, LifetimeParameterID,
        MemberID, Module, Struct, Trait, TraitConstant, TraitFunction,
        TraitImplementationConstant, TraitImplementationFunction,
        TraitImplementationType, TraitType, Type, TypeParameter,
        TypeParameterID,
    },
    type_system::{
        model::{Default, Model},
        term::{
            self, constant,
            lifetime::{Forall, Lifetime},
            r#type::{self, Qualifier, SymbolID},
            GenericArguments, Local, Term, TupleElement,
        },
    },
};

/// An enumeration of the symbols that accepts generic parameters and are not a
/// member of another symbol.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::From,
)]
#[allow(missing_docs)]
pub enum GenericID {
    Struct(ID<Struct>),
    Enum(ID<Enum>),
    Function(ID<Function>),
    Trait(ID<Trait>),
    Constant(ID<Constant>),
    Type(ID<Type>),
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
        }
    }
}

/// A resolution for a symbol that accepts generic parameters and is not a
/// member of another symbol.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Generic<M: Model> {
    /// The resolved symbol.
    pub id: GenericID,

    /// The generic arguments supplied to the symbol.
    pub generic_arguments: GenericArguments<M>,
}

/// An enumeration of the symbols that accepts generic parameters for both
/// the symbol and the parent symbol that it is a member of.
#[allow(missing_docs)]
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::From,
)]
pub enum MemberGenericID {
    TraitFunction(ID<TraitFunction>),
    TraitType(ID<TraitType>),
    TraitConstant(ID<TraitConstant>),
    AdtImplementationFunction(ID<AdtImplementationFunction>),
    AdtImplementationType(ID<AdtImplementationType>),
    AdtImplementationConstant(ID<AdtImplementationConstant>),
    TraitImplementationFunction(ID<TraitImplementationFunction>),
    TraitImplementationType(ID<TraitImplementationType>),
    TraitImplementationConstant(ID<TraitImplementationConstant>),
}

impl From<MemberGenericID> for symbol::GenericID {
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
            MemberGenericID::TraitImplementationFunction(id) => {
                Self::TraitImplementationFunction(id)
            }
            MemberGenericID::TraitImplementationType(id) => {
                Self::TraitImplementationType(id)
            }
            MemberGenericID::TraitImplementationConstant(id) => {
                Self::TraitImplementationConstant(id)
            }
        }
    }
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
            MemberGenericID::TraitImplementationFunction(id) => {
                Self::TraitImplementationFunction(id)
            }
            MemberGenericID::TraitImplementationType(id) => {
                Self::TraitImplementationType(id)
            }
            MemberGenericID::TraitImplementationConstant(id) => {
                Self::TraitImplementationConstant(id)
            }
        }
    }
}

/// A resolution for a symbol that accepts generic parameters for both the
/// symbol and the parent symbol that it is a member of.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MemberGeneric<M: Model> {
    /// The resolved symbol.
    pub id: MemberGenericID,

    /// The generic arguments supplied to the parent symbol.
    pub parent_generic_arguments: GenericArguments<M>,

    /// The generic arguments supplied to the symbol.
    pub generic_arguments: GenericArguments<M>,
}

/// A resolution for an enum-variant.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Variant<M: Model> {
    /// The resolved variant.
    pub variant: ID<symbol::Variant>,

    /// The generic arguments supplied to the enum.
    pub generic_arguments: GenericArguments<M>,
}

trait GenericParameter<M: Model>: Sized + 'static {
    type SyntaxTree;
    type Argument: Term;

    fn resolve<S: State>(
        table: &Table<S>,
        syntax_tree: &Self::SyntaxTree,
        referring_site: GlobalID,
        config: Config<S, M>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Self::Argument, ResolveTermError>;

    fn generic_kind() -> GenericKind;

    fn get_ellided_term_provider<'a, S: State>(
        config: &'a mut Config<S, M>,
    ) -> Option<&'a mut dyn EliidedTermProvider<Self::Argument>>;

    fn on_resolved<S: State>(
        table: &Table<S>,
        referring_site: GlobalID,
        handler: &dyn Handler<Box<dyn error::Error>>,
        config: Config<S, M>,
        resolved: &Self::Argument,
        parameter: &Self::SyntaxTree,
    );
}

impl<M: Model> GenericParameter<M> for LifetimeParameter {
    type SyntaxTree = syntax_tree::Lifetime;
    type Argument = Lifetime<M>;

    fn resolve<S: State>(
        table: &Table<S>,
        syntax_tree: &Self::SyntaxTree,
        referring_site: GlobalID,
        config: Config<S, M>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Self::Argument, ResolveTermError> {
        table.resolve_lifetime(syntax_tree, referring_site, config, handler)
    }

    fn get_ellided_term_provider<'a, S: State>(
        config: &'a mut Config<S, M>,
    ) -> Option<&'a mut dyn EliidedTermProvider<Self::Argument>> {
        #[allow(clippy::option_if_let_else)]
        match &mut config.ellided_lifetime_provider {
            Some(provider) => Some(&mut **provider),
            None => None,
        }
    }

    fn generic_kind() -> GenericKind { GenericKind::Lifetime }

    fn on_resolved<S: State>(
        _: &Table<S>,
        _: GlobalID,
        _: &dyn Handler<Box<dyn error::Error>>,
        _: Config<S, M>,
        _: &Self::Argument,
        _: &Self::SyntaxTree,
    ) {
    }
}

impl<M: Model> GenericParameter<M> for TypeParameter {
    type SyntaxTree = syntax_tree::r#type::Type;
    type Argument = term::r#type::Type<M>;

    fn resolve<S: State>(
        table: &Table<S>,
        syntax_tree: &Self::SyntaxTree,
        referring_site: GlobalID,
        config: Config<S, M>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Self::Argument, ResolveTermError> {
        table.resolve_type(syntax_tree, referring_site, config, handler)
    }

    fn get_ellided_term_provider<'a, S: State>(
        config: &'a mut Config<S, M>,
    ) -> Option<&'a mut dyn EliidedTermProvider<Self::Argument>> {
        #[allow(clippy::option_if_let_else)]
        match &mut config.ellided_type_provider {
            Some(provider) => Some(&mut **provider),
            None => None,
        }
    }

    fn generic_kind() -> GenericKind { GenericKind::Type }

    fn on_resolved<S: State>(
        _: &Table<S>,
        _: GlobalID,
        _: &dyn Handler<Box<dyn error::Error>>,
        _: Config<S, M>,
        _: &Self::Argument,
        _: &Self::SyntaxTree,
    ) {
    }
}

impl<M: Model> GenericParameter<M> for ConstantParameter {
    type SyntaxTree = syntax_tree::expression::Expression;
    type Argument = term::constant::Constant<M>;

    fn resolve<S: State>(
        table: &Table<S>,
        syntax_tree: &Self::SyntaxTree,
        referring_site: GlobalID,
        _: Config<S, M>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Self::Argument, ResolveTermError> {
        match table.evaluate(syntax_tree, referring_site, handler) {
            Ok(constant) => Ok(M::from_default_constant(constant)),
            Err(evaluate::Error::InvalidReferringSiteID) => {
                Err(ResolveTermError::InvalidReferringSiteID)
            }

            Err(
                evaluate::Error::SemanticError | evaluate::Error::Suboptimal,
            ) => Ok(constant::Constant::Error(term::Error)),
        }
    }

    fn get_ellided_term_provider<'a, S: State>(
        config: &'a mut Config<S, M>,
    ) -> Option<&'a mut dyn EliidedTermProvider<Self::Argument>> {
        #[allow(clippy::option_if_let_else)]
        match &mut config.ellided_constant_provider {
            Some(provider) => Some(&mut **provider),
            None => None,
        }
    }

    fn generic_kind() -> GenericKind { GenericKind::Constant }

    fn on_resolved<S: State>(
        table: &Table<S>,
        referring_site: GlobalID,
        handler: &dyn Handler<Box<dyn error::Error>>,
        mut config: Config<S, M>,
        constant: &Self::Argument,
        syntax_tree: &Self::SyntaxTree,
    ) {
        if let Some(observer) = config.observer.as_mut() {
            observer.on_constant_arguments_resolved(
                table,
                referring_site,
                handler,
                constant,
                syntax_tree,
            );
        }

        // TODO: check if the constant's type matches the expected type
    }
}

/// Represents a resolution for a symbol.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum Resolution<M: Model> {
    Module(ID<Module>),
    Variant(Variant<M>),
    Generic(Generic<M>),
    MemberGeneric(MemberGeneric<M>),
}

impl<M: Model> Resolution<M> {
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
    #[error("the given `referring_site` id does not exist in the table")]
    InvalidReferringSiteID,

    #[error("encountered a fatal semantic error that aborts the process")]
    SemanticError,

    #[error(
        "the resolution observer requested to abort the resolution process"
    )]
    Abort,
}
impl From<ResolveTermError> for Error {
    fn from(value: ResolveTermError) -> Self {
        match value {
            ResolveTermError::InvalidReferringSiteID => {
                Self::InvalidReferringSiteID
            }
        }
    }
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

/// An error returned by [`Table::resolve_type()`] and
/// [`Table::resolve_lifetime()`].
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[allow(missing_docs)]
pub enum ResolveTermError {
    #[error("the given `referring_site` id does not exist in the table")]
    InvalidReferringSiteID,
}

/// A trait for providing elided terms.
pub trait EliidedTermProvider<T>: Debug {
    /// Creates a new instance of the term to supply the required missing term.
    fn create(&mut self) -> T;
}

/// A trait for observing the resolution process.
///
/// The trait will be notified when a type, lifetime, or constant is resolved
/// during the resolution process.
pub trait Observer<T: State, M: Model>: Debug {
    /// Notifies the observer when a global ID is resolved.
    ///
    /// Returns `true` if the resolution should continue, otherwise `false`.
    ///
    /// When the resolution process is stopped, the term resolution will be
    /// replaced with the `*::Error` variant.
    fn on_global_id_resolved(
        &mut self,
        table: &Table<T>,
        referring_site: GlobalID,
        handler: &dyn Handler<Box<dyn error::Error>>,
        global_id: GlobalID,
        identifier: &Identifier,
    ) -> bool;

    /// Notifies the observer when a resolution is resolved.
    fn on_resolution_resolved(
        &mut self,
        table: &Table<T>,
        referring_site: GlobalID,
        handler: &dyn Handler<Box<dyn error::Error>>,
        resolution: &Resolution<M>,
        generic_identifier: &GenericIdentifier,
    );

    /// Notifies the observer when a type is resolved.
    fn on_type_resolved(
        &mut self,
        table: &Table<T>,
        referring_site: GlobalID,
        handler: &dyn Handler<Box<dyn error::Error>>,
        ty: &r#type::Type<M>,
        syntax_tree: &syntax_tree::r#type::Type,
    );

    /// Notifies the observer when a lifetime is resolved.
    fn on_lifetime_resolved(
        &mut self,
        table: &Table<T>,
        referring_site: GlobalID,
        handler: &dyn Handler<Box<dyn error::Error>>,
        lifetime: &Lifetime<M>,
        syntax_tree: &syntax_tree::Lifetime,
    );

    /// Notifies the observer when a constant is resolved.
    fn on_constant_arguments_resolved(
        &mut self,
        table: &Table<T>,
        referring_site: GlobalID,
        handler: &dyn Handler<Box<dyn error::Error>>,
        constant: &constant::Constant<M>,
        syntax_tree: &syntax_tree::expression::Expression,
    );

    /// Notifies the observer when a type is resolved as an unpacked element
    /// in a tuple type.
    fn on_unpacked_type_resolved(
        &mut self,
        table: &Table<T>,
        referring_site: GlobalID,
        handler: &dyn Handler<Box<dyn error::Error>>,
        ty: &r#type::Type<M>,
        syntax_tree: &syntax_tree::r#type::Type,
    );

    /// Notifies the observer when a constant is resolved as an unpacked element
    /// in a tuple constant.
    fn on_unpacked_constant_resolved(
        &mut self,
        table: &Table<T>,
        referring_site: GlobalID,
        handler: &dyn Handler<Box<dyn error::Error>>,
        constant: &constant::Constant<M>,
        syntax_tree: &syntax_tree::expression::Expression,
    );
}

/// The struct implementing the [`Observer`] trait that does nothing.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct NoOpObserver;

impl<T: State, M: Model> Observer<T, M> for NoOpObserver {
    fn on_global_id_resolved(
        &mut self,
        _: &Table<T>,
        _: GlobalID,
        _: &dyn Handler<Box<dyn error::Error>>,
        _: GlobalID,
        _: &Identifier,
    ) -> bool {
        true
    }

    fn on_resolution_resolved(
        &mut self,
        _: &Table<T>,
        _: GlobalID,
        _: &dyn Handler<Box<dyn error::Error>>,
        _: &Resolution<M>,
        _: &GenericIdentifier,
    ) {
    }

    fn on_type_resolved(
        &mut self,
        _: &Table<T>,
        _: GlobalID,
        _: &dyn Handler<Box<dyn error::Error>>,
        _: &r#type::Type<M>,
        _: &syntax_tree::r#type::Type,
    ) {
    }

    fn on_lifetime_resolved(
        &mut self,
        _: &Table<T>,
        _: GlobalID,
        _: &dyn Handler<Box<dyn error::Error>>,
        _: &Lifetime<M>,
        _: &syntax_tree::Lifetime,
    ) {
    }

    fn on_constant_arguments_resolved(
        &mut self,
        _: &Table<T>,
        _: GlobalID,
        _: &dyn Handler<Box<dyn error::Error>>,
        _: &constant::Constant<M>,
        _: &syntax_tree::expression::Expression,
    ) {
    }

    fn on_unpacked_type_resolved(
        &mut self,
        _: &Table<T>,
        _: GlobalID,
        _: &dyn Handler<Box<dyn error::Error>>,
        _: &r#type::Type<M>,
        _: &syntax_tree::r#type::Type,
    ) {
    }

    fn on_unpacked_constant_resolved(
        &mut self,
        _: &Table<T>,
        _: GlobalID,
        _: &dyn Handler<Box<dyn error::Error>>,
        _: &constant::Constant<M>,
        _: &syntax_tree::expression::Expression,
    ) {
    }
}

/// The configuration struct specifying the behaviour of the resolution process.
#[derive(Debug, Default)]
pub struct Config<'lp, 'tp, 'cp, 'ob, 'hrlt, S: State, M: Model> {
    /// If specified, when the lifetime argument is elided, the provider will
    /// be used to supply the missing required lifetimes.
    pub ellided_lifetime_provider:
        Option<&'lp mut dyn EliidedTermProvider<Lifetime<M>>>,

    /// If specified, when the type argument is elided, the provider will be
    /// used to supply the missing required types.
    pub ellided_type_provider:
        Option<&'tp mut dyn EliidedTermProvider<term::r#type::Type<M>>>,

    /// If specified, when the constant argument is elided, the provider will
    /// be used to supply the missing required constants.
    pub ellided_constant_provider:
        Option<&'cp mut dyn EliidedTermProvider<term::constant::Constant<M>>>,

    /// If specified, during the resolution process, the observer will be
    /// notified each time a type, lifetime, or constant is resolved.
    pub observer: Option<&'ob mut dyn Observer<S, M>>,

    /// If specified, when resolving a lifetime parameter, these higher-ranked
    /// lifetimes map will be taken into consideration.
    pub higher_ranked_liftimes: Option<&'hrlt HashMap<String, Forall>>,
}

impl<'lp, 'tp, 'cp, 'ob, 'hrlt, S: State, M: Model>
    Config<'lp, 'tp, 'cp, 'ob, 'hrlt, S, M>
{
    /// Creates a new instance of the config.
    #[allow(clippy::option_if_let_else)]
    pub fn reborrow(&mut self) -> Config<S, M> {
        Config {
            ellided_lifetime_provider: match &mut self.ellided_lifetime_provider
            {
                Some(provider) => Some(&mut **provider),
                None => None,
            },
            ellided_type_provider: match &mut self.ellided_type_provider {
                Some(provider) => Some(&mut **provider),
                None => None,
            },
            ellided_constant_provider: match &mut self.ellided_constant_provider
            {
                Some(provider) => Some(&mut **provider),
                None => None,
            },
            observer: match &mut self.observer {
                Some(observer) => Some(&mut **observer),
                None => None,
            },
            higher_ranked_liftimes: self.higher_ranked_liftimes,
        }
    }
}

impl<S: State> Table<S> {
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
                self.root_module_ids_by_name()
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
                .get_member_of(lastest_resolution, identifier.span.str())
                .map_err(|_| {
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
            .filter_map(|x| self.get_member_of(x, identifier.span.str()).ok())
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
            if let Ok(id) =
                self.get_member_of(referring_site, identifier.span.str())
            {
                return Ok(id);
            }

            let global_symbol = self
                .get_global(referring_site)
                .ok_or(Error::InvalidReferringSiteID)?;

            if let Some(parent_id) = global_symbol.parent_global_id() {
                referring_site = parent_id;
            } else {
                // try to search the symbol in the root scope
                if let Some(id) = self
                    .root_module_ids_by_name()
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

    fn resolve_single_no_generics<M: Model>(
        &self,
        identifier: &Identifier,
        latest_resolution: &Option<Resolution<M>>,
        referring_site: GlobalID,
        search_from_root: bool,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<GlobalID, Error> {
        let (resolved_global_id, searched_global_id) = match latest_resolution {
            Some(resolution) => {
                // gets the global id from the resolution
                let parent_global_id = resolution.global_id();

                (
                    self.get_member_of(parent_global_id, identifier.span.str())
                        .map_or_else(
                            |err| match err {
                                representation::GetMemberError::InvalidID => {
                                    unreachable!("invalid ID detected!")
                                }
                                representation::GetMemberError::MemberNotFound => None,
                            },
                            Some,
                        ),
                    Some(parent_global_id),
                )
            }
            None => {
                if search_from_root {
                    (
                        self.root_module_ids_by_name()
                            .get(identifier.span.str())
                            .copied()
                            .map(Into::into),
                        None,
                    )
                } else {
                    return self.resolve_root_relative(
                        identifier,
                        referring_site,
                        handler,
                    );
                }
            }
        };

        resolved_global_id.map_or_else(
            || {
                handler.receive(Box::new(SymbolNotFound {
                    searched_global_id,
                    resolution_span: identifier.span.clone(),
                }));
                Err(Error::SemanticError)
            },
            |id| {
                // non-fatal error, no need to return early
                if !self.symbol_accessible(referring_site, id).unwrap() {
                    handler.receive(Box::new(error::SymbolIsNotAccessible {
                        referring_site,
                        referred: id,
                        referred_span: identifier.span.clone(),
                    }));
                }

                Ok(id)
            },
        )
    }

    /// Resolves an [`Identifier`] as a [`LifetimeParameterID`].
    ///
    /// # Errors
    ///
    /// See [`Error`] for more information
    pub fn resolve_lifetime_parameter(
        &self,
        identifier: &Identifier,
        referring_site: GlobalID,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Option<LifetimeParameterID>, ResolveTermError> {
        for scope in self
            .scope_walker(referring_site)
            .ok_or(ResolveTermError::InvalidReferringSiteID)?
        {
            let Ok(generic_id) = symbol::GenericID::try_from(scope) else {
                continue;
            };

            let generic_symbol =
                self.get_generic(generic_id).expect("should've been valid");

            if let Some(lifetime_id) = generic_symbol
                .generic_declaration()
                .parameters
                .lifetime_parameter_ids_by_name()
                .get(identifier.span.str())
                .copied()
            {
                return Ok(Some(MemberID {
                    parent: generic_id,
                    id: lifetime_id,
                }));
            }
        }

        handler.receive(Box::new(LifetimeParameterNotFound {
            referred_span: identifier.span(),
            referring_site,
        }));
        Ok(None)
    }

    /// Resolves a [`syntax_tree::Lifetime`] to a [`Lifetime`].
    ///
    /// # Errors
    ///
    /// See [`Error`] for more information.
    pub fn resolve_lifetime<M: Model>(
        &self,
        lifetime_argument: &syntax_tree::Lifetime,
        referring_site: GlobalID,
        config: Config<S, M>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Lifetime<M>, ResolveTermError> {
        let lifetime = match lifetime_argument.identifier() {
            LifetimeIdentifier::Static(..) => Lifetime::Static,
            LifetimeIdentifier::Identifier(ident) => {
                if let Some(higher_ranked_lifetimes) =
                    config.higher_ranked_liftimes
                {
                    if let Some(forall) =
                        higher_ranked_lifetimes.get(ident.span.str())
                    {
                        return Ok(Lifetime::Forall(*forall));
                    }
                }

                self.resolve_lifetime_parameter(ident, referring_site, handler)?
                    .map_or(Lifetime::Error(term::Error), Lifetime::Parameter)
            }
        };

        if let Some(observer) = config.observer {
            observer.on_lifetime_resolved(
                self,
                referring_site,
                handler,
                &lifetime,
                lifetime_argument,
            );
        }

        Ok(lifetime)
    }

    /// Resolves a [`GenericArguments`] from the given generic arguments syntax
    /// tree
    ///
    /// # Errors
    ///
    /// See [`Error`] for more information.
    #[allow(clippy::too_many_lines)]
    pub fn resolve_generic_arguments<M: Model>(
        &self,
        generic_identifier: &GenericIdentifier,
        referring_site: GlobalID,
        resolved_id: GlobalID,
        mut config: Config<S, M>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Option<GenericArguments<M>>, Error> {
        let Ok(generic_id) = symbol::GenericID::try_from(resolved_id) else {
            if let Some(generic_arguments) =
                generic_identifier.generic_arguments()
            {
                // non-fatal error, keep going
                handler.receive(Box::new(NoGenericArgumentsRequired {
                    global_id: resolved_id,
                    generic_argument_span: generic_arguments.span(),
                }));
            }

            return Ok(None);
        };

        let mut lifetime_argument_syns = Vec::new();
        let mut type_argument_syns = Vec::new();
        let mut constant_argument_syns = Vec::new();

        // extracts the generic arguments from the syntax tree to the list of
        // syntax trees
        for generic_arguments in
            generic_identifier.generic_arguments().iter().flat_map(|x| {
                x.argument_list().iter().flat_map(ConnectedList::elements)
            })
        {
            match generic_arguments {
                syntax_tree::GenericArgument::Constant(arg) => {
                    constant_argument_syns.push(&**arg.expression());
                }
                syntax_tree::GenericArgument::Type(arg)
                    if constant_argument_syns.is_empty() =>
                {
                    type_argument_syns.push(&**arg);
                }
                syntax_tree::GenericArgument::Lifetime(arg)
                    if constant_argument_syns.is_empty()
                        && type_argument_syns.is_empty() =>
                {
                    lifetime_argument_syns.push(arg);
                }
                arg => {
                    handler.receive(Box::new(MisOrderedGenericArgument {
                        generic_kind: match arg {
                            syntax_tree::GenericArgument::Type(_) => {
                                GenericKind::Type
                            }
                            syntax_tree::GenericArgument::Constant(_) => {
                                GenericKind::Constant
                            }
                            syntax_tree::GenericArgument::Lifetime(_) => {
                                GenericKind::Lifetime
                            }
                        },
                        generic_argument: arg.span(),
                    }));
                    return Err(Error::SemanticError);
                }
            }
        }

        let (
            lifetime_parameter_orders,
            type_parameter_orders,
            constant_parameter_ords,
            default_type_arguments,
            default_constant_arguments,
        ) = {
            let generic_declaration =
                self.get_generic(generic_id).expect("should've been valid");

            (
                generic_declaration
                    .generic_declaration()
                    .parameters
                    .lifetime_parameters_as_order()
                    .map(|(_, x)| x)
                    .cloned()
                    .collect::<Vec<_>>(),
                generic_declaration
                    .generic_declaration()
                    .parameters
                    .type_parameters_as_order()
                    .map(|(_, x)| x)
                    .cloned()
                    .collect::<Vec<_>>(),
                generic_declaration
                    .generic_declaration()
                    .parameters
                    .constant_parameters_as_order()
                    .map(|(_, x)| x)
                    .cloned()
                    .collect::<Vec<_>>(),
                constant_argument_syns.is_empty().then(|| {
                    generic_declaration
                        .generic_declaration()
                        .parameters
                        .default_type_parameters()
                        .clone()
                }),
                generic_declaration
                    .generic_declaration()
                    .parameters
                    .default_constant_parameters()
                    .clone(),
            )
        };

        Ok(Some(GenericArguments {
            lifetimes: self.resolve_generic_arguments_kinds(
                lifetime_argument_syns.into_iter(),
                lifetime_parameter_orders.iter(),
                Option::<std::iter::Empty<_>>::None,
                generic_identifier.span(),
                referring_site,
                config.reborrow(),
                handler,
            )?,
            types: self.resolve_generic_arguments_kinds(
                type_argument_syns.into_iter(),
                type_parameter_orders.iter(),
                default_type_arguments.as_ref().map(|x| x.iter()),
                generic_identifier.span(),
                referring_site,
                config.reborrow(),
                handler,
            )?,
            constants: self.resolve_generic_arguments_kinds(
                constant_argument_syns.into_iter(),
                constant_parameter_ords.iter(),
                Some(default_constant_arguments.iter()),
                generic_identifier.span(),
                referring_site,
                config,
                handler,
            )?,
        }))
    }

    fn resolution_to_type<M: Model>(
        resolution: Resolution<M>,
    ) -> Result<r#type::Type<M>, Resolution<M>> {
        match resolution {
            Resolution::Generic(symbol) => {
                let id = match symbol.id {
                    GenericID::Struct(id) => SymbolID::Struct(id),
                    GenericID::Enum(id) => SymbolID::Enum(id),
                    GenericID::Type(id) => SymbolID::Type(id),

                    _ => return Err(Resolution::Generic(symbol)),
                };

                Ok(r#type::Type::Symbol(term::Symbol {
                    id,
                    generic_arguments: symbol.generic_arguments,
                }))
            }
            Resolution::MemberGeneric(symbol) => match symbol.id {
                MemberGenericID::TraitType(id) => {
                    Ok(r#type::Type::TraitMember(r#type::TraitMember {
                        id,
                        member_generic_arguments: symbol.generic_arguments,
                        parent_generic_arguments: symbol
                            .parent_generic_arguments,
                    }))
                }
                MemberGenericID::AdtImplementationType(id) => {
                    Ok(r#type::Type::MemberSymbol(term::MemberSymbol {
                        id: r#type::MemberSymbolID::AdtImplementation(id),
                        member_generic_arguments: symbol.generic_arguments,
                        parent_generic_arguments: symbol
                            .parent_generic_arguments,
                    }))
                }

                MemberGenericID::TraitImplementationType(id) => {
                    Ok(r#type::Type::MemberSymbol(term::MemberSymbol {
                        id: r#type::MemberSymbolID::TraitImplementation(id),
                        member_generic_arguments: symbol.generic_arguments,
                        parent_generic_arguments: symbol
                            .parent_generic_arguments,
                    }))
                }

                _ => Err(Resolution::MemberGeneric(symbol)),
            },
            resolution => Err(resolution),
        }
    }

    /// Resolves a [`syntax_tree::r#type::Type`] to a [`r#type::Type`].
    ///
    /// # Errors
    ///
    /// See [`Error`] for more information.
    #[allow(clippy::too_many_lines)]
    pub fn resolve_type<M: Model>(
        &self,
        syntax_tree: &syntax_tree::r#type::Type,
        referring_site: GlobalID,
        mut config: Config<S, M>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<r#type::Type<M>, ResolveTermError> {
        let ty = match syntax_tree {
            syntax_tree::r#type::Type::Primitive(primitive) => {
                r#type::Type::Primitive(match primitive {
                    syntax_tree::r#type::Primitive::Bool(_) => {
                        r#type::Primitive::Bool
                    }
                    syntax_tree::r#type::Primitive::Float32(_) => {
                        r#type::Primitive::Float32
                    }
                    syntax_tree::r#type::Primitive::Float64(_) => {
                        r#type::Primitive::Float64
                    }
                    syntax_tree::r#type::Primitive::Int8(_) => {
                        r#type::Primitive::Int8
                    }
                    syntax_tree::r#type::Primitive::Int16(_) => {
                        r#type::Primitive::Int16
                    }
                    syntax_tree::r#type::Primitive::Int32(_) => {
                        r#type::Primitive::Int32
                    }
                    syntax_tree::r#type::Primitive::Int64(_) => {
                        r#type::Primitive::Int64
                    }
                    syntax_tree::r#type::Primitive::Uint8(_) => {
                        r#type::Primitive::Uint8
                    }
                    syntax_tree::r#type::Primitive::Uint16(_) => {
                        r#type::Primitive::Uint16
                    }
                    syntax_tree::r#type::Primitive::Uint32(_) => {
                        r#type::Primitive::Uint32
                    }
                    syntax_tree::r#type::Primitive::Uint64(_) => {
                        r#type::Primitive::Uint64
                    }
                    syntax_tree::r#type::Primitive::Usize(_) => {
                        r#type::Primitive::Usize
                    }
                    syntax_tree::r#type::Primitive::Isize(_) => {
                        r#type::Primitive::Isize
                    }
                })
            }
            syntax_tree::r#type::Type::Local(local) => {
                r#type::Type::Local(Local(Box::new(self.resolve_type(
                    local.ty(),
                    referring_site,
                    config.reborrow(),
                    handler,
                )?)))
            }
            syntax_tree::r#type::Type::QualifiedIdentifier(
                qualified_identifier,
            ) => self.resolve_qualified_identifier_type(
                qualified_identifier,
                referring_site,
                config.reborrow(),
                handler,
            )?,
            syntax_tree::r#type::Type::Reference(reference) => {
                let lifetime = reference
                    .lifetime()
                    .as_ref()
                    .map(|x| {
                        self.resolve_lifetime(
                            x,
                            referring_site,
                            config.reborrow(),
                            handler,
                        )
                    })
                    .transpose()?;

                let lifetime = if let Some(lifetime) = lifetime {
                    lifetime
                } else if let Some(provider) =
                    config.ellided_lifetime_provider.as_mut()
                {
                    provider.create()
                } else {
                    handler.receive(Box::new(ExpectLifetime {
                        expected_span: reference.span(),
                    }));
                    Lifetime::Error(term::Error)
                };

                let qualifier = match reference.qualifier() {
                    Some(syntax_tree::Qualifier::Mutable(..)) => {
                        Qualifier::Mutable
                    }
                    Some(syntax_tree::Qualifier::Unique(..)) => {
                        Qualifier::Unique
                    }
                    None => Qualifier::Immutable,
                };

                let pointee = Box::new(self.resolve_type(
                    reference.operand(),
                    referring_site,
                    config.reborrow(),
                    handler,
                )?);

                r#type::Type::Reference(r#type::Reference {
                    qualifier,
                    lifetime,
                    pointee,
                })
            }
            syntax_tree::r#type::Type::Pointer(pointer_ty) => {
                let qualifier = match pointer_ty.qualifier() {
                    Some(syntax_tree::Qualifier::Mutable(..)) => {
                        Qualifier::Mutable
                    }
                    Some(syntax_tree::Qualifier::Unique(..)) => {
                        Qualifier::Unique
                    }
                    None => Qualifier::Immutable,
                };

                let pointee = Box::new(self.resolve_type(
                    pointer_ty.operand(),
                    referring_site,
                    config.reborrow(),
                    handler,
                )?);

                r#type::Type::Pointer(r#type::Pointer { qualifier, pointee })
            }
            syntax_tree::r#type::Type::Tuple(syntax_tree) => {
                let mut elements = Vec::new();

                for element in syntax_tree
                    .unpackable_list()
                    .iter()
                    .flat_map(ConnectedList::elements)
                {
                    let ty = self.resolve_type(
                        element.ty(),
                        referring_site,
                        config.reborrow(),
                        handler,
                    )?;

                    if element.ellipsis().is_some() {
                        match ty {
                            r#type::Type::Tuple(tuple) => {
                                elements.extend(tuple.elements.into_iter());
                            }
                            ty => {
                                if let Some(observer) = config.observer.as_mut()
                                {
                                    observer.on_unpacked_type_resolved(
                                        self,
                                        referring_site,
                                        handler,
                                        &ty,
                                        element.ty(),
                                    );
                                }

                                elements.push(TupleElement {
                                    term: ty,
                                    is_unpacked: true,
                                });
                            }
                        }
                    } else {
                        elements.push(TupleElement {
                            term: ty,
                            is_unpacked: false,
                        });
                    }
                }

                // check if there is more than one unpacked type
                if elements.iter().filter(|x| x.is_unpacked).count() > 1 {
                    handler.receive(Box::new(MoreThanOneUnpackedInTupleType {
                        illegal_tuple_type_span: syntax_tree.span(),
                    }));
                    return Ok(r#type::Type::Error(term::Error));
                }

                r#type::Type::Tuple(r#type::Tuple { elements })
            }

            syntax_tree::r#type::Type::Array(array) => {
                let length = M::from_default_constant(
                    match self.evaluate(
                        array.expression(),
                        referring_site,
                        handler,
                    ) {
                        Ok(value) => value,

                        Err(evaluate::Error::InvalidReferringSiteID) => {
                            return Err(
                                ResolveTermError::InvalidReferringSiteID,
                            );
                        }

                        Err(
                            evaluate::Error::SemanticError
                            | evaluate::Error::Suboptimal,
                        ) => constant::Constant::Error(term::Error),
                    },
                );
                let element_ty = self.resolve_type(
                    array.operand(),
                    referring_site,
                    config.reborrow(),
                    handler,
                )?;

                r#type::Type::Array(r#type::Array {
                    length,
                    r#type: Box::new(element_ty),
                })
            }

            syntax_tree::r#type::Type::Phantom(phantom) => {
                let ty = self.resolve_type(
                    phantom.r#type(),
                    referring_site,
                    config.reborrow(),
                    handler,
                )?;

                r#type::Type::Phantom(r#type::Phantom(Box::new(ty)))
            }
        };

        if let Some(observer) = config.observer.as_mut() {
            observer.on_type_resolved(
                self,
                referring_site,
                handler,
                &ty,
                syntax_tree,
            );
        }

        Ok(ty)
    }

    fn resolve_qualified_identifier_type<M: Model>(
        &self,
        syntax_tree: &syntax_tree::QualifiedIdentifier,
        referring_site: GlobalID,
        config: Config<S, M>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<r#type::Type<M>, ResolveTermError> {
        let is_simple_identifier = syntax_tree.rest().is_empty()
            && syntax_tree.leading_scope_separator().is_none()
            && syntax_tree.first().generic_arguments().is_none();

        // try to resolve the identifier as a type parameter
        if is_simple_identifier {
            for global_id in self
                .scope_walker(referring_site)
                .ok_or(ResolveTermError::InvalidReferringSiteID)?
            {
                let Ok(generic_id) = symbol::GenericID::try_from(global_id)
                else {
                    continue;
                };

                let generic_symbol =
                    self.get_generic(generic_id).expect("should've been valid");

                if let Some(type_parameter_id) = generic_symbol
                    .generic_declaration()
                    .parameters
                    .type_parameter_ids_by_name()
                    .get(syntax_tree.first().identifier().span.str())
                    .copied()
                {
                    return Ok(r#type::Type::Parameter(TypeParameterID {
                        parent: generic_id,
                        id: type_parameter_id,
                    }));
                }
            }
        }
        let resolution =
            match self.resolve(syntax_tree, referring_site, config, handler) {
                Ok(resolution) => resolution,

                Err(Error::InvalidReferringSiteID) => {
                    return Err(ResolveTermError::InvalidReferringSiteID);
                }

                Err(Error::Abort | Error::SemanticError) => {
                    return Ok(r#type::Type::Error(term::Error));
                }
            };

        match Self::resolution_to_type(resolution) {
            Ok(ty) => Ok(ty),
            Err(resolution) => {
                handler.receive(Box::new(ExpectType {
                    non_type_symbol_span: syntax_tree.span(),
                    resolved_global_id: resolution.global_id(),
                }));

                Ok(r#type::Type::Error(term::Error))
            }
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn resolve_generic_arguments_kinds<'a, M: Model, G: GenericParameter<M>>(
        &self,
        syntax_trees: impl ExactSizeIterator<Item = &'a G::SyntaxTree>,
        parameters: impl ExactSizeIterator<Item = &'a G>,
        defaults: Option<
            impl ExactSizeIterator<
                Item = &'a <G::Argument as Term>::Rebind<Default>,
            >,
        >,
        generic_identifier_span: Span,
        referring_site: GlobalID,
        mut config: Config<S, M>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Vec<G::Argument>, Error>
    where
        G::Argument: Clone,
    {
        if syntax_trees.len() == 0 {
            if parameters.len() == 0 {
                return Ok(Vec::new());
            }

            let Some(provider) = G::get_ellided_term_provider(&mut config)
            else {
                handler.receive(Box::new(MismatchedGenericArgumentCount {
                    generic_kind: G::generic_kind(),
                    generic_identifier_span,
                    expected_count: parameters.len(),
                    supplied_count: syntax_trees.len(),
                }));
                return Err(Error::SemanticError);
            };

            Ok(parameters.map(|_| provider.create()).collect())
        } else {
            let valid_count = defaults.as_ref().map_or_else(
                || parameters.len() == syntax_trees.len(),
                |defaults| {
                    let expected_range =
                        (parameters.len() - defaults.len())..=parameters.len();

                    expected_range.contains(&syntax_trees.len())
                },
            );

            // check if the number of supplied generic arugmnets is valid
            if !valid_count {
                handler.receive(Box::new(MismatchedGenericArgumentCount {
                    generic_identifier_span,
                    generic_kind: G::generic_kind(),
                    expected_count: parameters.len(),
                    supplied_count: syntax_trees.len(),
                }));
                return Err(Error::SemanticError);
            }

            let mut arguments = syntax_trees
                .take(parameters.len())
                .map(|syntax_tree| {
                    let resolved = G::resolve(
                        self,
                        syntax_tree,
                        referring_site,
                        config.reborrow(),
                        handler,
                    );

                    // report the on resolved
                    if let Ok(resolved) = &resolved {
                        G::on_resolved(
                            self,
                            referring_site,
                            handler,
                            config.reborrow(),
                            resolved,
                            syntax_tree,
                        );
                    }

                    resolved
                })
                .collect::<Result<Vec<_>, _>>()?;

            if let Some(defaults) = defaults {
                let leftovers = parameters.len() - arguments.len();
                let default_fill_count = leftovers.min(defaults.len());
                let default_len = defaults.len();

                arguments.extend(
                    defaults
                        .skip(default_len - default_fill_count)
                        .cloned()
                        .map(G::Argument::from_default_model),
                );
            }

            Ok(arguments)
        }
    }

    fn get_parent_generic_arguments_from_latest_resolution<M: Model>(
        &self,
        parent_generic_id: symbol::GenericID,
        latest_resolution: Option<Resolution<M>>,
    ) -> GenericArguments<M> {
        latest_resolution.map_or_else(
            || {
                self.get_generic(parent_generic_id)
                    .unwrap()
                    .generic_declaration()
                    .parameters
                    .create_identity_generic_arguments(parent_generic_id)
            },
            |resolution| resolution.into_generic().unwrap().generic_arguments,
        )
    }

    #[allow(clippy::too_many_lines)]
    fn to_resolution<M: Model>(
        &self,
        resolved_id: GlobalID,
        generic_arguments: Option<GenericArguments<M>>,
        latest_resolution: Option<Resolution<M>>,
    ) -> Resolution<M> {
        match resolved_id {
            GlobalID::Module(id) => Resolution::Module(id),
            GlobalID::Struct(id) => Resolution::Generic(Generic {
                id: id.into(),
                generic_arguments: generic_arguments.unwrap(),
            }),
            GlobalID::Trait(id) => Resolution::Generic(Generic {
                id: id.into(),
                generic_arguments: generic_arguments.unwrap(),
            }),
            GlobalID::Enum(id) => Resolution::Generic(Generic {
                id: id.into(),
                generic_arguments: generic_arguments.unwrap(),
            }),
            GlobalID::Type(id) => Resolution::Generic(Generic {
                id: id.into(),
                generic_arguments: generic_arguments.unwrap(),
            }),
            GlobalID::Constant(id) => Resolution::Generic(Generic {
                id: id.into(),
                generic_arguments: generic_arguments.unwrap(),
            }),
            GlobalID::Function(id) => Resolution::Generic(Generic {
                id: id.into(),
                generic_arguments: generic_arguments.unwrap(),
            }),
            GlobalID::Variant(id) => Resolution::Variant(Variant {
                generic_arguments: self
                    .get_parent_generic_arguments_from_latest_resolution(
                        self.get(id).unwrap().parent_enum_id.into(),
                        latest_resolution,
                    ),
                variant: id,
            }),
            GlobalID::TraitType(id) => {
                Resolution::MemberGeneric(MemberGeneric {
                    id: id.into(),
                    parent_generic_arguments: self
                        .get_parent_generic_arguments_from_latest_resolution(
                            self.get(id).unwrap().parent_id.into(),
                            latest_resolution,
                        ),
                    generic_arguments: generic_arguments.unwrap(),
                })
            }
            GlobalID::TraitFunction(id) => {
                Resolution::MemberGeneric(MemberGeneric {
                    id: id.into(),
                    parent_generic_arguments: self
                        .get_parent_generic_arguments_from_latest_resolution(
                            self.get(id).unwrap().parent_id.into(),
                            latest_resolution,
                        ),
                    generic_arguments: generic_arguments.unwrap(),
                })
            }
            GlobalID::TraitConstant(id) => {
                Resolution::MemberGeneric(MemberGeneric {
                    id: id.into(),
                    parent_generic_arguments: self
                        .get_parent_generic_arguments_from_latest_resolution(
                            self.get(id).unwrap().parent_id.into(),
                            latest_resolution,
                        ),
                    generic_arguments: generic_arguments.unwrap(),
                })
            }

            GlobalID::AdtImplementation(_)
            | GlobalID::NegativeTraitImplementation(_)
            | GlobalID::PositiveTraitImplementation(_) => {
                unreachable!("impossible to refer to a trait implementation")
            }

            GlobalID::AdtImplementationFunction(id) => {
                Resolution::MemberGeneric(MemberGeneric {
                    id: id.into(),
                    parent_generic_arguments: self
                        .get_parent_generic_arguments_from_latest_resolution(
                            self.get(id).unwrap().parent_id.into(),
                            latest_resolution,
                        ),
                    generic_arguments: generic_arguments.unwrap(),
                })
            }
            GlobalID::AdtImplementationType(id) => {
                Resolution::MemberGeneric(MemberGeneric {
                    id: id.into(),
                    parent_generic_arguments: self
                        .get_parent_generic_arguments_from_latest_resolution(
                            self.get(id).unwrap().parent_id.into(),
                            latest_resolution,
                        ),
                    generic_arguments: generic_arguments.unwrap(),
                })
            }
            GlobalID::AdtImplementationConstant(id) => {
                Resolution::MemberGeneric(MemberGeneric {
                    id: id.into(),
                    parent_generic_arguments: self
                        .get_parent_generic_arguments_from_latest_resolution(
                            self.get(id).unwrap().parent_id.into(),
                            latest_resolution,
                        ),
                    generic_arguments: generic_arguments.unwrap(),
                })
            }

            GlobalID::TraitImplementationFunction(id) => {
                assert!(latest_resolution.is_none());

                let parent_arguments = GenericArguments::from_default_model(
                    self.get(self.get(id).unwrap().parent_id)
                        .unwrap()
                        .arguments
                        .clone(),
                );

                Resolution::MemberGeneric(MemberGeneric {
                    id: id.into(),
                    parent_generic_arguments: parent_arguments,
                    generic_arguments: generic_arguments.unwrap(),
                })
            }

            GlobalID::TraitImplementationType(id) => {
                assert!(latest_resolution.is_none());

                let parent_arguments = GenericArguments::from_default_model(
                    self.get(self.get(id).unwrap().parent_id)
                        .unwrap()
                        .arguments
                        .clone(),
                );

                Resolution::MemberGeneric(MemberGeneric {
                    id: id.into(),
                    parent_generic_arguments: parent_arguments,
                    generic_arguments: generic_arguments.unwrap(),
                })
            }

            GlobalID::TraitImplementationConstant(id) => {
                assert!(latest_resolution.is_none());

                let parent_arguments = GenericArguments::from_default_model(
                    self.get(self.get(id).unwrap().parent_id)
                        .unwrap()
                        .arguments
                        .clone(),
                );

                Resolution::MemberGeneric(MemberGeneric {
                    id: id.into(),
                    parent_generic_arguments: parent_arguments,
                    generic_arguments: generic_arguments.unwrap(),
                })
            }
        }
    }

    /// Resolves the symbol for the given qualified identifiers.
    ///
    /// # Parameters
    ///
    /// - `qualified_identifier`: The qualified identifier to resolve the symbol
    ///  for.
    /// - `referring_site`: The global ID of the symbol that is referring to the
    /// symbol to resolve.
    /// - `config`:
    /// - `handler`: The handler for the diagnostics.
    ///
    /// # Errors
    ///
    /// See [`Error`] for more information
    pub fn resolve<M: Model>(
        &self,
        qualified_identifier: &QualifiedIdentifier,
        referring_site: GlobalID,
        mut config: Config<S, M>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Resolution<M>, Error> {
        // check if the given `referring_site` is a valid ID.
        drop(
            self.get_global(referring_site)
                .ok_or(Error::InvalidReferringSiteID)?,
        );

        // create the current root
        let mut latest_resolution = None;
        let has_leading_scope_separator =
            qualified_identifier.leading_scope_separator().is_some();

        for generic_identifier in qualified_identifier.generic_identifiers() {
            // resolves the identifier
            let global_id = self.resolve_single_no_generics(
                generic_identifier.identifier(),
                &latest_resolution,
                referring_site,
                has_leading_scope_separator,
                handler,
            )?;

            // invoke the observer
            if let Some(observer) = config.observer.as_mut() {
                if !observer.on_global_id_resolved(
                    self,
                    referring_site,
                    handler,
                    global_id,
                    generic_identifier.identifier(),
                ) {
                    return Err(Error::Abort);
                }
            }

            let generic_arguments = self.resolve_generic_arguments(
                generic_identifier,
                referring_site,
                global_id,
                config.reborrow(),
                handler,
            )?;

            let next_resolution = self.to_resolution(
                global_id,
                generic_arguments,
                latest_resolution,
            );

            if let Some(observer) = config.observer.as_mut() {
                observer.on_resolution_resolved(
                    self,
                    referring_site,
                    handler,
                    &next_resolution,
                    generic_identifier,
                );
            }

            latest_resolution = Some(next_resolution);
        }

        Ok(latest_resolution.expect("should at least have one resolution"))
    }
}
