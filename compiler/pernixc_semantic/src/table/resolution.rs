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

use super::{evaluate, Index, State, Table};
use crate::{
    arena::ID,
    error::{
        self, ExpectLifetime, ExpectType, LifetimeParameterNotFound,
        MisMatchedGenericArgumentCount, MisOrderedGenericArgument,
        MoreThanOneUnpackedInTupleType, NoGenericArgumentsRequired,
        ResolutionAmbiguity, SymbolNotFound,
    },
    semantic::term::{
        self, constant,
        lifetime::{Forall, Lifetime},
        r#type::{self, Qualifier, SymbolID},
        GenericArguments, Local, MemberSymbol, TupleElement,
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
trait GenericParameter: Sized + 'static {
    type SyntaxTree;
    type Argument;

    fn resolve(
        table: &Table<impl State>,
        syntax_tree: &Self::SyntaxTree,
        referring_site: GlobalID,
        config: Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Self::Argument, Error>;

    fn generic_kind() -> GenericKind;

    fn get_ellided_term_provider<'a>(
        config: &'a mut Config,
    ) -> Option<&'a mut dyn EliidedTermProvider<Self::Argument>>;

    fn on_resolved(
        table: &Table<impl State>,
        config: Config,
        resolved: &Self::Argument,
        parameter: &Self::SyntaxTree,
    );
}

impl GenericParameter for LifetimeParameter {
    type SyntaxTree = syntax_tree::Lifetime;
    type Argument = Lifetime;

    fn resolve(
        table: &Table<impl State>,
        syntax_tree: &Self::SyntaxTree,
        referring_site: GlobalID,
        config: Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Self::Argument, Error> {
        table.resolve_lifetime(syntax_tree, referring_site, config, handler)
    }

    fn get_ellided_term_provider<'a>(
        config: &'a mut Config,
    ) -> Option<&'a mut dyn EliidedTermProvider<Self::Argument>> {
        #[allow(clippy::option_if_let_else)]
        match &mut config.ellided_lifetime_provider {
            Some(provider) => Some(&mut **provider),
            None => None,
        }
    }

    fn generic_kind() -> GenericKind { GenericKind::Lifetime }

    fn on_resolved(
        _: &Table<impl State>,
        _: Config,
        _: &Self::Argument,
        _: &Self::SyntaxTree,
    ) {
    }
}

impl GenericParameter for TypeParameter {
    type SyntaxTree = syntax_tree::r#type::Type;
    type Argument = term::r#type::Type;

    fn resolve(
        table: &Table<impl State>,
        syntax_tree: &Self::SyntaxTree,
        referring_site: GlobalID,
        config: Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Self::Argument, Error> {
        table.resolve_type(syntax_tree, referring_site, config, handler)
    }

    fn get_ellided_term_provider<'a>(
        config: &'a mut Config,
    ) -> Option<&'a mut dyn EliidedTermProvider<Self::Argument>> {
        #[allow(clippy::option_if_let_else)]
        match &mut config.ellided_type_provider {
            Some(provider) => Some(&mut **provider),
            None => None,
        }
    }

    fn generic_kind() -> GenericKind { GenericKind::Type }

    fn on_resolved(
        _: &Table<impl State>,
        _: Config,
        _: &Self::Argument,
        _: &Self::SyntaxTree,
    ) {
    }
}

impl GenericParameter for ConstantParameter {
    type SyntaxTree = syntax_tree::expression::Expression;
    type Argument = term::constant::Constant;

    fn resolve(
        table: &Table<impl State>,
        syntax_tree: &Self::SyntaxTree,
        referring_site: GlobalID,
        _: Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Self::Argument, Error> {
        table.evaluate(syntax_tree, referring_site, handler).map_err(
            |x| match x {
                evaluate::Error::InvalidReferringSiteID => {
                    Error::InvalidReferringSiteID
                }
                evaluate::Error::Suboptimal
                | evaluate::Error::SemanticError => Error::SemanticError,
            },
        )
    }

    fn get_ellided_term_provider<'a>(
        config: &'a mut Config,
    ) -> Option<&'a mut dyn EliidedTermProvider<Self::Argument>> {
        #[allow(clippy::option_if_let_else)]
        match &mut config.ellided_constant_provider {
            Some(provider) => Some(&mut **provider),
            None => None,
        }
    }

    fn generic_kind() -> GenericKind { GenericKind::Constant }

    fn on_resolved(
        _: &Table<impl State>,
        mut config: Config,
        constant: &Self::Argument,
        syntax_tree: &Self::SyntaxTree,
    ) {
        if let Some(observer) = config.observer.as_mut() {
            observer.on_constant_arguments_resolved(constant, syntax_tree);
        }
    }
}

/// Represents a resolution for a symbol.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
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

/// A trait for providing elided terms.
pub trait EliidedTermProvider<T>: Debug {
    /// Creates a new instance of the term to supply the required missing term.
    fn create(&mut self) -> T;
}

/// A trait for observing the resolution process.
///
/// The trait will notified when a type, lifetime, or constant is resolved
/// during the resolution process.
pub trait Observer: Debug {
    /// Notifies the observer when a type is resolved.
    fn on_type_resolved(
        &mut self,
        ty: &r#type::Type,
        syntax_tree: &syntax_tree::r#type::Type,
    );

    /// Notifies the observer when a lifetime is resolved.
    fn on_lifetime_resolved(
        &mut self,
        lifetime: &Lifetime,
        syntax_tree: &syntax_tree::Lifetime,
    );

    /// Notifies the observer when a constant is resolved.
    fn on_constant_arguments_resolved(
        &mut self,
        constant: &constant::Constant,
        syntax_tree: &syntax_tree::expression::Expression,
    );
}

/// The configuration struct specifying the behaviour of the resolution process.
#[derive(Debug)]
pub struct Config<'lp, 'tp, 'cp, 'ob, 'hrlt> {
    /// If specified, when the lifetime argument is elided, the provider will
    /// be used to supply the missing required lifetimes.
    pub ellided_lifetime_provider:
        Option<&'lp mut dyn EliidedTermProvider<Lifetime>>,

    /// If specified, when the type argument is elided, the provider will be
    /// used to supply the missing required types.
    pub ellided_type_provider:
        Option<&'tp mut dyn EliidedTermProvider<term::r#type::Type>>,

    /// If specified, when the constant argument is elided, the provider will
    /// be used to supply the missing required constants.
    pub ellided_constant_provider:
        Option<&'cp mut dyn EliidedTermProvider<term::constant::Constant>>,

    /// If specified, during the resolution process, the observer will be
    /// notified each time a type, lifetime, or constant is resolved.
    pub observer: Option<&'ob mut dyn Observer>,

    /// If specified, when resolving a lifetime parameter, these higher-ranked
    /// lifetimes map will be taken into consideration.
    pub higher_ranked_liftimes: Option<&'hrlt HashMap<String, Forall>>,
}

impl<'lp, 'tp, 'cp, 'ob, 'hrlt> Config<'lp, 'tp, 'cp, 'ob, 'hrlt> {
    /// Creates a new instance of the config.
    #[allow(clippy::option_if_let_else)]
    pub fn reborrow(&mut self) -> Config {
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

    fn resolve_single_no_generics(
        &self,
        identifier: &Identifier,
        latest_resolution: &Option<Resolution>,
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
                                super::GetMemberError::InvalidID => {
                                    unreachable!("invalid ID detected!")
                                }
                                super::GetMemberError::MemberNotFound => None,
                            },
                            Some,
                        ),
                    Some(parent_global_id),
                )
            }
            None => {
                if search_from_root {
                    (
                        self.root_module_ids_by_name
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
    ) -> Result<LifetimeParameterID, Error> {
        for scope in self
            .scope_walker(referring_site)
            .ok_or(Error::InvalidReferringSiteID)?
        {
            let Ok(generic_id) = symbol::GenericID::try_from(scope) else {
                continue;
            };

            let generic_symbol =
                self.get_generic(generic_id).expect("should've been valid");

            if let Some(lifetime_id) = generic_symbol
                .generic_declaration()
                .parameters
                .lifetime_parameter_ids_by_name
                .get(identifier.span.str())
                .copied()
            {
                return Ok(MemberID { parent: generic_id, id: lifetime_id });
            }
        }

        handler.receive(Box::new(LifetimeParameterNotFound {
            referred_span: identifier.span(),
            referring_site,
        }));
        Err(Error::SemanticError)
    }

    /// Resolves a [`syntax_tree::Lifetime`] to a [`Lifetime`].
    ///
    /// # Errors
    ///
    /// See [`Error`] for more information.
    pub fn resolve_lifetime(
        &self,
        lifetime_argument: &syntax_tree::Lifetime,
        referring_site: GlobalID,
        config: Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Lifetime, Error> {
        let lifetime = match lifetime_argument.identifier() {
            LifetimeIdentifier::Static(..) => Ok(Lifetime::Static),
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

                self.resolve_lifetime_parameter(ident, referring_site, handler)
                    .map(Lifetime::Parameter)
            }
        }?;

        if let Some(observer) = config.observer {
            observer.on_lifetime_resolved(&lifetime, lifetime_argument);
        }

        Ok(lifetime)
    }

    #[allow(clippy::too_many_lines)]
    fn resolve_generic_arguments(
        &self,
        generic_identifier: &GenericIdentifier,
        referring_site: GlobalID,
        resolved_id: GlobalID,
        mut config: Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Option<GenericArguments>, Error> {
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

        // NOTE: If there's any dead lock, it's might be because of this
        let generic_declaration =
            self.get_generic(generic_id).expect("should've been valid");

        Ok(Some(GenericArguments {
            lifetimes: self.resolve_generic_arguments_kinds(
                lifetime_argument_syns.into_iter(),
                generic_declaration
                    .generic_declaration()
                    .parameters
                    .lifetime_order
                    .iter()
                    .map(|x| {
                        &generic_declaration
                            .generic_declaration()
                            .parameters
                            .lifetimes[*x]
                    }),
                Option::<std::iter::Empty<_>>::None,
                generic_identifier.span(),
                referring_site,
                config.reborrow(),
                handler,
            )?,
            types: self.resolve_generic_arguments_kinds(
                type_argument_syns.into_iter(),
                generic_declaration
                    .generic_declaration()
                    .parameters
                    .type_order
                    .iter()
                    .map(|x| {
                        &generic_declaration
                            .generic_declaration()
                            .parameters
                            .types[*x]
                    }),
                constant_argument_syns.is_empty().then(|| {
                    generic_declaration
                        .generic_declaration()
                        .parameters
                        .default_type_parameters
                        .iter()
                }),
                generic_identifier.span(),
                referring_site,
                config.reborrow(),
                handler,
            )?,
            constants: self.resolve_generic_arguments_kinds(
                constant_argument_syns.into_iter(),
                generic_declaration
                    .generic_declaration()
                    .parameters
                    .constant_order
                    .iter()
                    .map(|x| {
                        &generic_declaration
                            .generic_declaration()
                            .parameters
                            .constants[*x]
                    }),
                Some(
                    generic_declaration
                        .generic_declaration()
                        .parameters
                        .default_constant_parameters
                        .iter(),
                ),
                generic_identifier.span(),
                referring_site,
                config,
                handler,
            )?,
        }))
    }

    fn resolution_to_type(
        &self,
        resolution: Resolution,
    ) -> Result<r#type::Type, Resolution> {
        match resolution {
            Resolution::Generic(symbol) => {
                let id = match symbol.id {
                    GenericID::Struct(id) => SymbolID::Struct(id),
                    GenericID::Enum(id) => SymbolID::Enum(id),
                    GenericID::Type(id) => SymbolID::Type(id),

                    GenericID::TraitImplementationType(id) => {
                        return Ok(r#type::Type::MemberSymbol(MemberSymbol {
                            id: id.into(),
                            member_generic_arguments: symbol.generic_arguments,
                            parent_generic_arguments: {
                                let implementation_id =
                                    self.get(id).unwrap().parent_id;

                                self.get(implementation_id)
                                    .unwrap()
                                    .signature
                                    .generic_declaration
                                    .parameters
                                    .create_identity_generic_arguments(
                                        implementation_id.into(),
                                    )
                            },
                        }));
                    }
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
    pub fn resolve_type(
        &self,
        syntax_tree: &syntax_tree::r#type::Type,
        referring_site: GlobalID,
        mut config: Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<r#type::Type, Error> {
        let ty = match syntax_tree {
            syntax_tree::r#type::Type::Primitive(primitive) => {
                Ok(r#type::Type::Primitive(match primitive {
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
                }))
            }
            syntax_tree::r#type::Type::Local(local) => {
                Ok(r#type::Type::Local(Local(Box::new(self.resolve_type(
                    local.ty(),
                    referring_site,
                    config.reborrow(),
                    handler,
                )?))))
            }
            syntax_tree::r#type::Type::QualifiedIdentifier(
                qualified_identifier,
            ) => self.resolve_qualified_identifier_type(
                qualified_identifier,
                referring_site,
                config.reborrow(),
                handler,
            ),
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
                    return Err(Error::SemanticError);
                };

                let qualifier = match reference.qualifier() {
                    Some(syntax_tree::Qualifier::Mutable(..)) => {
                        Qualifier::Mutable
                    }
                    Some(syntax_tree::Qualifier::Restrict(..)) => {
                        Qualifier::Restrict
                    }
                    None => Qualifier::Immutable,
                };

                let pointee = Box::new(self.resolve_type(
                    reference.operand(),
                    referring_site,
                    config.reborrow(),
                    handler,
                )?);

                Ok(r#type::Type::Reference(r#type::Reference {
                    qualifier,
                    lifetime,
                    pointee,
                }))
            }
            syntax_tree::r#type::Type::Pointer(pointer_ty) => {
                let qualifier = match pointer_ty.qualifier() {
                    Some(syntax_tree::Qualifier::Mutable(..)) => {
                        Qualifier::Mutable
                    }
                    Some(syntax_tree::Qualifier::Restrict(..)) => {
                        Qualifier::Restrict
                    }
                    None => Qualifier::Immutable,
                };

                let pointee = Box::new(self.resolve_type(
                    pointer_ty.operand(),
                    referring_site,
                    config.reborrow(),
                    handler,
                )?);

                Ok(r#type::Type::Pointer(r#type::Pointer {
                    qualifier,
                    pointee,
                }))
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
                            ty @ (r#type::Type::TraitMember(_)
                            | r#type::Type::Parameter(_)) => {
                                elements.push(TupleElement::Unpacked(ty));
                            }
                            r#type::Type::Tuple(tuple) => {
                                elements.extend(tuple.elements.into_iter());
                            }
                            ty => elements.push(TupleElement::Regular(ty)),
                        }
                    } else {
                        elements.push(TupleElement::Regular(ty));
                    }
                }

                // check if there is more than one unpacked type
                if elements.iter().filter(|x| x.is_unpacked()).count() > 1 {
                    handler.receive(Box::new(MoreThanOneUnpackedInTupleType {
                        illegal_tuple_type_span: syntax_tree.span(),
                    }));
                    return Err(Error::SemanticError);
                }

                Ok(r#type::Type::Tuple(r#type::Tuple { elements }))
            }
            syntax_tree::r#type::Type::Array(array) => {
                let length = self
                    .evaluate(array.expression(), referring_site, handler)
                    .map_err(|x| match x {
                        evaluate::Error::InvalidReferringSiteID => {
                            Error::InvalidReferringSiteID
                        }
                        evaluate::Error::SemanticError
                        | evaluate::Error::Suboptimal => Error::SemanticError,
                    })?;
                let element_ty = self.resolve_type(
                    array.operand(),
                    referring_site,
                    config.reborrow(),
                    handler,
                )?;

                Ok(r#type::Type::Array(r#type::Array {
                    length,
                    r#type: Box::new(element_ty),
                }))
            }
        }?;

        if let Some(observer) = config.observer.as_mut() {
            observer.on_type_resolved(&ty, syntax_tree);
        }

        Ok(ty)
    }

    fn resolve_qualified_identifier_type(
        &self,
        syntax_tree: &syntax_tree::QualifiedIdentifier,
        referring_site: GlobalID,
        config: Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<r#type::Type, Error> {
        let is_simple_identifier = syntax_tree.rest().is_empty()
            && syntax_tree.leading_scope_separator().is_none()
            && syntax_tree.first().generic_arguments().is_none();

        // try to resolve the identifier as a type parameter
        if is_simple_identifier {
            for global_id in self
                .scope_walker(referring_site)
                .ok_or(Error::InvalidReferringSiteID)?
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
                    .type_parameter_ids_by_name
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

        self.resolution_to_type(self.resolve(
            syntax_tree,
            referring_site,
            config,
            handler,
        )?)
        .map_err(|err| {
            handler.receive(Box::new(ExpectType {
                non_type_symbol_span: syntax_tree.span(),
                resolved_global_id: err.global_id(),
            }));
            Error::SemanticError
        })
    }

    #[allow(clippy::too_many_arguments)]
    fn resolve_generic_arguments_kinds<'a, G: GenericParameter>(
        &self,
        syntax_trees: impl ExactSizeIterator<Item = &'a G::SyntaxTree>,
        parameters: impl ExactSizeIterator<Item = &'a G>,
        defaults: Option<impl ExactSizeIterator<Item = &'a G::Argument>>,
        generic_identifier_span: Span,
        referring_site: GlobalID,
        mut config: Config,
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
                handler.receive(Box::new(MisMatchedGenericArgumentCount {
                    generic_kind: G::generic_kind(),
                    generic_identifier_span,
                    expected_count: parameters.len(),
                    supplied_count: syntax_trees.len(),
                }));
                return Err(Error::SemanticError);
            };

            Ok(parameters.map(|_| provider.create()).collect())
        } else if matches!(&defaults, Some(defaults) if defaults.len() == parameters.len())
        {
            Ok(defaults.unwrap().cloned().collect())
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
                handler.receive(Box::new(MisMatchedGenericArgumentCount {
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
                    defaults.skip(default_len - default_fill_count).cloned(),
                );
            }

            Ok(arguments)
        }
    }

    fn get_parent_generic_arguments_from_latest_resolution(
        &self,
        parent_generic_id: symbol::GenericID,
        latest_resolution: Option<Resolution>,
    ) -> GenericArguments {
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
    fn to_resolution(
        &self,
        resolved_id: GlobalID,
        generic_arguments: Option<GenericArguments>,
        latest_resolution: Option<Resolution>,
    ) -> Resolution {
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
            | GlobalID::TraitImplementation(_) => {
                unreachable!("impossible to refer to a trait implementation")
            }
            GlobalID::TraitImplementationFunction(id) => {
                Resolution::Generic(Generic {
                    id: id.into(),
                    generic_arguments: generic_arguments.unwrap(),
                })
            }
            GlobalID::TraitImplementationType(id) => {
                Resolution::Generic(Generic {
                    id: id.into(),
                    generic_arguments: generic_arguments.unwrap(),
                })
            }
            GlobalID::TraitImplementationConstant(id) => {
                Resolution::Generic(Generic {
                    id: id.into(),
                    generic_arguments: generic_arguments.unwrap(),
                })
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
    pub fn resolve(
        &self,
        qualified_identifier: &QualifiedIdentifier,
        referring_site: GlobalID,
        mut config: Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Resolution, Error> {
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

            // invoke state-specific logic on global_id_resolved
            T::on_global_id_resolved(self, global_id, referring_site, handler);

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

            latest_resolution = Some(next_resolution);
        }

        Ok(latest_resolution.expect("should at least have one resolution"))
    }
}