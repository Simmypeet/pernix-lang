//! Contains the logic for resolving symbols and types.

use std::{collections::HashMap, convert::Into, fmt::Debug};

use enum_as_inner::EnumAsInner;
use pernixc_base::{
    handler::Handler,
    source_file::{SourceElement, Span},
};
use pernixc_lexical::token::Identifier;
use pernixc_syntax::syntax_tree::{
    self, ConnectedList, LifetimeIdentifier, QualifiedIdentifier,
    QualifiedIdentifierRoot, SimplePath, SimplePathRoot,
};

use super::{
    evaluate,
    representation::{self, Container, Index, Representation},
    State, Table,
};
use crate::{
    arena::ID,
    error::{
        self, ExpectType, LifetimeParameterNotFound,
        MismatchedGenericArgumentCount, MisorderedGenericArgument,
        MoreThanOneUnpackedInTupleType, NoGenericArgumentsRequired,
        SymbolNotFound, ThisNotFound, UnexpectedInference,
    },
    symbol::{
        self, AdtID, AdtImplementationFunction, Constant, ConstantParameter,
        Enum, Function, GenericKind, GlobalID, LifetimeParameter,
        LifetimeParameterID, Marker, MemberID, Module,
        PositiveTraitImplementation, Struct, Trait, TraitConstant,
        TraitFunction, TraitImplementationConstant,
        TraitImplementationFunction, TraitImplementationType, TraitType, Type,
        TypeParameter, TypeParameterID,
    },
    type_system::{
        instantiation::{self, Instantiation},
        model::{Default, Model},
        term::{
            self, constant,
            lifetime::{Forall, Lifetime},
            r#type::{self, Qualifier},
            GenericArguments, Local, Term, TupleElement,
        },
    },
};

/// An enumeration of the symbols that accepts generic parameters and are not a
/// member of another symbol.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    derive_more::From,
    enum_as_inner::EnumAsInner,
)]
#[allow(missing_docs)]
pub enum GenericID {
    Struct(ID<Struct>),
    Enum(ID<Enum>),
    Function(ID<Function>),
    Trait(ID<Trait>),
    Constant(ID<Constant>),
    Type(ID<Type>),
    Marker(ID<Marker>),

    /*
    Trait implementations are included here instead of in the `MemberGenericID`
    because you can never refer to a trait implementation member with specifying
    the trait implementation itself.
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
            GenericID::Marker(id) => Self::Marker(id),

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
            GenericID::Marker(id) => Self::Marker(id),

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
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    derive_more::From,
    enum_as_inner::EnumAsInner,
)]
pub enum MemberGenericID {
    TraitFunction(ID<TraitFunction>),
    TraitType(ID<TraitType>),
    TraitConstant(ID<TraitConstant>),
    AdtImplementationFunction(ID<AdtImplementationFunction>),
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

    fn generic_kind() -> GenericKind;

    fn get_ellided_term_provider<'a, S: State>(
        config: &'a mut Config<S, M>,
    ) -> Option<&'a mut dyn ElidedTermProvider<Self::Argument>>;
}

impl<M: Model> GenericParameter<M> for LifetimeParameter {
    type SyntaxTree = syntax_tree::Lifetime;
    type Argument = Lifetime<M>;

    fn get_ellided_term_provider<'a, S: State>(
        config: &'a mut Config<S, M>,
    ) -> Option<&'a mut dyn ElidedTermProvider<Self::Argument>> {
        #[allow(clippy::option_if_let_else)]
        match &mut config.elided_lifetime_provider {
            Some(provider) => Some(&mut **provider),
            None => None,
        }
    }

    fn generic_kind() -> GenericKind { GenericKind::Lifetime }
}

impl<M: Model> GenericParameter<M> for TypeParameter {
    type SyntaxTree = syntax_tree::r#type::Type;
    type Argument = term::r#type::Type<M>;

    fn get_ellided_term_provider<'a, S: State>(
        config: &'a mut Config<S, M>,
    ) -> Option<&'a mut dyn ElidedTermProvider<Self::Argument>> {
        #[allow(clippy::option_if_let_else)]
        match &mut config.elided_type_provider {
            Some(provider) => Some(&mut **provider),
            None => None,
        }
    }

    fn generic_kind() -> GenericKind { GenericKind::Type }
}

impl<M: Model> GenericParameter<M> for ConstantParameter {
    type SyntaxTree = syntax_tree::Constant;
    type Argument = term::constant::Constant<M>;

    fn get_ellided_term_provider<'a, S: State>(
        config: &'a mut Config<S, M>,
    ) -> Option<&'a mut dyn ElidedTermProvider<Self::Argument>> {
        #[allow(clippy::option_if_let_else)]
        match &mut config.elided_constant_provider {
            Some(provider) => Some(&mut **provider),
            None => None,
        }
    }

    fn generic_kind() -> GenericKind { GenericKind::Constant }
}

/// Represents a resolution for a symbol.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum Resolution<M: Model> {
    Module(ID<Module>),
    Variant(Variant<M>),
    Generic(Generic<M>),
    MemberGeneric(MemberGeneric<M>),
    PositiveTraitImplementation(ID<PositiveTraitImplementation>),
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
            Self::PositiveTraitImplementation(implementation) => {
                (*implementation).into()
            }
        }
    }
}

/// An error returned by [`Table::resolve_type`], [`Table::resolve_lifetime`],
/// [`Table::resolve_generic_arguments`].
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[allow(missing_docs)]
pub enum ResolveTermError {
    #[error("the given `referring_site` id does not exist in the table")]
    InvalidReferringSiteID,
}

impl From<ResolveTermError> for ResolveQualifiedIdentifierError {
    fn from(value: ResolveTermError) -> Self {
        match value {
            ResolveTermError::InvalidReferringSiteID => {
                Self::InvalidReferringSiteID
            }
        }
    }
}

/// An error returned by [`Table::resolve`].
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[allow(missing_docs)]
pub enum ResolveQualifiedIdentifierError {
    #[error("the given `referring_site` id does not exist in the table")]
    InvalidReferringSiteID,

    #[error("encountered a fatal semantic error that aborts the process")]
    SemanticError,

    #[error(
        "the resolution observer requested to abort the resolution process"
    )]
    Abort,
}

/// An error returned by [`Representation::resolve_sequence`].
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[allow(missing_docs)]
pub enum ResolveSequenceError {
    #[error("the given 'simple_path' iterator was empty")]
    EmptyIterator,

    #[error(transparent)]
    ResolutionError(#[from] ResolveQualifiedIdentifierError),
}

/// A trait for providing elided terms.
pub trait ElidedTermProvider<T> {
    /// Creates a new instance of the term to supply the required missing term.
    fn create(&mut self) -> T;
}

/// A struct for chaining two resolution observer together.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Chain<'f, 's, F, S> {
    first: &'f mut F,
    second: &'s mut S,
}

impl<'f, 's, T: State, M: Model, F: Observer<T, M>, S: Observer<T, M>>
    Observer<T, M> for Chain<'f, 's, F, S>
{
    fn on_global_id_resolved(
        &mut self,
        table: &Table<T>,
        referring_site: GlobalID,
        handler: &dyn Handler<Box<dyn error::Error>>,
        global_id: GlobalID,
        span: &Span,
    ) -> bool {
        self.first.on_global_id_resolved(
            table,
            referring_site,
            handler,
            global_id,
            span,
        ) && self.second.on_global_id_resolved(
            table,
            referring_site,
            handler,
            global_id,
            span,
        )
    }

    fn on_resolution_resolved(
        &mut self,
        table: &Table<T>,
        referring_site: GlobalID,
        handler: &dyn Handler<Box<dyn error::Error>>,
        resolution: &Resolution<M>,
        span: &Span,
    ) -> bool {
        self.first.on_resolution_resolved(
            table,
            referring_site,
            handler,
            resolution,
            span,
        ) && self.second.on_resolution_resolved(
            table,
            referring_site,
            handler,
            resolution,
            span,
        )
    }

    fn on_type_resolved(
        &mut self,
        table: &Table<T>,
        referring_site: GlobalID,
        handler: &dyn Handler<Box<dyn error::Error>>,
        mut ty: r#type::Type<M>,
        syntax_tree: &syntax_tree::r#type::Type,
    ) -> Option<r#type::Type<M>> {
        ty = self.first.on_type_resolved(
            table,
            referring_site,
            handler,
            ty,
            syntax_tree,
        )?;
        self.second.on_type_resolved(
            table,
            referring_site,
            handler,
            ty,
            syntax_tree,
        )
    }

    fn on_lifetime_resolved(
        &mut self,
        table: &Table<T>,
        referring_site: GlobalID,
        handler: &dyn Handler<Box<dyn error::Error>>,
        mut lifetime: Lifetime<M>,
        syntax_tree: &syntax_tree::Lifetime,
    ) -> Option<Lifetime<M>> {
        lifetime = self.first.on_lifetime_resolved(
            table,
            referring_site,
            handler,
            lifetime,
            syntax_tree,
        )?;
        self.second.on_lifetime_resolved(
            table,
            referring_site,
            handler,
            lifetime,
            syntax_tree,
        )
    }

    fn on_constant_arguments_resolved(
        &mut self,
        table: &Table<T>,
        referring_site: GlobalID,
        handler: &dyn Handler<Box<dyn error::Error>>,
        constant: &constant::Constant<M>,
        syntax_tree: &syntax_tree::Constant,
    ) {
        self.first.on_constant_arguments_resolved(
            table,
            referring_site,
            handler,
            constant,
            syntax_tree,
        );
        self.second.on_constant_arguments_resolved(
            table,
            referring_site,
            handler,
            constant,
            syntax_tree,
        );
    }

    fn on_unpacked_type_resolved(
        &mut self,
        table: &Table<T>,
        referring_site: GlobalID,
        handler: &dyn Handler<Box<dyn error::Error>>,
        ty: &r#type::Type<M>,
        syntax_tree: &syntax_tree::r#type::Type,
    ) {
        self.first.on_unpacked_type_resolved(
            table,
            referring_site,
            handler,
            ty,
            syntax_tree,
        );
        self.second.on_unpacked_type_resolved(
            table,
            referring_site,
            handler,
            ty,
            syntax_tree,
        );
    }

    fn on_unpacked_constant_resolved(
        &mut self,
        table: &Table<T>,
        referring_site: GlobalID,
        handler: &dyn Handler<Box<dyn error::Error>>,
        constant: &constant::Constant<M>,
        syntax_tree: &syntax_tree::expression::Expression,
    ) {
        self.first.on_unpacked_constant_resolved(
            table,
            referring_site,
            handler,
            constant,
            syntax_tree,
        );
        self.second.on_unpacked_constant_resolved(
            table,
            referring_site,
            handler,
            constant,
            syntax_tree,
        );
    }
}

/// A trait for observing the resolution process.
///
/// The trait will be notified when a type, lifetime, or constant is resolved
/// during the resolution process.
///
/// This is primarily used for dependency injection.
pub trait Observer<T: State, M: Model> {
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
        span: &Span,
    ) -> bool;

    /// Notifies the observer when a resolution is resolved.
    ///
    /// Returns `true` if the resolution should continue, otherwise `false`.
    ///
    /// When the resolution process is stopped, the term resolution will be
    /// replaced with the `*::Error` variant.
    fn on_resolution_resolved(
        &mut self,
        table: &Table<T>,
        referring_site: GlobalID,
        handler: &dyn Handler<Box<dyn error::Error>>,
        resolution: &Resolution<M>,
        span: &Span,
    ) -> bool;

    /// Notifies the observer when a type is resolved.
    fn on_type_resolved(
        &mut self,
        table: &Table<T>,
        referring_site: GlobalID,
        handler: &dyn Handler<Box<dyn error::Error>>,
        ty: r#type::Type<M>,
        syntax_tree: &syntax_tree::r#type::Type,
    ) -> Option<r#type::Type<M>>;

    /// Notifies the observer when a lifetime is resolved.
    fn on_lifetime_resolved(
        &mut self,
        table: &Table<T>,
        referring_site: GlobalID,
        handler: &dyn Handler<Box<dyn error::Error>>,
        lifetime: Lifetime<M>,
        syntax_tree: &syntax_tree::Lifetime,
    ) -> Option<Lifetime<M>>;

    /// Notifies the observer when a constant is resolved.
    fn on_constant_arguments_resolved(
        &mut self,
        table: &Table<T>,
        referring_site: GlobalID,
        handler: &dyn Handler<Box<dyn error::Error>>,
        constant: &constant::Constant<M>,
        syntax_tree: &syntax_tree::Constant,
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

    /// Chains the observer operation together
    fn chain<'f, 's, U: Observer<T, M>>(
        &'f mut self,
        another: &'s mut U,
    ) -> Chain<'f, 's, Self, U>
    where
        Self: Sized,
    {
        Chain { first: self, second: another }
    }
}

/// The struct implementing the [`Observer`] trait that does nothing.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct NoOp;

impl<T: State, M: Model> Observer<T, M> for NoOp {
    fn on_global_id_resolved(
        &mut self,
        _: &Table<T>,
        _: GlobalID,
        _: &dyn Handler<Box<dyn error::Error>>,
        _: GlobalID,
        _: &Span,
    ) -> bool {
        true
    }

    fn on_resolution_resolved(
        &mut self,
        _: &Table<T>,
        _: GlobalID,
        _: &dyn Handler<Box<dyn error::Error>>,
        _: &Resolution<M>,
        _: &Span,
    ) -> bool {
        true
    }

    fn on_type_resolved(
        &mut self,
        _: &Table<T>,
        _: GlobalID,
        _: &dyn Handler<Box<dyn error::Error>>,
        ty: r#type::Type<M>,
        _: &syntax_tree::r#type::Type,
    ) -> Option<r#type::Type<M>> {
        Some(ty)
    }

    fn on_lifetime_resolved(
        &mut self,
        _: &Table<T>,
        _: GlobalID,
        _: &dyn Handler<Box<dyn error::Error>>,
        lt: Lifetime<M>,
        _: &syntax_tree::Lifetime,
    ) -> Option<Lifetime<M>> {
        Some(lt)
    }

    fn on_constant_arguments_resolved(
        &mut self,
        _: &Table<T>,
        _: GlobalID,
        _: &dyn Handler<Box<dyn error::Error>>,
        _: &constant::Constant<M>,
        _: &syntax_tree::Constant,
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
#[derive(Default)]
#[allow(missing_debug_implementations)]
pub struct Config<'lp, 'tp, 'cp, 'ob, 'hrlt, S: State, M: Model> {
    /// If specified, when the lifetime argument is elided, the provider will
    /// be used to supply the missing required lifetimes.
    pub elided_lifetime_provider:
        Option<&'lp mut dyn ElidedTermProvider<Lifetime<M>>>,

    /// If specified, when the type argument is elided, the provider will be
    /// used to supply the missing required types.
    pub elided_type_provider:
        Option<&'tp mut dyn ElidedTermProvider<term::r#type::Type<M>>>,

    /// If specified, when the constant argument is elided, the provider will
    /// be used to supply the missing required constants.
    pub elided_constant_provider:
        Option<&'cp mut dyn ElidedTermProvider<term::constant::Constant<M>>>,

    /// If specified, during the resolution process, the observer will be
    /// notified each time a type, lifetime, or constant is resolved.
    pub observer: Option<&'ob mut dyn Observer<S, M>>,

    /// If specified, when resolving a lifetime parameter, these higher-ranked
    /// lifetimes map will be taken into consideration.
    pub higher_ranked_lifetimes: Option<&'hrlt HashMap<String, Forall>>,
}

impl<'lp, 'tp, 'cp, 'ob, 'hrlt, S: State, M: Model>
    Config<'lp, 'tp, 'cp, 'ob, 'hrlt, S, M>
{
    /// Creates a new instance of the config.
    #[allow(clippy::option_if_let_else)]
    pub fn reborrow(&mut self) -> Config<S, M> {
        Config {
            elided_lifetime_provider: match &mut self.elided_lifetime_provider {
                Some(provider) => Some(&mut **provider),
                None => None,
            },
            elided_type_provider: match &mut self.elided_type_provider {
                Some(provider) => Some(&mut **provider),
                None => None,
            },
            elided_constant_provider: match &mut self.elided_constant_provider {
                Some(provider) => Some(&mut **provider),
                None => None,
            },
            observer: match &mut self.observer {
                Some(observer) => Some(&mut **observer),
                None => None,
            },
            higher_ranked_lifetimes: self.higher_ranked_lifetimes,
        }
    }
}

impl<S: Container> Representation<S> {
    /// Resolves a [`SimplePath`] as a [`GlobalID`].
    ///
    /// # Errors
    ///
    /// See [`ResolveQualifiedIdentifierError`] for more information.
    pub fn resolve_simple_path(
        &self,
        simple_path: &SimplePath,
        referring_site: GlobalID,
        start_from_root: bool,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<GlobalID, ResolveQualifiedIdentifierError> {
        let root: GlobalID = match simple_path.root() {
            SimplePathRoot::Target(_) => {
                self.get_root_module_id(referring_site).unwrap().into()
            }
            SimplePathRoot::Identifier(ident) => {
                if start_from_root {
                    let Some(id) = self
                        .root_module_ids_by_name()
                        .get(ident.span.str())
                        .copied()
                    else {
                        handler.receive(Box::new(SymbolNotFound {
                            searched_global_id: None,
                            resolution_span: ident.span.clone(),
                        }));

                        return Err(
                            ResolveQualifiedIdentifierError::SemanticError,
                        );
                    };

                    id.into()
                } else {
                    let closet_module_id = self
                        .get_closet_module_id(referring_site)
                        .ok_or(ResolveQualifiedIdentifierError::InvalidReferringSiteID)?;

                    let module = self.get(closet_module_id).unwrap();

                    let Some(id) = module
                        .member_ids_by_name()
                        .get(ident.span.str())
                        .copied()
                        .or_else(|| {
                            module.imports.get(ident.span.str()).map(|x| x.0)
                        })
                    else {
                        handler.receive(Box::new(SymbolNotFound {
                            searched_global_id: Some(closet_module_id.into()),
                            resolution_span: ident.span.clone(),
                        }));

                        return Err(
                            ResolveQualifiedIdentifierError::SemanticError,
                        );
                    };

                    id.into()
                }
            }
        };

        match self.resolve_sequence(
            simple_path.rest().iter().map(|x| &x.1),
            referring_site,
            root,
            handler,
        ) {
            Ok(id) => Ok(id),
            Err(err) => match err {
                ResolveSequenceError::EmptyIterator => Ok(root),
                ResolveSequenceError::ResolutionError(err) => Err(err),
            },
        }
    }

    /// Resolves a sequence of identifier starting of from the given `root`.
    ///
    /// # Errors
    ///
    /// See [`ResolveSequenceError`] for more information.
    pub fn resolve_sequence<'a>(
        &self,
        simple_path: impl Iterator<Item = &'a Identifier>,
        referring_site: GlobalID,
        root: GlobalID,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<GlobalID, ResolveSequenceError> {
        let mut lastest_resolution = root;
        for identifier in simple_path {
            let new_id = self
                .get_member_of(lastest_resolution, identifier.span.str())
                .map_err(|_| {
                    handler.receive(Box::new(SymbolNotFound {
                        searched_global_id: Some(lastest_resolution),
                        resolution_span: identifier.span.clone(),
                    }));
                    ResolveQualifiedIdentifierError::SemanticError
                })?;

            // non-fatal error, no need to return early
            if !self.symbol_accessible(referring_site, new_id).ok_or(
                ResolveQualifiedIdentifierError::InvalidReferringSiteID,
            )? {
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
}

impl<S: State> Table<S> {
    /// Resolves an [`Identifier`] as a [`LifetimeParameterID`].
    ///
    /// # Errors
    ///
    /// See [`ResolveTermError`] for more information
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
    /// See [`ResolveTermError`] for more information.
    pub fn resolve_lifetime<M: Model>(
        &self,
        lifetime_argument: &syntax_tree::Lifetime,
        referring_site: GlobalID,
        config: Config<S, M>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Lifetime<M>, ResolveTermError> {
        let mut lifetime = match lifetime_argument.identifier() {
            LifetimeIdentifier::Static(..) => Lifetime::Static,
            LifetimeIdentifier::Identifier(ident) => {
                if let Some(higher_ranked_lifetimes) =
                    config.higher_ranked_lifetimes
                {
                    if let Some(forall) =
                        higher_ranked_lifetimes.get(ident.span.str())
                    {
                        return Ok(Lifetime::Forall(forall.clone()));
                    }
                }

                self.resolve_lifetime_parameter(ident, referring_site, handler)?
                    .map_or(Lifetime::Error(term::Error), Lifetime::Parameter)
            }
            LifetimeIdentifier::Elided(elided) => {
                config.elided_lifetime_provider.map_or_else(
                    || {
                        handler.receive(Box::new(UnexpectedInference {
                            unexpected_span: elided.span(),
                            generic_kind: GenericKind::Lifetime,
                        }));
                        Lifetime::Error(term::Error)
                    },
                    ElidedTermProvider::create,
                )
            }
        };

        if let Some(observer) = config.observer {
            lifetime = observer
                .on_lifetime_resolved(
                    self,
                    referring_site,
                    handler,
                    lifetime,
                    lifetime_argument,
                )
                .unwrap_or(Lifetime::Error(term::Error));
        }

        Ok(lifetime)
    }

    /// Resolves a [`GenericArguments`] from the given generic arguments syntax
    /// tree.
    ///
    /// # Errors
    ///
    /// See [`ResolveTermError`] for more information.
    pub fn resolve_generic_arguments<M: Model>(
        &self,
        generic_arguments: &syntax_tree::GenericArguments,
        referring_site: GlobalID,
        mut config: Config<S, M>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<GenericArguments<M>, ResolveTermError> {
        let mut lifetime_argument_syns = Vec::new();
        let mut type_argument_syns = Vec::new();
        let mut constant_argument_syns = Vec::new();

        // extracts the generic arguments from the syntax tree to the list of
        // syntax trees
        for generic_argument in generic_arguments
            .connected_list()
            .iter()
            .flat_map(ConnectedList::elements)
        {
            let misordered = match generic_argument {
                syntax_tree::GenericArgument::Constant(arg) => {
                    constant_argument_syns.push(arg.tree());

                    false
                }
                syntax_tree::GenericArgument::Type(arg) => {
                    type_argument_syns.push(&**arg);

                    !constant_argument_syns.is_empty()
                }
                syntax_tree::GenericArgument::Lifetime(arg) => {
                    lifetime_argument_syns.push(arg);

                    !constant_argument_syns.is_empty()
                        || !type_argument_syns.is_empty()
                }
            };

            if misordered {
                handler.receive(Box::new(MisorderedGenericArgument {
                    generic_kind: match generic_argument {
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
                    generic_argument: generic_argument.span(),
                }));
            }
        }

        Ok(GenericArguments {
            lifetimes: lifetime_argument_syns
                .into_iter()
                .map(|x| {
                    self.resolve_lifetime(
                        x,
                        referring_site,
                        config.reborrow(),
                        handler,
                    )
                })
                .collect::<Result<_, _>>()?,
            types: type_argument_syns
                .into_iter()
                .map(|x| {
                    self.resolve_type(
                        x,
                        referring_site,
                        config.reborrow(),
                        handler,
                    )
                })
                .collect::<Result<_, _>>()?,
            constants: constant_argument_syns
                .into_iter()
                .map(|x| match x {
                    syntax_tree::Constant::Expression(expression) => match self
                        .evaluate(&*expression, referring_site, handler)
                    {
                        Ok(value) => Ok(M::from_default_constant(value)),

                        Err(evaluate::Error::InvalidReferringSiteID) => {
                            Err(ResolveTermError::InvalidReferringSiteID)
                        }

                        Err(
                            evaluate::Error::Suboptimal
                            | evaluate::Error::SemanticError,
                        ) => Ok(term::constant::Constant::Error(term::Error)),
                    },
                    syntax_tree::Constant::Elided(elided) => Ok(config
                        .elided_constant_provider
                        .as_mut()
                        .map_or_else(
                            || {
                                handler.receive(Box::new(
                                    UnexpectedInference {
                                        unexpected_span: elided.span(),
                                        generic_kind: GenericKind::Constant,
                                    },
                                ));
                                term::constant::Constant::Error(term::Error)
                            },
                            |provider| provider.create(),
                        )),
                })
                .collect::<Result<_, _>>()?,
        })
    }

    /// Verifies that the given `generic_arguments` have the right amount of
    /// arguments by supplying the default arguments if necessary.
    pub fn verify_generic_arguments_for<M: Model>(
        &self,
        generic_arguments: GenericArguments<M>,
        generic_id: symbol::GenericID,
        generic_identifier_span: Span,
        mut config: Config<S, M>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> GenericArguments<M> {
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
                generic_arguments.constants.is_empty().then(|| {
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

        GenericArguments {
            lifetimes: Self::resolve_generic_arguments_kinds(
                generic_arguments.lifetimes.into_iter(),
                lifetime_parameter_orders.iter(),
                Option::<std::iter::Empty<_>>::None,
                generic_identifier_span.clone(),
                config.reborrow(),
                handler,
            ),
            types: Self::resolve_generic_arguments_kinds(
                generic_arguments.types.into_iter(),
                type_parameter_orders.iter(),
                default_type_arguments.as_ref().map(|x| x.iter()),
                generic_identifier_span.clone(),
                config.reborrow(),
                handler,
            ),
            constants: Self::resolve_generic_arguments_kinds(
                generic_arguments.constants.into_iter(),
                constant_parameter_ords.iter(),
                Some(default_constant_arguments.iter()),
                generic_identifier_span,
                config,
                handler,
            ),
        }
    }

    /// Resolves a [`GenericArguments`] for the `resolved_id` from the given
    /// [`syntax_tree::GenericIdentifier`].
    ///
    /// # Errors
    ///
    /// See [`ResolveTermError`] for more information.
    #[allow(clippy::too_many_lines)]
    pub fn resolve_generic_arguments_for<M: Model>(
        &self,
        resolved_id: symbol::GenericID,
        generic_identifier: &syntax_tree::GenericIdentifier,
        referring_site: GlobalID,
        mut config: Config<S, M>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<GenericArguments<M>, ResolveTermError> {
        let generic_arguments = if let Some(generic_arguments) =
            generic_identifier.generic_arguments().as_ref()
        {
            self.resolve_generic_arguments(
                generic_arguments,
                referring_site,
                config.reborrow(),
                handler,
            )?
        } else {
            GenericArguments::default()
        };

        Ok(self.verify_generic_arguments_for(
            generic_arguments,
            resolved_id,
            generic_identifier.span(),
            config,
            handler,
        ))
    }

    #[allow(clippy::result_large_err)]
    fn resolution_to_type<M: Model>(
        &self,
        resolution: Resolution<M>,
    ) -> Result<r#type::Type<M>, Resolution<M>> {
        match resolution {
            Resolution::Generic(symbol) => match symbol.id {
                GenericID::Struct(_)
                | GenericID::Enum(_)
                | GenericID::Type(_) => {
                    let id = match symbol.id {
                        GenericID::Struct(id) => AdtID::Struct(id),
                        GenericID::Enum(id) => AdtID::Enum(id),
                        GenericID::Type(id) => {
                            let type_symbol = self.get(id).unwrap();

                            let instantiation =
                                Instantiation::from_generic_arguments(
                                    symbol.generic_arguments,
                                    id.into(),
                                    &type_symbol.generic_declaration.parameters,
                                )
                                .unwrap();

                            let mut result_ty = M::from_default_type(
                                type_symbol.r#type.clone(),
                            );

                            instantiation::instantiate(
                                &mut result_ty,
                                &instantiation,
                            );

                            return Ok(result_ty);
                        }

                        _ => unreachable!(),
                    };

                    Ok(r#type::Type::Symbol(term::Symbol {
                        id,
                        generic_arguments: symbol.generic_arguments,
                    }))
                }

                GenericID::TraitImplementationType(id) => {
                    let trait_implementation_type_symbol =
                        self.get(id).unwrap();

                    let instantiation = Instantiation::from_generic_arguments(
                        symbol.generic_arguments,
                        id.into(),
                        &trait_implementation_type_symbol
                            .generic_declaration
                            .parameters,
                    )
                    .unwrap();

                    let mut result_ty = M::from_default_type(
                        trait_implementation_type_symbol.r#type.clone(),
                    );

                    instantiation::instantiate(&mut result_ty, &instantiation);

                    Ok(result_ty)
                }

                _ => Err(Resolution::Generic(symbol)),
            },

            Resolution::MemberGeneric(symbol) => match symbol.id {
                MemberGenericID::TraitType(id) => {
                    Ok(r#type::Type::TraitMember(r#type::TraitMember {
                        id,
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

    /// Resolves a [`syntax_tree::type::Type`] to a [`type::Type`].
    ///
    /// # Errors
    ///
    /// See [`ResolveTermError`] for more information.
    #[allow(clippy::too_many_lines)]
    pub fn resolve_type<M: Model>(
        &self,
        syntax_tree: &syntax_tree::r#type::Type,
        referring_site: GlobalID,
        mut config: Config<S, M>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<r#type::Type<M>, ResolveTermError> {
        let mut ty = match syntax_tree {
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
                    local.r#type(),
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
                    config.elided_lifetime_provider.as_mut()
                {
                    provider.create()
                } else {
                    handler.receive(Box::new(UnexpectedInference {
                        unexpected_span: reference.ampersand().span(),
                        generic_kind: GenericKind::Lifetime,
                    }));
                    Lifetime::Error(term::Error)
                };

                let qualifier = if reference.mutable_keyword().is_some() {
                    Qualifier::Mutable
                } else {
                    Qualifier::Immutable
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
                let pointee = Box::new(self.resolve_type(
                    pointer_ty.operand(),
                    referring_site,
                    config.reborrow(),
                    handler,
                )?);

                r#type::Type::Pointer(r#type::Pointer {
                    mutable: pointer_ty.mutable_keyword().is_some(),
                    pointee,
                })
            }
            syntax_tree::r#type::Type::Tuple(syntax_tree) => {
                let mut elements = Vec::new();

                for element in syntax_tree
                    .connected_list()
                    .iter()
                    .flat_map(ConnectedList::elements)
                {
                    let ty = self.resolve_type(
                        element.r#type(),
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
                                        element.r#type(),
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
                let length = match array.constant() {
                    syntax_tree::Constant::Expression(expression) => match self
                        .evaluate(expression, referring_site, handler)
                    {
                        Ok(value) => M::from_default_constant(value),

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
                    syntax_tree::Constant::Elided(elided) => {
                        config.elided_constant_provider.as_mut().map_or_else(
                            || {
                                handler.receive(Box::new(
                                    UnexpectedInference {
                                        unexpected_span: elided.span(),
                                        generic_kind: GenericKind::Constant,
                                    },
                                ));
                                constant::Constant::Error(term::Error)
                            },
                            |x| x.create(),
                        )
                    }
                };
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
            syntax_tree::r#type::Type::Elided(elided) => {
                config.elided_type_provider.as_mut().map_or_else(
                    || {
                        handler.receive(Box::new(UnexpectedInference {
                            unexpected_span: elided.span(),
                            generic_kind: GenericKind::Type,
                        }));
                        r#type::Type::Error(term::Error)
                    },
                    |provider| provider.create(),
                )
            }
        };

        if let Some(observer) = config.observer.as_mut() {
            ty = observer
                .on_type_resolved(
                    self,
                    referring_site,
                    handler,
                    ty,
                    syntax_tree,
                )
                .unwrap_or(r#type::Type::Error(term::Error));
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
            && syntax_tree
                .root()
                .as_generic_identifier()
                .map_or(false, |x| x.generic_arguments().is_none());

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
                    .get(
                        syntax_tree
                            .root()
                            .as_generic_identifier()
                            .unwrap()
                            .identifier()
                            .span
                            .str(),
                    )
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

                Err(
                    ResolveQualifiedIdentifierError::InvalidReferringSiteID,
                ) => {
                    return Err(ResolveTermError::InvalidReferringSiteID);
                }

                Err(
                    ResolveQualifiedIdentifierError::Abort
                    | ResolveQualifiedIdentifierError::SemanticError,
                ) => {
                    return Ok(r#type::Type::Error(term::Error));
                }
            };

        match self.resolution_to_type(resolution) {
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
    fn resolve_generic_arguments_kinds<
        'a,
        M: Model,
        G: GenericParameter<M> + std::fmt::Debug,
    >(
        generic_arguments: impl ExactSizeIterator<Item = G::Argument>,
        parameters: impl ExactSizeIterator<Item = &'a G>,
        defaults: Option<
            impl ExactSizeIterator<
                Item = &'a <G::Argument as Term>::Rebind<Default>,
            >,
        >,
        generic_identifier_span: Span,
        mut config: Config<S, M>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Vec<G::Argument>
    where
        G::Argument: Clone,
    {
        if generic_arguments.len() == 0 {
            if parameters.len() == 0 {
                return Vec::new();
            }

            let Some(provider) = G::get_ellided_term_provider(&mut config)
            else {
                handler.receive(Box::new(MismatchedGenericArgumentCount {
                    generic_kind: G::generic_kind(),
                    generic_identifier_span,
                    expected_count: parameters.len(),
                    supplied_count: generic_arguments.len(),
                }));

                // return the error terms
                return parameters.map(|_| term::Error.into()).collect();
            };

            parameters.map(|_| provider.create()).collect()
        } else {
            let valid_count = defaults.as_ref().map_or_else(
                || parameters.len() == generic_arguments.len(),
                |defaults| {
                    let expected_range =
                        (parameters.len() - defaults.len())..=parameters.len();

                    expected_range.contains(&generic_arguments.len())
                },
            );

            // check if the number of supplied generic arugmnets is valid
            if !valid_count {
                handler.receive(Box::new(MismatchedGenericArgumentCount {
                    generic_identifier_span,
                    generic_kind: G::generic_kind(),
                    expected_count: parameters.len(),
                    supplied_count: generic_arguments.len(),
                }));
            }

            let mut arguments =
                generic_arguments.take(parameters.len()).collect::<Vec<_>>();

            if valid_count {
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
            } else {
                // extend the arguments with error term
                let extra_term_count = parameters.len() - arguments.len();

                arguments.extend(
                    std::iter::repeat(term::Error.into())
                        .take(extra_term_count),
                );
            }

            assert!(
                arguments.len() == parameters.len(),
                "{:#?} {:#?} {:#?}",
                arguments,
                parameters.into_iter().collect::<Vec<_>>(),
                valid_count
            );

            arguments
        }
    }

    fn get_parent_generic_arguments_from_latest_resolution<M: Model>(
        latest_resolution: Resolution<M>,
    ) -> GenericArguments<M> {
        latest_resolution.into_generic().unwrap().generic_arguments
    }

    #[allow(clippy::too_many_lines)]
    fn to_resolution<M: Model>(
        resolved_id: GlobalID,
        generic_arguments: Option<GenericArguments<M>>,
        latest_resolution: Resolution<M>,
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
                generic_arguments:
                    Self::get_parent_generic_arguments_from_latest_resolution(
                        latest_resolution,
                    ),
                variant: id,
            }),
            GlobalID::Marker(id) => Resolution::Generic(Generic {
                id: id.into(),
                generic_arguments: generic_arguments.unwrap(),
            }),
            GlobalID::TraitType(id) => {
                Resolution::MemberGeneric(MemberGeneric {
                    id: id.into(),
                    parent_generic_arguments: latest_resolution
                        .into_generic()
                        .unwrap()
                        .generic_arguments,
                    generic_arguments: generic_arguments.unwrap(),
                })
            }
            GlobalID::TraitFunction(id) => {
                Resolution::MemberGeneric(MemberGeneric {
                    id: id.into(),
                    parent_generic_arguments: latest_resolution
                        .into_generic()
                        .unwrap()
                        .generic_arguments,
                    generic_arguments: generic_arguments.unwrap(),
                })
            }
            GlobalID::TraitConstant(id) => {
                Resolution::MemberGeneric(MemberGeneric {
                    id: id.into(),
                    parent_generic_arguments: latest_resolution
                        .into_generic()
                        .unwrap()
                        .generic_arguments,
                    generic_arguments: generic_arguments.unwrap(),
                })
            }

            GlobalID::AdtImplementation(_)
            | GlobalID::NegativeTraitImplementation(_)
            | GlobalID::PositiveTraitImplementation(_)
            | GlobalID::PositiveMarkerImplementation(_)
            | GlobalID::NegativeMarkerImplementation(_) => {
                unreachable!("impossible to refer to a trait implementation")
            }

            GlobalID::AdtImplementationFunction(id) => {
                Resolution::MemberGeneric(MemberGeneric {
                    id: id.into(),
                    parent_generic_arguments: match latest_resolution {
                        Resolution::Generic(Generic {
                            id: GenericID::Struct(_) | GenericID::Enum(_),
                            generic_arguments,
                        }) => generic_arguments,
                        found => {
                            panic!("unexpected resolution {found:?}");
                        }
                    },
                    generic_arguments: generic_arguments.unwrap(),
                })
            }

            GlobalID::TraitImplementationFunction(id) => {
                assert!(matches!(
                    latest_resolution,
                    Resolution::PositiveTraitImplementation(_)
                ));

                Resolution::Generic(Generic {
                    id: id.into(),
                    generic_arguments: generic_arguments.unwrap(),
                })
            }

            GlobalID::TraitImplementationType(id) => {
                assert!(matches!(
                    latest_resolution,
                    Resolution::PositiveTraitImplementation(_)
                ));

                Resolution::Generic(Generic {
                    id: id.into(),
                    generic_arguments: generic_arguments.unwrap(),
                })
            }

            GlobalID::TraitImplementationConstant(id) => {
                assert!(matches!(
                    latest_resolution,
                    Resolution::PositiveTraitImplementation(_)
                ));

                Resolution::Generic(Generic {
                    id: id.into(),
                    generic_arguments: generic_arguments.unwrap(),
                })
            }
        }
    }

    /// Resolves for the root of the qualified identifier.
    ///
    /// # Errors
    ///
    /// See [`ResolveQualifiedIdentifierError`] for more information.
    #[allow(clippy::too_many_lines)]
    pub fn resolve_qualified_identifier_root<M: Model>(
        &self,
        root_syn: &QualifiedIdentifierRoot,
        referring_site: GlobalID,
        mut config: Config<S, M>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Resolution<M>, ResolveQualifiedIdentifierError> {
        let span = root_syn.span();
        match root_syn {
            QualifiedIdentifierRoot::Target(_) => {
                let root = self.get_root_module_id(referring_site).ok_or(
                    ResolveQualifiedIdentifierError::InvalidReferringSiteID,
                )?;

                if let Some(observer) = config.observer.as_mut() {
                    if !observer.on_global_id_resolved(
                        self,
                        referring_site,
                        handler,
                        root.into(),
                        &span,
                    ) {
                        return Err(ResolveQualifiedIdentifierError::Abort);
                    }
                }

                let resolution = Resolution::Module(root);

                if let Some(observer) = config.observer.as_mut() {
                    if !observer.on_resolution_resolved(
                        self,
                        referring_site,
                        handler,
                        &resolution,
                        &span,
                    ) {
                        return Err(ResolveQualifiedIdentifierError::Abort);
                    }
                }

                Ok(resolution)
            }
            QualifiedIdentifierRoot::This(this) => {
                let found_this = self
                    .scope_walker(referring_site)
                    .ok_or(
                        ResolveQualifiedIdentifierError::InvalidReferringSiteID,
                    )?
                    .find_map(|x| match x {
                        id @ (GlobalID::Trait(_)
                        | GlobalID::PositiveTraitImplementation(_)
                        | GlobalID::AdtImplementation(_)) => Some(id),
                        _ => None,
                    });

                let Some(this) = found_this else {
                    handler
                        .receive(Box::new(ThisNotFound { span: this.span() }));
                    return Err(ResolveQualifiedIdentifierError::SemanticError);
                };

                if let Some(observer) = config.observer.as_mut() {
                    if !observer.on_global_id_resolved(
                        self,
                        referring_site,
                        handler,
                        this,
                        &span,
                    ) {
                        return Err(ResolveQualifiedIdentifierError::Abort);
                    }
                }

                let resolution = match this {
                    GlobalID::Trait(trait_id) => Resolution::Generic(Generic {
                        id: trait_id.into(),
                        generic_arguments: self
                            .get(trait_id)
                            .unwrap()
                            .generic_declaration
                            .parameters
                            .create_identity_generic_arguments(trait_id.into()),
                    }),
                    GlobalID::PositiveTraitImplementation(id) => {
                        Resolution::PositiveTraitImplementation(id)
                    }
                    GlobalID::AdtImplementation(id) => {
                        let implementation = self.get(id).unwrap();
                        let generic_arguments =
                            GenericArguments::from_default_model(
                                implementation.arguments.clone(),
                            );

                        Resolution::Generic(Generic {
                            id: match implementation.implemented_id {
                                AdtID::Struct(id) => GenericID::Struct(id),
                                AdtID::Enum(id) => GenericID::Enum(id),
                            },
                            generic_arguments,
                        })
                    }
                    _ => unreachable!(),
                };

                if let Some(observer) = config.observer.as_mut() {
                    if !observer.on_resolution_resolved(
                        self,
                        referring_site,
                        handler,
                        &resolution,
                        &span,
                    ) {
                        return Err(ResolveQualifiedIdentifierError::Abort);
                    }
                }

                Ok(resolution)
            }
            QualifiedIdentifierRoot::GenericIdentifier(generic_identifier) => {
                let current_module_id =
                    self.get_closet_module_id(referring_site).ok_or(
                        ResolveQualifiedIdentifierError::InvalidReferringSiteID,
                    )?;
                let module = self.get(current_module_id).unwrap();

                let id = module
                    .member_ids_by_name()
                    .get(generic_identifier.identifier().span.str())
                    .copied()
                    .or_else(|| {
                        module
                            .imports
                            .get(generic_identifier.identifier().span.str())
                            .map(|x| x.0)
                    });

                let Some(id) = id else {
                    handler.receive(Box::new(SymbolNotFound {
                        searched_global_id: Some(current_module_id.into()),
                        resolution_span: generic_identifier
                            .identifier()
                            .span
                            .clone(),
                    }));

                    return Err(ResolveQualifiedIdentifierError::SemanticError);
                };

                if let Some(observer) = config.observer.as_mut() {
                    if !observer.on_global_id_resolved(
                        self,
                        referring_site,
                        handler,
                        id.into(),
                        &generic_identifier.identifier().span,
                    ) {
                        return Err(ResolveQualifiedIdentifierError::Abort);
                    }
                }

                let generic_arguments = if let Ok(generic_id) =
                    symbol::GenericID::try_from(GlobalID::from(id))
                {
                    Some(self.resolve_generic_arguments_for(
                        generic_id,
                        generic_identifier,
                        referring_site,
                        config.reborrow(),
                        handler,
                    )?)
                } else {
                    if let Some(gen_args) =
                        generic_identifier.generic_arguments().as_ref()
                    {
                        handler.receive(Box::new(NoGenericArgumentsRequired {
                            global_id: id.into(),
                            generic_argument_span: gen_args.span(),
                        }));
                    }

                    None
                };

                let resolution = match id {
                    symbol::ModuleMemberID::Module(id) => {
                        assert!(generic_arguments.is_none());
                        Resolution::Module(id)
                    }
                    symbol::ModuleMemberID::Enum(id) => {
                        Resolution::Generic(Generic {
                            id: id.into(),
                            generic_arguments: generic_arguments.unwrap(),
                        })
                    }
                    symbol::ModuleMemberID::Struct(id) => {
                        Resolution::Generic(Generic {
                            id: id.into(),
                            generic_arguments: generic_arguments.unwrap(),
                        })
                    }
                    symbol::ModuleMemberID::Trait(id) => {
                        Resolution::Generic(Generic {
                            id: id.into(),
                            generic_arguments: generic_arguments.unwrap(),
                        })
                    }
                    symbol::ModuleMemberID::Type(id) => {
                        Resolution::Generic(Generic {
                            id: id.into(),
                            generic_arguments: generic_arguments.unwrap(),
                        })
                    }
                    symbol::ModuleMemberID::Function(id) => {
                        Resolution::Generic(Generic {
                            id: id.into(),
                            generic_arguments: generic_arguments.unwrap(),
                        })
                    }
                    symbol::ModuleMemberID::Constant(id) => {
                        Resolution::Generic(Generic {
                            id: id.into(),
                            generic_arguments: generic_arguments.unwrap(),
                        })
                    }
                    symbol::ModuleMemberID::Marker(id) => {
                        Resolution::Generic(Generic {
                            id: id.into(),
                            generic_arguments: generic_arguments.unwrap(),
                        })
                    }
                };

                if let Some(observer) = config.observer.as_mut() {
                    if !observer.on_resolution_resolved(
                        self,
                        referring_site,
                        handler,
                        &resolution,
                        &span,
                    ) {
                        return Err(ResolveQualifiedIdentifierError::Abort);
                    }
                }

                Ok(resolution)
            }
        }
    }

    /// Resolves the symbol for the given qualified identifiers.
    ///
    /// # Parameters
    ///
    /// - `qualified_identifier`: The qualified identifier to resolve the symbol
    ///   for.
    /// - `referring_site`: The global ID of the symbol that is referring to the
    ///   symbol to resolve.
    /// - `config`: The configuration to used resolution.
    /// - `handler`: The handler for the diagnostics.
    ///
    /// # Errors
    ///
    /// See [`ResolveQualifiedIdentifierError`] for more information
    pub fn resolve<M: Model>(
        &self,
        qualified_identifier: &QualifiedIdentifier,
        referring_site: GlobalID,
        mut config: Config<S, M>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Resolution<M>, ResolveQualifiedIdentifierError> {
        // check if the given `referring_site` is a valid ID.
        drop(
            self.get_global(referring_site).ok_or(
                ResolveQualifiedIdentifierError::InvalidReferringSiteID,
            )?,
        );

        // create the current root
        let mut latest_resolution = self.resolve_qualified_identifier_root(
            qualified_identifier.root(),
            referring_site,
            config.reborrow(),
            handler,
        )?;

        for (_, generic_identifier) in qualified_identifier.rest() {
            let global_id = match self.get_member_of(
                latest_resolution.global_id(),
                generic_identifier.identifier().span.str(),
            ) {
                Ok(id) => id,
                Err(error) => match error {
                    representation::GetMemberError::InvalidID => unreachable!(),
                    representation::GetMemberError::MemberNotFound => {
                        handler.receive(Box::new(SymbolNotFound {
                            searched_global_id: Some(
                                latest_resolution.global_id(),
                            ),
                            resolution_span: generic_identifier
                                .identifier()
                                .span
                                .clone(),
                        }));

                        return Err(
                            ResolveQualifiedIdentifierError::SemanticError,
                        );
                    }
                },
            };

            // invoke the observer
            if let Some(observer) = config.observer.as_mut() {
                if !observer.on_global_id_resolved(
                    self,
                    referring_site,
                    handler,
                    global_id,
                    &generic_identifier.identifier().span,
                ) {
                    return Err(ResolveQualifiedIdentifierError::Abort);
                }
            }

            let generic_arguments = if let Ok(generic_id) =
                symbol::GenericID::try_from(global_id)
            {
                Some(self.resolve_generic_arguments_for(
                    generic_id,
                    generic_identifier,
                    referring_site,
                    config.reborrow(),
                    handler,
                )?)
            } else {
                if let Some(gen_args) =
                    generic_identifier.generic_arguments().as_ref()
                {
                    handler.receive(Box::new(NoGenericArgumentsRequired {
                        global_id,
                        generic_argument_span: gen_args.span(),
                    }));
                }

                None
            };

            let next_resolution = Self::to_resolution(
                global_id,
                generic_arguments,
                latest_resolution,
            );

            if let Some(observer) = config.observer.as_mut() {
                if !observer.on_resolution_resolved(
                    self,
                    referring_site,
                    handler,
                    &next_resolution,
                    &generic_identifier.span(),
                ) {
                    return Err(ResolveQualifiedIdentifierError::Abort);
                }
            }

            latest_resolution = next_resolution;
        }

        Ok(latest_resolution)
    }
}
