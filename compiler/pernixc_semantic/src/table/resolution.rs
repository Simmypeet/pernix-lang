//! Contains logic related to symbol resolution.

use enum_as_inner::EnumAsInner;
use pernixc_base::{
    diagnostic::Handler,
    source_file::{SourceElement, Span},
};
use pernixc_lexical::token::Identifier;
use pernixc_syntax::syntax_tree::{
    self, item::ModulePath, ConnectedList, GenericIdentifier, LifetimeIdentifier,
    QualifiedIdentifier,
};

use super::{Error, Index, State, Table};
use crate::{
    arena::{Arena, ID},
    error::{
        self, GenericArgumentCountMismatch, LifetimeExpected, LifetimeParameterNotFound,
        MisorderedGenericArgument, ModuleExpected, MoreThanOneUnpackedInTupleType,
        NoGenericArgumentsRequired, ResolutionAmbiguity, SymbolIsNotAccessible, SymbolNotFound,
        TraitExpectedInImplemenation, TypeExpected,
    },
    semantic::{
        model::{Entity, Model},
        substitution::{Substitute, Substitution},
        term::{
            constant,
            lifetime::Lifetime,
            r#type::{self, Algebraic, Local, Qualifier},
            GenericArguments, TupleElement, Unpacked,
        },
    },
    symbol::{
        self, semantic::Symbolic, AdtImplementationMemberID, AlgebraicKind, ConstantParameter,
        Function, GenericID, GenericKind, GlobalID, LifetimeParameter, LifetimeParameterID,
        MemberID, Module, Trait, TraitConstant, TraitFunction, TraitType, TypeParameter,
        TypeParameterID,
    },
};

trait GenericParameter: Sized + 'static {
    type SyntaxTree;
    type Argument<S: Model>;

    fn elision_placeholder<S: Model>(config: &mut dyn Config<S>) -> Option<Self::Argument<S>>;

    fn resolve<S: Model>(
        table: &Table<impl State>,
        syntax_tree: &Self::SyntaxTree,
        referring_site: GlobalID,
        config: &mut dyn Config<S>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Self::Argument<S>, Error>;

    fn generic_kind() -> GenericKind;

    fn on_resolved<S: Model>(
        config: &mut dyn Config<S>,
        argument: &Self::Argument<S>,
        parameter: &Self,
        span: &Span,
    );

    #[allow(clippy::too_many_arguments)]
    fn resolve_generic_parameters<'a, 'b, S, I, D>(
        table: &Table<impl State>,
        syntax_trees: I,
        parameters: &Arena<Self>,
        defaults: D,
        allows_default: bool,
        generic_identifier_span: Span,
        referring_site: GlobalID,
        config: &mut dyn Config<S>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Vec<Self::Argument<S>>, Error>
    where
        S: Model,
        I: ExactSizeIterator<Item = &'a Self::SyntaxTree>,
        D: ExactSizeIterator<Item = Self::Argument<S>>,
    {
        let parameters = parameters.iter();

        if syntax_trees.len() == 0 {
            // no arguments required
            if parameters.len() == 0 {
                return Ok(Vec::new());
            }

            if let Some(first) = Self::elision_placeholder(config) {
                let mut arguments = vec![first];

                // fortunate
                if arguments.len() == parameters.len() {
                    return Ok(arguments);
                }

                while let Some(argument) = Self::elision_placeholder(config) {
                    arguments.push(argument);

                    if arguments.len() == parameters.len() {
                        return Ok(arguments);
                    }
                }

                // no more placeholder available
                handler.receive(Box::new(GenericArgumentCountMismatch {
                    expected_count: parameters.len(),
                    supplied_count: 0,
                    generic_identifier_span,
                    generic_kind: Self::generic_kind(),
                }));

                Err(Error::SemanticError)
            } else if allows_default && defaults.len() == parameters.len() {
                // use default arguments
                Ok(defaults.collect())
            } else {
                // no arguments supplied
                handler.receive(Box::new(GenericArgumentCountMismatch {
                    expected_count: parameters.len(),
                    supplied_count: 0,
                    generic_identifier_span,
                    generic_kind: Self::generic_kind(),
                }));

                Err(Error::SemanticError)
            }
        } else {
            let valid_count = if allows_default {
                let expected_range = parameters.len() - defaults.len()..=parameters.len();

                expected_range.contains(&parameters.len())
            } else {
                parameters.len() == syntax_trees.len()
            };

            if !valid_count {
                handler.receive(Box::new(GenericArgumentCountMismatch {
                    expected_count: parameters.len(),
                    supplied_count: syntax_trees.len(),
                    generic_identifier_span,
                    generic_kind: Self::generic_kind(),
                }));
                return Err(Error::SemanticError);
            }

            let mut arguments = Vec::new();

            for syntax_tree in syntax_trees.take(parameters.len()) {
                arguments.push(Self::resolve(
                    table,
                    syntax_tree,
                    referring_site,
                    config,
                    handler,
                )?);
            }

            let left = parameters.len() - arguments.len();
            let default_fill = left.min(defaults.len());
            let default_len = defaults.len();

            arguments.extend(defaults.skip(default_len - default_fill));

            Ok(arguments)
        }
    }
}

impl GenericParameter for TypeParameter {
    type Argument<S: Model> = r#type::Type<S>;
    type SyntaxTree = syntax_tree::r#type::Type;

    fn elision_placeholder<S: Model>(config: &mut dyn Config<S>) -> Option<Self::Argument<S>> {
        config.type_arguments_elision_placeholder()
    }

    fn resolve<S: Model>(
        table: &Table<impl State>,
        syntax_tree: &Self::SyntaxTree,
        referring_site: GlobalID,
        config: &mut dyn Config<S>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Self::Argument<S>, Error> {
        table.resolve_type(syntax_tree, referring_site, config, handler)
    }

    fn generic_kind() -> GenericKind { GenericKind::Type }

    fn on_resolved<S: Model>(_: &mut dyn Config<S>, _: &Self::Argument<S>, _: &Self, _: &Span) {}
}

impl GenericParameter for LifetimeParameter {
    type Argument<S: Model> = Lifetime<S>;
    type SyntaxTree = syntax_tree::Lifetime;

    fn elision_placeholder<S: Model>(config: &mut dyn Config<S>) -> Option<Self::Argument<S>> {
        config.lifetime_arguments_elision_placeholder()
    }

    fn resolve<S: Model>(
        table: &Table<impl State>,
        syntax_tree: &Self::SyntaxTree,
        referring_site: GlobalID,
        config: &mut dyn Config<S>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Self::Argument<S>, Error> {
        table.resolve_lifetime(syntax_tree, referring_site, config, handler)
    }

    fn generic_kind() -> GenericKind { GenericKind::Lifetime }

    fn on_resolved<S: Model>(_: &mut dyn Config<S>, _: &Self::Argument<S>, _: &Self, _: &Span) {}
}

impl GenericParameter for ConstantParameter {
    type Argument<S: Model> = constant::Constant<S>;
    type SyntaxTree = syntax_tree::expression::Expression;

    fn elision_placeholder<S: Model>(config: &mut dyn Config<S>) -> Option<Self::Argument<S>> {
        config.constant_arguments_elision_placeholder()
    }

    fn resolve<S: Model>(
        table: &Table<impl State>,
        syntax_tree: &Self::SyntaxTree,
        referring_site: GlobalID,
        config: &mut dyn Config<S>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Self::Argument<S>, Error> {
        Ok(table.evaluate(syntax_tree, referring_site, config, handler)?)
    }

    fn generic_kind() -> GenericKind { GenericKind::Constant }

    fn on_resolved<S: Model>(
        config: &mut dyn Config<S>,
        argument: &Self::Argument<S>,
        parameter: &Self,
        span: &Span,
    ) {
        config.on_constant_arguments_resolved(
            argument,
            &parameter.r#type.clone().into_other_model(),
            span,
        );
    }
}

/// The error type returned by resolution.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Error)]
#[allow(missing_docs)]
pub enum Error {
    #[error("The given `referring_site` id does not exist in the table")]
    InvalidReferringSiteID,

    #[error("Encountered a fatal semantic error that aborts the process")]
    SemanticError,
}

/// Represents a resolution of a symbol with generic arguments supplied.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct WithGenerics<ID, S: Model> {
    /// The ID of the resolved symbol.
    pub id: ID,

    /// The generic arguments supplied to the symbol.
    pub generic_arguments: GenericArguments<S>,
}

/// Enum variant symbol resolution.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Variant<S: Model> {
    /// The generic arguments supplied to the enum.
    pub enum_generic_arguments: GenericArguments<S>,

    /// The resolved enum variant ID.
    pub id: ID<symbol::Variant>,
}

/// Symbol resolution with parent resolution.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct WithParent<Member, S: Model> {
    /// The parent resolution of the member.
    pub parent_generic_arguments: GenericArguments<S>,

    /// The member resolution.
    pub member: Member,
}

/// Symbol resolution in an adt implementation.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AdtMember<S: Model> {
    /// The id of the resolved adt implementation member.
    pub adt_implementation_member_id: ID<AdtImplementationMemberID>,

    /// The deduced generic arguments of the adt implementation.
    pub deduced_implementation_generic_arguments: GenericArguments<S>,

    /// The generic arugments supplied to the adt implementation member.
    pub member_generic_arguments: GenericArguments<S>,
}

/// A subset of [`GenericID`] that is not a member of an another symbol.
///
/// For example, function declaration declared in a module can be categorized as a non-member
/// generic ID. However, a function declared in a trait is not.
/// Result of a resolution.
///
/// Trait implementation members are included here since we can't refer the trait implementation
/// part (i.e. trait[Args]).
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, derive_more::From,
)]
#[allow(missing_docs)]
pub enum NonMemberGenericID {
    Struct(ID<symbol::Struct>),
    Enum(ID<symbol::Enum>),
    Function(ID<Function>),
    Trait(ID<Trait>),
    Constant(ID<symbol::Constant>),
    Type(ID<symbol::Type>),

    TraitImplementationFunction(ID<symbol::TraitImplementationFunction>),
    TraitImplementationType(ID<symbol::TraitImplementationType>),
    TraitImplementationConstant(ID<symbol::TraitImplementationConstant>),
}

impl From<NonMemberGenericID> for GlobalID {
    fn from(value: NonMemberGenericID) -> Self {
        match value {
            NonMemberGenericID::Struct(id) => id.into(),
            NonMemberGenericID::Enum(id) => id.into(),
            NonMemberGenericID::Function(id) => id.into(),
            NonMemberGenericID::Trait(id) => id.into(),
            NonMemberGenericID::Constant(id) => id.into(),
            NonMemberGenericID::Type(id) => id.into(),
            NonMemberGenericID::TraitImplementationFunction(id) => id.into(),
            NonMemberGenericID::TraitImplementationType(id) => id.into(),
            NonMemberGenericID::TraitImplementationConstant(id) => id.into(),
        }
    }
}

impl From<NonMemberGenericID> for GenericID {
    fn from(value: NonMemberGenericID) -> Self {
        match value {
            NonMemberGenericID::Struct(id) => id.into(),
            NonMemberGenericID::Enum(id) => id.into(),
            NonMemberGenericID::Function(id) => id.into(),
            NonMemberGenericID::Trait(id) => id.into(),
            NonMemberGenericID::Constant(id) => id.into(),
            NonMemberGenericID::Type(id) => id.into(),
            NonMemberGenericID::TraitImplementationFunction(id) => id.into(),
            NonMemberGenericID::TraitImplementationType(id) => id.into(),
            NonMemberGenericID::TraitImplementationConstant(id) => id.into(),
        }
    }
}

/// A resolution of a non-member generic symbol.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NonMemberGeneric<S: Model> {
    /// The generic ID of the resolved symbol.
    pub id: NonMemberGenericID,

    /// The generic arguments supplied to the symbol.
    pub generic_arguments: GenericArguments<S>,
}

/// A subset of [`GenericID`] that is a member of an another symbol.
///
/// See [`NonMemberGenericID`] for more information.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, derive_more::From,
)]
#[allow(missing_docs)]
pub enum MemberGenericID {
    TraitFunction(ID<TraitFunction>),
    TraitType(ID<TraitType>),
    TraitConstant(ID<TraitConstant>),
    AdtImplementationFunction(ID<symbol::TraitImplementationFunction>),
    AdtImplementationType(ID<symbol::TraitImplementationType>),
    AdtImplementationConstant(ID<symbol::TraitImplementationConstant>),
}

impl From<MemberGenericID> for GlobalID {
    fn from(value: MemberGenericID) -> Self {
        match value {
            MemberGenericID::TraitFunction(id) => id.into(),
            MemberGenericID::TraitType(id) => id.into(),
            MemberGenericID::TraitConstant(id) => id.into(),
            MemberGenericID::AdtImplementationFunction(id) => id.into(),
            MemberGenericID::AdtImplementationType(id) => id.into(),
            MemberGenericID::AdtImplementationConstant(id) => id.into(),
        }
    }
}

impl From<MemberGenericID> for GenericID {
    fn from(value: MemberGenericID) -> Self {
        match value {
            MemberGenericID::TraitFunction(id) => id.into(),
            MemberGenericID::TraitType(id) => id.into(),
            MemberGenericID::TraitConstant(id) => id.into(),
            MemberGenericID::AdtImplementationFunction(id) => id.into(),
            MemberGenericID::AdtImplementationType(id) => id.into(),
            MemberGenericID::AdtImplementationConstant(id) => id.into(),
        }
    }
}

/// A resolution of a member generic symbol.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MemberGeneric<S: Model> {
    /// The generic ID of the resolved symbol.
    pub id: MemberGenericID,

    /// The generic arguments supplied to the parent symbol.
    pub parent_generic_arguments: GenericArguments<S>,

    /// The generic arguments supplied to the member symbol.
    pub member_generic_arguments: GenericArguments<S>,
}

/// Result of a resolution.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum Resolution<S: Model> {
    Module(ID<Module>),
    Variant(Variant<S>),
    NonMemberGeneric(NonMemberGeneric<S>),
    MemberGeneric(MemberGeneric<S>),
}

impl<S: Model> Resolution<S> {
    /// Gets the [`GlobalID`] of the resolved symbol.
    #[must_use]
    pub fn global_id(&self) -> GlobalID {
        match self {
            Self::Module(id) => (*id).into(),
            Self::Variant(id) => id.id.into(),
            Self::NonMemberGeneric(id) => id.id.into(),
            Self::MemberGeneric(id) => id.id.into(),
        }
    }
}

/// A configuration for the resolution.
pub trait Config<S: Model> {
    /// Obtains the lifetime where the lifetime arguments aren't supplied.
    ///
    /// Returns `None` if the lifetime arguments are **explicitly** required.
    fn lifetime_arguments_elision_placeholder(&mut self) -> Option<Lifetime<S>>;

    /// Obtains the type where the type arguments aren't supplied.
    ///
    /// Returns `None` if the type arguments are **explicitly** required.
    fn type_arguments_elision_placeholder(&mut self) -> Option<r#type::Type<S>>;

    /// Obtains the constant where the constant arguments aren't supplied.
    ///
    /// Returns `None` if the constant arguments are **explicitly** required.
    fn constant_arguments_elision_placeholder(&mut self) -> Option<constant::Constant<S>>;

    /// Gets notified when a global ID is resolved.
    fn on_global_id_resolved(&mut self, global_id: GlobalID, identifier_span: &Span);

    /// Gets notified when a single generic identifier is resolved.
    fn on_resolved(&mut self, resolution: &Resolution<S>, generic_identifier_span: &Span);

    /// Gets notified when a constant is evaluated in a generic constant arguments.
    fn on_constant_arguments_resolved(
        &mut self,
        constant: &constant::Constant<S>,
        expected_type: &r#type::Type<S>,
        constant_expression_span: &Span,
    );

    /// Gets notified when a type is resolved.
    fn on_type_resolved(&mut self, ty: &r#type::Type<S>, type_span: &Span);

    /// Will be called every resolution to a lifetime. Allows the user to provide an extra source
    /// of lifetime.
    ///
    /// This is primarily used for the higher-ranked lifetimes.
    fn extra_lifetime_provider(&self, _: &str) -> Option<Lifetime<S>>;
}

impl<T: State> Table<T> {
    /// Resolves a module path to a module ID>
    ///
    /// # Errors
    ///
    /// See [`Error`] for more information.
    pub fn resolve_module_path(
        &self,
        module_path: &ModulePath,
        referring_site: GlobalID,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<ID<Module>, Error> {
        let mut current_module_id: Option<ID<Module>> = None;

        // loop through the module path and resolve each module name to a module ID.
        for path in module_path.paths() {
            // the next module ID to search for.
            let next = match current_module_id {
                Some(id) => self
                    .get(id)
                    .unwrap()
                    .module_child_ids_by_name
                    .get(path.span.str())
                    .copied()
                    .map(|x| match x.into_module() {
                        Ok(mod_id) => Ok(mod_id),
                        Err(found_id) => {
                            handler.receive(Box::new(ModuleExpected {
                                module_path: path.span.clone(),
                                found_id: found_id.into(),
                            }));
                            Err(Error::SemanticError)
                        }
                    })
                    .transpose()?,
                None => self.root_module_ids_by_name.get(path.span.str()).copied(),
            };

            // if the next module ID is not found, emit an error.
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
                handler.receive(Box::new(SymbolIsNotAccessible {
                    referring_site,
                    referred: next.into(),
                    referred_span: path.span.clone(),
                }));
            }

            current_module_id = Some(next);
        }

        Ok(current_module_id.unwrap())
    }

    /// Converts a [`Resolution`] into a [`r#type::Type`].
    ///
    /// # Errors
    ///
    /// If the resolution cannot be converted into a [`r#type::Type`].
    pub fn resolution_to_type<S: Model>(
        &self,
        resolution: Resolution<S>,
    ) -> Result<r#type::Type<S>, Resolution<S>> {
        match resolution {
            // substitute the generic arguments
            Resolution::NonMemberGeneric(NonMemberGeneric {
                id: NonMemberGenericID::Type(id),
                generic_arguments,
            }) => {
                let mut aliased = self.get(id).unwrap().r#type.clone().into_other_model();

                aliased.apply(&Substitution::from_generic_arguments(
                    generic_arguments,
                    id.into(),
                ));

                Ok(aliased)
            }
            Resolution::NonMemberGeneric(NonMemberGeneric {
                id: NonMemberGenericID::TraitImplementationType(id),
                generic_arguments,
            }) => {
                let mut aliased = self.get(id).unwrap().r#type.clone().into_other_model();

                aliased.apply(&Substitution::from_generic_arguments(
                    generic_arguments,
                    id.into(),
                ));

                Ok(aliased)
            }
            Resolution::MemberGeneric(MemberGeneric {
                id: MemberGenericID::AdtImplementationType(id),
                parent_generic_arguments,
                member_generic_arguments,
            }) => {
                let mut aliased = self.get(id).unwrap().r#type.clone().into_other_model();
                let parent_id = self.get(id).unwrap().parent_id;

                let mut substitution = Substitution::from_generic_arguments(
                    parent_generic_arguments,
                    parent_id.into(),
                );
                substitution = substitution
                    .append_from_generic_arguments(member_generic_arguments, id.into())
                    .unwrap();

                aliased.apply(&substitution);

                Ok(aliased)
            }

            Resolution::NonMemberGeneric(NonMemberGeneric {
                id: NonMemberGenericID::Enum(id),
                generic_arguments,
            }) => Ok(r#type::Type::Algebraic(Algebraic {
                kind: AlgebraicKind::Enum(id),
                generic_arguments,
            })),

            Resolution::NonMemberGeneric(NonMemberGeneric {
                id: NonMemberGenericID::Struct(id),
                generic_arguments,
            }) => Ok(r#type::Type::Algebraic(Algebraic {
                kind: AlgebraicKind::Struct(id),
                generic_arguments,
            })),

            Resolution::MemberGeneric(MemberGeneric {
                id: MemberGenericID::TraitType(trait_type_id),
                parent_generic_arguments,
                member_generic_arguments,
            }) => Ok(r#type::Type::TraitMember(r#type::TraitMember {
                trait_type_id,
                trait_generic_arguments: parent_generic_arguments,
                member_generic_arguments,
            })),

            resolution => Err(resolution),
        }
    }

    fn resolve_root_down_the_tree(
        &self,
        identifier: &Identifier,
        mut referring_site: GlobalID,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<GlobalID, Error> {
        // NOTE: Accessibility is not checked here because the symbol searching is done within the
        // same module ancestor tree.

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

    fn resolve_single_first_pass<S: Model>(
        &self,
        identifier: &Identifier,
        latest_resolution: &Option<Resolution<S>>,
        referring_site: GlobalID,
        search_from_root: bool,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<GlobalID, Error> {
        let (global_id, parent_global_id) = match latest_resolution {
            Some(resolution) => {
                // gets the global id from the latest resolution
                let parent_global_id: GlobalID = resolution.global_id();

                (
                    self.get_global(parent_global_id)
                        .ok_or(Error::InvalidReferringSiteID)?
                        .get_member(identifier.span.str()),
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
                    return self.resolve_root_relative(identifier, referring_site, handler);
                }
            }
        };

        global_id.map_or_else(
            || {
                handler.receive(Box::new(SymbolNotFound {
                    searched_global_id: parent_global_id,
                    resolution_span: identifier.span.clone(),
                }));
                Err(Error::SemanticError)
            },
            |global_id| {
                if !self.symbol_accessible(referring_site, global_id).unwrap() {
                    // non-fatal error, keep going
                    handler.receive(Box::new(SymbolIsNotAccessible {
                        referring_site,
                        referred: global_id,
                        referred_span: identifier.span.clone(),
                    }));
                }

                Ok(global_id)
            },
        )
    }

    #[allow(clippy::significant_drop_tightening)]
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

        candidates.next().map_or(Ok(Some(result)), |other| {
            handler.receive(Box::new(ResolutionAmbiguity {
                resolution_span: identifier.span.clone(),
                candidates: [result, other].into_iter().chain(candidates).collect(),
            }));
            Err(Error::SemanticError)
        })
    }

    fn resolve_root_relative(
        &self,
        identifier: &Identifier,
        referring_site: GlobalID,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<GlobalID, Error> {
        if let Some(first_attemp) =
            self.resolve_root_from_using_and_self(identifier, referring_site, handler)?
        {
            return Ok(first_attemp);
        }

        self.resolve_root_down_the_tree(identifier, referring_site, handler)
    }

    /// Resolves a qualified identifier to a [`Trait`] ID.
    ///
    /// This function is usally for for resolving a trait in `implements` declaration.
    ///
    /// # Errors
    ///
    /// See [`Error`] for more information.
    pub fn resolve_trait_path(
        &self,
        qualified_identifier: &QualifiedIdentifier,
        referring_site: GlobalID,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<ID<Trait>, Error> {
        let mut latest_resolution = None;

        let mut iter = qualified_identifier.generic_identifiers().peekable();

        while let Some(generic_identifier) = iter.next() {
            let is_last = iter.peek().is_none();

            let global_id = self.resolve_single_first_pass::<Symbolic>(
                generic_identifier.identifier(),
                &latest_resolution,
                referring_site,
                qualified_identifier.leading_scope_separator().is_some(),
                handler,
            )?;

            if !is_last {
                let GlobalID::Module(module_id) = global_id else {
                    handler.receive(Box::new(ModuleExpected {
                        module_path: generic_identifier.span(),
                        found_id: global_id,
                    }));
                    return Err(Error::SemanticError);
                };

                if let Some(generic_arguments) = generic_identifier.generic_arguments() {
                    // non-fatal error, keep going
                    handler.receive(Box::new(NoGenericArgumentsRequired {
                        global_id: module_id.into(),
                        generic_argument_span: generic_arguments.span(),
                    }));
                }

                latest_resolution = Some(Resolution::Module(module_id));
            } else if let GlobalID::Trait(trait_id) = global_id {
                return Ok(trait_id);
            } else {
                handler.receive(Box::new(TraitExpectedInImplemenation {
                    found_id: global_id,
                    trait_path: qualified_identifier.span(),
                }));
                return Err(Error::SemanticError);
            }
        }

        unreachable!()
    }

    fn resolve_qualified_identifier_type<S: Model>(
        &self,
        syntax_tree: &syntax_tree::QualifiedIdentifier,
        referring_site: GlobalID,
        config: &mut dyn Config<S>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<r#type::Type<S>, Error> {
        let is_simple_identifier = syntax_tree.rest().is_empty()
            && syntax_tree.leading_scope_separator().is_none()
            && syntax_tree.first().generic_arguments().is_none();

        // try to resolve the identifier as a type parameter
        if is_simple_identifier {
            for global_id in self
                .scope_walker(referring_site)
                .ok_or(Error::InvalidReferringSiteID)?
            {
                let Ok(generic_id) = GenericID::try_from(global_id) else {
                    continue;
                };

                let generic_symbol = self.get_generic(generic_id).expect("should've been valid");

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

        self.resolution_to_type(self.resolve(syntax_tree, referring_site, config, handler)?)
            .map_err(|err| {
                handler.receive(Box::new(TypeExpected {
                    non_type_symbol_span: syntax_tree.span(),
                    resolved_global_id: err.global_id(),
                }));
                Error::SemanticError
            })
    }

    /// Resolves a [`syntax_tree::r#type::Type`] to a [`r#type::Type`].
    ///
    /// # Errors
    ///
    /// See [`Error`] for more information.
    #[allow(clippy::too_many_lines)]
    pub fn resolve_type<S: Model>(
        &self,
        syntax_tree: &syntax_tree::r#type::Type,
        referring_site: GlobalID,
        config: &mut dyn Config<S>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<r#type::Type<S>, Error> {
        match syntax_tree {
            syntax_tree::r#type::Type::Primitive(primitive) => {
                Ok(r#type::Type::Primitive(match primitive {
                    syntax_tree::r#type::Primitive::Bool(_) => r#type::Primitive::Bool,
                    syntax_tree::r#type::Primitive::Float32(_) => r#type::Primitive::Float32,
                    syntax_tree::r#type::Primitive::Float64(_) => r#type::Primitive::Float64,
                    syntax_tree::r#type::Primitive::Int8(_) => r#type::Primitive::Int8,
                    syntax_tree::r#type::Primitive::Int16(_) => r#type::Primitive::Int16,
                    syntax_tree::r#type::Primitive::Int32(_) => r#type::Primitive::Int32,
                    syntax_tree::r#type::Primitive::Int64(_) => r#type::Primitive::Int64,
                    syntax_tree::r#type::Primitive::Uint8(_) => r#type::Primitive::Uint8,
                    syntax_tree::r#type::Primitive::Uint16(_) => r#type::Primitive::Uint16,
                    syntax_tree::r#type::Primitive::Uint32(_) => r#type::Primitive::Uint32,
                    syntax_tree::r#type::Primitive::Uint64(_) => r#type::Primitive::Uint64,
                    syntax_tree::r#type::Primitive::Usize(_) => r#type::Primitive::Usize,
                    syntax_tree::r#type::Primitive::Isize(_) => r#type::Primitive::Isize,
                }))
            }
            syntax_tree::r#type::Type::Local(local) => Ok(r#type::Type::Local(Local(Box::new(
                self.resolve_type(local.ty(), referring_site, config, handler)?,
            )))),
            syntax_tree::r#type::Type::QualifiedIdentifier(qualified_identifier) => self
                .resolve_qualified_identifier_type(
                    qualified_identifier,
                    referring_site,
                    config,
                    handler,
                ),
            syntax_tree::r#type::Type::Reference(reference) => {
                let lifetime = reference
                    .lifetime()
                    .as_ref()
                    .map(|x| self.resolve_lifetime(x, referring_site, config, handler))
                    .transpose()?
                    .map_or_else(
                        || {
                            config.lifetime_arguments_elision_placeholder().map_or_else(
                                || {
                                    handler.receive(Box::new(LifetimeExpected {
                                        expected_span: reference.span(),
                                    }));

                                    Err(Error::SemanticError)
                                },
                                Ok,
                            )
                        },
                        Ok,
                    )?;

                let qualifier = match reference.qualifier() {
                    Some(syntax_tree::Qualifier::Mutable(..)) => Qualifier::Mutable,
                    Some(syntax_tree::Qualifier::Restrict(..)) => Qualifier::Restrict,
                    None => Qualifier::Immutable,
                };

                let pointee = Box::new(self.resolve_type(
                    reference.operand(),
                    referring_site,
                    config,
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
                    Some(syntax_tree::Qualifier::Mutable(..)) => Qualifier::Mutable,
                    Some(syntax_tree::Qualifier::Restrict(..)) => Qualifier::Restrict,
                    None => Qualifier::Immutable,
                };

                let pointee = Box::new(self.resolve_type(
                    pointer_ty.operand(),
                    referring_site,
                    config,
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
                    let ty = self.resolve_type(element.ty(), referring_site, config, handler)?;

                    if element.ellipsis().is_some() {
                        match ty {
                            r#type::Type::TraitMember(ty) => {
                                elements.push(TupleElement::Unpacked(Unpacked::TraitMember(ty)));
                            }
                            r#type::Type::Parameter(ty) => {
                                elements.push(TupleElement::Unpacked(Unpacked::Parameter(ty)));
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
                let length = self.evaluate(array.expression(), referring_site, config, handler)?;
                let element_ty =
                    self.resolve_type(array.operand(), referring_site, config, handler)?;

                Ok(r#type::Type::Array(r#type::Array {
                    length,
                    element: Box::new(element_ty),
                }))
            }
        }
        .map(|result| {
            config.on_type_resolved(&result, &syntax_tree.span());
            result
        })
    }

    /// Resolves an [`Identifier`] as a [`LifetimeParameterID`].
    ///
    /// # Errors
    ///
    /// - [`Error::InvalidID`]: If the `referring_site` is not a valid ID.
    /// - [`Error::SemanticError`]: If encountered a fatal semantic error e.g. symbol not found.
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
            let Ok(generic_id) = GenericID::try_from(scope) else {
                continue;
            };

            let generic_symbol = self.get_generic(generic_id).expect("should've been valid");

            if let Some(lifetime_id) = generic_symbol
                .generic_declaration()
                .parameters
                .lifetime_parameter_ids_by_name
                .get(identifier.span.str())
                .copied()
            {
                return Ok(MemberID {
                    parent: generic_id,
                    id: lifetime_id,
                });
            }
        }

        handler.receive(Box::new(LifetimeParameterNotFound {
            referred_span: identifier.span(),
            referring_site,
        }));
        Err(Error::SemanticError)
    }

    /// Resolves a [`syntax_tree::LifetimeArgument`] to a [`Lifetime`].
    ///
    /// # Errors
    ///
    /// See [`Error`] for more information.
    pub fn resolve_lifetime<S: Model>(
        &self,
        lifetime_argument: &syntax_tree::Lifetime,
        referring_site: GlobalID,
        config: &dyn Config<S>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Lifetime<S>, Error> {
        match lifetime_argument.identifier() {
            LifetimeIdentifier::Static(..) => Ok(Lifetime::Static),
            LifetimeIdentifier::Identifier(ident) => {
                if let Some(lifetime) = config.extra_lifetime_provider(ident.span.str()) {
                    return Ok(lifetime);
                }

                self.resolve_lifetime_parameter(ident, referring_site, handler)
                    .map(Lifetime::Parameter)
            }
        }
    }

    // resolve the generic arguments required for `resolved_id`.
    pub(super) fn resolve_generic_arguments<S: Model>(
        &self,
        generic_identifier: &GenericIdentifier,
        referring_site: GlobalID,
        resolved_id: GlobalID,
        config: &mut dyn Config<S>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Option<GenericArguments<S>>, Error> {
        let Ok(generic_id) = GenericID::try_from(resolved_id) else {
            if let Some(generic_arguments) = generic_identifier.generic_arguments() {
                // non-fatal error, keep going
                handler.receive(Box::new(NoGenericArgumentsRequired {
                    global_id: resolved_id,
                    generic_argument_span: generic_arguments.span(),
                }));
            }

            return Ok(None);
        };

        let generic_symbol = self.get_generic(generic_id).unwrap();

        let mut lifetime_argument_syns = Vec::new();
        let mut type_argument_syns = Vec::new();
        let mut constant_argument_syns = Vec::new();

        // extracts the generic arguments from the syntax tree to the list of syntax trees
        for generic_arguments in generic_identifier
            .generic_arguments()
            .iter()
            .flat_map(|x| x.argument_list().iter().flat_map(ConnectedList::elements))
        {
            match generic_arguments {
                syntax_tree::GenericArgument::Constant(arg) => {
                    constant_argument_syns.push(&**arg.expression());
                }
                syntax_tree::GenericArgument::Type(arg) if constant_argument_syns.is_empty() => {
                    type_argument_syns.push(&**arg);
                }
                syntax_tree::GenericArgument::Lifetime(arg)
                    if constant_argument_syns.is_empty() && type_argument_syns.is_empty() =>
                {
                    lifetime_argument_syns.push(arg);
                }
                arg => {
                    handler.receive(Box::new(MisorderedGenericArgument {
                        generic_kind: match arg {
                            syntax_tree::GenericArgument::Type(_) => GenericKind::Type,
                            syntax_tree::GenericArgument::Constant(_) => GenericKind::Constant,
                            syntax_tree::GenericArgument::Lifetime(_) => GenericKind::Lifetime,
                        },
                        generic_argument: arg.span(),
                    }));
                    return Err(Error::SemanticError);
                }
            }
        }

        // extract the arguments syntax
        Ok(Some(GenericArguments {
            lifetimes: LifetimeParameter::resolve_generic_parameters(
                self,
                lifetime_argument_syns.into_iter(),
                &generic_symbol.generic_declaration().parameters.lifetimes,
                std::iter::empty(),
                false,
                generic_identifier.span(),
                referring_site,
                config,
                handler,
            )?,
            types: TypeParameter::resolve_generic_parameters(
                self,
                type_argument_syns.into_iter(),
                &generic_symbol.generic_declaration().parameters.types,
                generic_symbol
                    .generic_declaration()
                    .parameters
                    .default_type_parameters
                    .iter()
                    .map(|x| x.clone().into_other_model()),
                constant_argument_syns.is_empty(),
                generic_identifier.span(),
                referring_site,
                config,
                handler,
            )?,
            constants: ConstantParameter::resolve_generic_parameters(
                self,
                constant_argument_syns.into_iter(),
                &generic_symbol.generic_declaration().parameters.constants,
                generic_symbol
                    .generic_declaration()
                    .parameters
                    .default_constant_parameters
                    .iter()
                    .map(|x| x.clone().into_other_model()),
                true,
                generic_identifier.span(),
                referring_site,
                config,
                handler,
            )?,
        }))
    }

    #[allow(clippy::too_many_lines)]
    fn final_pass<S: Model>(
        &self,
        resolved_id: GlobalID,
        generic_arguments: Option<GenericArguments<S>>,
        latest_resolution: Option<Resolution<S>>,
    ) -> Resolution<S> {
        match resolved_id {
            GlobalID::Module(id) => Resolution::Module(id),
            GlobalID::Struct(id) => Resolution::NonMemberGeneric(NonMemberGeneric {
                id: id.into(),
                generic_arguments: generic_arguments.unwrap(),
            }),
            GlobalID::Trait(id) => Resolution::NonMemberGeneric(NonMemberGeneric {
                id: id.into(),
                generic_arguments: generic_arguments.unwrap(),
            }),
            GlobalID::Enum(id) => Resolution::NonMemberGeneric(NonMemberGeneric {
                id: id.into(),
                generic_arguments: generic_arguments.unwrap(),
            }),
            GlobalID::Type(id) => Resolution::NonMemberGeneric(NonMemberGeneric {
                id: id.into(),
                generic_arguments: generic_arguments.unwrap(),
            }),
            GlobalID::Constant(id) => Resolution::NonMemberGeneric(NonMemberGeneric {
                id: id.into(),
                generic_arguments: generic_arguments.unwrap(),
            }),
            GlobalID::Function(id) => Resolution::NonMemberGeneric(NonMemberGeneric {
                id: id.into(),
                generic_arguments: generic_arguments.unwrap(),
            }),
            GlobalID::Variant(id) => Resolution::Variant(Variant {
                enum_generic_arguments: latest_resolution.map_or_else(
                    || {
                        let parent_enum_id = self.get(id).unwrap().parent_enum_id;
                        self.get(parent_enum_id)
                            .unwrap()
                            .generic_declaration
                            .parameters
                            .create_identity_generic_arguments(parent_enum_id.into())
                    },
                    |x| {
                        let non_member = x.into_non_member_generic().unwrap();
                        assert!(non_member.id.is_enum());
                        non_member.generic_arguments
                    },
                ),
                id,
            }),
            GlobalID::TraitType(id) => Resolution::MemberGeneric(MemberGeneric {
                id: id.into(),
                parent_generic_arguments: {
                    latest_resolution.map_or_else(
                        || {
                            let parent_trait_id = self.get(id).unwrap().parent_id;
                            self.get(parent_trait_id)
                                .unwrap()
                                .generic_declaration
                                .parameters
                                .create_identity_generic_arguments(parent_trait_id.into())
                        },
                        |x| {
                            let non_member = x.into_non_member_generic().unwrap();
                            assert!(non_member.id.is_trait());
                            non_member.generic_arguments
                        },
                    )
                },
                member_generic_arguments: generic_arguments.unwrap(),
            }),
            GlobalID::TraitFunction(_) => todo!(),
            GlobalID::TraitConstant(_) => todo!(),
            GlobalID::TraitImplementation(_) => todo!(),
            GlobalID::NegativeTraitImplementation(_) => todo!(),
            GlobalID::TraitImplementationFunction(_) => todo!(),
            GlobalID::TraitImplementationType(_) => todo!(),
            GlobalID::TraitImplementationConstant(_) => todo!(),
            GlobalID::AdtImplementation(_) => todo!(),
            GlobalID::AdtImplementationFunction(_) => todo!(),
            GlobalID::AdtImplementationType(_) => todo!(),
            GlobalID::AdtImplementationConstant(_) => todo!(),
        }
    }

    /// Resolves a qualified identifier to a [`Resolution`]
    ///
    /// # Errors
    ///
    /// See [`Error`] for more information.
    pub fn resolve<S: Model>(
        &self,
        qualified_identifier: &QualifiedIdentifier,
        referring_site: GlobalID,
        config: &mut dyn Config<S>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Resolution<S>, Error> {
        // Checks if the given `referring_site` is a valid ID.
        drop(
            self.get_global(referring_site)
                .ok_or(Error::InvalidReferringSiteID)?,
        );

        // create the current root
        let mut latest_resolution = None;
        let has_leading_scope_separator = qualified_identifier.leading_scope_separator().is_some();

        for generic_identifier in qualified_identifier.generic_identifiers() {
            // gets the global id
            let global_id = self.resolve_single_first_pass(
                generic_identifier.identifier(),
                &latest_resolution,
                referring_site,
                has_leading_scope_separator,
                handler,
            )?;

            // checks if the symbol is finalized
            config.on_global_id_resolved(global_id, &generic_identifier.identifier().span);

            // generic arguments
            let generic_arguments = self.resolve_generic_arguments(
                generic_identifier,
                referring_site,
                global_id,
                config,
                handler,
            )?;

            let resolved = self.final_pass(global_id, generic_arguments, latest_resolution);

            config.on_resolved(&resolved, &generic_identifier.span());

            latest_resolution = Some(resolved);
        }

        Ok(latest_resolution.expect("should at least have one resolution"))
    }
}
