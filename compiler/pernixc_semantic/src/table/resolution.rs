//! Contains logic related to symbol resolution.

use enum_as_inner::EnumAsInner;
use paste::paste;
use pernixc_base::{
    diagnostic::Handler,
    source_file::{SourceElement, Span},
};
use pernixc_lexical::token::Identifier;
use pernixc_syntax::syntax_tree::{
    self, item::ModulePath, ConnectedList, GenericIdentifier, LifetimeArgumentIdentifier,
    QualifiedIdentifier,
};

use super::{Error, Index, Table};
use crate::{
    arena::ID,
    entity::{
        constant,
        predicate::{self, Predicate},
        r#type::{self, Qualifier},
        region::Region,
        Entity, GenericArguments, Model, Never, Substitution,
    },
    error::{
        self, GenericArgumentCountMismatch, LifetimeExpected, LifetimeParameterNotFound,
        MemberAccessOnExpression, MemberAccessOnType, MisorderedGenericArgument, ModuleExpected,
        MoreThanOneUnpackedInTupleType, NoGenericArgumentsRequired, ResolutionAmbiguity,
        SymbolIsNotAccessible, SymbolNotFound, TraitExpectedInImplemenation, TypeExpected,
    },
    symbol::{
        self, Enum, Function, Generic, GenericID, GenericKind, GlobalID, ImplementationFunction,
        LifetimeParameterID, MemberID, Module, Struct, Symbolic, Trait, TraitConstant,
        TraitFunction, TraitType, TypeParameterID,
    },
};

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
    pub variant_id: ID<symbol::Variant>,
}

/// Symbol resolution with parent resolution.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct WithParent<Member, S: Model> {
    /// The parent resolution of the member.
    pub parent_generic_arguments: GenericArguments<S>,

    /// The member resolution.
    pub member: Member,
}

/// Result of a resolution.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum Resolution<S: Model> {
    Enum(WithGenerics<ID<Enum>, S>),
    Struct(WithGenerics<ID<Struct>, S>),
    Module(ID<Module>),
    Function(WithGenerics<ID<Function>, S>),
    Trait(WithGenerics<ID<Trait>, S>),
    TraitFunction(WithParent<WithGenerics<ID<TraitFunction>, S>, S>),
    TraitType(WithParent<WithGenerics<ID<TraitType>, S>, S>),
    TraitConstant(WithParent<ID<TraitConstant>, S>),
    Variant(Variant<S>),
    NonGlobalIDType(NonGlobalIDType<S>),
    Constant(constant::Constant<Symbolic>),
    ImplementationFunction(WithGenerics<ID<ImplementationFunction>, S>),
}

/// Enumeration of all type variants that can't be represented into a global ID.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum NonGlobalIDType<S: Model> {
    Primitive(r#type::Primitive),
    Inference(S::TypeInference),
    Pointer(r#type::Pointer<S>),
    Reference(r#type::Reference<S>),
    Array(r#type::Array<S>),
    Parameter(TypeParameterID),
    Tuple(r#type::Tuple<S>),
}

impl<S: Model> From<NonGlobalIDType<S>> for r#type::Type<S> {
    fn from(value: NonGlobalIDType<S>) -> Self {
        match value {
            NonGlobalIDType::Primitive(primitive) => Self::Primitive(primitive),
            NonGlobalIDType::Inference(inference) => Self::Inference(inference),
            NonGlobalIDType::Pointer(pointer) => Self::Pointer(pointer),
            NonGlobalIDType::Reference(reference) => Self::Reference(reference),
            NonGlobalIDType::Array(array) => Self::Array(array),
            NonGlobalIDType::Parameter(parameter) => Self::Parameter(parameter),
            NonGlobalIDType::Tuple(tuple) => Self::Tuple(tuple),
        }
    }
}

impl<S: Model> TryInto<r#type::Type<S>> for Resolution<S> {
    type Error = Self;

    fn try_into(self) -> Result<r#type::Type<S>, Self::Error> {
        match self {
            Self::Enum(res) => Ok(r#type::Type::Algebraic(r#type::Algebraic {
                kind: r#type::AlgebraicKind::Enum(res.id),
                generic_arguments: res.generic_arguments,
            })),
            Self::Struct(res) => Ok(r#type::Type::Algebraic(r#type::Algebraic {
                kind: r#type::AlgebraicKind::Struct(res.id),
                generic_arguments: res.generic_arguments,
            })),
            Self::TraitType(res) => Ok(r#type::Type::TraitMember(r#type::TraitMember {
                trait_type_id: res.member.id,
                trait_generic_arguments: res.parent_generic_arguments,
                member_generic_arguments: res.member.generic_arguments,
            })),
            Self::NonGlobalIDType(res) => Ok(res.into()),

            err => Err(err),
        }
    }
}

impl<S: Model> From<r#type::Type<S>> for Resolution<S> {
    fn from(value: r#type::Type<S>) -> Self {
        match value {
            r#type::Type::Primitive(primitive) => {
                Self::NonGlobalIDType(NonGlobalIDType::Primitive(primitive))
            }
            r#type::Type::Inference(inference) => {
                Self::NonGlobalIDType(NonGlobalIDType::Inference(inference))
            }
            r#type::Type::Pointer(pointer) => {
                Self::NonGlobalIDType(NonGlobalIDType::Pointer(pointer))
            }
            r#type::Type::Reference(reference) => {
                Self::NonGlobalIDType(NonGlobalIDType::Reference(reference))
            }
            r#type::Type::Array(array) => Self::NonGlobalIDType(NonGlobalIDType::Array(array)),
            r#type::Type::Parameter(parameter) => {
                Self::NonGlobalIDType(NonGlobalIDType::Parameter(parameter))
            }
            r#type::Type::Tuple(tuple) => Self::NonGlobalIDType(NonGlobalIDType::Tuple(tuple)),
            r#type::Type::Algebraic(adt) => match adt.kind {
                r#type::AlgebraicKind::Enum(enum_id) => Self::Enum(WithGenerics {
                    id: enum_id,
                    generic_arguments: adt.generic_arguments,
                }),
                r#type::AlgebraicKind::Struct(struct_id) => Self::Struct(WithGenerics {
                    id: struct_id,
                    generic_arguments: adt.generic_arguments,
                }),
            },
            r#type::Type::TraitMember(trait_member) => Self::TraitType(WithParent {
                parent_generic_arguments: trait_member.trait_generic_arguments,
                member: WithGenerics {
                    id: trait_member.trait_type_id,
                    generic_arguments: trait_member.member_generic_arguments,
                },
            }),
        }
    }
}

/// Represents a semantic check occurred during the resolution. This check must be validated
/// in order to make sure that the program is semantically correct.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Checking<S: Model<ForallRegion = Never>> {
    /// The given predicate must be satisfied.
    Predicate(Predicate<S>),

    /// The given two types must be equal.
    TypeCheck(constant::Constant<S>, r#type::Type<S>),
}

/// A configuration for the resolution.
pub trait Config<S: Model<ForallRegion = Never>> {
    /// Obtains the region where the region arguments aren't supplied.
    ///
    /// Returns `None` if the region arguments are **explicitly** required.
    fn region_arguments_placeholder(&mut self) -> Option<Region<S>>;

    /// Obtains the type where the type arguments aren't supplied.
    ///
    /// Returns `None` if the type arguments are **explicitly** required.
    fn type_arguments_placeholder(&mut self) -> Option<r#type::Type<S>>;

    /// Obtains the constant where the constant arguments aren't supplied.
    ///
    /// Returns `None` if the constant arguments are **explicitly** required.
    fn constant_arguments_placeholder(&mut self) -> Option<constant::Constant<S>>;

    /// Recieves a semantic check occurred during the resolution.
    ///
    /// See [`Checking`] for more information.
    ///
    /// # Parameters
    ///
    /// - `checking`: The semantic check occurred during the resolution.
    /// - `span`: The span where the semantic check occurred; useful for emitting diagnostics.
    fn check(&mut self, checking: Checking<S>, span: Span);
}

macro_rules! handle_generic_arguments_supplied {
    (
        $self:ident,
        $generic_identifier_span:ident,
        $referring_site:ident,
        $generic_symbol:ident,
        $argument_syns:ident,
        $config:ident,
        $handler:ident,
        $kind:ident,
        $exrta_cond:expr,
        $syntax_tree:ident,
        $param:pat,
        $resolve_function:expr
    ) => {
        {
        paste! {
            let expected_count = $generic_symbol.generic_declaration().parameters.[<$kind:lower s>].len();
            let default_count = $generic_symbol
                .generic_declaration()
                .parameters
                .[<default_ $kind:lower _parameters>]
                .len();

            let expected_count_range = std::ops::RangeInclusive::new(expected_count - default_count, expected_count);

            // filled with type inference / default
            if $argument_syns.is_empty() && $exrta_cond {
                let inference_fill_count = expected_count - default_count;
                let mut arguments = Vec::new();

                for _ in 0..inference_fill_count {
                    let Some(placeholder) = $config.[<$kind:lower _arguments_placeholder>]() else {
                        $handler.receive(error::Error::GenericArgumentCountMismatch(
                            GenericArgumentCountMismatch {
                                generic_kind: GenericKind::$kind,
                                $generic_identifier_span,
                                expected_count,
                                supplied_count: 0,
                            },
                        ));
                        return Err(Error::SemanticError);
                    };

                    arguments.push(placeholder);
                }

                arguments.extend(
                    $generic_symbol
                        .generic_declaration()
                        .parameters
                        .[<default_ $kind:lower _parameters>]
                        .iter()
                        .map(|x| x.clone().into_other_model()),
                );

                Ok(arguments)
            } else if expected_count_range.contains(&$argument_syns.len()) {
                let default_argument_fill = expected_count - $argument_syns.len();

                $argument_syns
                    .into_iter()
                    .zip($generic_symbol.generic_declaration().parameters.[<$kind:lower s>].values())
                    .map(|($syntax_tree, $param)| $resolve_function)
                    .chain(
                        $generic_symbol
                            .generic_declaration()
                            .parameters
                            .[<default_ $kind:lower _parameters>][default_count - default_argument_fill..]
                            .iter()
                            .map(|x| Ok(x.clone().into_other_model())),
                    )
                    .collect::<Result<Vec<_>, _>>()
            } else {
                // type arguments count mismatch
                $handler.receive(error::Error::GenericArgumentCountMismatch(
                    GenericArgumentCountMismatch {
                        generic_kind: GenericKind::$kind,
                        $generic_identifier_span,
                        expected_count,
                        supplied_count: $argument_syns.len(),
                    },
                ));
                Err(Error::SemanticError)
            }
        }
        }
    };
}

impl Table {
    /// Resolves a module path to a module ID>
    ///
    /// # Errors
    ///
    /// - [`Error::InvalidID`]: if the `referring_site` is not a valid ID.
    /// - [`Error::SemanticError`]: if encountered a fatal semantic error e.g. a module not found.
    pub fn resolve_module_path(
        &self,
        module_path: &ModulePath,
        referring_site: GlobalID,
        handler: &dyn Handler<error::Error>,
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
                            handler.receive(error::Error::ModuleExpected(ModuleExpected {
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
                handler.receive(error::Error::SymbolNotFound(SymbolNotFound {
                    searched_global_id: current_module_id.map(Into::into),
                    resolution_span: path.span.clone(),
                }));
                return Err(Error::SemanticError);
            };

            // check if accessible
            if !self
                .symbol_accessible(referring_site, next.into())
                .ok_or(Error::InvalidID)?
            {
                handler.receive(error::Error::SymbolIsNotAccessible(SymbolIsNotAccessible {
                    referring_site,
                    referred: next.into(),
                    referred_span: path.span.clone(),
                }));
            }

            current_module_id = Some(next);
        }

        Ok(current_module_id.unwrap())
    }

    fn resolve_root_down_the_tree(
        &self,
        identifier: &Identifier,
        mut referring_site: GlobalID,
        handler: &dyn Handler<error::Error>,
    ) -> Result<GlobalID, Error> {
        // NOTE: Accessibility is not checked here because the symbol searching is done within the
        // same module ancestor tree.

        loop {
            // try to find the symbol in the current scope
            let global_symbol = self.get_global(referring_site).ok_or(Error::InvalidID)?;

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

                handler.receive(error::Error::SymbolNotFound(SymbolNotFound {
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
        handler: &dyn Handler<error::Error>,
    ) -> Result<GlobalID, Error> {
        let (global_id, parent_global_id) = match latest_resolution {
            Some(resolution) => {
                // gets the global id from the latest resolution
                let parent_global_id: GlobalID = match resolution {
                    Resolution::Enum(res) => res.id.into(),
                    Resolution::Struct(res) => res.id.into(),
                    Resolution::Module(id) => (*id).into(),
                    Resolution::Function(res) => res.id.into(),
                    Resolution::Trait(res) => res.id.into(),
                    Resolution::Variant(res) => res.variant_id.into(),
                    Resolution::TraitFunction(res) => res.member.id.into(),
                    Resolution::TraitType(res) => res.member.id.into(),
                    Resolution::TraitConstant(res) => res.member.into(),
                    Resolution::ImplementationFunction(res) => res.id.into(),
                    Resolution::NonGlobalIDType(..) => {
                        handler.receive(error::Error::MemberAccessOnType(MemberAccessOnType {
                            access_span: identifier.span.clone(),
                        }));
                        return Err(Error::SemanticError);
                    }
                    Resolution::Constant(..) => {
                        handler.receive(error::Error::MemberAccessOnExpression(
                            MemberAccessOnExpression {
                                access_span: identifier.span.clone(),
                            },
                        ));
                        return Err(Error::SemanticError);
                    }
                };

                (
                    self.get_global(parent_global_id)
                        .ok_or(Error::InvalidID)?
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
                handler.receive(error::Error::SymbolNotFound(SymbolNotFound {
                    searched_global_id: parent_global_id,
                    resolution_span: identifier.span.clone(),
                }));
                Err(Error::SemanticError)
            },
            |global_id| {
                if !self.symbol_accessible(referring_site, global_id).unwrap() {
                    // non-fatal error, keep going
                    handler.receive(error::Error::SymbolIsNotAccessible(SymbolIsNotAccessible {
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
        handler: &dyn Handler<error::Error>,
    ) -> Result<Option<GlobalID>, Error> {
        let closet_module_id = self
            .get_closet_module_id(referring_site)
            .ok_or(Error::InvalidID)?;
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
            handler.receive(error::Error::ResolutionAmbiguity(ResolutionAmbiguity {
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
        handler: &dyn Handler<error::Error>,
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
    /// - [`Error::InvalidID`]: if the `referring_site` is not a valid ID.
    /// - [`Error::SemanticError`]: if encountered a fatal semantic error e.g. symbol not found.
    pub fn resolve_trait_path(
        &self,
        qualified_identifier: &QualifiedIdentifier,
        referring_site: GlobalID,
        handler: &dyn Handler<error::Error>,
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
                    handler.receive(error::Error::ModuleExpected(ModuleExpected {
                        module_path: generic_identifier.span(),
                        found_id: global_id,
                    }));
                    return Err(Error::SemanticError);
                };

                if let Some(generic_arguments) = generic_identifier.generic_arguments() {
                    // non-fatal error, keep going
                    handler.receive(error::Error::NoGenericArgumentsRequired(
                        NoGenericArgumentsRequired {
                            global_id: module_id.into(),
                            generic_argument_span: generic_arguments.span(),
                        },
                    ));
                }

                latest_resolution = Some(Resolution::Module(module_id));
            } else if let GlobalID::Trait(trait_id) = global_id {
                return Ok(trait_id);
            } else {
                handler.receive(error::Error::TraitExpectedInImplemenation(
                    TraitExpectedInImplemenation {
                        found_id: global_id,
                        trait_path: qualified_identifier.span(),
                    },
                ));
                return Err(Error::SemanticError);
            }
        }

        unreachable!()
    }

    /// Resolves a qualified identifier to a [`r#type::Type`].
    ///
    /// # Errors
    ///
    /// - [`Error::InvalidID`]: if the `referring_site` is not a valid ID.
    /// - [`Error::SemanticError`]: if encountered a fatal semantic error e.g. symbol not found.
    pub fn resolve_qualified_identifier_type<S: Model<ForallRegion = Never>>(
        &self,
        syntax_tree: &syntax_tree::QualifiedIdentifier,
        referring_site: GlobalID,
        config: &mut dyn Config<S>,
        handler: &dyn Handler<error::Error>,
    ) -> Result<r#type::Type<S>, Error> {
        let is_simple_identifier = syntax_tree.rest().is_empty()
            && syntax_tree.leading_scope_separator().is_none()
            && syntax_tree.first().generic_arguments().is_none();

        // try to resolve the identifier as a type parameter
        if is_simple_identifier {
            for global_id in self.scope_walker(referring_site).ok_or(Error::InvalidID)? {
                let Ok(generic_id) = GenericID::try_from(global_id) else {
                    continue;
                };

                let generic_symbol = self.get_generic(generic_id).expect("should've been valid");

                if let Some(type_parameter_id) = generic_symbol
                    .generic_declaration()
                    .parameters
                    .types
                    .get_id(syntax_tree.first().identifier().span.str())
                {
                    return Ok(r#type::Type::Parameter(TypeParameterID {
                        parent: generic_id,
                        id: type_parameter_id,
                    }));
                }
            }
        }

        let resolution = self.resolve(syntax_tree, referring_site, config, handler)?;

        resolution.try_into().map_or_else(
            |_| {
                handler.receive(error::Error::TypeExpected(TypeExpected {
                    non_type_symbol_span: syntax_tree.span(),
                }));
                Err(Error::SemanticError)
            },
            Ok,
        )
    }

    /// Resolves a [`syntax_tree::r#type::Type`] to a [`r#type::Type`].
    ///
    /// # Errors
    ///
    /// - [`Error::InvalidID`]: If the `referring_site` is not a valid ID.
    /// - [`Error::SemanticError`]: If encountered a fatal semantic error e.g. symbol not found.
    #[allow(clippy::too_many_lines)]
    pub fn resolve_type<S: Model<ForallRegion = Never>>(
        &self,
        syntax_tree: &syntax_tree::r#type::Type,
        referring_site: GlobalID,
        config: &mut dyn Config<S>,
        handler: &dyn Handler<error::Error>,
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
            syntax_tree::r#type::Type::QualifiedIdentifier(qualified_identifier) => self
                .resolve_qualified_identifier_type(
                    qualified_identifier,
                    referring_site,
                    config,
                    handler,
                ),
            syntax_tree::r#type::Type::Reference(reference) => {
                let region = reference
                    .lifetime_argument()
                    .as_ref()
                    .map(|x| self.resolve_lifetime_argument(x, referring_site, handler))
                    .transpose()?
                    .map_or_else(
                        || {
                            config.region_arguments_placeholder().map_or_else(
                                || {
                                    handler.receive(error::Error::LifetimeExpected(
                                        LifetimeExpected {
                                            expected_span: reference.span(),
                                        },
                                    ));
                                    Err(Error::SemanticError)
                                },
                                Ok,
                            )
                        },
                        Ok,
                    )?;

                let qualifier = match reference.qualifier() {
                    Some(syntax_tree::r#type::Qualifier::Mutable(..)) => Qualifier::Mutable,
                    Some(syntax_tree::r#type::Qualifier::Restrict(..)) => Qualifier::Restrict,
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
                    region,
                    pointee,
                }))
            }
            syntax_tree::r#type::Type::Pointer(pointer_ty) => {
                let qualifier = match pointer_ty.qualifier() {
                    Some(syntax_tree::r#type::Qualifier::Mutable(..)) => Qualifier::Mutable,
                    Some(syntax_tree::r#type::Qualifier::Restrict(..)) => Qualifier::Restrict,
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
                            r#type::Type::TraitMember(ty) => elements.push(
                                r#type::TupleElement::Unpacked(r#type::Unpacked::TraitMember(ty)),
                            ),
                            r#type::Type::Parameter(ty) => elements.push(
                                r#type::TupleElement::Unpacked(r#type::Unpacked::Parameter(ty)),
                            ),
                            r#type::Type::Tuple(tuple) => {
                                elements.extend(tuple.elements.into_iter());
                            }
                            ty => elements.push(r#type::TupleElement::Regular(ty)),
                        }
                    } else {
                        elements.push(r#type::TupleElement::Regular(ty));
                    }
                }

                // check if there is more than one unpacked type
                if elements.iter().filter(|x| x.is_unpacked()).count() > 1 {
                    handler.receive(error::Error::MoreThanOneUnpackedInTupleType(
                        MoreThanOneUnpackedInTupleType {
                            illegal_tuple_type_span: syntax_tree.span(),
                        },
                    ));
                    return Err(Error::SemanticError);
                }

                Ok(r#type::Type::Tuple(r#type::Tuple { elements }))
            }
            syntax_tree::r#type::Type::Array(array) => {
                let length = self.evaluate(array.expression(), referring_site, handler)?;
                let element_ty =
                    self.resolve_type(array.operand(), referring_site, config, handler)?;

                Ok(r#type::Type::Array(r#type::Array {
                    length,
                    element: Box::new(element_ty),
                }))
            }
        }
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
        handler: &dyn Handler<error::Error>,
    ) -> Result<LifetimeParameterID, Error> {
        for scope in self.scope_walker(referring_site).ok_or(Error::InvalidID)? {
            let Ok(generic_id) = GenericID::try_from(scope) else {
                continue;
            };

            let generic_symbol = self.get_generic(generic_id).expect("should've been valid");

            if let Some(lifetime_id) = generic_symbol
                .generic_declaration()
                .parameters
                .lifetimes
                .get_id(identifier.span.str())
            {
                return Ok(MemberID {
                    parent: generic_id,
                    id: lifetime_id,
                });
            }
        }

        handler.receive(error::Error::LifetimeParameterNotFound(
            LifetimeParameterNotFound {
                referred_span: identifier.span(),
                referring_site,
            },
        ));
        Err(Error::SemanticError)
    }

    /// Resolves a [`syntax_tree::LifetimeArgument`] to a [`Region`].
    ///
    /// # Errors
    ///
    /// - [`Error::InvalidID`]: If the `referring_site` is not a valid ID.
    /// - [`Error::SemanticError`]: If encountered a fatal semantic error e.g. symbol not found.
    pub fn resolve_lifetime_argument<S: Model>(
        &self,
        lifetime_argument: &syntax_tree::LifetimeArgument,
        referring_site: GlobalID,
        handler: &dyn Handler<error::Error>,
    ) -> Result<Region<S>, Error> {
        match lifetime_argument.identifier() {
            LifetimeArgumentIdentifier::Static(..) => Ok(Region::Static),
            LifetimeArgumentIdentifier::Identifier(ident) => self
                .resolve_lifetime_parameter(ident, referring_site, handler)
                .map(Region::Named),
        }
    }

    fn handle_lifetime_arguments_supplied<S: Model<ForallRegion = Never>>(
        &self,
        generic_identifier_span: Span,
        referring_site: GlobalID,
        generic_symbol: &dyn Generic,
        lifetime_argument_syns: Vec<&syntax_tree::LifetimeArgument>,
        config: &mut dyn Config<S>,
        handler: &dyn Handler<error::Error>,
    ) -> Result<Vec<Region<S>>, Error> {
        let expected_count = generic_symbol
            .generic_declaration()
            .parameters
            .lifetimes
            .len();

        // resolve the lifetime arguments
        if lifetime_argument_syns.is_empty() {
            let mut lifetime_arguments = Vec::new();

            for _ in 0..generic_symbol
                .generic_declaration()
                .parameters
                .lifetimes
                .len()
            {
                let Some(placeholder) = config.region_arguments_placeholder() else {
                    handler.receive(error::Error::GenericArgumentCountMismatch(
                        GenericArgumentCountMismatch {
                            generic_kind: GenericKind::Lifetime,
                            generic_identifier_span,
                            expected_count: generic_symbol
                                .generic_declaration()
                                .parameters
                                .lifetimes
                                .len(),
                            supplied_count: 0,
                        },
                    ));
                    return Err(Error::SemanticError);
                };

                lifetime_arguments.push(placeholder);
            }

            Ok(lifetime_arguments)
        } else if lifetime_argument_syns.len() == expected_count {
            Ok(lifetime_argument_syns
                .into_iter()
                .map(|x| self.resolve_lifetime_argument(x, referring_site, handler))
                .collect::<Result<Vec<_>, _>>()?)
        } else {
            // lifetime arguments count mismatch
            handler.receive(error::Error::GenericArgumentCountMismatch(
                GenericArgumentCountMismatch {
                    generic_kind: GenericKind::Lifetime,
                    generic_identifier_span,
                    expected_count,
                    supplied_count: lifetime_argument_syns.len(),
                },
            ));
            Err(Error::SemanticError)
        }
    }

    fn handle_constant_arguments_supplied<S: Model<ForallRegion = Never>>(
        &self,
        generic_identifier_span: Span,
        referring_site: GlobalID,
        generic_symbol: &dyn Generic,
        constant_argument_syns: Vec<&syntax_tree::expression::Expression>,
        config: &mut dyn Config<S>,
        handler: &dyn Handler<error::Error>,
    ) -> Result<Vec<constant::Constant<S>>, Error> {
        handle_generic_arguments_supplied!(
            self,
            generic_identifier_span,
            referring_site,
            generic_symbol,
            constant_argument_syns,
            config,
            handler,
            Constant,
            true,
            syntax_tree,
            param,
            {
                self.evaluate(syntax_tree, referring_site, handler)
                    .map(|val| {
                        config.check(
                            Checking::TypeCheck(
                                val.clone(),
                                param.r#type.clone().into_other_model(),
                            ),
                            generic_identifier_span.clone(),
                        );

                        val
                    })
            }
        )
    }

    #[allow(clippy::too_many_arguments)]
    fn handle_type_arguments_supplied<S: Model<ForallRegion = Never>>(
        &self,
        generic_identifier_span: Span,
        referring_site: GlobalID,
        generic_symbol: &dyn Generic,
        type_argument_syns: Vec<&syntax_tree::r#type::Type>,
        constant_arguments_is_empty: bool,
        config: &mut dyn Config<S>,
        handler: &dyn Handler<error::Error>,
    ) -> Result<Vec<r#type::Type<S>>, Error> {
        handle_generic_arguments_supplied!(
            self,
            generic_identifier_span,
            referring_site,
            generic_symbol,
            type_argument_syns,
            config,
            handler,
            Type,
            constant_arguments_is_empty,
            syntax_tree,
            _,
            self.resolve_type(syntax_tree, referring_site, config, handler)
        )
    }

    // resolve the generic arguments required for `resolved_id`.
    pub(super) fn resolve_generic_arguments<S: Model<ForallRegion = Never>>(
        &self,
        generic_identifier: &GenericIdentifier,
        referring_site: GlobalID,
        resolved_id: GlobalID,
        config: &mut dyn Config<S>,
        handler: &dyn Handler<error::Error>,
    ) -> Result<Option<GenericArguments<S>>, Error> {
        let Ok(generic_id) = GenericID::try_from(resolved_id) else {
            if let Some(generic_arguments) = generic_identifier.generic_arguments() {
                // non-fatal error, keep going
                handler.receive(error::Error::NoGenericArgumentsRequired(
                    NoGenericArgumentsRequired {
                        global_id: resolved_id,
                        generic_argument_span: generic_arguments.span(),
                    },
                ));
            }

            return Ok(None);
        };

        let generic_symbol = self.get_generic(generic_id).ok_or(Error::InvalidID)?;

        let mut lifetime_argument_syns = Vec::new();
        let mut type_argument_syns = Vec::new();
        let mut constant_argument_syns = Vec::new();

        // extracts the generic arguments from the syntax tree to the list of syntax trees
        for generic_arguments in generic_identifier
            .generic_arguments()
            .iter()
            .flat_map(|x| x.argument_list().elements())
        {
            match generic_arguments {
                syntax_tree::GenericArgument::Constant(arg) => {
                    constant_argument_syns.push(&**arg.expression());
                }
                syntax_tree::GenericArgument::Type(arg) if constant_argument_syns.is_empty() => {
                    type_argument_syns.push(&**arg);
                }
                syntax_tree::GenericArgument::Lifetime(arg)
                    if lifetime_argument_syns.is_empty() && type_argument_syns.is_empty() =>
                {
                    lifetime_argument_syns.push(arg);
                }
                arg => {
                    handler.receive(error::Error::MisorderedGenericArguments(
                        MisorderedGenericArgument {
                            generic_kind: match arg {
                                syntax_tree::GenericArgument::Type(_) => GenericKind::Type,
                                syntax_tree::GenericArgument::Constant(_) => GenericKind::Constant,
                                syntax_tree::GenericArgument::Lifetime(_) => GenericKind::Lifetime,
                            },
                            generic_argument: arg.span(),
                        },
                    ));
                    return Err(Error::SemanticError);
                }
            }
        }

        // extract the arguments syntax
        Ok(Some(GenericArguments {
            regions: self.handle_lifetime_arguments_supplied(
                generic_identifier.span(),
                referring_site,
                &*generic_symbol,
                lifetime_argument_syns,
                config,
                handler,
            )?,
            types: self.handle_type_arguments_supplied(
                generic_identifier.span(),
                referring_site,
                &*generic_symbol,
                type_argument_syns,
                constant_argument_syns.is_empty(),
                config,
                handler,
            )?,
            constants: self.handle_constant_arguments_supplied(
                generic_identifier.span(),
                referring_site,
                &*generic_symbol,
                constant_argument_syns,
                config,
                handler,
            )?,
        }))
    }

    #[allow(clippy::too_many_lines)]
    fn final_pass<S: Model<ForallRegion = Never>>(
        &self,
        resolved_id: GlobalID,
        generic_arguments: Option<GenericArguments<S>>,
        latest_resolution: Option<Resolution<S>>,
        span: &Span,
        config: &mut dyn Config<S>,
    ) -> Resolution<S> {
        // if trait is resolved, add trait bound predicate
        if let GlobalID::Trait(trait_id) = resolved_id {
            let predicate = Predicate::Trait(predicate::Trait {
                trait_id,
                generic_arguments: generic_arguments
                    .clone()
                    .expect("should have generic arugmentsj")
                    .into_other_model(),
                const_trait: false, // true, unless explicitly specified in the where clause
            });

            config.check(Checking::Predicate(predicate), span.clone());
        }

        // generic id, do where clause check
        if let Ok(generic_id) = GenericID::try_from(resolved_id) {
            let parent_arguments = latest_resolution.as_ref().and_then(|x| match x {
                // its generic argumnets don't matter
                Resolution::Enum(_)
                | Resolution::Struct(_)
                | Resolution::Module(_)
                | Resolution::Function(_) => None,

                Resolution::Trait(res) => Some((&res.generic_arguments, res.id.into())),

                // impossible, these resolution can't be resolved any further
                Resolution::TraitFunction(_)
                | Resolution::TraitType(_)
                | Resolution::TraitConstant(_)
                | Resolution::NonGlobalIDType(_)
                | Resolution::Constant(_)
                | Resolution::ImplementationFunction(_)
                | Resolution::Variant(_) => unreachable!(),
            });

            let mut substitution = Substitution::from_generic_arguments(
                generic_arguments.clone().unwrap(),
                generic_id,
            );
            if let Some((parent_arguments, parent_generic_id)) = parent_arguments {
                substitution = substitution
                    .append_from_generic_arguments(parent_arguments.clone(), parent_generic_id)
                    .expect("should've been a valid substitution");
            }

            let predicates = self
                .get_generic(generic_id)
                .expect("should've been valid")
                .generic_declaration()
                .predicates
                .iter()
                .map(|x| &x.predicate)
                .cloned()
                .collect::<Vec<_>>();

            for predicate in predicates {
                let mut predicate = predicate.into_other_model::<S>();
                predicate.apply(&substitution);

                config.check(Checking::Predicate(predicate), span.clone());
            }
        }

        match resolved_id {
            GlobalID::Module(module_id) => Resolution::Module(module_id),
            GlobalID::Struct(struct_id) => Resolution::Struct(WithGenerics {
                id: struct_id,
                generic_arguments: generic_arguments.expect("should have generic arguments"),
            }),
            GlobalID::Trait(trait_id) => Resolution::Trait(WithGenerics {
                id: trait_id,
                generic_arguments: generic_arguments.expect("should have generic arguments"),
            }),
            GlobalID::Enum(enum_id) => Resolution::Enum(WithGenerics {
                id: enum_id,
                generic_arguments: generic_arguments.expect("should have generic arguments"),
            }),
            GlobalID::Type(type_id) => {
                let mut ty = self
                    .get(type_id)
                    .expect("should have been valid ")
                    .r#type
                    .clone()
                    .into_other_model();

                // substitution from the generic arguments
                let substitution = Substitution::from_generic_arguments(
                    generic_arguments.expect("should have generic arguments"),
                    GenericID::Type(type_id),
                );

                ty.apply(&substitution);

                ty.into()
            }
            GlobalID::Constant(constant_id) => {
                let constant = self
                    .get(constant_id)
                    .expect("should have been valid ")
                    .constant
                    .clone();

                Resolution::Constant(constant)
            }
            GlobalID::Function(function_id) => Resolution::Function(WithGenerics {
                id: function_id,
                generic_arguments: generic_arguments.expect("should have generic arguments"),
            }),
            GlobalID::Variant(variant_id) => Resolution::Variant(Variant {
                enum_generic_arguments: generic_arguments.unwrap_or_else(|| {
                    let parent_enum_id = self.get(variant_id).unwrap().parent_enum_id;
                    self.get(parent_enum_id)
                        .unwrap()
                        .generic_declaration
                        .parameters
                        .create_identity_generic_arguments(parent_enum_id.into())
                }),
                variant_id,
            }),
            GlobalID::TraitType(trait_type_id) => Resolution::TraitType(WithParent {
                parent_generic_arguments: latest_resolution.map_or_else(
                    || {
                        let parent_trait_id = self.get(trait_type_id).unwrap().parent_trait_id;
                        self.get(parent_trait_id)
                            .unwrap()
                            .generic_declaration
                            .parameters
                            .create_identity_generic_arguments(parent_trait_id.into())
                    },
                    |x| {
                        let trait_res = x
                            .into_trait()
                            .expect("should be a trait as its parent resolution");

                        trait_res.generic_arguments
                    },
                ),
                member: WithGenerics {
                    id: trait_type_id,
                    generic_arguments: generic_arguments.expect("should have generic arguments"),
                },
            }),
            GlobalID::TraitFunction(trait_function_id) => Resolution::TraitFunction(WithParent {
                parent_generic_arguments: latest_resolution.map_or_else(
                    || {
                        let parent_trait_id = self.get(trait_function_id).unwrap().parent_trait_id;
                        self.get(parent_trait_id)
                            .unwrap()
                            .generic_declaration
                            .parameters
                            .create_identity_generic_arguments(parent_trait_id.into())
                    },
                    |x| {
                        let trait_res = x
                            .into_trait()
                            .expect("should be a trait as its parent resolution");

                        trait_res.generic_arguments
                    },
                ),
                member: WithGenerics {
                    id: trait_function_id,
                    generic_arguments: generic_arguments.expect("should have generic arguments"),
                },
            }),

            GlobalID::TraitConstant(trait_constant_id) => Resolution::TraitConstant(WithParent {
                parent_generic_arguments: latest_resolution.map_or_else(
                    || {
                        let parent_trait_id = self.get(trait_constant_id).unwrap().parent_trait_id;
                        self.get(parent_trait_id)
                            .unwrap()
                            .generic_declaration
                            .parameters
                            .create_identity_generic_arguments(parent_trait_id.into())
                    },
                    |x| {
                        let trait_res = x
                            .into_trait()
                            .expect("should be a trait as its parent resolution");

                        trait_res.generic_arguments
                    },
                ),
                member: trait_constant_id,
            }),

            GlobalID::Implementation(_) | GlobalID::NegativeImplementation(_) => {
                unreachable!("impossible to refer to the implementation")
            }

            GlobalID::ImplementationFunction(implementation_function_id) => {
                assert!(latest_resolution.is_none());

                Resolution::ImplementationFunction(WithGenerics {
                    id: implementation_function_id,
                    generic_arguments: generic_arguments.expect("should have generic arguments"),
                })
            }
            GlobalID::ImplementationType(implementation_type_id) => {
                assert!(latest_resolution.is_none());

                let mut ty = self
                    .get(implementation_type_id)
                    .expect("should have been valid ")
                    .r#type
                    .clone()
                    .into_other_model();

                // substitution from the generic arguments
                let substitution = Substitution::from_generic_arguments(
                    generic_arguments.expect("should have generic arguments"),
                    GenericID::ImplementationType(implementation_type_id),
                );

                ty.apply(&substitution);

                ty.into()
            }
            GlobalID::ImplementationConstant(implementation_constant_id) => {
                assert!(latest_resolution.is_none());

                let constant = self
                    .get(implementation_constant_id)
                    .expect("should have been valid ")
                    .constant
                    .clone();

                Resolution::Constant(constant)
            }
        }
    }

    /// Resolves a qualified identifier to a [`Resolution`]
    ///
    /// # Errors
    ///
    /// - [`Error::InvalidID`]: If the `referring_site` is not a valid ID.
    /// - [`Error::SemanticError`]: If encountered a fatal semantic error e.g. symbol not found.
    pub fn resolve<S: Model<ForallRegion = Never>>(
        &self,
        qualified_identifier: &QualifiedIdentifier,
        referring_site: GlobalID,
        config: &mut dyn Config<S>,
        handler: &dyn Handler<error::Error>,
    ) -> Result<Resolution<S>, Error> {
        // Checks if the given `referring_site` is a valid ID.
        drop(self.get_global(referring_site).ok_or(Error::InvalidID)?);

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
            let _ = self.finalize(global_id, Some(referring_site), handler);

            // generic arguments
            let generic_arguments = self.resolve_generic_arguments(
                generic_identifier,
                referring_site,
                global_id,
                config,
                handler,
            )?;

            latest_resolution = Some(self.final_pass(
                global_id,
                generic_arguments,
                latest_resolution,
                &generic_identifier.span(),
                config,
            ));
        }

        Ok(latest_resolution.expect("should at least have one resolution"))
    }
}
