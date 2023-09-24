//! Contains all the logic related to symbol resolution.

use enum_as_inner::EnumAsInner;
use pernixc_lexical::token::Identifier;
use pernixc_source::{SourceElement, Span};
use pernixc_syntax::syntax_tree::{self, GenericIdentifier, QualifiedIdentifier};
use pernixc_system::diagnostic::Handler;

use super::Table;
use crate::{
    constant,
    error::{
        self, ConstantArgumentCountMismatch, LifetimeArgumentCountMismatch,
        LifetimeArgumentSuppliedAfterConstantOrTypeArgument, LifetimeArgumentsRequired,
        LifetimeParameterNotFound, ModuleExpected, ModuleNotFound, NoGenericArgumentsRequired,
        NoMemberOnExpression, ResolutionAmbiguity, SymbolNotAccessible, SymbolNotFound,
        TargetNotFound, TraitExpected, TypeArgumentCountMismatch,
        TypeArgumentSuppliedAfterConstantArgument,
    },
    symbol::{
        ConstantRef, EnumRef, FunctionRef, GenericItemRef, GlobalItemRef, HigherRankedableLifetime,
        ImplementsAssociatedRef, ImplementsFunctionRef, ImplementsRef, LifetimeBoundOperand,
        LifetimeParameterRef, LocalImplementsAssociatedRef, LocalImplementsRef,
        LocalLifetimeParameterRef, LocalSubstitution, LocalTraitAssociatedRef,
        LocalTraitConstantRef, LocalTraitFunctionRef, LocalTraitTypeRef, ModuleMemberRef,
        ModuleRef, StructRef, Substitution, TraitAssociatedRef, TraitRef, VariantRef, WhereClause,
    },
    ty::{self, ElidedLifetime, Lifetime},
};

#[derive(Clone)]
pub(super) struct ToCheckingWithSpanHandler<'a> {
    pub(super) span: Span,
    pub(super) handler: &'a dyn Handler<CheckingWithSpan>,
}

impl<'a> Handler<Checking> for ToCheckingWithSpanHandler<'a> {
    fn receive(&self, error: Checking) {
        self.handler.receive(CheckingWithSpan {
            checking: error,
            span: self.span.clone(),
        });
    }
}

impl std::fmt::Debug for ToCheckingWithSpanHandler<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ToCheckingWithSpanHandler")
            .field("span", &self.span)
            .finish()
    }
}

/// Represents a lifetime bound checking.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct LifetimeBound {
    pub operand: LifetimeBoundOperand,
    pub bound: Lifetime,
}

/// Reprsents a trait associated type bound.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct TraitAssociatedTypeBound {
    pub trait_associated_type: ty::TraitAssociated,
    pub bound: ty::Type,
}

/// Represents a trait associated constant bound.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct TraitAssociatedConstant {
    pub trait_associated_constant: ty::TraitAssociated,
    pub constant: constant::Constant,
}

/// Represents a trait bound.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct TraitBound {
    pub trait_ref: TraitRef,
    pub type_substituions: Vec<ty::Type>,
    pub lifetime_substituions: Vec<HigherRankedableLifetime>,
    pub constant_substituions: Vec<constant::Constant>,
    pub is_const: bool,
}

/// Represents a type checking occurred when substituting a constant parameter.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct ConstantTypeCheck {
    pub value_type: ty::Type,
    pub expected_type: ty::Type,
}

/// An enumeration containing all kinds of checking occurred when resolving a symbol.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Checking {
    LifetimeBound(LifetimeBound),
    TraitAssociatedTypeBound(TraitAssociatedTypeBound),
    TraitAssociatedConstant(TraitAssociatedConstant),
    TraitBound(TraitBound),
    ConstantTypeCheck(ConstantTypeCheck),
}

/// A structure containing both the checking required and the span to the source code where checking
/// occurred.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CheckingWithSpan {
    /// The checking required.
    pub checking: Checking,

    /// The span to the source code where checking occurred.
    pub span: Span,
}

/// Indicates how the checking should be performed when resolving a symbol.
#[derive(Clone, Copy)]
pub enum CheckingBehavior<'a> {
    /// Immediately check all the requirements and report errors to the handler if any.
    ///
    /// When the checking fails, the resolution will return `None` error immediately.
    Immediate,

    /// Defer the checking to the given handler.
    ///
    /// The checking will not performed immediately by the resolver. Instead, the resolver will
    /// pass all the checking requirements to the given handler.
    ///
    /// The resolver will assume everything is correct and return the resolution. The callee will
    /// then perform the checking and determine whether the resolution is valid.
    Defer(Option<&'a dyn Handler<CheckingWithSpan>>),
}

impl std::fmt::Debug for CheckingBehavior<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CheckingBehavior::Immediate => write!(f, "CheckingBehavior::Immediate"),
            CheckingBehavior::Defer(_) => write!(f, "CheckingBehavior::Defer"),
        }
    }
}

/// Indicates whether the lifetime arguments are required to be explicitly specified.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ExplicitLifetimeRequired {
    /// The lifetime arguments are required to be explicitly specified.
    True,

    /// The lifetime arguments are not required to be explicitly specified.
    ///
    /// The [`Lifetime::Elided`] will be used with the given [`GenericItemRef`] instead when the
    /// lifetime arguments are not supplied.
    False(GenericItemRef),
}

/// A configuration used for symbol resolution.
#[derive(Debug)]
#[allow(missing_docs)]
pub struct Config<'a> {
    /// The site where the symbol is referred from.
    pub referring_site: GlobalItemRef,
    pub checking: CheckingBehavior<'a>,
    pub explicit_lifetime_required: ExplicitLifetimeRequired,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Root {
    referring_site: GlobalItemRef,
    latest_resolution: Option<Resolution>,
}

/// Represents a resolution to a symbol with generic arguments.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Generic<Ref> {
    /// The reference referring to the symbol in the [`crate::table::Table`].
    ///
    /// Depending on the context, the symbol might be a struct, an enum, a function, a trait, etc.
    pub symbol_ref: Ref,

    /// The generic arguments substituted to the symbol.
    pub substitution: LocalSubstitution,
}

/// Represents a successful resolution to an implements of a trait.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Implements {
    /// The reference referring to the implements in the [`crate::symbol::Trait::implements`]
    /// field.
    pub local_implements_ref: LocalImplementsRef,

    /// The generic arguments deduced from the implements.
    pub deduced_substituion: LocalSubstitution,
}

/// Represents a resolution to a trait.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Trait {
    /// The reference referring to the trait in the [`crate::table::Table::traits`] field.
    pub trait_ref: TraitRef,

    /// The generic arguments substituted to the trait.
    pub substitution: LocalSubstitution,

    /// The reference refers to the implements in the [`crate::symbol::Trait::implements`] field.
    ///
    /// If the trait is not resolved to an implements, this field will be `None`.
    pub resolved_implements: Option<Implements>,
}

/// Represents a resolution to a trait associated item that contains generic arguments.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitAssociatedGenericItem<LocalRef> {
    /// The reference referring to the trait in the [`crate::table::Table::traits`] field.
    pub trait_ref: TraitRef,

    /// The generic arguments substituted to the trait.
    pub trait_substitution: LocalSubstitution,

    /// The reference referring to the associated item in the trait.
    ///
    /// Depending on the context, the associated item might be a function or a type.
    pub local_ref: LocalRef,

    /// The generic arguments substituted to the associated item.
    pub associated_substitution: LocalSubstitution,
}

/// Represents a resolution to a trait associated constant.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitConstant {
    /// The reference referring to the trait in the [`crate::table::Table::traits`] field.
    pub trait_ref: TraitRef,

    /// The generic arguments substituted to the trait.
    pub trait_substitution: LocalSubstitution,

    /// The index referring to the associated constant in the [`crate::symbol::Trait::constants`]
    /// field.
    pub local_ref: LocalTraitConstantRef,
}

/// Represents a resolution to an implements associated function.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ImplementsFunction {
    /// The reference referring to the implements function.
    pub implements_function_ref: ImplementsFunctionRef,

    /// The generic arguments deduced from the implements.
    pub deduced_substitution: LocalSubstitution,

    /// The generic arguments substituted to the associated function.
    pub function_substitution: LocalSubstitution,
}

/// Represents a resolution of a symbol.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum Resolution {
    Pointer(ty::Pointer),
    Primitive(ty::Primitive),
    Reference(ty::Reference),
    Tuple(ty::Tuple),
    Module(ModuleRef),
    Struct(Generic<StructRef>),
    Enum(Generic<EnumRef>),
    Function(Generic<FunctionRef>),
    Variant(Generic<VariantRef>),
    Constant(ConstantRef),
    Trait(Trait),
    TraitFunction(TraitAssociatedGenericItem<LocalTraitFunctionRef>),
    TraitType(TraitAssociatedGenericItem<LocalTraitTypeRef>),
    TraitConstant(TraitConstant),
    ImplementsFunction(ImplementsFunction),
}

impl Table {
    #[allow(clippy::type_complexity)]
    fn retrieve_generic_argument_syntax_trees<'a>(
        generic_identifier: &'a GenericIdentifier,
        handler: &impl Handler<error::Error>,
    ) -> Result<
        (
            Vec<&'a syntax_tree::LifetimeArgument>,
            Vec<&'a syntax_tree::ty::Type>,
            Vec<&'a syntax_tree::ConstantArgument>,
        ),
        super::Error,
    > {
        let mut lifetime_argument_syntax_trees = Vec::new();
        let mut type_argument_syntax_trees = Vec::new();
        let mut constant_argument_syntax_trees = Vec::new();

        for generic_argument in generic_identifier
            .generic_arguments()
            .iter()
            .flat_map(|x| x.argument_list().elements())
        {
            match generic_argument {
                syntax_tree::GenericArgument::Constant(constant_argument) => {
                    constant_argument_syntax_trees.push(constant_argument);
                }
                syntax_tree::GenericArgument::Type(type_argument) => {
                    if !constant_argument_syntax_trees.is_empty() {
                        handler.receive(error::Error::TypeArgumentSuppliedAfterConstantArgument(
                            TypeArgumentSuppliedAfterConstantArgument {
                                type_argument_span: type_argument.span(),
                            },
                        ));
                        return Err(super::Error::SemanticError);
                    }

                    type_argument_syntax_trees.push(type_argument.as_ref());
                }
                syntax_tree::GenericArgument::Lifetime(lifetime_argument) => {
                    if !type_argument_syntax_trees.is_empty()
                        || !constant_argument_syntax_trees.is_empty()
                    {
                        handler.receive(
                            error::Error::LifetimeArgumentSuppliedAfterConstantOrTypeArgument(
                                LifetimeArgumentSuppliedAfterConstantOrTypeArgument {
                                    lifetime_argument_span: lifetime_argument.span(),
                                },
                            ),
                        );
                        return Err(super::Error::SemanticError);
                    }

                    lifetime_argument_syntax_trees.push(lifetime_argument);
                }
            }
        }

        Ok((
            lifetime_argument_syntax_trees,
            type_argument_syntax_trees,
            constant_argument_syntax_trees,
        ))
    }

    #[allow(clippy::too_many_lines)]
    fn resolve_substitution_with_finalization(
        &mut self,
        generic_identifier: &GenericIdentifier,
        global_item_ref: GlobalItemRef,
        _active_where_clause: &WhereClause,
        config: &Config,
        handler: &impl Handler<error::Error>,
    ) -> Result<LocalSubstitution, super::Error> {
        let mut local_substitution = LocalSubstitution::default();

        // check if the item requires generic arguments
        let Ok(generic_item_ref) = GenericItemRef::try_from(global_item_ref) else {
            if let Some(generic_arguments) = generic_identifier.generic_arguments() {
                handler.receive(error::Error::NoGenericArgumentsRequired(
                    NoGenericArgumentsRequired {
                        generic_arguments_span: generic_arguments.span(),
                        global_item_ref,
                    },
                ));
            }

            return Ok(local_substitution);
        };

        let (
            lifetime_argument_syntax_trees,
            type_argument_syntax_trees,
            constant_argument_syntax_trees,
        ) = Self::retrieve_generic_argument_syntax_trees(generic_identifier, handler)?;

        match (
            self.get_generic_item(generic_item_ref)
                .unwrap()
                .generic_parameters()
                .lifetimes
                .len(),
            lifetime_argument_syntax_trees.len(),
            config.explicit_lifetime_required,
        ) {
            // lifetime arguments must be supplied
            (1.., 0, ExplicitLifetimeRequired::True) => {
                handler.receive(error::Error::LifetimeArgumentsRequired(
                    LifetimeArgumentsRequired {
                        generic_identifier_span: generic_identifier.span(),
                    },
                ));
                return Err(super::Error::SemanticError);
            }

            // either supplied or not at all
            (required @ 1.., 0, ExplicitLifetimeRequired::False(generic_item_ref)) => {
                for _ in 0..required {
                    local_substitution
                        .lifetimes
                        .push(Lifetime::Elided(ElidedLifetime { generic_item_ref }));
                }
            }

            // well formed lifetime arguments
            (required, supplied, _) if required == supplied => {
                for lifetime_argument_syntax_tree in lifetime_argument_syntax_trees {
                    local_substitution
                        .lifetimes
                        .push(self.resolve_lifetime_argument(
                            global_item_ref,
                            lifetime_argument_syntax_tree,
                            handler,
                        )?);
                }
            }

            // lifetime arguments count mismatch
            (required, supplied, _) => {
                handler.receive(error::Error::LifetimeArgumentCountMismatch(
                    LifetimeArgumentCountMismatch {
                        expected_count: supplied,
                        actual_count: required,
                        generic_identifier_span: generic_identifier.span(),
                    },
                ));

                return Err(super::Error::SemanticError);
            }
        }

        // check if the argument count and parameter count matches
        {
            let generic_item = self.get_generic_item(generic_item_ref).unwrap();

            if generic_item.generic_parameters().types.len() != type_argument_syntax_trees.len() {
                handler.receive(error::Error::TypeArgumentCountMismatch(
                    TypeArgumentCountMismatch {
                        expected_count: generic_item.generic_parameters().types.len(),
                        actual_count: type_argument_syntax_trees.len(),
                        generic_identifier_span: generic_identifier.span(),
                    },
                ));

                return Err(super::Error::SemanticError);
            }

            if generic_item.generic_parameters().constants.len()
                != constant_argument_syntax_trees.len()
            {
                handler.receive(error::Error::ConstantArgumentCountMismatch(
                    ConstantArgumentCountMismatch {
                        expected_count: generic_item.generic_parameters().constants.len(),
                        actual_count: constant_argument_syntax_trees.len(),
                        generic_identifier_span: generic_identifier.span(),
                    },
                ));

                return Err(super::Error::SemanticError);
            }
        }

        for type_argument_syntax_tree in type_argument_syntax_trees {
            local_substitution
                .types
                .push(self.resolve_type_with_finalization(
                    type_argument_syntax_tree,
                    config,
                    handler,
                )?);
        }

        for (index, constant_argument_syntax_tree) in
            constant_argument_syntax_trees.into_iter().enumerate()
        {
            let substitution = Substitution::from_local(&local_substitution, generic_item_ref);

            let constant = self.evaluate_constant(
                constant_argument_syntax_tree.expression(),
                global_item_ref,
                config.checking,
                handler,
            )?;

            todo!("perform type checking")
        }

        Ok(local_substitution)
    }

    /// Resolves for a [`Lifetime`] with tthe given [`syntax_tree::LifetimeArgument`].
    ///
    /// # Errors
    ///
    /// - [`super::Error::SemanticError`]: If the lifetime parameter cannot be found. The error will
    ///  be reported to the handler.
    /// - [`super::Error::InvalidReference`]: If the `referring_site` is invalid.
    pub fn resolve_lifetime_argument(
        &self,
        referring_site: GlobalItemRef,
        lifetime_argument: &syntax_tree::LifetimeArgument,
        handler: &impl Handler<error::Error>,
    ) -> Result<Lifetime, super::Error> {
        match lifetime_argument.identifier() {
            syntax_tree::LifetimeArgumentIdentifier::Identifier(identifier) => self
                .resolve_lifetime_parameter(referring_site, identifier, handler)
                .map(Lifetime::Parameter),
            syntax_tree::LifetimeArgumentIdentifier::Static(_) => Ok(Lifetime::Static),
        }
    }

    /// Resolves for a lifetime parameter with the given identifier.
    ///
    /// # Errors
    ///
    /// - [`super::Error::SemanticError`]: If the lifetime parameter cannot be found. The error will
    ///  be reported to the handler.
    /// - [`super::Error::InvalidReference`]: If the `referring_site` is invalid.
    pub fn resolve_lifetime_parameter(
        &self,
        referring_site: GlobalItemRef,
        identifier: &Identifier,
        handler: &impl Handler<error::Error>,
    ) -> Result<LifetimeParameterRef, super::Error> {
        let scope_walker = self
            .scope_walker(referring_site)
            .ok_or(super::Error::InvalidReference)?;

        for id in scope_walker {
            let Ok(generic_item_ref) = GenericItemRef::try_from(id) else {
                continue;
            };

            let generic_item = self.get_generic_item(generic_item_ref).unwrap();

            if let Some(index) = generic_item
                .generic_parameters()
                .lifetimes
                .get_index_by_name(identifier.span().str())
            {
                return Ok(LifetimeParameterRef {
                    generic_item_ref,
                    local_ref: LocalLifetimeParameterRef(index),
                });
            }
        }

        handler.receive(error::Error::LifetimeParameterNotFound(
            LifetimeParameterNotFound {
                unknown_lifetime_parameter_span: identifier.span.clone(),
            },
        ));
        Err(super::Error::SemanticError)
    }

    pub(crate) fn resolve_type_with_finalization(
        &mut self,
        _ty: &syntax_tree::ty::Type,
        _config: &Config,
        _handler: &impl Handler<error::Error>,
    ) -> Result<ty::Type, super::Error> {
        todo!()
    }

    pub(super) fn resolve_with_finalization(
        &mut self,
        qualified_identifier: &QualifiedIdentifier,
        config: &Config,
        handler: &impl Handler<error::Error>,
    ) -> Result<Resolution, super::Error> {
        let mut current_root = Root {
            referring_site: config.referring_site,
            latest_resolution: None,
        };

        for generics_identifier in std::iter::once(qualified_identifier.first())
            .chain(qualified_identifier.rest().iter().map(|x| &x.1))
        {
            let global_ref = self.resolve_single_first_pass(
                generics_identifier.identifier(),
                &current_root,
                qualified_identifier.leading_scope_separator().is_some(),
                handler,
            )?;

            if let Ok(drafted_symbol_ref) = global_ref.try_into() {
                self.finalize_symbol_if_required(drafted_symbol_ref, handler);
            }

            todo!()
        }

        Ok(current_root.latest_resolution.unwrap())
    }

    fn resolve_single_first_pass(
        &self,
        identifier: &Identifier,
        root: &Root,
        search_from_root: bool,
        handler: &impl Handler<error::Error>,
    ) -> Result<GlobalItemRef, super::Error> {
        let global_ref = if let Some(latest_resolution) = &root.latest_resolution {
            let resolution_as_global_ref = match latest_resolution {
                Resolution::Primitive(_) => todo!(),
                Resolution::Reference(_) => todo!(),
                Resolution::Tuple(_) => todo!(),
                Resolution::Pointer(_) => todo!(),
                Resolution::Constant(_) => {
                    handler.receive(error::Error::NoMemberOnExpression(NoMemberOnExpression {
                        member_reference_span: identifier.span.clone(),
                    }));
                    return Err(super::Error::SemanticError);
                }
                Resolution::Module(id) => GlobalItemRef::Module(*id),
                Resolution::Struct(id) => GlobalItemRef::Struct(id.symbol_ref),
                Resolution::Enum(id) => GlobalItemRef::Enum(id.symbol_ref),
                Resolution::Function(id) => GlobalItemRef::Function(id.symbol_ref),
                Resolution::Variant(id) => GlobalItemRef::Variant(id.symbol_ref),
                Resolution::Trait(id) => id.resolved_implements.as_ref().map_or(
                    GlobalItemRef::Trait(id.trait_ref),
                    |implements| {
                        GlobalItemRef::Implements(ImplementsRef {
                            trait_ref: id.trait_ref,
                            local_ref: implements.local_implements_ref,
                        })
                    },
                ),
                Resolution::TraitFunction(id) => {
                    GlobalItemRef::TraitAssociated(TraitAssociatedRef {
                        trait_ref: id.trait_ref,
                        local_ref: LocalTraitAssociatedRef::Function(id.local_ref),
                    })
                }
                Resolution::TraitType(id) => GlobalItemRef::TraitAssociated(TraitAssociatedRef {
                    trait_ref: id.trait_ref,
                    local_ref: LocalTraitAssociatedRef::Type(id.local_ref),
                }),
                Resolution::TraitConstant(id) => {
                    GlobalItemRef::TraitAssociated(TraitAssociatedRef {
                        trait_ref: id.trait_ref,
                        local_ref: LocalTraitAssociatedRef::Constant(id.local_ref),
                    })
                }
                Resolution::ImplementsFunction(id) => {
                    GlobalItemRef::ImplementsAssociated(ImplementsAssociatedRef {
                        implements_ref: id.implements_function_ref.implements_ref,
                        local_ref: LocalImplementsAssociatedRef::Function(
                            id.implements_function_ref.local_ref,
                        ),
                    })
                }
            };

            let Some(child_global_ref) = self
                .get_global_item(resolution_as_global_ref)
                .ok_or(super::Error::InvalidReference)?
                .get_member(identifier.span.str())
            else {
                handler.receive(error::Error::SymbolNotFound(SymbolNotFound {
                    symbol_reference_span: identifier.span.clone(),
                }));
                return Err(super::Error::SemanticError);
            };

            child_global_ref
        } else if search_from_root {
            GlobalItemRef::Module(
                self.target_root_module_indices_by_name
                    .get(identifier.span.str())
                    .copied()
                    .ok_or_else(|| {
                        handler.receive(error::Error::TargetNotFound(TargetNotFound {
                            target_name_span: identifier.span.clone(),
                        }));
                        super::Error::SemanticError
                    })?,
            )
        } else {
            // perform relative root resolution
            self.resolve_root_relative(identifier, root.referring_site, handler)?
        };

        if !self
            .symbol_accessible(global_ref, root.referring_site)
            .unwrap()
        {
            handler.receive(error::Error::SymbolNotAccessible(SymbolNotAccessible {
                reference_span: identifier.span.clone(),
                referring_site_ref: root.referring_site,
                referred_ref: global_ref,
            }));
        }

        Ok(global_ref)
    }

    fn resolve_root_relative_second_pass(
        &self,
        identifier: &Identifier,
        mut referring_site: GlobalItemRef,
        handler: &impl Handler<error::Error>,
    ) -> Result<GlobalItemRef, super::Error> {
        // NOTE: Accessibility is not checked here because the symbol searching is done within the
        // same module ancestor tree.

        loop {
            // try to find the symbol in the current scope
            if let Some(id) = self
                .get_global_item(referring_site)
                .ok_or(super::Error::InvalidReference)?
                .get_member(identifier.span.str())
            {
                return Ok(id);
            }

            if let Some(parent_id) = self
                .get_global_item(referring_site)
                .expect("should've already been checked")
                .parent()
            {
                referring_site = parent_id;
            } else {
                // try to search the symbol in the root scope
                if let Some(id) = self
                    .target_root_module_indices_by_name
                    .get(identifier.span.str())
                    .copied()
                {
                    return Ok(GlobalItemRef::Module(id));
                }

                handler.receive(error::Error::SymbolNotFound(SymbolNotFound {
                    symbol_reference_span: identifier.span.clone(),
                }));
                return Err(super::Error::SemanticError);
            }
        }
    }

    fn resolve_root_relative_first_pass(
        &self,
        identifier: &Identifier,
        referring_site: GlobalItemRef,
        handler: &impl Handler<error::Error>,
    ) -> Result<Option<GlobalItemRef>, super::Error> {
        let closest_module = self
            .get_closet_module_ref(referring_site)
            .ok_or(super::Error::InvalidReference)?;

        let mut candidates = vec![];

        {
            let global_search_locations = self.modules[closest_module.0]
                .usings
                .keys()
                .copied()
                .map(GlobalItemRef::Module)
                .chain(std::iter::once(referring_site))
                .chain(std::iter::once(GlobalItemRef::Module(
                    self.target_root_module_indices_by_name["@core"],
                )));

            for location in global_search_locations {
                if let Some(global_ref) = self
                    .get_global_item(location)
                    .expect("should've already been checked")
                    .get_member(identifier.span().str())
                {
                    if self
                        .symbol_accessible(global_ref, referring_site)
                        .ok_or(super::Error::InvalidReference)?
                    {
                        candidates.push(global_ref);
                    }
                }
            }
        }

        match (candidates.get(0).cloned(), candidates.len()) {
            (Some(single), 1) => Ok(Some(single)),
            (None, 0) => Ok(None),
            _ => {
                handler.receive(error::Error::ResolutionAmbiguity(ResolutionAmbiguity {
                    identifier_span: identifier.span(),
                    candidates,
                }));

                Err(super::Error::SemanticError)
            }
        }
    }

    /// Resolves the root for the first resolution if the qualified identifier is not prefixed with
    /// leading `::`.
    fn resolve_root_relative(
        &self,
        identifier: &Identifier,
        referring_site: GlobalItemRef,
        handler: &impl Handler<error::Error>,
    ) -> Result<GlobalItemRef, super::Error> {
        let found_symbol_id = if let Some(id) =
            self.resolve_root_relative_first_pass(identifier, referring_site, handler)?
        {
            id
        } else {
            self.resolve_root_relative_second_pass(identifier, referring_site, handler)?
        };

        Ok(found_symbol_id)
    }

    #[allow(clippy::too_many_lines)]
    pub(super) fn resolve_trait_path(
        &self,
        qualified_identifier: &QualifiedIdentifier,
        module_ref: ModuleRef,
        handler: &impl Handler<error::Error>,
    ) -> Result<TraitRef, super::Error> {
        let mut current_root = Root {
            referring_site: GlobalItemRef::Module(module_ref),
            latest_resolution: None,
        };

        let mut iter = std::iter::once(qualified_identifier.first())
            .chain(qualified_identifier.rest().iter().map(|x| &x.1))
            .peekable();

        while let Some(generic_identifier) = iter.next() {
            let is_last = iter.peek().is_none();

            let global_ref = self.resolve_single_first_pass(
                generic_identifier.identifier(),
                &current_root,
                qualified_identifier.leading_scope_separator().is_some(),
                handler,
            )?;

            if !is_last {
                let GlobalItemRef::Module(module_id) = global_ref else {
                    handler.receive(error::Error::ModuleExpected(ModuleExpected {
                        module_path_span: generic_identifier.span(),
                    }));
                    return Err(super::Error::SemanticError);
                };

                if let Some(generic_arguments_syn) = generic_identifier.generic_arguments().as_ref()
                {
                    handler.receive(error::Error::NoGenericArgumentsRequired(
                        NoGenericArgumentsRequired {
                            generic_arguments_span: generic_arguments_syn.span(),
                            global_item_ref: global_ref,
                        },
                    ));
                    return Err(super::Error::SemanticError);
                }

                current_root.latest_resolution = Some(Resolution::Module(module_id));
            } else if let GlobalItemRef::Trait(trait_id) = global_ref {
                return Ok(trait_id);
            } else {
                handler.receive(error::Error::TraitExpected(TraitExpected {
                    trait_path_span: generic_identifier.span(),
                }));
                return Err(super::Error::SemanticError);
            }
        }

        unreachable!()
    }

    /// Resolves for the module index from the given [`syntax_tree::item::ModulePath`].
    pub fn resolve_module_path(
        &self,
        module_path: &syntax_tree::item::ModulePath,
        handler: &impl Handler<error::Error>,
    ) -> Option<ModuleRef> {
        let mut current_module_ref: Option<ModuleRef> = None;

        for path in module_path.paths() {
            let next = match current_module_ref {
                // continue searching from current module
                Some(current_module_ref) => 'a: {
                    let Some(id) = ({
                        // search from current module id
                        self.modules[current_module_ref.0]
                            .module_member_refs_by_name
                            .get(path.span.str())
                            .copied()
                    }) else {
                        break 'a None;
                    };

                    // must be a module
                    let ModuleMemberRef::Module(module_index) = id else {
                        handler.receive(error::Error::ModuleExpected(ModuleExpected {
                            module_path_span: path.span.clone(),
                        }));
                        return None;
                    };

                    Some(module_index)
                }

                // search from root
                None => self
                    .target_root_module_indices_by_name
                    .get(path.span.str())
                    .copied(),
            };

            let Some(next) = next else {
                handler.receive(error::Error::ModuleNotFound(ModuleNotFound {
                    module_path_span: path.span.clone(),
                    searched_module_ref: current_module_ref,
                }));
                return None;
            };

            current_module_ref = Some(next);
        }

        current_module_ref
    }
}
