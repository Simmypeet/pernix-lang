//! Contains all the logic related to symbol resolution.

use enum_as_inner::EnumAsInner;
use pernixc_lexical::token::Identifier;
use pernixc_source::{SourceElement, Span};
use pernixc_syntax::syntax_tree::{self, QualifiedIdentifier};
use pernixc_system::diagnostic::Handler;

use super::Table;
use crate::{
    constant,
    error::{
        self, ModuleExpected, ModuleNotFound, NoGenericArgumentsRequired, NoMemberOnExpression,
        ResolutionAmbiguity, SymbolNotAccessible, SymbolNotFound, TargetNotFound, TraitExpected,
    },
    symbol::{
        AssociatedItemRef, GlobalItemRef, HigherRankedableLifetime, ImplementsAssociatedRef, Index,
        LifetimeBoundOperand, LocalSubstitution, ModuleMemberRef, TraitAssociatedRef,
    },
    ty::{self, Lifetime},
};

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
    pub trait_index: Index,
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
pub enum CheckingBehavior<'a> {
    /// Immediately check all the requirements and report errors to the handler if any.
    Immediate,

    /// Defer the checking to the given handler.
    ///
    /// The checking will not performed immediately by the resolver. Instead, the resolver will
    /// pass all the checking requirements to the given handler.
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
    /// The [`Lifetime::Elided`] will be used with the given [`GlobalItemRef`] instead when the
    /// lifetime arguments are not supplied.
    False(GlobalItemRef),
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
pub struct Generic {
    /// The index referring to the symbol in the [`crate::table::Table`].
    ///
    /// Depending on the context, the symbol might be a struct, an enum, a function, a trait, etc.
    pub index: Index,

    /// The generic arguments substituted to the symbol.
    pub substitution: LocalSubstitution,
}

/// Represents a resolution to a variant.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Variant {
    /// The resolution to the enum.
    pub enum_resolution: Generic,

    /// The index referring to the variant in the [`crate::symbol::Enum::variants`] field.
    pub variant_index: Index,
}

/// Represents a resolution to a trait.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Trait {
    /// The index referring to the trait in the [`crate::table::Table::traits`] field.
    pub trait_index: Index,

    /// The generic arguments substituted to the trait.
    pub substitution: LocalSubstitution,

    /// The index refers to the implements in the [`crate::symbol::Trait::implements`] field.
    ///
    /// If the trait is not resolved to an implements, this field will be `None`.
    pub resolved_implements: Option<Index>,
}

/// Represents a resolution to a trait associated item that contains generic arguments.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitAssociatedGenericItem {
    /// The index referring to the trait in the [`crate::table::Table::traits`] field.
    pub trait_index: Index,

    /// The generic arguments substituted to the trait.
    pub trait_substitution: LocalSubstitution,

    /// The index referring to the associated item in the trait.
    ///
    /// Depending on the context, the associated item might be a function or a type.
    pub associated_index: Index,

    /// The generic arguments substituted to the associated item.
    pub associated_substitution: LocalSubstitution,
}

/// Represents a resolution to a trait associated constant.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitConstant {
    /// The index referring to the trait in the [`crate::table::Table::traits`] field.
    pub trait_index: Index,

    /// The generic arguments substituted to the trait.
    pub trait_substitution: LocalSubstitution,

    /// The index referring to the associated constant in the [`crate::symbol::Trait::constants`]
    /// field.
    pub constant_index: Index,
}

/// Represents a resolution to an implements associated function.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ImplementsFunction {
    /// The index referring to the trait in the [`crate::table::Table::traits`] field.
    pub trait_index: Index,

    /// The index refers to the implements in the [`crate::symbol::Trait::implements`] field.
    pub implements_index: Index,

    /// The generic arguments deduced from the implements.
    pub deduced_substitution: LocalSubstitution,

    /// The index referring to the associated function in the
    /// [`crate::symbol::Implements::functions`] field.
    pub function_index: Index,

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
    Module(Index),
    Struct(Generic),
    Enum(Generic),
    Function(Generic),
    Variant(Variant),
    Constant(Index),
    Trait(Trait),
    TraitFunction(TraitAssociatedGenericItem),
    TraitType(TraitAssociatedGenericItem),
    TraitConstant(TraitConstant),
    ImplementsFunction(ImplementsFunction),
}

/*
Two Kinds of Resolution

You might have noticed that there are two kinds of resolution in this module

- `resolve_*` 
 */

impl Table {
    pub(crate) fn resolve_type_with_finalization(
        &mut self,
        ty: &syntax_tree::ty::Type,
        config: &Config,
        handler: &impl Handler<error::Error>,
    ) -> Result<ty::Type, super::Error> {
        todo!()
    }

    pub(super) fn resolve_with_finalization(
        &mut self,
        qualified_identifier: &QualifiedIdentifier,
        config: &Config,
        handler: &impl Handler<error::Error>,
    ) -> Result<Resolution, super::Error> {
        todo!()
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
                Resolution::Struct(id) => GlobalItemRef::Struct(id.index),
                Resolution::Enum(id) => GlobalItemRef::Enum(id.index),
                Resolution::Function(id) => GlobalItemRef::Function(id.index),
                Resolution::Variant(id) => GlobalItemRef::Variant(AssociatedItemRef {
                    parent_ref: id.enum_resolution.index,
                    associated_item_ref: id.variant_index,
                }),
                Resolution::Trait(id) => id.resolved_implements.map_or(
                    GlobalItemRef::Trait(id.trait_index),
                    |implements_index| {
                        GlobalItemRef::Implements(AssociatedItemRef {
                            parent_ref: id.trait_index,
                            associated_item_ref: implements_index,
                        })
                    },
                ),
                Resolution::TraitFunction(id) => {
                    GlobalItemRef::TraitAssociated(AssociatedItemRef {
                        parent_ref: id.trait_index,
                        associated_item_ref: TraitAssociatedRef::Function(id.associated_index),
                    })
                }
                Resolution::TraitType(id) => GlobalItemRef::TraitAssociated(AssociatedItemRef {
                    parent_ref: id.trait_index,
                    associated_item_ref: TraitAssociatedRef::Type(id.associated_index),
                }),
                Resolution::TraitConstant(id) => {
                    GlobalItemRef::TraitAssociated(AssociatedItemRef {
                        parent_ref: id.trait_index,
                        associated_item_ref: TraitAssociatedRef::Constant(id.constant_index),
                    })
                }
                Resolution::ImplementsFunction(id) => {
                    GlobalItemRef::ImplementsAssociated(AssociatedItemRef {
                        parent_ref: AssociatedItemRef {
                            parent_ref: id.trait_index,
                            associated_item_ref: id.implements_index,
                        },
                        associated_item_ref: ImplementsAssociatedRef::Function(id.function_index),
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
                referring_site_qualified_name: self
                    .get_qualified_name(root.referring_site)
                    .ok_or(super::Error::SemanticError)?,
                referred_site_qualified_name: self.get_qualified_name(global_ref).unwrap(),
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
            .get_closet_module_index(referring_site)
            .ok_or(super::Error::InvalidReference)?;

        let mut candidates = vec![];

        {
            let global_search_locations = self.modules[closest_module]
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

        match &candidates[..] {
            [] => Ok(None),
            [single] => Ok(Some(*single)),
            candidates => {
                handler.receive(error::Error::ResolutionAmbiguity(ResolutionAmbiguity {
                    identifier_span: identifier.span(),
                    candidate_qualified_names: candidates
                        .iter()
                        .map(|&global_ref| self.get_qualified_name(global_ref).unwrap())
                        .collect(),
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
        module_index: Index,
        handler: &impl Handler<error::Error>,
    ) -> Result<Index, super::Error> {
        let mut current_root = Root {
            referring_site: GlobalItemRef::Module(module_index),
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
                            qualified_name: self.get_qualified_name(global_ref).unwrap(),
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
    ) -> Option<Index> {
        let mut current_module_index: Option<Index> = None;

        for path in module_path.paths() {
            let next = match current_module_index {
                // continue searching from current module
                Some(current_module_index) => 'a: {
                    let Some(id) = ({
                        // search from current module id
                        self.modules[current_module_index]
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
                    searched_module_name: current_module_index.map(|x| {
                        self.get_qualified_name(GlobalItemRef::Module(x))
                            .expect("should've been a valid index")
                    }),
                }));
                return None;
            };

            current_module_index = Some(next);
        }

        current_module_index
    }
}
