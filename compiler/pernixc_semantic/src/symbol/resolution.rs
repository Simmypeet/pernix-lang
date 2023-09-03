use std::{env::current_dir, panic::PanicInfo};

use enum_as_inner::EnumAsInner;
use pernixc_lexical::token::Identifier;
use pernixc_source::SourceElement;
use pernixc_syntax::syntax_tree::{GenericIdentifier, QualifiedIdentifier};
use pernixc_system::{arena, diagnostic::Handler};

use super::{DraftingSymbolRef, GlobalRef, LocalSubstitution, Module, OperationError, Table};
use crate::{
    constant,
    error::{
        Error, ModuleExpected, NoGenericArgumentsRequired, NoMemberOnExpression,
        ResolutionAmbiguity, SymbolNotAccessible, SymbolNotFound, TargetNotFound, TraitExpected,
    },
    symbol::{
        self, ImplementsFunctionRef, ImplementsRef, TraitFunctionRef, TraitTypeRef, VariantRef,
    },
    ty,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BuiltinTraitImplements {
    Copy,
    Drop,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Generic<Symbol> {
    pub symbol: arena::ID<Symbol>,
    pub substitution: LocalSubstitution,
}

#[derive(Debug, Clone)]
pub struct Variant {
    pub parent_enum: Enum,
    pub variant_id: arena::ID<symbol::Variant>,
}

#[derive(Debug, Clone)]
pub struct Implements {
    pub trait_id: arena::ID<symbol::Trait>,
    pub implements_id: arena::ID<symbol::Implements>,
    pub deduced_substitution: LocalSubstitution,
}

/// Represents a resolution to the trait symbol.
#[derive(Debug, Clone)]
pub struct Trait {
    pub trait_id: arena::ID<symbol::Trait>,
    pub substitution: LocalSubstitution,
    pub resolved_implements: Option<Implements>,
}

#[derive(Debug, Clone)]
pub struct TraitMember<Member> {
    pub trait_resolution: Trait,
    pub member_resolution: Member,
}

#[derive(Debug, Clone)]
pub struct ImplementsFunction {
    pub implements: Implements,
    pub implements_function_id: arena::ID<symbol::ImplementsFunction>,
    pub function_substitution: LocalSubstitution,
}

pub type Struct = Generic<symbol::Struct>;
pub type Function = Generic<symbol::Function>;
pub type Enum = Generic<symbol::Enum>;
pub type TraitConstant = TraitMember<symbol::TraitConstant>;
pub type TraitFunction = TraitMember<Generic<symbol::TraitFunction>>;
pub type TraitType = TraitMember<Generic<symbol::TraitType>>;

#[derive(Debug, Clone, EnumAsInner)]
pub enum Resolution {
    Pointer(ty::Pointer),
    Primitive(ty::Primitive),
    Reference(ty::Reference),
    Tuple(ty::Tuple),
    Module(arena::ID<Module>),
    Struct(Struct),
    Enum(Enum),
    Function(Function),
    Variant(Variant),
    Constant(constant::Constant),
    Trait(Trait),
    TraitFunction(TraitFunction),
    TraitType(TraitType),
    ImplementsFunction(ImplementsFunction),
}

#[derive(Debug, Clone)]
pub(super) struct Root {
    pub(super) referring_site: GlobalRef,
    pub(super) latest_resolution: Option<Resolution>,
}

/// Specifies which bound should be checked during the resolution process
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub enum BoundChecking {
    /// All the bounds are checked.
    #[default]
    Default,

    /// The lifetime bounds are ignored if elided.
    IgnoreLifetimeChecksIfElided,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Config {
    pub referring_site: GlobalRef,
    pub bound_checking: BoundChecking,
    pub explicit_lifetime_required: bool,
}

impl Table {
    fn resolve_root_relative_second_pass(
        &self,
        identifier: &Identifier,
        mut referring_site: GlobalRef,
        handler: &impl Handler<Error>,
    ) -> Result<GlobalRef, OperationError> {
        // NOTE: Accessibility is not checked here because the symbol searching is done within the
        // same module ancestor tree.

        loop {
            // try to find the symbol in the current scope
            if let Some(id) = self
                .get_global(referring_site)
                .ok_or(OperationError::InvalidReference)?
                .get_child_symbol(identifier.span.str())
            {
                return Ok(id);
            }

            if let Some(parent_id) = self.get_global(referring_site).unwrap().parent() {
                referring_site = parent_id;
            } else {
                // try to search the symbol in the root scope
                if let Some(id) = self
                    .target_root_module_ids_by_name
                    .read()
                    .get(identifier.span.str())
                    .copied()
                {
                    return Ok(GlobalRef::Module(id));
                }

                handler.receive(Error::SymbolNotFound(SymbolNotFound {
                    symbol_reference_span: identifier.span.clone(),
                }));
                return Err(OperationError::SemanticError);
            }
        }
    }

    #[allow(clippy::significant_drop_tightening)]
    fn resolve_root_relative_first_pass(
        &self,
        identifier: &Identifier,
        referring_site: GlobalRef,
        handler: &impl Handler<Error>,
    ) -> Result<Option<GlobalRef>, OperationError> {
        let closest_module = self
            .get_closet_module_id(referring_site)
            .ok_or(OperationError::InvalidReference)?;

        let mut candidates = Vec::new();

        {
            let contaienr = self.container.read();
            let global_search_locations = contaienr.modules[closest_module]
                .usings
                .keys()
                .copied()
                .map(GlobalRef::Module)
                .chain(std::iter::once(referring_site));

            for location in global_search_locations {
                if let Some(global_ref) = self
                    .get_global(location)
                    .ok_or(OperationError::InvalidReference)?
                    .get_child_symbol(identifier.span().str())
                {
                    if self
                        .symbol_accessible(global_ref, referring_site)
                        .ok_or(OperationError::InvalidReference)?
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
                handler.receive(Error::ResolutionAmbiguity(ResolutionAmbiguity {
                    identifier_span: identifier.span(),
                    candidate_qualified_names: candidates
                        .iter()
                        .map(|&global_ref| self.get_qualified_name(global_ref).unwrap())
                        .collect(),
                }));

                Err(OperationError::SemanticError)
            }
        }
    }

    /// Resolves the root for the first resolution if the qualified identifier is not prefixed with
    /// leading `::`.
    fn resolve_root_relative(
        &self,
        identifier: &Identifier,
        referring_site: GlobalRef,
        handler: &impl Handler<Error>,
    ) -> Result<GlobalRef, OperationError> {
        let found_symbol_id = if let Some(id) =
            self.resolve_root_relative_first_pass(identifier, referring_site, handler)?
        {
            id
        } else {
            self.resolve_root_relative_second_pass(identifier, referring_site, handler)?
        };

        Ok(found_symbol_id)
    }

    pub(super) fn resolve_single_first_pass(
        &self,
        identifier: &Identifier,
        root: &Root,
        search_from_root: bool,
        handler: &impl Handler<Error>,
    ) -> Result<GlobalRef, OperationError> {
        let global_ref = if let Some(latest_resolution) = &root.latest_resolution {
            let resolution_as_global_ref = match latest_resolution {
                Resolution::Primitive(_) => todo!(),
                Resolution::Reference(_) => todo!(),
                Resolution::Tuple(_) => todo!(),
                Resolution::Pointer(_) => todo!(),
                Resolution::Constant(_) => {
                    handler.receive(Error::NoMemberOnExpression(NoMemberOnExpression {
                        member_reference_span: identifier.span.clone(),
                    }));
                    return Err(OperationError::SemanticError);
                }
                Resolution::Module(id) => GlobalRef::Module(*id),
                Resolution::Struct(id) => GlobalRef::Struct(id.symbol),
                Resolution::Enum(id) => GlobalRef::Enum(id.symbol),
                Resolution::Function(id) => GlobalRef::Function(id.symbol),
                Resolution::Variant(id) => GlobalRef::Variant(VariantRef {
                    parent: id.parent_enum.symbol,
                    associated_item: id.variant_id,
                }),
                Resolution::Trait(id) => GlobalRef::Trait(id.trait_id),
                Resolution::TraitFunction(id) => GlobalRef::TraitFunction(TraitFunctionRef {
                    parent: id.trait_resolution.trait_id,
                    associated_item: id.member_resolution.symbol,
                }),
                Resolution::TraitType(id) => GlobalRef::TraitType(TraitTypeRef {
                    parent: id.trait_resolution.trait_id,
                    associated_item: id.member_resolution.symbol,
                }),
                Resolution::ImplementsFunction(id) => {
                    GlobalRef::ImplementsFunction(ImplementsFunctionRef {
                        reference: ImplementsRef {
                            trait_id: id.implements.trait_id,
                            implements_id: id.implements.implements_id,
                        },
                        associated_item: id.implements_function_id,
                    })
                }
            };

            let Some(child_global_ref) = self
                .get_global(resolution_as_global_ref)
                .ok_or(OperationError::InvalidReference)?
                .get_child_symbol(identifier.span.str())
            else {
                handler.receive(Error::SymbolNotFound(SymbolNotFound {
                    symbol_reference_span: identifier.span.clone(),
                }));
                return Err(OperationError::SemanticError);
            };

            child_global_ref
        } else if search_from_root {
            GlobalRef::Module(
                self.target_root_module_ids_by_name
                    .read()
                    .get(identifier.span.str())
                    .copied()
                    .ok_or_else(|| {
                        handler.receive(Error::TargetNotFound(TargetNotFound {
                            target_name_span: identifier.span.clone(),
                        }));
                        OperationError::SemanticError
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
            handler.receive(Error::SymbolNotAccessible(SymbolNotAccessible {
                reference_span: identifier.span.clone(),
                referring_site_qualified_name: self
                    .get_qualified_name(root.referring_site)
                    .ok_or(OperationError::SemanticError)?,
                referred_site_qualified_name: self.get_qualified_name(global_ref).unwrap(),
            }));
        }

        Ok(global_ref)
    }

    fn resolve_substitution(
        &self,
        generic_identifier: &GenericIdentifier,
        found_global_ref: GlobalRef,
        config: &Config,
        handler: &impl Handler<Error>,
    ) -> Result<LocalSubstitution, OperationError> {
        let mut local_substitution = LocalSubstitution::default();

        if matches!(
            found_global_ref,
            GlobalRef::Module(_)
                | GlobalRef::Variant(_)
                | GlobalRef::Constant(_)
                | GlobalRef::TraitConstant(_)
                | GlobalRef::ImplementsConstant(_)
        ) {
            if let Some(generic_arguments) = generic_identifier.generic_arguments() {
                handler.receive(Error::NoGenericArgumentsRequired(
                    NoGenericArgumentsRequired {
                        generic_arguments_span: generic_arguments.span(),
                        qualified_name: self
                            .get_qualified_name(found_global_ref)
                            .expect("should've been a valid ref already"),
                    },
                ));
                return Err(OperationError::SemanticError);
            }

            // empty substitution
            return Ok(local_substitution);
        }

        let mut lifetime_argument_syntax_trees = Vec::new();
        let mut type_argument_syntax_trees = Vec::new();
        let mut constant_argument_syntax_trees = Vec::new();

        todo!()
    }

    /// Resolves the given qualified identifier into a symbol.
    ///
    /// # Errors
    ///
    /// - [`OperationError::SemanticError`] if the resolution fails.
    /// - [`OperationError::InvalidReference`] if the referring site contained in the `config` is
    /// invalid.
    pub fn resolve(
        &self,
        config: &Config,
        qualified_identifier: &QualifiedIdentifier,
        handler: &impl Handler<Error>,
    ) -> Result<Resolution, OperationError> {
        let mut current_root = Root {
            referring_site: config.referring_site,
            latest_resolution: None,
        };

        for generic_identifier in std::iter::once(qualified_identifier.first())
            .chain(qualified_identifier.rest().iter().map(|x| &x.1))
        {
            let global_ref = self.resolve_single_first_pass(
                generic_identifier.identifier(),
                &current_root,
                qualified_identifier.leading_scope_separator().is_some(),
                handler,
            )?;

            let drafting_symbol_ref = match global_ref {
                GlobalRef::Module(_)
                | GlobalRef::Variant(_)
                | GlobalRef::TraitFunction(_)
                | GlobalRef::TraitType(_)
                | GlobalRef::Implements(_)
                | GlobalRef::ImplementsFunction(_)
                | GlobalRef::ImplementsConstant(_)
                | GlobalRef::ImplementsType(_)
                | GlobalRef::TraitConstant(_) => None,
                GlobalRef::Struct(struct_id) => Some(DraftingSymbolRef::Struct(struct_id)),
                GlobalRef::Enum(enum_id) => Some(DraftingSymbolRef::Enum(enum_id)),
                GlobalRef::Function(function_id) => Some(DraftingSymbolRef::Function(function_id)),
                GlobalRef::Type(type_id) => Some(DraftingSymbolRef::Type(type_id)),
                GlobalRef::Constant(constant_id) => Some(DraftingSymbolRef::Constant(constant_id)),
                GlobalRef::Trait(trait_id) => Some(DraftingSymbolRef::Trait(trait_id)),
            };

            // finalize the symbol first
            if let Some(drafting_symbol_ref) = drafting_symbol_ref {
                let _ = self.finalize(drafting_symbol_ref, handler);
            }

            // resolve the substitution
            let local_substitution =
                self.resolve_substitution(generic_identifier, global_ref, config, handler);
        }

        Ok(current_root.latest_resolution.unwrap())
    }

    #[allow(clippy::too_many_lines)]
    pub(super) fn resolve_trait_path(
        &self,
        qualified_identifier: &QualifiedIdentifier,
        module_id: arena::ID<Module>,
        handler: &impl Handler<Error>,
    ) -> Result<arena::ID<symbol::Trait>, OperationError> {
        let mut current_root = Root {
            referring_site: GlobalRef::Module(module_id),
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
                let GlobalRef::Module(module_id) = global_ref else {
                    handler.receive(Error::ModuleExpected(ModuleExpected {
                        module_path_span: generic_identifier.span(),
                    }));
                    return Err(OperationError::SemanticError);
                };

                if let Some(generic_arguments_syn) = generic_identifier.generic_arguments().as_ref()
                {
                    handler.receive(Error::NoGenericArgumentsRequired(
                        NoGenericArgumentsRequired {
                            generic_arguments_span: generic_arguments_syn.span(),
                            qualified_name: self.get_qualified_name(global_ref).unwrap(),
                        },
                    ));
                    return Err(OperationError::SemanticError);
                }

                current_root.latest_resolution = Some(Resolution::Module(module_id));
            } else if let GlobalRef::Trait(trait_id) = global_ref {
                return Ok(trait_id);
            } else {
                handler.receive(Error::TraitExpected(TraitExpected {
                    trait_path_span: generic_identifier.span(),
                }));
                return Err(OperationError::SemanticError);
            }
        }

        unreachable!()
    }
}
