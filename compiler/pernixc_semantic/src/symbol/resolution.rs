use enum_as_inner::EnumAsInner;
use pernixc_lexical::token::Identifier;
use pernixc_source::SourceElement;
use pernixc_syntax::syntax_tree::QualifiedIdentifier;
use pernixc_system::{arena, diagnostic::Handler};

use super::{GlobalRef, LocalSubstitution, Module, OperationError, Table};
use crate::{
    constant,
    error::{
        Error, ModuleExpected, NoGenericArgumentsRequired, ResolutionAmbiguity, SymbolNotFound,
        TraitExpected,
    },
    symbol, ty,
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ImplementsFunction {
    pub implements_function_id: arena::ID<symbol::ImplementsFunction>,
    pub implements_deduced_substitution: LocalSubstitution,
    pub function_substitution: LocalSubstitution,
}

pub type Struct = Generic<symbol::Struct>;
pub type Function = Generic<symbol::Function>;
pub type Enum = Generic<symbol::Enum>;
pub type TraitConstant = TraitMember<arena::ID<symbol::TraitConstant>>;
pub type TraitFunction = TraitMember<Generic<arena::ID<symbol::TraitFunction>>>;
pub type TraitType = TraitMember<Generic<arena::ID<symbol::TraitType>>>;

#[derive(Debug, Clone, EnumAsInner)]
pub enum Resolution {
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

    fn resolve_root_relative_first_pass(
        &self,
        identifier: &Identifier,
        referring_site: GlobalRef,
        handler: &impl Handler<Error>,
    ) -> Result<Option<GlobalRef>, OperationError> {
        let closest_module = self
            .get_closet_module_id(referring_site)
            .ok_or(OperationError::InvalidReference)?;

        let global_search_locations = self.modules[closest_module]
            .usings
            .keys()
            .copied()
            .map(GlobalRef::Module)
            .chain(std::iter::once(referring_site));

        let mut candidates = Vec::new();
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
        todo!()
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
