use pernixc_lexical::token::Identifier;
use pernixc_source::SourceElement;
use pernixc_syntax::syntax_tree::{self, QualifiedIdentifier, TypeSpecifier};
use pernixc_system::{arena, diagnostic::Handler};

use super::{drafting::States, Table};
use crate::{
    error::{self, LifetimeArgumentRequired},
    table,
    ty::{self, PrimitiveType, ReferenceType},
    GenericParameters, GlobalID, Struct, Substitution, TypeParameter, ID,
};

impl Table {
    pub(super) fn finalize_symbol(
        &mut self,
        id: ID,
        states: &mut States,
        handler: &impl Handler<error::Error>,
    ) {
        match id {
            ID::Module(..) | ID::Enum(..) | ID::EnumVariant(_) => {
                // currently, there is nothing to finalize for this symbol
            }
            ID::Struct(struct_id) => {
                self.finalize_struct(struct_id, states, handler);
            }
            ID::Function(_) => todo!(),
            ID::Type(_) => todo!(),
            ID::Field(_) => todo!(),
            ID::FunctionParameter(_) => todo!(),
            ID::TraitFunctionParameter(_) => todo!(),
            ID::ImplementsFunctionParameter(_) => todo!(),
            ID::Trait(_) => todo!(),
            ID::TraitType(_) => todo!(),
            ID::TypeParameter(_) => todo!(),
            ID::LifetimeParameter(_) => todo!(),
            ID::TraitFunction(_) => todo!(),
            ID::Implements(_) => todo!(),
            ID::ImplementsFunction(_) => todo!(),
            ID::ImplementsType(_) => todo!(),
        }
    }

    pub(super) fn resolve_type_parameter(
        &mut self,
        referring_site: ID,
        extra_generic_parameters: Option<&GenericParameters>,
        identifier: &Identifier,
    ) -> Option<arena::ID<TypeParameter>> {
        todo!()
    }

    pub(super) fn resolve_symbol_with_finalization(
        &mut self,
        referring_site: ID,
        extra_generic_parameters: Option<&GenericParameters>,
        qualified_identifier: &QualifiedIdentifier,
        explicit_lifetime_required: bool,
        states: &mut States,
        handler: &impl Handler<error::Error>,
    ) -> Result<(GlobalID, Substitution), table::Error> {
        todo!()
    }

    pub(super) fn resolve_type_with_finalization(
        &mut self,
        referring_site: ID,
        extra_generic_parameters: Option<&GenericParameters>,
        type_specifier: &TypeSpecifier,
        explicit_lifetime_required: bool,
        states: &mut States,
        handler: &impl Handler<error::Error>,
    ) -> Result<ty::Type, table::Error> {
        match type_specifier {
            TypeSpecifier::Primitive(primitive_type) => {
                Ok(ty::Type::Primitive(match primitive_type {
                    syntax_tree::PrimitiveTypeSpecifier::Bool(_) => PrimitiveType::Bool,
                    syntax_tree::PrimitiveTypeSpecifier::Void(_) => PrimitiveType::Void,
                    syntax_tree::PrimitiveTypeSpecifier::Float32(_) => PrimitiveType::Float32,
                    syntax_tree::PrimitiveTypeSpecifier::Float64(_) => PrimitiveType::Float64,
                    syntax_tree::PrimitiveTypeSpecifier::Int8(_) => PrimitiveType::Int8,
                    syntax_tree::PrimitiveTypeSpecifier::Int16(_) => PrimitiveType::Int16,
                    syntax_tree::PrimitiveTypeSpecifier::Int32(_) => PrimitiveType::Int32,
                    syntax_tree::PrimitiveTypeSpecifier::Int64(_) => PrimitiveType::Int64,
                    syntax_tree::PrimitiveTypeSpecifier::Uint8(_) => PrimitiveType::Uint8,
                    syntax_tree::PrimitiveTypeSpecifier::Uint16(_) => PrimitiveType::Uint16,
                    syntax_tree::PrimitiveTypeSpecifier::Uint32(_) => PrimitiveType::Uint32,
                    syntax_tree::PrimitiveTypeSpecifier::Uint64(_) => PrimitiveType::Uint64,
                }))
            }
            TypeSpecifier::QualifiedIdentifier(qualified_identifier) => {
                // If the qualified identifier is a simple identifier, then try to resolve it as a
                // type parameter.
                if qualified_identifier.leading_scope_separator.is_none()
                    && qualified_identifier.rest.is_empty()
                    && qualified_identifier.first.generic_arguments.is_none()
                {
                    if let Some(ty) = self.resolve_type_parameter(
                        referring_site,
                        extra_generic_parameters,
                        &qualified_identifier.first.identifier,
                    ) {
                        return Ok(ty::Type::Parameter(ty));
                    }
                }

                let (global_id, substitution) = self.resolve_symbol_with_finalization(
                    referring_site,
                    extra_generic_parameters,
                    qualified_identifier,
                    explicit_lifetime_required,
                    states,
                    handler,
                )?;

                Ok(match global_id {
                    GlobalID::Module(..)
                    | GlobalID::EnumVariant(..)
                    | GlobalID::Trait(..)
                    | GlobalID::TraitFunction(..)
                    | GlobalID::Function(..) => todo!(),

                    GlobalID::Type(_) => {
                        unreachable!("should've been aliased to an another type already")
                    }

                    GlobalID::Struct(struct_id) => ty::Type::Struct(ty::Struct {
                        struct_id,
                        substitution,
                    }),
                    GlobalID::Enum(enum_id) => {
                        assert!(
                            substitution.is_empty(),
                            "substitution for enum is not empty"
                        );

                        ty::Type::Enum(enum_id)
                    }
                    GlobalID::TraitType(trait_type_id) => ty::Type::TraitType(ty::TraitType {
                        trait_type_id,
                        substitution,
                    }),
                })
            }
            TypeSpecifier::Reference(reference_type) => Ok(ty::Type::Reference(ReferenceType {
                operand: Box::new(self.resolve_type_with_finalization(
                    referring_site,
                    extra_generic_parameters,
                    &reference_type.operand_type,
                    explicit_lifetime_required,
                    states,
                    handler,
                )?),
                qualifier: reference_type.qualifier.as_ref().map(|x| match x {
                    syntax_tree::ReferenceQualifier::Mutable(_) => ty::ReferenceQualifier::Mutable,
                    syntax_tree::ReferenceQualifier::Restrict(_) => {
                        ty::ReferenceQualifier::Restrict
                    }
                }),
                lifetime_argument: match &reference_type.lifetime_argument {
                    Some(x) => Some(self.resolve_lifetime_argument(referring_site, x, handler)?),
                    None => {
                        if explicit_lifetime_required {
                            handler.receive(error::Error::LifetimeArgumentRequired(
                                LifetimeArgumentRequired {
                                    span: reference_type.span()?,
                                },
                            ));
                            return Err(table::Error::FatalSemantic);
                        }

                        None
                    }
                },
            })),
        }
    }

    pub(super) fn finalize_struct(
        &mut self,
        struct_id: arena::ID<Struct>,
        states: &mut States,
        handler: &impl Handler<error::Error>,
    ) {
        if let Err(err) = states.mark_as_constructing(struct_id.into()) {
            handler.receive(error::Error::CyclicDependency(err));
            return;
        };

        let struct_symbol = &self.structs[struct_id];

        // span out the fields
        for field_id in struct_symbol.field_order.iter().copied() {
            states.add_drafted_symbol(field_id.into());
        }

        states.remove_constructing_symbol(struct_id.into());
    }
}
