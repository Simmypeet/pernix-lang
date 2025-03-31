use pernixc_handler::Handler;
use pernixc_resolution::qualified_identifier::Resolution;
use pernixc_semantic::{
    component::{
        derived::{generic_parameters::GenericParameters, variant},
        input::{Parent, SymbolKind},
    },
    diagnostic::Diagnostic,
    table::GlobalID,
    term::{
        instantiation::{self, Instantiation},
        r#type::Qualifier,
        Model,
    },
};
use pernixc_source_file::SourceElement;
use pernixc_syntax::syntax_tree;

use super::{Bind, Config, Expression};
use crate::{
    binding::{
        diagnostic::{
            ExpectedAssociatedValue, SymbolCannotBeUsedAsAnExpression,
        },
        expression::{LValue, Target},
        infer, Binder, BindingError, Error,
    },
    value::{
        literal::{self, Literal},
        register::{Assignment, Load, Variant},
        Value,
    },
};

impl Bind<&syntax_tree::QualifiedIdentifier> for Binder<'_> {
    #[allow(clippy::too_many_lines)]
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::QualifiedIdentifier,
        config: Config,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<Expression, Error> {
        let is_simple_identifier = syntax_tree.rest.is_empty()
            && syntax_tree
                .root
                .as_generic_identifier()
                .is_some_and(|x| x.generic_arguments.is_none());

        // search for the variable/parameter in the stack
        #[allow(clippy::unnecessary_operation)]
        'out: {
            if is_simple_identifier {
                let Some(name) = self.stack.search(
                    syntax_tree
                        .root
                        .as_generic_identifier()
                        .unwrap()
                        .identifier
                        .span
                        .str(),
                ) else {
                    break 'out;
                };

                let address = name.load_address.clone();

                match config.target {
                    Target::RValue => {
                        let register_id = self.create_register_assignmnet(
                            Assignment::Load(Load { address }),
                            syntax_tree.span(),
                        );

                        return Ok(Expression::RValue(Value::Register(
                            register_id,
                        )));
                    }
                    Target::Statement | Target::LValue => {
                        let name_qualifier = if name.mutable {
                            Qualifier::Mutable
                        } else {
                            Qualifier::Immutable
                        };

                        let final_qualifier = self
                            .get_behind_reference_qualifier(
                                &name.load_address,
                                handler,
                            )?
                            .map_or(name_qualifier, |x| x.min(name_qualifier));

                        return Ok(Expression::LValue(LValue {
                            address: name.load_address.clone(),
                            span: syntax_tree.span(),
                            qualifier: final_qualifier,
                        }));
                    }
                }
            }
        };

        let Ok(resolution) = self
            .resolve_qualified_identifier_with_inference(syntax_tree, handler)
        else {
            return Err(Error::Binding(BindingError(syntax_tree.span())));
        };

        let id = match resolution {
            Resolution::Variant(variant_res) => {
                let variant = self
                    .table
                    .query::<variant::Variant>(variant_res.variant_id)?;
                let parent_enum_id = GlobalID::new(
                    variant_res.variant_id.target_id,
                    self.table.get::<Parent>(variant_res.variant_id).unwrap(),
                );

                let enum_generic_parameters =
                    self.table.query::<GenericParameters>(parent_enum_id)?;

                // expected a variant type
                if variant.associated_type.is_some() {
                    handler.receive(Box::new(ExpectedAssociatedValue {
                        span: syntax_tree.span(),
                        variant_id: variant_res.variant_id,
                    }));
                }

                let associated_value = if let Some(associated_type) =
                    variant.associated_type.clone()
                {
                    let mut associated_type =
                        infer::Model::from_default_type(associated_type);

                    let instantiation = Instantiation::from_generic_arguments(
                        variant_res.generic_arguments.clone(),
                        parent_enum_id,
                        &enum_generic_parameters,
                    )
                    .unwrap();

                    instantiation::instantiate(
                        &mut associated_type,
                        &instantiation,
                    );

                    let associated_value =
                        Value::Literal(Literal::Error(literal::Error {
                            r#type: associated_type,
                            span: Some(syntax_tree.span()),
                        }));

                    Some(associated_value)
                } else {
                    None
                };

                let register_id = self.create_register_assignmnet(
                    Assignment::Variant(Variant {
                        variant_id: variant_res.variant_id,
                        associated_value,
                        generic_arguments: variant_res.generic_arguments,
                    }),
                    syntax_tree.span(),
                );

                return Ok(Expression::RValue(Value::Register(register_id)));
            }

            Resolution::Generic(generic) => {
                let symbol_kind = *self.table.get::<SymbolKind>(generic.id);

                if symbol_kind == SymbolKind::Constant {
                    todo!("handle constant evaluation");
                }

                generic.id
            }

            Resolution::MemberGeneric(member_generic) => {
                let symbol_kind =
                    *self.table.get::<SymbolKind>(member_generic.id);

                if symbol_kind == SymbolKind::TraitImplementationConstant {
                    todo!("handle constant evaluation");
                }

                member_generic.id
            }

            resolution => resolution.global_id(),
        };

        handler.receive(Box::new(SymbolCannotBeUsedAsAnExpression {
            span: syntax_tree.span(),
            symbol: id,
        }));

        Err(Error::Binding(BindingError(syntax_tree.span())))
    }
}

#[cfg(test)]
mod test;
