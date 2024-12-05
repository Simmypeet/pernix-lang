use pernixc_base::{handler::Handler, source_file::SourceElement};
use pernixc_syntax::syntax_tree;

use super::{Bind, Config, Expression};
use crate::{
    error,
    ir::{
        self,
        representation::binding::{
            expression::{LValue, Target},
            infer, Binder, Error, SemanticError,
        },
        value::{
            literal::{self, Literal},
            register::{Assignment, Load, Variant},
            Value,
        },
    },
    symbol::table::{self, representation::Index, resolution},
    type_system::{
        self,
        instantiation::{self, Instantiation},
        model::Model,
        term::r#type::Qualifier,
    },
};

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>,
    > Bind<&syntax_tree::QualifiedIdentifier> for Binder<'t, S, RO, TO>
{
    #[allow(clippy::too_many_lines)]
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::QualifiedIdentifier,
        config: Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        let is_simple_identifier = syntax_tree.rest().is_empty()
            && syntax_tree
                .root()
                .as_generic_identifier()
                .map_or(false, |x| x.generic_arguments().is_none());

        // search for the variable/parameter in the stack
        #[allow(clippy::unnecessary_operation)]
        'out: {
            if is_simple_identifier {
                let Some(name) = self.stack.search(
                    syntax_tree
                        .root()
                        .as_generic_identifier()
                        .unwrap()
                        .identifier()
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
                            .get_behind_reference_qualifier(&name.load_address)
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

        let resolution = self
            .resolve_with_inference(syntax_tree, handler)
            .ok_or(Error::Semantic(SemanticError(syntax_tree.span())))?;

        match resolution {
            resolution::Resolution::Variant(variant_res) => {
                let variant = self.table.get(variant_res.variant).unwrap();

                // expected a variant type
                if variant.associated_type.is_some() {
                    self.create_handler_wrapper(handler).receive(Box::new(
                        error::ExpectAssociatedValue {
                            span: syntax_tree.span(),
                            variant_id: variant_res.variant,
                        },
                    ));
                }

                let associated_value = if let Some(associated_type) =
                    variant.associated_type.clone()
                {
                    let mut associated_type =
                        infer::Model::from_default_type(associated_type);

                    let instantiation = Instantiation::from_generic_arguments(
                        variant_res.generic_arguments.clone(),
                        variant.parent_enum_id().into(),
                        &self
                            .table
                            .get(variant.parent_enum_id())
                            .unwrap()
                            .generic_declaration
                            .parameters,
                    )
                    .unwrap();

                    instantiation::instantiate(
                        &mut associated_type,
                        &instantiation,
                    );

                    let associated_value =
                        Value::Literal(Literal::Error(literal::Error {
                            r#type: associated_type,
                            span: syntax_tree.span(),
                        }));

                    Some(associated_value)
                } else {
                    None
                };

                let register_id = self.create_register_assignmnet(
                    Assignment::Variant(Variant {
                        variant_id: variant_res.variant,
                        associated_value,
                        generic_arguments: variant_res.generic_arguments,
                    }),
                    syntax_tree.span(),
                );

                Ok(Expression::RValue(Value::Register(register_id)))
            }

            resolution::Resolution::Generic(resolution::Generic {
                id: resolution::GenericID::Constant(_id),
                generic_arguments: _,
            }) => todo!("handle constant evaluation"),

            resolution::Resolution::Generic(resolution::Generic {
                id: resolution::GenericID::TraitImplementationConstant(_id),
                generic_arguments: _,
            }) => todo!("handle constant evaluation"),

            resolution => {
                self.create_handler_wrapper(handler).receive(Box::new(
                    error::SymbolCannotBeUsedAsAnExpression {
                        span: syntax_tree.span(),
                        symbol: resolution.global_id(),
                    },
                ));

                Err(Error::Semantic(SemanticError(syntax_tree.span())))
            }
        }
    }
}
