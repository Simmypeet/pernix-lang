use pernixc_base::{handler::Handler, source_file::SourceElement};
use pernixc_syntax::syntax_tree;

use super::{Bind, Config, Expression};
use crate::{
    error::{
        self, FloatingPointLiteralHasIntegralSuffix, InvalidNumericSuffix,
    },
    ir::{
        self,
        representation::{
            binding::{infer, Binder, Error, SemanticError},
            borrow,
        },
        value::{
            literal::{Literal, Numeric},
            Value,
        },
    },
    symbol::table::{self, resolution},
    type_system::{
        self,
        term::r#type::{self, Type},
    },
};

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>
            + type_system::observer::Observer<borrow::Model, S>,
    > Bind<&syntax_tree::expression::Numeric> for Binder<'t, S, RO, TO>
{
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::Numeric,
        _: Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        let numeric_ty = if let Some(suffix) = syntax_tree.suffix() {
            // the literal type is specified, no need to infer
            let primitive_type = match suffix.span.str() {
                "i8" => r#type::Primitive::Int8,
                "i16" => r#type::Primitive::Int16,
                "i32" => r#type::Primitive::Int32,
                "i64" => r#type::Primitive::Int64,
                "u8" => r#type::Primitive::Uint8,
                "u16" => r#type::Primitive::Uint16,
                "u32" => r#type::Primitive::Uint32,
                "u64" => r#type::Primitive::Uint64,
                "f32" => r#type::Primitive::Float32,
                "f64" => r#type::Primitive::Float64,
                "us" => r#type::Primitive::Usize,
                "is" => r#type::Primitive::Isize,
                _ => {
                    handler.receive(Box::new(
                        InvalidNumericSuffix { suffix_span: suffix.span() },
                    ));

                    return Err(Error::Semantic(SemanticError(
                        syntax_tree.span(),
                    )));
                }
            };

            let primitive_type_is_integral = matches!(
                primitive_type,
                r#type::Primitive::Int8
                    | r#type::Primitive::Int16
                    | r#type::Primitive::Int32
                    | r#type::Primitive::Int64
                    | r#type::Primitive::Uint8
                    | r#type::Primitive::Uint16
                    | r#type::Primitive::Uint32
                    | r#type::Primitive::Uint64
                    | r#type::Primitive::Usize
                    | r#type::Primitive::Isize
            );

            // check if the type is integer but the numeric literal has
            // decimal point

            if syntax_tree.decimal().is_some() && primitive_type_is_integral {
                handler.receive(Box::new(
                    FloatingPointLiteralHasIntegralSuffix {
                        numeric_literal_span: syntax_tree.span(),
                    },
                ));
                return Err(Error::Semantic(SemanticError(syntax_tree.span())));
            }

            Type::Primitive(primitive_type)
        } else {
            // infer the type
            let constraint = if syntax_tree.decimal().is_some() {
                r#type::Constraint::Floating
            } else {
                r#type::Constraint::Number
            };

            Type::Inference(self.create_type_inference(constraint))
        };

        Ok(Expression::RValue(Value::Literal(Literal::Numeric(Numeric {
            integer_string: syntax_tree.numeric().span.str().to_string(),
            decimal_stirng: syntax_tree
                .decimal()
                .as_ref()
                .map(|x| x.numeric().span.str().to_owned()),
            r#type: numeric_ty,
            span: syntax_tree.span(),
        }))))
    }
}
