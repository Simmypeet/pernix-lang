use pernixc_handler::{Handler, Storage};
use pernixc_ir::value::{
    literal::{Literal, Numeric},
    Value,
};
use pernixc_source_file::SourceElement;
use pernixc_term::r#type::{Primitive, Type};

use crate::{
    bind::{
        expression::numeric::diagnostic::{
            FloatingPointLiteralHasIntegralSuffix, InvalidNumericSuffix,
        },
        Bind, Config, Expression,
    },
    binder::{Binder, BindingError, Error},
    diagnostic::Diagnostic,
    inference_context::constraint,
};

pub mod diagnostic;

impl Bind<pernixc_syntax::expression::unit::Numeric> for Binder<'_> {
    async fn bind(
        &mut self,
        syntax_tree: pernixc_syntax::expression::unit::Numeric,
        _: Config,
        handler: &Storage<Diagnostic>,
    ) -> Result<Expression, Error> {
        let numeric_ty = if let Some(suffix) = syntax_tree.identifier() {
            // the literal type is specified, no need to infer
            let primitive_type = match suffix.kind.0.as_str() {
                "i8" => Primitive::Int8,
                "i16" => Primitive::Int16,
                "i32" => Primitive::Int32,
                "i64" => Primitive::Int64,
                "u8" => Primitive::Uint8,
                "u16" => Primitive::Uint16,
                "u32" => Primitive::Uint32,
                "u64" => Primitive::Uint64,
                "f32" => Primitive::Float32,
                "f64" => Primitive::Float64,
                "us" => Primitive::Usize,
                "is" => Primitive::Isize,
                _ => {
                    handler.receive(
                        diagnostic::Diagnostic::InvalidNumericSuffix(
                            InvalidNumericSuffix { suffix_span: suffix.span() },
                        ),
                    );

                    return Err(Error::Binding(BindingError(
                        syntax_tree.span(),
                    )));
                }
            };

            let primitive_type_is_integral = matches!(
                primitive_type,
                Primitive::Int8
                    | Primitive::Int16
                    | Primitive::Int32
                    | Primitive::Int64
                    | Primitive::Uint8
                    | Primitive::Uint16
                    | Primitive::Uint32
                    | Primitive::Uint64
                    | Primitive::Usize
                    | Primitive::Isize
            );

            // check if the type is integer but the numeric literal has
            // decimal point

            if syntax_tree.decimal().is_some() && primitive_type_is_integral {
                handler.receive(diagnostic::Diagnostic::FloatingPointLiteralHasIntegralSuffix(FloatingPointLiteralHasIntegralSuffix {
                                    numeric_literal_span: syntax_tree.span(),
                                }));
                return Err(Error::Binding(BindingError(syntax_tree.span())));
            }

            Type::Primitive(primitive_type)
        } else {
            // infer the type
            let constraint = if syntax_tree.decimal().is_some() {
                constraint::Type::Floating
            } else {
                constraint::Type::Number
            };

            Type::Inference(self.create_type_inference(constraint))
        };

        Ok(Expression::RValue(Value::Literal(Literal::Numeric(Numeric {
            integer_string: syntax_tree.numeric().unwrap().kind.0,
            decimal_string: syntax_tree
                .decimal()
                .and_then(|x| x.digits())
                .map(|x| x.kind.0),
            r#type: numeric_ty,
            span: Some(syntax_tree.span()),
        }))))
    }
}
