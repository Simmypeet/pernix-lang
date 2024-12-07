use pernixc_base::{handler::Handler, source_file::SourceElement};
use pernixc_syntax::syntax_tree;

use super::{Bind, Config, Expression};
use crate::{
    error,
    ir::{
        self,
        representation::{
            binding::{
                infer::{self, InferenceVariable},
                Binder, Error,
            },
            borrow,
        },
        value::{
            register::{Array, Assignment},
            Value,
        },
    },
    symbol::table::{self, resolution},
    type_system::{
        self,
        term::r#type::{Constraint, Expected, Type},
    },
};

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>
            + type_system::observer::Observer<borrow::Model, S>,
    > Bind<&syntax_tree::expression::Array> for Binder<'t, S, RO, TO>
{
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::Array,
        _: Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        let Some(arguments) = syntax_tree.arguments().connected_list() else {
            let inference = InferenceVariable::new();

            assert!(self
                .inference_context
                .register::<Type<_>>(inference, Constraint::All(false)));

            let register_id = self.create_register_assignmnet(
                Assignment::Array(Array {
                    elements: Vec::new(),
                    element_type: Type::Inference(inference),
                }),
                syntax_tree.span(),
            );

            return Ok(Expression::RValue(Value::Register(register_id)));
        };

        let mut elements = Vec::new();

        for element_syn in arguments.elements().map(|x| &**x) {
            elements.push(self.bind_value_or_error(element_syn, handler)?);
        }

        // do type check against the first elemen
        let mut iter = elements.iter();

        let first_element = iter.next().unwrap();
        let first_ty = self.type_of_value(first_element)?;

        for element in iter {
            let element_ty = self.type_of_value(element)?;

            let _ = self.type_check(
                &element_ty,
                Expected::Known(first_ty.clone()),
                match element {
                    Value::Register(register_id) => self
                        .intermediate_representation
                        .values
                        .registers
                        .get(*register_id)
                        .unwrap()
                        .span
                        .clone(),
                    Value::Literal(literal) => literal.span().clone(),
                },
                true,
                handler,
            )?;
        }

        let value = Value::Register(self.create_register_assignmnet(
            Assignment::Array(Array { elements, element_type: first_ty }),
            syntax_tree.span(),
        ));

        Ok(Expression::RValue(value))
    }
}
