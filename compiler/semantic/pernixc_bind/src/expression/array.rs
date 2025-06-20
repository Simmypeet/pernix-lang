use pernixc_handler::Handler;
use pernixc_semantic::{
    component::derived::ir::{
        model::Constraint,
        value::{
            register::{Array, Assignment},
            Value,
        },
    },
    diagnostic::Diagnostic,
    term::r#type::Type,
};
use pernixc_source_file::SourceElement;
use pernixc_syntax::syntax_tree;

use super::{Bind, Config, Expression};
use crate::{
    infer::{Expected, InferenceVariable},
    Binder, Error,
};

impl Bind<&syntax_tree::expression::unit::Array> for Binder<'_> {
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::unit::Array,
        _: Config,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<Expression, Error> {
        let Some(arguments) = &syntax_tree.arguments.connected_list else {
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
        let first_ty = self.type_of_value(first_element, handler)?;

        for element in iter {
            let element_ty = self.type_of_value(element, handler)?;

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
                        .clone()
                        .unwrap(),
                    Value::Literal(literal) => literal.span().cloned().unwrap(),
                },
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

#[cfg(test)]
mod test;
