use pernixc_handler::Handler;
use pernixc_ir::{
    instruction::{self, Instruction, ScopePop, Terminator},
    value::{
        literal::{self, Literal, Unit},
        Value,
    },
};
use pernixc_semantic_element::return_type::get_return_type;
use pernixc_source_file::SourceElement;
use pernixc_symbol::kind::get_kind;
use pernixc_term::r#type::Type;

use crate::{
    bind::{Bind, Expression, Guidance},
    binder::{stack::Scope, Binder, BindingError, Error},
    diagnostic::{Diagnostic, ReturnIsNotAllowed},
    inference_context::constraint,
};

impl Bind<&pernixc_syntax::expression::terminator::Return> for Binder<'_> {
    async fn bind(
        &mut self,
        syntax_tree: &pernixc_syntax::expression::terminator::Return,
        _: &Guidance<'_>,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<Expression, Error> {
        let current_site_kind =
            self.engine().get_kind(self.current_site()).await;

        if !current_site_kind.has_function_signature() {
            handler.receive(
                ReturnIsNotAllowed { return_span: syntax_tree.span() }.into(),
            );

            return Err(Error::Binding(BindingError(syntax_tree.span())));
        }
        let return_type =
            self.engine().get_return_type(self.current_site()).await?;

        let value = match syntax_tree.binary() {
            Some(syn) => {
                self.bind_value_or_error(&syn, Some(&return_type), handler)
                    .await?
            }
            None => Value::Literal(Literal::Unit(Unit {
                span: Some(syntax_tree.span()),
            })),
        };

        // pop all the needed scopes
        #[allow(clippy::needless_collect)]
        for popping_scope in self
            .stack()
            .scopes()
            .iter()
            .map(Scope::scope_id)
            .rev()
            .collect::<Vec<_>>()
        //it's mutable borrow issue
        {
            self.push_instruction(Instruction::ScopePop(ScopePop(
                popping_scope,
            )));
        }

        // insert the return instruction
        self.insert_terminator(Terminator::Return(instruction::Return {
            value,
            span: Some(syntax_tree.span()),
        }));

        let value =
            Value::Literal(Literal::Unreachable(literal::Unreachable {
                r#type: Type::Inference(
                    self.create_type_inference(constraint::Type::All(true)),
                ),
                span: Some(syntax_tree.span()),
            }));

        Ok(Expression::RValue(value))
    }
}
