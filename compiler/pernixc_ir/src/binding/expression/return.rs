use pernixc_component::function_signature::FunctionSignature;
use pernixc_handler::Handler;
use pernixc_source_file::SourceElement;
use pernixc_syntax::syntax_tree;
use pernixc_table::{component::SymbolKind, diagnostic::Diagnostic};
use pernixc_term::{r#type::Type, Model};

use super::{Bind, Config, Expression};
use crate::{
    binding::{
        diagnostic::ReturnIsNotAllowed,
        infer::{self, Expected, InferenceVariable},
        stack::Scope,
        Binder, BindingError, Error,
    },
    instruction::{self, Instruction, ScopePop, Terminator},
    model::Constraint,
    value::{
        literal::{self, Literal, Unit},
        Value,
    },
};

impl Bind<&syntax_tree::expression::Return> for Binder<'_> {
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::Return,
        _: Config,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<Expression, Error> {
        let current_site_kind = self.table.get::<SymbolKind>(self.current_site);
        if !current_site_kind.has_function_signature() {
            handler.receive(Box::new(ReturnIsNotAllowed {
                span: syntax_tree.span(),
            }));

            return Err(Error::Binding(BindingError(syntax_tree.span())));
        }
        let function_signature =
            self.table.query::<FunctionSignature>(self.current_site)?;

        let return_type = infer::Model::from_default_type(
            function_signature.return_type.clone(),
        );

        let value = syntax_tree.binary().as_ref().map_or_else(
            || {
                Ok(Value::Literal(Literal::Unit(Unit {
                    span: Some(syntax_tree.span()),
                })))
            },
            |syn| self.bind_value_or_error(syn, handler),
        )?;
        let value_ty = self.type_of_value(&value, handler)?;

        // do type check
        self.type_check(
            &value_ty,
            Expected::Known(return_type),
            syntax_tree.span(),
            handler,
        )?;

        // pop all the needed scopes
        for popping_scope in
            self.stack.scopes().iter().map(Scope::scope_id).rev()
        {
            let _ = self
                .intermediate_representation
                .control_flow_graph
                .get_block_mut(self.current_block_id)
                .unwrap()
                .add_instruction(Instruction::ScopePop(ScopePop(
                    popping_scope,
                )));
        }

        // insert the return instruction
        self.intermediate_representation.control_flow_graph.insert_terminator(
            self.current_block_id,
            Terminator::Return(instruction::Return {
                value,
                span: Some(syntax_tree.span()),
            }),
        );

        let value =
            Value::Literal(Literal::Unreachable(literal::Unreachable {
                r#type: {
                    let inference = InferenceVariable::new();

                    assert!(self
                        .inference_context
                        .register(inference, Constraint::All(true)));

                    Type::Inference(inference)
                },
                span: Some(syntax_tree.span()),
            }));

        Ok(Expression::RValue(value))
    }
}
