use pernixc_handler::Handler;
use pernixc_semantic::diagnostic::Diagnostic;
use pernixc_source_file::SourceElement;
use pernixc_syntax::syntax_tree;
use pernixc_term::r#type::Type;

use super::{Bind, Config, Expression};
use crate::{
    binding::{
        diagnostic::LoopControlFlow, infer::InferenceVariable, stack::Scope,
        Binder, Error,
    },
    instruction::{Instruction, Jump, ScopePop, Terminator, UnconditionalJump},
    model::Constraint,
    value::{
        literal::{Literal, Unreachable},
        Value,
    },
};

impl Bind<&syntax_tree::expression::terminator::Continue> for Binder<'_> {
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::terminator::Continue,
        _: Config,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<Expression, Error> {
        let loop_scope_id = self.find_loop_scope_id(
            LoopControlFlow::Continue,
            syntax_tree.label.as_ref().map(|x| &x.identifier),
            syntax_tree.span(),
            handler,
        )?;

        // pop all the needed scopes
        for popping_scope in self
            .stack
            .scopes()
            .iter()
            .rev()
            .map(Scope::scope_id)
            .take_while(|x| *x != loop_scope_id)
            .chain(std::iter::once(loop_scope_id))
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

        // jump to the loop block
        self.intermediate_representation.control_flow_graph.insert_terminator(
            self.current_block_id,
            Terminator::Jump(Jump::Unconditional(UnconditionalJump {
                target: self
                    .loop_states_by_scope_id
                    .get(&loop_scope_id)
                    .unwrap()
                    .loop_block_id,
            })),
        );

        let value = Value::Literal(Literal::Unreachable(Unreachable {
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
