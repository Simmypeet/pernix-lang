use pernixc_base::{handler::Handler, source_file::SourceElement};
use pernixc_syntax::syntax_tree::{self, Label};

use super::{Bind, Config, Expression};
use crate::{
    error::{self, LoopControlFlow},
    ir::{
        self,
        control_flow_graph::InsertTerminatorError,
        instruction::{
            Instruction, Jump, ScopePop, Terminator, UnconditionalJump,
        },
        representation::{
            binding::{
                infer::{self, InferenceVariable},
                stack::Scope,
                Binder, Error,
            },
            borrow,
        },
        value::{
            literal::{Literal, Unreachable},
            Value,
        },
    },
    symbol::table::{self, resolution},
    type_system::{
        self,
        term::r#type::{Constraint, Type},
    },
};

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>
            + type_system::observer::Observer<borrow::Model, S>,
    > Bind<&syntax_tree::expression::Continue> for Binder<'t, S, RO, TO>
{
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::Continue,
        _: Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        let loop_scope_id = self.find_loop_scope_id(
            LoopControlFlow::Continue,
            syntax_tree.label().as_ref().map(Label::identifier),
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
                .insert_instruction(Instruction::ScopePop(ScopePop(
                    popping_scope,
                )));
        }

        // jump to the loop block
        if let Err(InsertTerminatorError::InvalidBlockID(_)) = self
            .intermediate_representation
            .control_flow_graph
            .insert_terminator(
                self.current_block_id,
                Terminator::Jump(Jump::Unconditional(UnconditionalJump {
                    target: self
                        .loop_states_by_scope_id
                        .get(&loop_scope_id)
                        .unwrap()
                        .loop_block_id,
                })),
            )
        {
            panic!("invalid block id");
        }

        let value = Value::Literal(Literal::Unreachable(Unreachable {
            r#type: {
                let inference = InferenceVariable::new();

                assert!(self
                    .inference_context
                    .register(inference, Constraint::All(true)));

                Type::Inference(inference)
            },
            span: (syntax_tree.span()),
        }));

        Ok(Expression::RValue(value))
    }
}
