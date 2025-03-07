use std::{collections::HashMap, num::NonZeroUsize};

use pernixc_handler::Handler;
use pernixc_source_file::SourceElement;
use pernixc_syntax::syntax_tree;
use pernixc_table::diagnostic::Diagnostic;
use pernixc_term::r#type::Type;

use super::{Bind, Config, Expression};
use crate::{
    binding::{infer::InferenceVariable, Binder, Error, LoopKind, LoopState},
    instruction::{
        Instruction, Jump, ScopePop, ScopePush, Terminator, UnconditionalJump,
    },
    model::Constraint,
    value::{
        literal::{Literal, Unreachable},
        register::{Assignment, Phi},
        Value,
    },
};

impl Bind<&syntax_tree::expression::block::Loop> for Binder<'_> {
    #[allow(clippy::too_many_lines)]
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::block::Loop,
        _: Config,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<Expression, Error> {
        let label = syntax_tree
            .group
            .label
            .as_ref()
            .map(|x| x.identifier.span.str().to_owned());

        let loop_block_id =
            self.intermediate_representation.control_flow_graph.new_block();
        let exit_block_id =
            self.intermediate_representation.control_flow_graph.new_block();

        let loop_scope_id = {
            let result = self
                .intermediate_representation
                .scope_tree
                .new_child_branch(
                    self.stack.current_scope().scope_id(),
                    NonZeroUsize::new(1).unwrap(),
                )
                .unwrap();

            result[0]
        };

        // jump to the loop header block
        self.intermediate_representation.control_flow_graph.insert_terminator(
            self.current_block_id,
            Terminator::Jump(Jump::Unconditional(UnconditionalJump {
                target: loop_block_id,
            })),
        );

        // set the current block to the loop header block
        self.current_block_id = loop_block_id;
        let _ = self
            .current_block_mut()
            .add_instruction(Instruction::ScopePush(ScopePush(loop_scope_id)));
        self.stack.push_scope(loop_scope_id);
        self.loop_states_by_scope_id.insert(loop_scope_id, LoopState {
            label,
            kind: LoopKind::Loop {
                incoming_values: HashMap::new(),
                break_type: None,
            },
            loop_block_id,
            exit_block_id,
        });

        // bind the loop block
        for statement in syntax_tree
            .group
            .statements
            .statements
            .iter()
            .filter_map(|x| x.as_option())
        {
            self.bind_statement(statement, handler)?;
        }

        // pop the loop scope
        assert_eq!(
            self.stack.pop_scope().map(|x| x.scope_id()),
            Some(loop_scope_id)
        );
        let _ = self
            .current_block_mut()
            .add_instruction(Instruction::ScopePop(ScopePop(loop_scope_id)));
        let loop_state =
            self.loop_states_by_scope_id.remove(&loop_scope_id).unwrap();

        // jump to the loop header block
        self.intermediate_representation.control_flow_graph.insert_terminator(
            self.current_block_id,
            Terminator::Jump(Jump::Unconditional(UnconditionalJump {
                target: loop_block_id,
            })),
        );

        // set the current block to the exit block
        self.current_block_id = loop_state.exit_block_id;
        let (mut incoming_values, break_type) =
            loop_state.kind.into_loop().unwrap();

        let value = if let Some(break_type) = break_type {
            assert!(self
                .current_block()
                .predecessors()
                .iter()
                .copied()
                .all(|block_id| incoming_values.contains_key(&block_id)));

            // filter out the incoming values that are unreachable
            #[allow(clippy::needless_collect)]
            for remove in incoming_values
                .keys()
                .copied()
                .filter(|x| !self.current_block().predecessors().contains(x))
                .collect::<Vec<_>>()
            {
                incoming_values.remove(&remove);
            }

            match incoming_values.len() {
                0 => Value::Literal(Literal::Unreachable(Unreachable {
                    r#type: break_type,
                    span: Some(syntax_tree.span()),
                })),

                // only one incoming value, just return it
                1 => incoming_values.into_iter().next().unwrap().1,

                // multiple incoming values, create a phi node
                _ => {
                    let phi_register_id = self.create_register_assignmnet(
                        Assignment::Phi(Phi {
                            r#type: break_type,
                            incoming_values,
                        }),
                        syntax_tree.span(),
                    );

                    Value::Register(phi_register_id)
                }
            }
        } else {
            assert!(incoming_values.is_empty());
            assert!(self.current_block().is_unreachable_or_terminated());

            Value::Literal(Literal::Unreachable(Unreachable {
                r#type: {
                    let inference = InferenceVariable::new();

                    assert!(self
                        .inference_context
                        .register(inference, Constraint::All(true)));

                    Type::Inference(inference)
                },
                span: Some(syntax_tree.span()),
            }))
        };

        Ok(Expression::RValue(value))
    }
}

#[cfg(test)]
mod test;
