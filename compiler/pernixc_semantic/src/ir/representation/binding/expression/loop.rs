use std::{collections::HashMap, num::NonZeroUsize};

use pernixc_base::{handler::Handler, source_file::SourceElement};
use pernixc_syntax::syntax_tree::{self};

use super::{Bind, Config, Expression};
use crate::{
    error::{self},
    ir::{
        self,
        control_flow_graph::InsertTerminatorError,
        instruction::{
            Instruction, Jump, ScopePop, ScopePush, Terminator,
            UnconditionalJump,
        },
        representation::binding::{
            infer::{self, InferenceVariable},
            Binder, Error, LoopKind, LoopState,
        },
        value::{
            literal::{Literal, Unreachable},
            register::{Assignment, Phi},
            Value,
        },
    },
    symbol::table::{
        self,
        resolution::{self},
    },
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
            + type_system::observer::Observer<ir::Model, S>,
    > Bind<&syntax_tree::expression::Loop> for Binder<'t, S, RO, TO>
{
    #[allow(clippy::too_many_lines)]
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::Loop,
        _: Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        let label = syntax_tree
            .block()
            .label_specifier()
            .as_ref()
            .map(|x| x.label().identifier().span.str().to_owned());

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
        if let Err(InsertTerminatorError::InvalidBlockID(_)) = self
            .intermediate_representation
            .control_flow_graph
            .insert_terminator(
                self.current_block_id,
                Terminator::Jump(Jump::Unconditional(UnconditionalJump {
                    target: loop_block_id,
                })),
            )
        {
            panic!("invalid block id");
        }

        // set the current block to the loop header block
        self.current_block_id = loop_block_id;
        let _ = self.current_block_mut().insert_instruction(
            Instruction::ScopePush(ScopePush(loop_scope_id)),
        );
        self.stack.push_scope(loop_scope_id);
        self.loop_states_by_scope_id.insert(loop_scope_id, LoopState {
            label,
            kind: LoopKind::Loop {
                incoming_values: HashMap::new(),
                break_type: None,
            },
            loop_block_id,
            exit_block_id,
            span: syntax_tree.span(),
        });

        // bind the loop block
        for statement in syntax_tree.block().statements().tree() {
            self.bind_statement(statement, handler)?;
        }

        // pop the loop scope
        assert_eq!(
            self.stack.pop_scope().map(|x| x.scope_id()),
            Some(loop_scope_id)
        );
        let _ = self
            .current_block_mut()
            .insert_instruction(Instruction::ScopePop(ScopePop(loop_scope_id)));
        let loop_state =
            self.loop_states_by_scope_id.remove(&loop_scope_id).unwrap();

        // jump to the loop header block
        if let Err(InsertTerminatorError::InvalidBlockID(_)) = self
            .intermediate_representation
            .control_flow_graph
            .insert_terminator(
                self.current_block_id,
                Terminator::Jump(Jump::Unconditional(UnconditionalJump {
                    target: loop_block_id,
                })),
            )
        {
            panic!("invalid block id");
        }

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
                    span: (syntax_tree.span()),
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
                span: (syntax_tree.span()),
            }))
        };

        Ok(Expression::RValue(value))
    }
}
