use std::num::NonZeroUsize;

use pernixc_handler::Handler;
use pernixc_semantic::diagnostic::Diagnostic;
use pernixc_source_file::SourceElement;
use pernixc_syntax::syntax_tree;
use pernixc_term::r#type::{Primitive, Type};

use super::{Bind, Config, Expression};
use crate::{
    binding::{
        infer::{Expected, InferenceVariable},
        Binder, Error, LoopKind, LoopState,
    },
    instruction::{
        ConditionalJump, Instruction, Jump, ScopePop, ScopePush, Terminator,
        UnconditionalJump,
    },
    model::Constraint,
    value::{
        literal::{self, Literal, Unreachable},
        Value,
    },
};

impl Bind<&syntax_tree::expression::block::While> for Binder<'_> {
    #[allow(clippy::too_many_lines)]
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::block::While,
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
        let loop_body_block_id =
            self.intermediate_representation.control_flow_graph.new_block();
        let condition_fail_block_id =
            self.intermediate_representation.control_flow_graph.new_block();
        let exit_block_id =
            self.intermediate_representation.control_flow_graph.new_block();

        let while_scope_id = {
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

        /*
        current:
            ...

        loop_block:
            scope push $while_scope_id
            branch condition, loop_body_block, condition_fail_block

        loop_body_block:
            ...
            scope pop $while_scope_id
            jump loop_block

        condition_fail_block:
            scope pop $while_scope_id
            jump exit_block

        exit:
            ...
        */

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
            .add_instruction(Instruction::ScopePush(ScopePush(while_scope_id)));
        self.stack.push_scope(
            while_scope_id,
            syntax_tree.group.unsafe_keyword.is_some(),
        );
        self.loop_states_by_scope_id.insert(while_scope_id, LoopState {
            label,
            kind: LoopKind::While,
            loop_block_id,
            exit_block_id,
        });

        // bind the conditional value
        let condition =
            self.bind_value_or_error(&*syntax_tree.binary, handler)?;
        self.type_check(
            &self.type_of_value(&condition, handler)?,
            Expected::Known(Type::Primitive(Primitive::Bool)),
            syntax_tree.binary.span(),
            handler,
        )?;

        // based on the condition, jump to the loop body block or the condition
        // fail block
        self.intermediate_representation.control_flow_graph.insert_terminator(
            loop_block_id,
            Terminator::Jump(Jump::Conditional(ConditionalJump {
                condition,
                true_target: loop_body_block_id,
                false_target: condition_fail_block_id,
            })),
        );

        // handle condition fail block
        self.current_block_id = condition_fail_block_id;

        // pop the loop scope
        let _ = self
            .current_block_mut()
            .add_instruction(Instruction::ScopePop(ScopePop(while_scope_id)));
        // jump to the exit block
        self.intermediate_representation.control_flow_graph.insert_terminator(
            condition_fail_block_id,
            Terminator::Jump(Jump::Unconditional(UnconditionalJump {
                target: exit_block_id,
            })),
        );

        // handle loop body block
        self.current_block_id = loop_body_block_id;

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
        let _ = self
            .current_block_mut()
            .add_instruction(Instruction::ScopePop(ScopePop(while_scope_id)));

        // jump to the loop header block
        self.intermediate_representation.control_flow_graph.insert_terminator(
            self.current_block_id,
            Terminator::Jump(Jump::Unconditional(UnconditionalJump {
                target: loop_block_id,
            })),
        );

        // pop the loop scope
        assert_eq!(
            self.stack.pop_scope().map(|x| x.scope_id()),
            Some(while_scope_id)
        );

        // set the current block to the exit block
        self.current_block_id = exit_block_id;

        let value = if self.current_block().predecessors().is_empty() {
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
        } else {
            Value::Literal(Literal::Unit(literal::Unit {
                span: Some(syntax_tree.span()),
            }))
        };

        Ok(Expression::RValue(value))
    }
}
