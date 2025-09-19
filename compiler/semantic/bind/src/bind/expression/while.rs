use std::num::NonZeroUsize;

use pernixc_handler::Handler;
use pernixc_ir::{
    instruction::{
        ConditionalJump, Instruction, Jump, ScopePop, Terminator,
        UnconditionalJump,
    },
    value::{
        literal::{self, Literal, Unreachable},
        Value,
    },
};
use pernixc_source_file::SourceElement;
use pernixc_term::r#type::Type;

use crate::{
    bind::{Bind, Expression, Guidance},
    binder::{r#loop::LoopKind, Binder, BindingError, Error},
    diagnostic::Diagnostic,
    inference_context::constraint,
};

impl Bind<&pernixc_syntax::expression::block::While> for Binder<'_> {
    #[allow(clippy::too_many_lines)]
    async fn bind(
        &mut self,
        syntax_tree: &pernixc_syntax::expression::block::While,
        _: &Guidance<'_>,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<Expression, Error> {
        let Some(binary) = syntax_tree.binary() else {
            return Err(Error::Binding(BindingError(syntax_tree.span())));
        };

        let label = syntax_tree
            .group()
            .and_then(|x| x.label())
            .and_then(|x| x.identifier());

        let loop_block_id = self.new_block();
        let loop_body_block_id = self.new_block();
        let condition_fail_block_id = self.new_block();
        let exit_block_id = self.new_block();

        let while_scope_id = {
            let result = self.new_child_branch(NonZeroUsize::new(1).unwrap());

            result[0]
        };

        /*
        current:
            ...
            jump loop_block

        loop_block: <- continue would jump here
            scope push $while_scope_id
            branch condition, loop_body_block, condition_fail_block

        loop_body_block:
            body code ...
            scope pop $while_scope_id
            jump loop_block

        condition_fail_block: <- break would jump here
            scope pop $while_scope_id
            jump exit_block

        exit:
            ...
        */

        // jump to the loop header block
        self.insert_terminator(Terminator::Jump(Jump::Unconditional(
            UnconditionalJump { target: loop_block_id },
        )));

        // set the current block to the loop header block
        self.set_current_block_id(loop_block_id);
        self.push_scope_with(
            while_scope_id,
            syntax_tree.group().and_then(|x| x.unsafe_keyword()).is_some(),
        );
        self.push_loop_state(
            while_scope_id,
            label.map(|x| x.kind.0),
            loop_block_id,
            exit_block_id,
            LoopKind::While,
        );

        // bind the conditional value
        let condition = self
            .bind_value_or_error(&binary, Some(&Type::bool()), handler)
            .await?;

        // based on the condition, jump to the loop body block or the condition
        // fail block
        self.insert_terminator(Terminator::Jump(Jump::Conditional(
            ConditionalJump {
                condition,
                true_target: loop_body_block_id,
                false_target: condition_fail_block_id,
            },
        )));

        // handle condition fail block
        self.set_current_block_id(condition_fail_block_id);

        // pop the loop scope (don't pop the scope from the context stack yet)
        self.push_instruction(Instruction::ScopePop(ScopePop(while_scope_id)));

        // jump to the exit block
        self.insert_terminator(Terminator::Jump(Jump::Unconditional(
            UnconditionalJump { target: exit_block_id },
        )));

        // handle loop body block
        self.set_current_block_id(loop_body_block_id);

        // bind the loop block
        if let Some(statements) =
            syntax_tree.group().and_then(|x| x.statements())
        {
            for statement in
                statements.statements().filter_map(|x| x.into_line().ok())
            {
                self.bind_statement(&statement, handler).await?;
            }
        }

        // pop the loop scope
        self.push_instruction(Instruction::ScopePop(ScopePop(while_scope_id)));

        // jump to the loop header block
        self.insert_terminator(Terminator::Jump(Jump::Unconditional(
            UnconditionalJump { target: loop_block_id },
        )));

        // pop the loop scope
        self.pop_scope(while_scope_id);

        // set the current block to the exit block
        self.set_current_block_id(exit_block_id);

        self.pop_loop_state(while_scope_id);

        let value = if self.current_block().predecessors().is_empty() {
            Value::Literal(Literal::Unreachable(Unreachable {
                r#type: Type::Inference(
                    self.create_type_inference(constraint::Type::All(true)),
                ),
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
