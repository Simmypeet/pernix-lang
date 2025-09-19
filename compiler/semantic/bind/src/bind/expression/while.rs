use std::num::NonZeroUsize;

use pernixc_handler::Handler;
use pernixc_ir::instruction::{
    ConditionalJump, Instruction, Jump, ScopePop, Terminator, UnconditionalJump,
};
use pernixc_source_file::SourceElement;
use pernixc_term::r#type::Type;

use crate::{
    bind::{Bind, Expression, Guidance},
    binder::{r#loop::LoopKind, Binder, BindingError, Error},
    diagnostic::Diagnostic,
};

impl Bind<&pernixc_syntax::expression::block::While> for Binder<'_> {
    #[allow(clippy::too_many_lines)]
    async fn bind(
        &mut self,
        syntax_tree: &pernixc_syntax::expression::block::While,
        guidance: &Guidance<'_>,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<Expression, Error> {
        let (Some(binary), Some(while_kw)) =
            (syntax_tree.binary(), syntax_tree.while_keyword())
        else {
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

        condition_fail_block:
            scope pop $while_scope_id
            jump exit_block

        exit: <- break would jump here
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
            syntax_tree.span(),
            while_kw.span(),
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

        let loop_state = self.pop_loop_state(while_scope_id);
        let unit = Type::unit();

        Ok(Expression::RValue(
            self.bind_value_or_error(
                loop_state,
                match guidance {
                    Guidance::Expression(_) => None,
                    Guidance::Statement => Some(&unit),
                },
                handler,
            )
            .await?,
        ))
    }
}
