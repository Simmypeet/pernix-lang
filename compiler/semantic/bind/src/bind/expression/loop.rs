use std::num::NonZeroUsize;

use pernixc_handler::Handler;
use pernixc_ir::instruction::{Jump, Terminator, UnconditionalJump};
use pernixc_source_file::SourceElement;
use pernixc_term::r#type::Type;

use crate::{
    bind::{Bind, Expression, Guidance},
    binder::{Binder, BindingError, Error, r#loop::LoopKind},
    diagnostic::Diagnostic,
};

impl Bind<&pernixc_syntax::expression::block::Loop> for Binder<'_> {
    #[allow(clippy::too_many_lines)]
    async fn bind(
        &mut self,
        syntax_tree: &pernixc_syntax::expression::block::Loop,
        guidance: &Guidance<'_>,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<Expression, Error> {
        let Some(loop_kw) = syntax_tree.loop_keyword() else {
            return Err(Error::Binding(BindingError(syntax_tree.span())));
        };

        let label = syntax_tree
            .group()
            .and_then(|x| x.label())
            .and_then(|x| x.identifier());

        let loop_block_id = self.new_block();
        let exit_block_id = self.new_block();

        let loop_scope_id = {
            let result = self.new_child_branch(NonZeroUsize::new(1).unwrap());

            result[0]
        };

        // jump to the loop header block
        self.insert_terminator(Terminator::Jump(Jump::Unconditional(
            UnconditionalJump { target: loop_block_id },
        )));

        // set the current block to the loop header block
        self.set_current_block_id(loop_block_id);

        self.push_scope_with(
            loop_scope_id,
            syntax_tree.group().and_then(|x| x.unsafe_keyword()).is_some(),
        );
        self.push_loop_state(
            loop_scope_id,
            label.map(|x| x.kind.0),
            loop_block_id,
            exit_block_id,
            LoopKind::Loop,
            syntax_tree.span(),
            loop_kw.span(),
        );

        // bind the loop block
        if let Some(syntax_tree) =
            syntax_tree.group().and_then(|x| x.statements())
        {
            for statement in
                syntax_tree.statements().filter_map(|x| x.into_line().ok())
            {
                self.bind_statement(&statement, handler).await?;
            }
        }

        // pop the loop scope
        self.pop_scope(loop_scope_id);

        let loop_state = self.pop_loop_state(loop_scope_id);

        // jump to the loop header block
        self.insert_terminator(Terminator::Jump(Jump::Unconditional(
            UnconditionalJump { target: loop_block_id },
        )));

        self.set_current_block_id(loop_state.exit_block_id());

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
