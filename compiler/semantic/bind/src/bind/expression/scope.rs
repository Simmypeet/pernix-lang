use std::num::NonZeroUsize;

use pernixc_handler::Handler;
use pernixc_source_file::SourceElement;

use crate::{
    bind::{Bind, Expression, Guidance},
    binder::{Binder, Error},
    diagnostic::Diagnostic,
};

impl Bind<&pernixc_syntax::expression::block::Scope> for Binder<'_> {
    async fn bind(
        &mut self,
        syntax_tree: &pernixc_syntax::expression::block::Scope,
        guidance: &Guidance<'_>,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<Expression, Error> {
        let scope_id = self.new_child_branch(NonZeroUsize::new(1).unwrap())[0];
        let successor_block_id = self.new_block();

        let mut statement_syns = Vec::new();
        if let Some(statement) = syntax_tree.statements() {
            statement_syns.extend(
                statement.statements().filter_map(|x| x.into_line().ok()),
            );
        }

        let block_state = self
            .bind_block()
            .maybe_label(syntax_tree.label().as_ref())
            .statements(statement_syns.iter())
            .span(syntax_tree.span())
            .scope_id(scope_id)
            .successor_block_id(successor_block_id)
            .is_unsafe(syntax_tree.unsafe_keyword().is_some())
            .handler(handler)
            .call()
            .await?;

        // bind the block state as value
        self.bind(block_state, guidance, handler).await
    }
}
