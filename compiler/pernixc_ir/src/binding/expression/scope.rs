use std::num::NonZeroUsize;

use pernixc_handler::Handler;
use pernixc_source_file::SourceElement;
use pernixc_syntax::syntax_tree;
use pernixc_table::diagnostic::Diagnostic;

use super::{Bind, Config, Expression};
use crate::binding::{Binder, Error};

impl Bind<&syntax_tree::expression::block::Scope> for Binder<'_> {
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::block::Scope,
        config: Config,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<Expression, Error> {
        let scope_id = self
            .intermediate_representation
            .scope_tree
            .new_child_branch(
                self.stack.current_scope().scope_id(),
                NonZeroUsize::new(1).unwrap(),
            )
            .unwrap()[0];

        let successor_block_id =
            self.intermediate_representation.control_flow_graph.new_block();

        let block_state = self.bind_block(
            syntax_tree.label.as_ref(),
            syntax_tree
                .statements
                .statements
                .iter()
                .filter_map(|x| x.as_option()),
            syntax_tree.span(),
            scope_id,
            successor_block_id,
            syntax_tree.unsafe_keyword.is_some(),
            handler,
        )?;

        // bind the block state as value
        self.bind(block_state, config, handler)
    }
}

#[cfg(test)]
mod test;
