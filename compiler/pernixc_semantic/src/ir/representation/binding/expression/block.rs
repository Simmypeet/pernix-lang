use std::num::NonZeroUsize;

use pernixc_base::handler::Handler;
use pernixc_syntax::syntax_tree::{self};

use super::{Bind, Config, Expression};
use crate::{
    error::{self},
    ir::{
        self,
        representation::binding::{
            borrow,
            infer::{self},
            Binder, Error,
        },
    },
    symbol::table::{
        self,
        resolution::{self},
    },
    type_system::{self},
};

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>
            + type_system::observer::Observer<borrow::Model, S>,
    > Bind<&syntax_tree::expression::Block> for Binder<'t, S, RO, TO>
{
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::Block,
        config: Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
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
            syntax_tree,
            scope_id,
            successor_block_id,
            handler,
        )?;

        // bind the block state as value
        self.bind(block_state, config, handler)
    }
}
