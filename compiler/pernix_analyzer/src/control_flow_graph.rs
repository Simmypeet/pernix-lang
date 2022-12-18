use crate::binding::{
    bound_expression::BoundExpression,
    bound_statement::{BoundReturnStatement, BoundStatement},
    BoundFunction,
};

pub type BlockIndex = usize;

/// Represent a terminator instruction in the control flow graph (CFG).
#[derive(Debug)]
pub enum Terminator<'bound, 'table, 'parser, 'ast> {
    Jump(BlockIndex),
    ConditionalJump {
        expression: &'bound BoundExpression<'table, 'parser, 'ast>,
        true_block: BlockIndex,
        false_block: BlockIndex,
    },
    ReturnStatement(&'bound BoundReturnStatement<'table, 'parser, 'ast>),
}

/// Represent an instruction in the basic block.
#[derive(Debug)]
pub enum Instruction<'bound, 'table, 'parser, 'ast> {
    Statement(&'bound BoundStatement<'table, 'parser, 'ast>),
    Terminator(Terminator<'bound, 'table, 'parser, 'ast>),
}

/// Represent a basic block in the control flow graph (CFG).
#[derive(Debug)]
pub struct Block<'bound, 'table, 'parser, 'ast> {
    pub instructions: Vec<Instruction<'bound, 'table, 'parser, 'ast>>,
}

/// Represent a control flow graph (CFG).
#[derive(Debug)]
pub struct ControlFlowGraph<'bound, 'table, 'parser, 'ast> {
    blocks: Vec<Block<'bound, 'table, 'parser, 'ast>>,
}

struct ControlFlowGraphGenerator<'bound, 'table, 'parser, 'ast> {
    cfg: ControlFlowGraph<'bound, 'table, 'parser, 'ast>,
    current_index: BlockIndex,
}

impl<'bound, 'table: 'parser, 'parser: 'ast, 'ast>
    ControlFlowGraphGenerator<'bound, 'table, 'parser, 'ast>
{
    fn generate(
        &mut self,
        bound_statement: &'bound BoundStatement<'table, 'parser, 'ast>,
        parent_continuation: Option<BlockIndex>,
    ) -> (BlockIndex, BlockIndex) {
        self.current_index = self.cfg.blocks.len();
        let head_index = self.current_index;
        self.cfg.blocks.push(Block {
            instructions: Vec::new(),
        });

        let mut statement_stack = vec![bound_statement];
        let mut terminate_out = false;

        while let Some(statement) = statement_stack.pop() {
            match statement {
                BoundStatement::BoundReturnStatement(return_statement) => {
                    self.cfg
                        .get_block_mut(self.current_index)
                        .instructions
                        .push(Instruction::Terminator(
                            Terminator::ReturnStatement(return_statement),
                        ));

                    terminate_out = true;
                    break;
                }
                BoundStatement::BoundVariableDeclarationStatement(_)
                | BoundStatement::BoundExpressionStatement(_) => {
                    self.cfg
                        .get_block_mut(self.current_index)
                        .instructions
                        .push(Instruction::Statement(statement));
                }
                BoundStatement::BoundBlockScopeStatement(block) => {
                    for statement in block.statements.iter().rev() {
                        statement_stack.push(statement);
                    }
                }
                BoundStatement::BoundIfElseStatement(if_else) => {
                    /*
                     * If-Else Statement (with else) CFG Generation
                     *
                     *      THEN BLOCK
                     *     /          \
                     * COND            CONTINUE BLOCK
                     *     \          /
                     *      ELSE BLOCK
                     *
                     * If-Else Statement (without else) CFG Generation
                     *
                     *     THEN BLOCK
                     *    /          \
                     * COND --------- CONTINUE BLOCK
                     */

                    let current_block_index = self.current_index;

                    // if parent_continuation is None, then we are at the top
                    // level of the function, so we need to allocate a new
                    // block for the continuation

                    // check if we need to make continuation block
                    let continuation = statement_stack.len() > 0
                        || parent_continuation.is_none();

                    // if we need to make a continuation block, then we need
                    // to allocate a new block index for it
                    let continuation_index = if continuation {
                        let index = self.cfg.blocks.len();
                        self.cfg.blocks.push(Block {
                            instructions: Vec::new(),
                        });
                        Some(index)
                    } else {
                        None
                    };

                    // generate then block
                    let true_then_block_index = self.generate(
                        if_else.then_statement.as_ref(),
                        continuation_index,
                    );

                    let false_block_index =
                        if let Some(else_statement) = &if_else.else_statement {
                            // generate else block
                            let false_block_index = self.generate(
                                else_statement.as_ref(),
                                continuation_index,
                            );

                            false_block_index.0
                        } else {
                            if let Some(idx) = continuation_index {
                                idx
                            } else {
                                parent_continuation.unwrap()
                            }
                        };

                    // add the conditional jump to the if-else statement
                    self.cfg
                        .get_block_mut(current_block_index)
                        .instructions
                        .push(Instruction::Terminator(
                            Terminator::ConditionalJump {
                                expression: &if_else.condition,
                                true_block: true_then_block_index.0,
                                false_block: false_block_index,
                            },
                        ));

                    // keep doing the continuation block if it exists
                    if let Some(continuation_index) = continuation_index {
                        self.current_index = continuation_index;
                    } else {
                        break;
                    }
                }
            }
        }

        if !terminate_out {
            if let Some(continuation) = parent_continuation {
                self.cfg
                    .get_block_mut(self.current_index)
                    .instructions
                    .push(Instruction::Terminator(Terminator::Jump(
                        continuation,
                    )));
            }
        }

        (head_index, self.current_index)
    }
}

impl<'bound, 'table: 'parser, 'parser: 'ast, 'ast>
    ControlFlowGraph<'bound, 'table, 'parser, 'ast>
{
    /// Analyze the given bound function and generate a control flow graph for
    /// it.
    pub fn analyze(
        bound_func: &'bound BoundFunction<'table, 'parser, 'ast>,
    ) -> Self {
        let mut gen = ControlFlowGraphGenerator {
            cfg: ControlFlowGraph { blocks: vec![] },
            current_index: 0,
        };

        gen.generate(bound_func.body(), None);

        gen.cfg
    }

    fn get_block_mut(
        &mut self,
        index: BlockIndex,
    ) -> &mut Block<'bound, 'table, 'parser, 'ast> {
        &mut self.blocks[index]
    }

    /// Get a reference to the block at the given index.
    pub fn get_block(
        &self,
        index: BlockIndex,
    ) -> &Block<'bound, 'table, 'parser, 'ast> {
        &self.blocks[index]
    }

    /// Get a reference to the entry block of the control flow graph.
    pub fn get_entry_block(&self) -> &Block<'bound, 'table, 'parser, 'ast> {
        &self.blocks[0]
    }
}

#[cfg(test)]
mod test;
