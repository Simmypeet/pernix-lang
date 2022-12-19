//! This module handles the generation of the control flow graph (CFG) for the
//! Pernix programming language. It breaks down all the flow paths of the
//! program into a graph of basic blocks. Each basic block is a sequence of
//! instructions that are executed sequentially.
//!
//! The CFG breaks down all the control flow statements such as `if-else`,
//! `while`, `for`, `do-while`, `switch`, etc. into a sequence of connected'
//! basic blocks.
//!
//! The CFG is used by the code generator to generate the appropriate LLVM IR
//! instructions since LLVM IR does not have any control flow statements.

use crate::binding::{
    bound_expression::BoundExpression,
    bound_statement::{BoundReturnStatement, BoundStatement},
};

/// Represent the index of a basic block in the control flow graph (CFG).
pub type BlockIndex = usize;

/// Represent a terminator instruction in the control flow graph (CFG).
///
/// A terminator instruction is the last instruction in a basic block. It
/// determines the next basic block to be executed or the end of the function.
#[derive(Debug)]
pub enum Terminator<'table, 'ast> {
    /// Jump to the specified basic block directly.
    Jump(BlockIndex),

    /// Jump to the specified basic block if the expression evaluates to true.
    ConditionalJump {
        expression: BoundExpression<'table, 'ast>,
        true_block: BlockIndex,
        false_block: BlockIndex,
    },

    /// Return from the function.
    ReturnStatement(BoundReturnStatement<'table, 'ast>),
}

/// Represent an instruction in the basic block. An instruction can be either a
/// statement or a terminator instruction.
#[derive(Debug)]
pub enum Instruction<'table, 'ast> {
    Statement(BoundStatement<'table, 'ast>),
    Terminator(Terminator<'table, 'ast>),
}

/// Represent a basic block in the control flow graph (CFG). A basic block is a
/// sequence of instructions that are executed sequentially.
#[derive(Debug)]
pub struct Block<'table, 'ast> {
    pub instructions: Vec<Instruction<'table, 'ast>>,
}

/// Represent a control flow graph (CFG). The struct contains a list of well-
/// connected basic blocks. Each basic block can refer to other basic blocks
/// by using the `Jump` and `ConditionalJump` terminator instructions. The CFG
/// uses indices to refer to the basic blocks.
#[derive(Debug)]
pub struct ControlFlowGraph<'table, 'ast> {
    blocks: Vec<Block<'table, 'ast>>,
}

pub struct LoopInfo {
    pub jump_break_index: BlockIndex,
    pub jump_continue_index: BlockIndex,
}

struct ControlFlowGraphGenerator<'table, 'ast> {
    cfg: ControlFlowGraph<'table, 'ast>,
    current_index: BlockIndex,
    loop_info_stack: Vec<LoopInfo>,
}

impl<'table, 'ast> ControlFlowGraphGenerator<'table, 'ast> {
    fn allocate_block(&mut self) -> BlockIndex {
        self.cfg.blocks.push(Block {
            instructions: Vec::new(),
        });

        self.cfg.blocks.len() - 1
    }

    fn generate(
        &mut self,
        bound_statement: BoundStatement<'table, 'ast>,
        parent_continuation: Option<BlockIndex>,
    ) -> BlockIndex {
        self.current_index = self.allocate_block();
        let head_index = self.current_index;

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
                BoundStatement::BoundBlockScopeStatement(mut block) => {
                    block.statements.reverse();
                    for statement in block.statements {
                        statement_stack.push(statement);
                    }
                }
                BoundStatement::BoundWhileStatement(while_statement) => {
                    /*
                     * While Statement CFG Generation
                     *
                     * pre_header_block:
                     *     cond_jump COND -> loop_block, continue_block_index
                     *
                     * loop_block:
                     *      STATEMENT
                     *      jump pre_header_block
                     *
                     * out_block:
                     *      ...
                     *
                     */

                    let pre_header_block_index = self.allocate_block();

                    let out_block_index = self.allocate_block();

                    self.loop_info_stack.push(LoopInfo {
                        jump_break_index: out_block_index,
                        jump_continue_index: pre_header_block_index,
                    });

                    let loop_block_index_head = self.generate(
                        *while_statement.loop_statement,
                        Some(pre_header_block_index),
                    );

                    // connect pre_header_block to loop_block
                    self.cfg
                        .get_block_mut(pre_header_block_index)
                        .instructions
                        .push(Instruction::Terminator(
                            Terminator::ConditionalJump {
                                expression: while_statement.condition,
                                true_block: loop_block_index_head,
                                false_block: out_block_index,
                            },
                        ));

                    self.loop_info_stack.pop();
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
                        Some(self.allocate_block())
                    } else {
                        None
                    };

                    // generate then block
                    let true_then_block_index = self
                        .generate(*if_else.then_statement, continuation_index);

                    let false_block_index =
                        if let Some(else_statement) = if_else.else_statement {
                            // generate else block
                            let false_block_index = self
                                .generate(*else_statement, continuation_index);

                            false_block_index
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
                                expression: if_else.condition,
                                true_block: true_then_block_index,
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
                BoundStatement::BoundBreakStatement => {
                    // get the loop info
                    let loop_info = self.loop_info_stack.last().unwrap();

                    // add the jump to the break block
                    self.cfg
                        .get_block_mut(self.current_index)
                        .instructions
                        .push(Instruction::Terminator(Terminator::Jump(
                            loop_info.jump_break_index,
                        )));

                    terminate_out = true;
                    break;
                }
                BoundStatement::BoundContinueStatement => {
                    // get the loop info
                    let loop_info = self.loop_info_stack.last().unwrap();

                    // add the jump to the continue block
                    self.cfg
                        .get_block_mut(self.current_index)
                        .instructions
                        .push(Instruction::Terminator(Terminator::Jump(
                            loop_info.jump_continue_index,
                        )));

                    terminate_out = true;
                    break;
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

        head_index
    }
}

impl<'table, 'ast> ControlFlowGraph<'table, 'ast> {
    /// Analyze the given bound statement and generate a control flow graph.
    pub fn analyze(body: BoundStatement<'table, 'ast>) -> Self {
        let mut gen = ControlFlowGraphGenerator {
            cfg: ControlFlowGraph { blocks: vec![] },
            current_index: 0,
            loop_info_stack: vec![],
        };

        gen.generate(body, None);

        gen.cfg
    }

    fn get_block_mut(&mut self, index: BlockIndex) -> &mut Block<'table, 'ast> {
        &mut self.blocks[index]
    }

    /// Get a reference to the block at the given index.
    pub fn get_block(&self, index: BlockIndex) -> &Block<'table, 'ast> {
        &self.blocks[index]
    }

    /// Get a reference to the entry block of the control flow graph.
    pub fn get_entry_block(&self) -> &Block<'table, 'ast> {
        &self.blocks[0]
    }
}

#[cfg(test)]
mod test;
