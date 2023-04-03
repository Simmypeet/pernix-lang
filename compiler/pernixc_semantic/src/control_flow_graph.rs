//! Contains all the code for the control flow graph.

use std::{collections::HashMap, ops::Index};

use derive_more::From;
use derive_new::new;
use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_syntax::syntax_tree::statement::{
    Declarative as DeclarativeSyntaxTree, Expressive as ExpressiveSyntaxTree,
    Statement as StatementSyntaxTree,
};
use thiserror::Error;

use self::binder::ControlFlowGraphBinder;
use crate::{
    binding::Expression,
    errors::SemanticError,
    symbol::{ty::Type, FunctionID, LocalVariable, Table},
};

mod binder;

/// Is an identifier used to identify a basic block in the control flow graph.
///
/// The identifier is only valid for the control flow graph that it was created from.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, new)]
pub struct BasicBlockID(usize);

/// Represents a control-flow graph (CFG) for a function.
#[derive(Debug, Clone, Getters)]
pub struct ControlFlowGraph {
    /// Is the list of basic blocks in the control flow graph.
    #[get = "pub"]
    ids_by_basic_block: HashMap<BasicBlockID, BasicBlock>,

    /// Is the list of terminal blocks in the control flow graph.
    ///
    /// Terminal blocks are blocks that do not have any outgoing edges.
    #[get = "pub"]
    terminal_block_ids: Vec<BasicBlockID>,

    /// Is the list of local variables that were used in the control flow graph.
    #[get = "pub"]
    variables: Vec<LocalVariable>,

    /// Is the list of temporary variables that were used in the control flow graph.
    #[get = "pub"]
    temporary_variables: Vec<TemporaryVariable>,
}

impl Index<BasicBlockID> for ControlFlowGraph {
    type Output = BasicBlock;

    fn index(&self, index: BasicBlockID) -> &Self::Output { &self.ids_by_basic_block[&index] }
}

/// Represents an expression storage that is used for storing the result of an expression in an
/// imperative expression.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, new)]
pub struct TemporaryVariable {
    /// The type of the temporary expression storage.
    pub ty: Type,
}

/// Represents a basic block in the control flow graph.
///
/// The basic block contains a sequence of instructions that are executed in order.
#[derive(Debug, Clone)]
pub struct BasicBlock {
    /// The basic block id that is unique to the control flow graph that it was created
    /// from.
    pub basic_block_id: BasicBlockID,

    /// The list of block ids that are predecessors of this block.
    ///
    /// In other words, the list of blocks that can jump to this block.
    pub predecessor_block_ids: Vec<BasicBlockID>,

    /// The list of block ids that are successors of this block.
    ///
    /// In other words, the list of blocks that can be jumped to from this block.
    pub successor_block_ids: Vec<BasicBlockID>,

    /// The list of instructions that will be executed in the basic block.
    pub instructions: Vec<Instruction>,
}

/// Is an instruction that simply evaluates an expression.
#[derive(Debug, Clone, PartialEq, new)]
pub struct EvaluateInstruction {
    /// The expression that will be evaluated in the instruction.
    pub expression: Expression,
}

/// Is a return instruction that will be executed in the control flow graph.
#[derive(Debug, Clone, PartialEq)]
pub struct ReturnInstruction {
    /// The expression that will be returned in the return instruction.
    ///
    /// If the expression is `None`, then the return instruction will return void.
    pub expression: Option<Expression>,
}

/// Is a jump instruction that will be executed in the control flow graph.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct JumpInstruction {
    /// The index of the basic block that will be jumped to.
    pub target_block_index: BasicBlockID,
}

/// Is a conditional jump instruction that will be executed in the control flow graph.
#[derive(Debug, Clone, PartialEq)]
pub struct ConditionalJumpInstruction {
    /// The expression that will be evaluated to determine whether to jump to the target block.
    pub condition: Expression,

    /// The id of the basic block that will be jumped to if the condition is true.
    pub true_target_block_id: BasicBlockID,

    /// The id of the basic block that will be jumped to if the condition is false.
    pub false_target_block_id: BasicBlockID,
}

/// Is an address that the store instruction will store to.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum StoreAddress {
    LocalVariableSymbolIndex(usize),
    TemporaryVariableSymbolIndex(usize),
}

/// Repres ents an instruction that stores a value in a  variable.
#[derive(Debug, Clone, PartialEq)]
pub struct StoreInstruction {
    /// The address of variable that the value will be stored in.
    pub store_address: StoreAddress,

    /// The expression that will be stored in the local variable.
    pub expression: Expression,
}

/// Represents a non-terminal instruction that will be executed in the control flow graph.
#[derive(Debug, Clone, PartialEq, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum BasicInstruction {
    EvaluateInstruction(EvaluateInstruction),
    StoreInstruction(StoreInstruction),
}

/// Represents an instruction that will be executed in the control flow graph.
#[derive(Debug, Clone, PartialEq, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum Instruction {
    BasicInstruction(BasicInstruction),
    TerminalInstruction(TerminalInstruction),
}

/// Represents an instruction that cause the control flow graph to terminate.
#[derive(Debug, Clone, PartialEq, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum TerminalInstruction {
    ReturnInstruction(ReturnInstruction),
    JumpInstruction(JumpInstruction),
    ConditionalJumpInstruction(ConditionalJumpInstruction),
}

/// Is an error that can occur when analyzing a control flow graph from
/// [`ControlFlowGraph::analyze()`].
#[derive(Debug, Clone, PartialEq, Eq, Hash, From, EnumAsInner, Error)]
#[allow(missing_docs)]
pub enum FunctionAnalyzeError {
    #[error("Semantic errors occurred while analyzing the control flow graph.")]
    SemanticErrors(Vec<SemanticError>),

    #[error("The symbol id argument is invalid.")]
    InvalidFunctionSymbolIDArgument,
}

impl ControlFlowGraph {
    /// Analyzes the control flow graph for the function from the given function symbol id.
    ///
    /// # Arguments
    /// * `symbol_table` - The global symbol table that contains the function symbol.
    /// * `function_symbol_id` - The id of the function symbol in the global symbol table.
    ///
    /// # Errors
    /// - [`FunctionAnalyzeError::SemanticErrors`] - If semantic errors occurred while analyzing the
    ///   control flow graph.
    /// - [`FunctionAnalyzeError::InvalidFunctionSymbolIDArgument`] - If the symbol id is invalid
    pub fn analyze(
        symbol_table: &Table,
        function_symbol_id: FunctionID,
    ) -> Result<Self, FunctionAnalyzeError> {
        let mut binder = ControlFlowGraphBinder::new(symbol_table, function_symbol_id)
            .ok_or(FunctionAnalyzeError::InvalidFunctionSymbolIDArgument)?;

        for statement in &binder
            .function_symbol
            .syntax_tree
            .syntax_tree
            .block_without_label
            .statements
        {
            match statement {
                StatementSyntaxTree::Declarative(DeclarativeSyntaxTree::VariableDeclaration(
                    var,
                )) => {
                    let Some(instruction) = binder.bind_variable_declaration(var) else {
                        return Err(binder.terminate());
                    };

                    binder
                        .get_current_block_mut()
                        .instructions
                        .push(Instruction::BasicInstruction(instruction.into()));
                }
                StatementSyntaxTree::Expressive(expression) => {
                    // bind the expression
                    let result = match expression {
                        ExpressiveSyntaxTree::Semi(expr) => {
                            binder.bind_functional(&expr.expression, None)
                        }
                        ExpressiveSyntaxTree::Imperative(expr) => {
                            binder.bind_imperative(expr, None)
                        }
                    };

                    // `None` signals that fatal semantic errors occurred
                    let Some(expression) = result else {
                        return Err(binder.terminate());
                    };

                    binder.get_current_block_mut().instructions.push(
                        Instruction::BasicInstruction(EvaluateInstruction::new(expression).into()),
                    );
                }
            }
        }

        binder.into_control_flow_graph()
    }
}
