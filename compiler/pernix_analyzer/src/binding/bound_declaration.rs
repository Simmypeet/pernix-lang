use crate::{
    control_flow_graph::ControlFlowGraph,
    error::Error,
    symbol::{
        table::{FunctionSymbolTable, TypeSymbolTable},
        FunctionSymbol,
    },
};

use super::binder::Binder;

/// Represent a bound function. It has been checked for all semantic correctness
/// and is ready to be compiled. The struct contains a reference to the function
/// symbol and the control flow graph (CFG) of the function.
#[derive(Clone, Debug)]
pub struct BoundFunction<'table, 'ast> {
    function_symbol: &'table FunctionSymbol<'table, 'ast>,
    control_flow_graph: ControlFlowGraph<'table, 'ast>,
}

impl<'table: 'ast, 'ast> BoundFunction<'table, 'ast> {
    /// Binds the given `function_symbol` and returns a [`BoundFunction`] if the
    /// binding was successful. Otherwise, it returns a list of errors.
    ///
    /// # Arguments
    /// - `function_symbol`: The function symbol to bind.
    /// - `function_table`: The function symbol table to look up function calls.
    /// - `type_table`: The type symbol table to look up types.
    pub fn new(
        function_symbol: &'table FunctionSymbol<'table, 'ast>,
        function_table: &'table FunctionSymbolTable<'table, 'ast>,
        type_table: &'table TypeSymbolTable,
    ) -> Result<BoundFunction<'table, 'ast>, Vec<Error<'table, 'ast>>> {
        match Binder::bind(function_symbol, function_table, type_table) {
            Ok(bound_statement) => {
                let cfg = ControlFlowGraph::analyze(bound_statement);
                Ok(BoundFunction {
                    function_symbol,
                    control_flow_graph: cfg,
                })
            }
            Err(err) => Err(err),
        }
    }

    /// Return a reference to the function symbol of this [`BoundFunction`].
    pub fn function_symbol(&self) -> &'table FunctionSymbol<'table, 'ast> {
        self.function_symbol
    }

    /// Return a reference to the control flow graph of this [`BoundFunction`].
    pub fn control_flow_graph(&self) -> &ControlFlowGraph<'table, 'ast> {
        &self.control_flow_graph
    }
}
