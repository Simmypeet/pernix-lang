use builder_pattern::Builder;

use self::cfg::ControlFlowGraphManager;
use super::value::VariableID;
use crate::symbol::item::{Function, FunctionID, Table};

mod cfg;

struct Builder<'a> {
    control_flow_graph: ControlFlowGraphManager,
    function: &'a Function,
    table: &'a Table,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Default, Builder)]
struct BindConfiguration {
    /// Is an optional [`VariableID`] for those temporary variables that are not declared in the
    /// source code.
    ///
    /// Some expressions are required to be assigned to a temporary variable and most of the time
    /// it's redundant to declare a temporary variable just to be later assigned to another one.
    /// Therefore, if the variable is already declared in the source code and the expression
    /// requires to be assigned to a temporary variable, the temporary variable will
    /// be assigned to the declared variable instead.
    variable_id: Option<VariableID>,
}
