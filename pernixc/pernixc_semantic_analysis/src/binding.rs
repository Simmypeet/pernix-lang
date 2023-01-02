use std::{borrow::Borrow, collections::HashMap, hash::Hash};

use pernixc_syntactic_analysis::abstract_syntax_tree::{
    statement::{BlockScopeStatementAST, StatementAST},
    PositionWrapper,
};

use crate::{
    error::SemanticError,
    symbol_table::{
        FunctionSymbol, QualifiedTypeAnnotationSymbol, SymbolTable, VariableSymbol,
        VariableSymbolID,
    },
};

use self::statement::{BlockScopeStatement, Statement};

pub mod expression;
pub mod statement;

struct Binder<'table, 'ast, 'src> {
    table: &'table mut SymbolTable<'ast, 'src>,
    function_symbol: &'table FunctionSymbol<'ast, 'src>,
    local_stack: LocalScopeStack<&'src str, VariableSymbol>,
    variable_count: usize,
}

impl<'table, 'ast, 'src> Binder<'table, 'ast, 'src> {
    ////////////////////////////////////////////////////////////////////////////////////////////////
    /// STATEMENT BINDING
    ////////////////////////////////////////////////////////////////////////////////////////////////

    fn bind_statement(
        &mut self,
        statement: &'ast PositionWrapper<StatementAST<'src>>,
        is_in_loop: bool,
    ) -> Result<PositionWrapper<Statement<'src>>, SemanticError<'src, 'ast>> {
        match &statement.value {
            StatementAST::IfElseStatement(_) => todo!(),
            StatementAST::WhileStatement(_) => todo!(),
            StatementAST::ExpressionStatement(_) => todo!(),
            StatementAST::VariableDeclarationStatement(_) => todo!(),
            StatementAST::ReturnStatement(_) => todo!(),
            StatementAST::BlockScopeStatement(block) => Ok(self.bind_block_scope_statement(
                PositionWrapper {
                    position: statement.position.clone(),
                    value: &block,
                },
                is_in_loop,
            )?),
            StatementAST::BreakStatement | StatementAST::ContinueStatement => {
                // is in loop, the statement is valid
                if is_in_loop {
                    Ok(PositionWrapper {
                        position: statement.position.clone(),
                        value: match statement.value {
                            StatementAST::BreakStatement => Statement::BreakStatement,
                            StatementAST::ContinueStatement => Statement::ContinueStatement,
                            _ => unreachable!(),
                        },
                    })
                }
                // the statement is not valid
                else {
                    Err(SemanticError::BreakOrContinueOutsideLoop {
                        break_or_continue_ast: statement,
                    })
                }
            }
        }
    }

    fn bind_block_scope_statement(
        &mut self,
        statement: PositionWrapper<&'ast BlockScopeStatementAST<'src>>,
        is_in_loop: bool,
    ) -> Result<PositionWrapper<Statement<'src>>, SemanticError<'src, 'ast>> {
        self.local_stack.push();

        let mut statements = Vec::new();

        for statement in &statement.value.statements {
            match self.bind_statement(statement, is_in_loop) {
                Ok(statement) => statements.push(statement),
                Err(error) => return Err(error),
            }
        }

        self.local_stack.pop();

        Ok(PositionWrapper {
            position: statement.position.clone(),
            value: Statement::BlockScopeStatement(BlockScopeStatement { statements }),
        })
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////
    /// EXPRESSION BINDING
    ////////////////////////////////////////////////////////////////////////////////////////////////

    fn bind_expression(
        &mut self,
        expression: &'ast PositionWrapper<StatementAST<'src>>,
    ) -> Result<PositionWrapper<Statement<'src>>, SemanticError<'src, 'ast>> {
        todo!();
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////
    /// VARIABLE BINDING
    ////////////////////////////////////////////////////////////////////////////////////////////////
    fn declare_variable(
        &mut self,
        variable_name: &'src str,
        qualified_type_annotation_symbol: QualifiedTypeAnnotationSymbol,
    ) {
        // assign a new variable symbol id
        let variable_symbol_id = VariableSymbolID {
            id: self.variable_count,
        };

        // declare the variable
        self.local_stack.declare_variable(
            variable_name,
            VariableSymbol {
                id: variable_symbol_id,
                qualified_type_annotation_symbol,
            },
        );

        // increment the variable count
        self.variable_count += 1;
    }
}

pub fn bind<'table, 'ast, 'src>(
    table: &'table mut SymbolTable<'ast, 'src>,
    function_symbol: &'table FunctionSymbol<'ast, 'src>,
) -> Result<BlockScopeStatement<'src>, Vec<SemanticError<'ast, 'src>>>
where
    'ast: 'src,
    'table: 'ast,
{
    // the binder struct
    let mut binder = Binder {
        table,
        function_symbol,
        local_stack: LocalScopeStack::new(),
        variable_count: match function_symbol
            .parameters
            .iter()
            .map(|x| x.1.id.declaration_order())
            .max()
        {
            Some(x) => x + 1,
            None => 0,
        },
    };

    // list of errors
    let mut errors = Vec::new();

    // list of statements
    let mut statements = Vec::new();

    binder.local_stack.push();

    // declare the parameters
    for parameter in &function_symbol.parameters {
        binder
            .local_stack
            .declare_variable(parameter.0, parameter.1.clone());
    }

    for statement in &function_symbol
        .symbol_attribute
        .abstract_syntax_tree
        .value
        .body
        .value
        .statements
    {
        match binder.bind_statement(statement, false) {
            Ok(statement) => statements.push(statement),
            Err(error) => errors.push(error),
        }
    }

    binder.local_stack.pop();

    // return error if there's an error
    if errors.is_empty() {
        Ok(BlockScopeStatement { statements })
    } else {
        Err(errors)
    }
}

/// Represent a stack structure for local scopes. Each scope is a map from
/// variable name to variable symbol.
struct LocalScopeStack<K, V> {
    stack: Vec<HashMap<K, V>>,
}

impl<K, V> LocalScopeStack<K, V> {
    /// Create a new scope stack.
    pub fn new() -> Self {
        Self { stack: Vec::new() }
    }

    /// Push a new local scope to the stack.
    pub fn push(&mut self) {
        self.stack.push(HashMap::new());
    }

    /// Pop the last local scope from the stack.
    pub fn pop(&mut self) {
        self.stack.pop();
    }

    /// Declare a variable in the current scope.
    pub fn declare_variable(&mut self, key: K, val: V)
    where
        K: Eq + Hash,
    {
        self.stack.last_mut().unwrap().insert(key, val);
    }

    /// Get all the variables in the current scope.
    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.stack.iter().rev().flat_map(|scope| scope.values())
    }

    /// Lookup a variable in the current scope.
    pub fn lookup_variable<Q: ?Sized>(&self, name: &Q) -> Option<&V>
    where
        K: Hash + Eq + Borrow<Q>,
        Q: Hash + Eq,
    {
        for scope in self.stack.iter().rev() {
            if let Some(variable) = scope.get(name) {
                return Some(variable);
            }
        }

        None
    }
}
