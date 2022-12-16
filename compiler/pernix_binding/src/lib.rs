use std::collections::HashMap;

use bound_ast::{
    bound_expression::{
        BoundExpression, BoundUnaryExpression, ExpressionCategory,
        ExpressionType,
    },
    bound_statement::{BoundBlockScopeStatement, BoundStatement},
};
use error::Error;
use pernix_parser::{
    abstract_syntax_tree::{
        declaration::{Declaration, FunctionDeclaration},
        expression::{Expression, UnaryExpression},
        statement::{BlockScopeStatement, Statement},
        PositionWrapper, UnaryOperator,
    },
    File,
};
use scope::{ScopeInfo, ScopeTransverser};
use symbol::{
    table::{FunctionSymbolTable, TypeSymbolTable},
    FunctionSymbol, TypeSymbol, VariableSymbol,
};

use crate::symbol::PrimitiveType;

pub mod bound_ast;
pub mod error;
pub mod scope;
pub mod symbol;

/// Represents a bound function definition with its body.
pub struct FunctionDefinition<'table, 'parser, 'ast> {
    function_symbol: FunctionSymbol<'table, 'parser, 'ast>,
    body: BoundBlockScopeStatement<'table, 'parser, 'ast>,
}

pub struct Binder<'table, 'parser, 'ast> {
    type_table: &'table TypeSymbolTable,
    function_table: &'table FunctionSymbolTable<'table, 'parser, 'ast>,
    function_definition:
        HashMap<String, FunctionDefinition<'table, 'parser, 'ast>>,
    local_scope_stack: LockScopeStack<'table, 'ast>,
    return_type: Option<&'table TypeSymbol>,
    errors: Vec<Error<'table, 'parser, 'ast>>,
}

struct LocalScope<'table, 'ast> {
    variable_declaration: HashMap<String, VariableSymbol<'table, 'ast>>,
}

impl<'table, 'ast> LocalScope<'table, 'ast> {
    fn new() -> Self {
        Self {
            variable_declaration: HashMap::new(),
        }
    }

    fn declare_variable(&mut self, variable: VariableSymbol<'table, 'ast>) {
        self.variable_declaration
            .insert(variable.name.to_string(), variable);
    }

    fn lookup_variable(
        &self,
        name: &str,
    ) -> Option<&VariableSymbol<'table, 'ast>> {
        self.variable_declaration.get(name)
    }
}

struct LockScopeStack<'table, 'ast> {
    stack: Vec<LocalScope<'table, 'ast>>,
}

impl<'table, 'ast> LockScopeStack<'table, 'ast> {
    fn new() -> Self {
        Self { stack: Vec::new() }
    }

    fn push(&mut self) {
        self.stack.push(LocalScope::new());
    }

    fn pop(&mut self) {
        self.stack.pop();
    }

    fn declare_variable(&mut self, variable: VariableSymbol<'table, 'ast>) {
        self.stack.last_mut().unwrap().declare_variable(variable);
    }

    fn lookup_variable(
        &self,
        name: &str,
    ) -> Option<&VariableSymbol<'table, 'ast>> {
        for scope in self.stack.iter().rev() {
            if let Some(variable) = scope.lookup_variable(name) {
                return Some(variable);
            }
        }

        None
    }
}

impl<'table, 'parser: 'ast, 'ast> Binder<'table, 'parser, 'ast> {
    /// Creates a new [`Binder`] struct with the given type and function tables.
    ///
    /// ## Parameters
    /// - `type_table`: the type table that will be used to lookup the types.
    /// The table must be already populated with the types.
    /// - `function_table`: the function table that will be used to lookup the
    /// functions. The table must be already populated with the functions.
    pub fn new(
        type_table: &'table TypeSymbolTable,
        function_table: &'table FunctionSymbolTable<'table, 'parser, 'ast>,
    ) -> Self {
        Self {
            type_table,
            function_table,
            function_definition: HashMap::new(),
            local_scope_stack: LockScopeStack::new(),
            return_type: None,
            errors: Vec::new(),
        }
    }

    /// Bind the given AST file.
    pub fn bind_file(&mut self, ast: &'parser File<'ast>) {
        ScopeTransverser::new(ast).transverse(
            &mut |scope_info, declaration| {
                if let Declaration::FunctionDeclaration(ast) =
                    &declaration.value
                {
                    if let Some(def) = self.bind_function_definition(
                        scope_info,
                        PositionWrapper {
                            position: declaration.position.clone(),
                            value: ast,
                        },
                    ) {
                        self.function_definition.insert(
                            def.function_symbol
                                .full_qualified_name()
                                .to_string(),
                            def,
                        );
                    }
                }
            },
        );
    }

    fn bind_function_definition(
        &mut self,
        scope_info: ScopeInfo,
        ast: PositionWrapper<&'parser FunctionDeclaration<'ast>>,
    ) -> Option<FunctionDefinition<'table, 'parser, 'ast>> {
        // get the function symbol
        let function_symbol = {
            let full_qualified_name =
                if scope_info.current_namespace_scope.is_empty() {
                    ast.value.function_name.value.to_string()
                } else {
                    format!(
                        "{}::{}",
                        scope_info.current_namespace_scope,
                        ast.value.function_name.value
                    )
                };

            match self.function_table.get(&full_qualified_name) {
                Some(val) => val.value.clone(),
                None => {
                    match FunctionSymbol::bind(
                        scope_info,
                        ast.clone(),
                        self.type_table,
                    ) {
                        Ok(val) => val,
                        Err(mut new_errors) => {
                            self.errors.append(&mut new_errors);
                            return None;
                        }
                    }
                }
            }
        };

        self.return_type = Some(function_symbol.return_type());

        // bind the function body
        let body = self.bind_block_scope_statement(PositionWrapper {
            position: ast.value.body.position.clone(),
            value: &ast.value.body.value,
        })?;

        // create the function definition
        Some(FunctionDefinition {
            function_symbol,
            body: match body {
                BoundStatement::BoundBlockScopeStatement(val) => val,
                _ => unreachable!(),
            },
        })
    }

    ////////////////////////////////////////////////////////////////////////////
    /// EXPRESSION
    ////////////////////////////////////////////////////////////////////////////

    fn bind_expression(
        &mut self,
        ast: &'parser PositionWrapper<Expression<'ast>>,
    ) -> Option<BoundExpression<'table, 'parser, 'ast>> {
        todo!();
    }

    fn bind_unary_expression(
        &mut self,
        ast: PositionWrapper<&'parser UnaryExpression<'ast>>,
    ) -> Option<BoundExpression<'table, 'parser, 'ast>> {
        let operand = self.bind_expression(&ast.value.operand)?;

        match operand.get_type().type_symbol {
            TypeSymbol::PrimitiveType(primitive_type) => match primitive_type {
                PrimitiveType::Bool => {
                    if ast.value.operator.value != UnaryOperator::LogicalNot {
                        self.errors.push(Error::InvalidUnaryOperation {
                            operator: ast.value.operator.clone(),
                            operand: &ast.value.operand,
                            operand_type: operand.get_type().type_symbol,
                        });
                        return None;
                    }
                }
                PrimitiveType::Float32
                | PrimitiveType::Float64
                | PrimitiveType::Int8
                | PrimitiveType::Int16
                | PrimitiveType::Int32
                | PrimitiveType::Int64 => {
                    if ast.value.operator.value != UnaryOperator::Minus
                        && ast.value.operator.value != UnaryOperator::Plus
                    {
                        self.errors.push(Error::InvalidUnaryOperation {
                            operator: ast.value.operator.clone(),
                            operand: &ast.value.operand,
                            operand_type: operand.get_type().type_symbol,
                        });
                        return None;
                    }
                }
                _ => (),
            },
        }

        Some(BoundExpression::BoundUnaryExpression(
            BoundUnaryExpression {
                ast,
                expression_type: ExpressionType {
                    type_symbol: operand.get_type().type_symbol,
                    category: ExpressionCategory::RValue,
                },
                operand: Box::new(operand),
            },
        ))
    }

    ////////////////////////////////////////////////////////////////////////////
    /// STATEMENT
    ////////////////////////////////////////////////////////////////////////////

    fn bind_statement(
        &mut self,
        ast: &'parser PositionWrapper<Statement<'ast>>,
    ) -> Option<BoundStatement<'table, 'parser, 'ast>> {
        todo!();
    }

    fn bind_block_scope_statement(
        &mut self,
        ast: PositionWrapper<&'parser BlockScopeStatement<'ast>>,
    ) -> Option<BoundStatement<'table, 'parser, 'ast>> {
        // push a new local scope
        self.local_scope_stack.push();

        let mut statements = Vec::new();

        for statement in &ast.value.statements {
            statements.push(self.bind_statement(&statement)?);
        }

        // pop the local scope
        self.local_scope_stack.pop();

        Some(BoundStatement::BoundBlockScopeStatement(
            BoundBlockScopeStatement { ast, statements },
        ))
    }
}
