use pernix_parser::abstract_syntax_tree::{
    declaration::{FunctionDeclaration, QualifiedType},
    expression::Expression,
    PositionWrapper, UnaryOperator,
};

use crate::symbol::TypeSymbol;

/// Represent an enumeration containing all the semantic errors
pub enum Error<'table, 'parser, 'ast> {
    FunctionRedeclaration {
        previous_declaration:
            PositionWrapper<&'parser FunctionDeclaration<'ast>>,
        redeclaration: PositionWrapper<&'parser FunctionDeclaration<'ast>>,
    },
    ParameterRedeclaration {
        function_declaration:
            PositionWrapper<&'parser FunctionDeclaration<'ast>>,
        previous_declaration:
            &'parser PositionWrapper<(QualifiedType<'ast>, &'ast str)>,
        redeclaration:
            &'parser PositionWrapper<(QualifiedType<'ast>, &'ast str)>,
    },
    TypeNotFound {
        type_name: PositionWrapper<&'ast str>,
    },
    InvalidUnaryOperation {
        operator: PositionWrapper<UnaryOperator>,
        operand: &'parser PositionWrapper<Expression<'ast>>,
        operand_type: &'table TypeSymbol,
    },
}
