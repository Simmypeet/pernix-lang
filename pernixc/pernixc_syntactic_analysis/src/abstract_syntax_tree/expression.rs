use pernixc_lexical_analysis::token::LiteralConstantToken;

use super::{PositionWrapper, TypeUnitAST};

/// Represent an expression abstract syntax tree node containing a literal expression.
///
/// ANTLR4 grammar:
///
/// ``` txt
/// LiteralExpression: LiteralConstant;
/// ```
#[derive(Clone)]
pub struct LiteralExpressionAST<'src> {
    pub literal_expression: LiteralConstantToken<'src>,
}

/// Reprsent an expression abstract syntax tree node yielding the value by referencing a
/// particular symbol using qualified name.
///
/// ANTLR4 grammar:
///     
/// ``` txt
/// QualifiedNameExpression: QualifiedName;
/// ```
#[derive(Clone)]
pub struct QualifiedNameExpressionAST<'src> {
    pub qualified_name: &'src str,
}

/// Is an enumeration of all possible prefix operators.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum UnaryOperator {
    Identity,
    Negation,
    LogicalNegation,
    Increment,
    Decrement,
}

/// Represent an expression abstract syntax tree node containing a unary expression.
///
/// ANTLR4 grammar:
///
/// ``` txt
/// UnaryExpression:
///    UnaryOperator Expression;
/// ```
#[derive(Clone)]
pub struct UnaryExpressionAST<'src> {
    pub unary_operator: PositionWrapper<UnaryOperator>,
    pub expression: Box<PositionWrapper<ExpressionAST<'src>>>,
}

/// Is an enumeration of all possible binary operators.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperator {
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Modulo,
    CompoundAddition,
    CompoundSubtraction,
    CompoundMultiplication,
    CompoundDivision,
    CompoundModulo,
    Assignment,
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    LogicalAnd,
    LogicalOr,
}

/// Represent an expression abstract syntax tree node containing a binary expression.
///
/// ANTLR4 grammar:
///
/// ``` txt
/// BinaryExpression:
///     Expression BinaryOperator Expression;
/// ```
#[derive(Clone)]
pub struct BinaryExpressionAST<'src> {
    pub left_expression: Box<PositionWrapper<ExpressionAST<'src>>>,
    pub binary_operator: PositionWrapper<BinaryOperator>,
    pub right_expression: Box<PositionWrapper<ExpressionAST<'src>>>,
}

/// Represent an expression abstract syntax tree node yielding a function call
/// expression.
///
/// ANTLR4 grammar:
///
/// ``` txt
/// FunctionCallExpression:
///     QualifiedName '(' Expression? (',' Expression)* ')';
/// ```
#[derive(Clone)]
pub struct FunctionCallExpressionAST<'src> {
    pub qualified_name: PositionWrapper<&'src str>,
    pub arguments: Vec<PositionWrapper<ExpressionAST<'src>>>,
}

/// Reprsent an initialization statement for a class field.
///
/// ANTLR4 grammar:
///
/// ``` txt
/// ClassFiledInitialization:
///     QualifiedName '=' Expression;
/// ```
#[derive(Clone)]
pub struct ClassFiledInitializationAST<'src> {
    pub identifier: PositionWrapper<&'src str>,
    pub expression: PositionWrapper<ExpressionAST<'src>>,
}

/// Represent an expression abstract syntax tree node yielding from a class
/// instantiation
///
/// ANTLR4 grammar:
///
/// ``` txt
/// ClassFieldInitialization:List:
///     ClassFieldInitialization (',' ClassFieldInitialization)*;
///
/// ClassInstantiationExpression:
///     'new' TypeUnit '{' ClassFieldInitialization? '}';
/// ```
#[derive(Clone)]
pub struct ClassInstantiationExpressionAST<'src> {
    pub type_unit: PositionWrapper<TypeUnitAST<'src>>,
    pub field_initializations: Vec<PositionWrapper<ClassFiledInitializationAST<'src>>>,
}

/// Represent an expression abstract syntax tree node yielding from accessing a
/// class field
///
/// ANTLR4 grammar:
///
/// ``` txt
/// MemberFieldAccessExpression:
///     Expression '.' Identifier;
/// ```
#[derive(Clone)]
pub struct MemberFieldAccessExpressionAST<'src> {
    pub identifier: PositionWrapper<&'src str>,
    pub expression: Box<PositionWrapper<ExpressionAST<'src>>>,
}

/// Is an enumeration of all possible expression abstract syntax tree nodes.
///
/// ANTLR4 grammar:
///
/// ``` txt
/// Expression:
///     LiteralExpression
///     | QualifiedNameExpression
///     | UnaryExpression
///     | BinaryExpression
///     | FunctionCallExpression
///     | ClassInstantiationExpression
///     | MemberFieldAccessExpression;
/// ```
#[derive(Clone)]
pub enum ExpressionAST<'src> {
    LiteralExpression(LiteralExpressionAST<'src>),
    QualifiedNameExpression(QualifiedNameExpressionAST<'src>),
    UnaryExpression(UnaryExpressionAST<'src>),
    BinaryExpression(BinaryExpressionAST<'src>),
    FunctionCallExpression(FunctionCallExpressionAST<'src>),
    ClassInstantiationExpression(ClassInstantiationExpressionAST<'src>),
    MemberFieldAccessExpression(MemberFieldAccessExpressionAST<'src>),
}
