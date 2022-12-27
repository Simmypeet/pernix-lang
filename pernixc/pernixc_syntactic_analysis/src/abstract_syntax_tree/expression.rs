use pernixc_lexical_analysis::token::LiteralConstantToken;

use super::{PositionWrapper, TypeUnit};

/// Represent an expression abstract syntax tree node containing a literal expression.
///
/// ANTLR4 grammar:
///
/// ``` txt
/// LiteralExpression: LiteralConstant;
/// ```
#[derive(Debug, Clone)]
pub struct LiteralExpression<'src> {
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
#[derive(Debug, Clone)]
pub struct QualifiedNameExpression<'src> {
    pub qualified_name: &'src str,
}

/// Is an enumeration of all possible prefix operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
#[derive(Debug, Clone)]
pub struct UnaryExpression<'src> {
    pub unary_operator: PositionWrapper<UnaryOperator>,
    pub expression: Box<PositionWrapper<Expression<'src>>>,
}

/// Is an enumeration of all possible binary operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
#[derive(Debug, Clone)]
pub struct BinaryExpression<'src> {
    pub left_expression: Box<PositionWrapper<Expression<'src>>>,
    pub binary_operator: PositionWrapper<BinaryOperator>,
    pub right_expression: Box<PositionWrapper<Expression<'src>>>,
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
#[derive(Debug, Clone)]
pub struct FunctionCallExpression<'src> {
    pub qualified_name: PositionWrapper<&'src str>,
    pub arguments: Vec<PositionWrapper<Expression<'src>>>,
}

/// Reprsent an initialization statement for a class field.
///
/// ANTLR4 grammar:
///
/// ``` txt
/// ClassFiledInitialization:
///     QualifiedName '=' Expression;
/// ```
#[derive(Debug, Clone)]
pub struct ClassFiledInitialization<'src> {
    pub identifier: PositionWrapper<&'src str>,
    pub expression: PositionWrapper<Expression<'src>>,
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
#[derive(Debug, Clone)]
pub struct ClassInstantiationExpression<'src> {
    pub type_unit: PositionWrapper<TypeUnit<'src>>,
    pub field_initializations:
        Vec<PositionWrapper<ClassFiledInitialization<'src>>>,
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
#[derive(Debug, Clone)]
pub struct MemberFieldAccessExpression<'src> {
    pub identifier: PositionWrapper<&'src str>,
    pub expression: Box<PositionWrapper<Expression<'src>>>,
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
#[derive(Debug, Clone)]
pub enum Expression<'src> {
    LiteralExpression(LiteralExpression<'src>),
    QualifiedNameExpression(QualifiedNameExpression<'src>),
    UnaryExpression(UnaryExpression<'src>),
    BinaryExpression(BinaryExpression<'src>),
    FunctionCallExpression(FunctionCallExpression<'src>),
    ClassInstantiationExpression(ClassInstantiationExpression<'src>),
    MemberFieldAccessExpression(MemberFieldAccessExpression<'src>),
}
