//! Contains the syntax tree nodes for statements

use derive_more::From;
use enum_as_inner::EnumAsInner;
use pernixc_common::source_file::Span;
use pernixc_lexical::token::{Identifier, Keyword, KeywordKind, Punctuation, Token};

use super::{
    expression::{Expression, Functional, Imperative},
    SourceElement, TypeBindingSpecifier,
};
use crate::parser::{FirstOrSecond, Parser};

/// Represents a statement syntax tree node
///
/// Syntax Synopsis:
/// ``` text
/// Statement:
///     Declarative
///     | Expressive
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum Statement {
    Declarative(Declarative),
    Expressive(Expressive),
}

impl SourceElement for Statement {
    fn span(&self) -> Span {
        match self {
            Self::Declarative(declaration) => declaration.span(),
            Self::Expressive(expression) => expression.span(),
        }
    }
}

/// Represents a declarative statement syntax tree node
///
/// Declarative statements are statements that introduce a new symbol into the current scope.
///
/// Syntax Synopsis:
/// ``` text
/// Declarative:
///     VariableDeclaration
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum Declarative {
    VariableDeclaration(VariableDeclaration),
}

impl SourceElement for Declarative {
    fn span(&self) -> Span {
        match self {
            Self::VariableDeclaration(declaration) => declaration.span(),
        }
    }
}

/// Represents a let binding specifier syntax tree node for variable declarations.
///
/// Syntax Synopsis:
/// ``` text
/// LetBindingSpecifier:
///     'mutable' 'let'
///     ;
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct LetBindingSpecifier {
    pub mutable_keyword: Option<Keyword>,
    pub let_keyword: Keyword,
}

impl SourceElement for LetBindingSpecifier {
    fn span(&self) -> Span {
        self.mutable_keyword
            .as_ref()
            .map_or(self.let_keyword.span, |keyword| Span {
                start: keyword.span.start,
                end: self.let_keyword.span.end,
            })
    }
}

/// Represents a variable type binding specifier syntax tree node for variable declarations.
///
/// Syntax Synopsis:
/// ``` text
/// VariableTypeBindingSpecifier:
///     TypeBindingSpecifier
///     | LetBindingSpecifier
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum VariableTypeBindingSpecifier {
    TypeBindingSpecifier(TypeBindingSpecifier),
    LetBindingSpecifier(LetBindingSpecifier),
}

impl SourceElement for VariableTypeBindingSpecifier {
    fn span(&self) -> Span {
        match self {
            Self::TypeBindingSpecifier(binding) => binding.span(),
            Self::LetBindingSpecifier(binding) => binding.span(),
        }
    }
}

/// Represents a variable declaration syntax tree node
///
/// Syntax Synopsis:
/// ``` text
/// VariableDeclaration:
///     VariableTypeBindingSpecifier Identifier '=' Expression ';'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct VariableDeclaration {
    pub variable_type_binding_specifier: VariableTypeBindingSpecifier,
    pub identifier: Identifier,
    pub equals: Punctuation,
    pub expression: Expression,
    pub semicolon: Punctuation,
}

impl SourceElement for VariableDeclaration {
    fn span(&self) -> Span {
        Span {
            start: self.variable_type_binding_specifier.span().start,
            end: self.semicolon.span.end,
        }
    }
}

/// Represents an expressive statement syntax tree node
///
/// Expressive statements are statements that interest in the side effects of evaluating an
/// expression.
///
/// Syntax Synopsis:
/// ``` text
/// Expressive:
///     Semi
///     | Imperative
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum Expressive {
    Semi(Semi),
    Imperative(Imperative),
}

impl SourceElement for Expressive {
    fn span(&self) -> Span {
        match self {
            Self::Semi(expression) => expression.span(),
            Self::Imperative(expression) => expression.span(),
        }
    }
}

/// Represents a semi statement syntax tree node
///
/// Semi statements are statements that are a functional expression followed by a semicolon.
///
/// Syntax Synopsis:
/// ``` text
/// Semi:
///     Functional ';'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct Semi {
    pub expression: Functional,
    pub semicolon: Punctuation,
}

impl SourceElement for Semi {
    fn span(&self) -> Span {
        Span {
            start: self.expression.span().start,
            end: self.semicolon.span.end,
        }
    }
}

impl<'a> Parser<'a> {
    /// Parses a [Statement]
    pub fn parse_statement(&mut self) -> Option<Statement> {
        match self.peek_significant_token() {
            // Handles variable declaration statements
            Some(Token::Keyword(mutable_keyword))
                if mutable_keyword.keyword == KeywordKind::Mutable =>
            {
                // eat mutable keyword
                self.next_token();

                match self.peek_significant_token() {
                    Some(Token::Keyword(let_keyword))
                        if let_keyword.keyword == KeywordKind::Let =>
                    {
                        // eat let keyword
                        self.next_token();

                        self.parse_variable_declaration_statement(
                            VariableTypeBindingSpecifier::LetBindingSpecifier(
                                LetBindingSpecifier {
                                    mutable_keyword: Some(*mutable_keyword),
                                    let_keyword: *let_keyword,
                                },
                            ),
                        )
                    }
                    _ => {
                        let type_specifier = self.parse_type_specifier()?;
                        self.parse_variable_declaration_statement(
                            VariableTypeBindingSpecifier::TypeBindingSpecifier(
                                TypeBindingSpecifier {
                                    mutable_keyword: Some(*mutable_keyword),
                                    type_specifier,
                                },
                            ),
                        )
                    }
                }
            }

            // Handles variable declaration statements
            Some(Token::Keyword(let_keyword)) if let_keyword.keyword == KeywordKind::Let => {
                // eat let keyword
                self.next_token();

                self.parse_variable_declaration_statement(
                    VariableTypeBindingSpecifier::LetBindingSpecifier(LetBindingSpecifier {
                        mutable_keyword: None,
                        let_keyword: *let_keyword,
                    }),
                )
            }
            // Might be either a variable declaration statement or identifier expression statement
            Some(Token::Identifier(..)) => self.handle_identiier_statement(),
            // Handles variable declaration statements
            Some(Token::Keyword(primitive_type))
                if matches!(
                    primitive_type.keyword,
                    KeywordKind::Void
                        | KeywordKind::Float32
                        | KeywordKind::Float64
                        | KeywordKind::Int8
                        | KeywordKind::Int16
                        | KeywordKind::Int32
                        | KeywordKind::Int64
                        | KeywordKind::Uint8
                        | KeywordKind::Uint16
                        | KeywordKind::Uint32
                        | KeywordKind::Uint64
                        | KeywordKind::Bool
                ) =>
            {
                // parse variable declaration statement
                let type_specifier = self
                    .parse_type_specifier()
                    .expect("should be a valid primitive type of something.");

                let equals = self.expect_punctuation('=')?;
                let expression = self.parse_expression()?;
                let semicolon = self.expect_punctuation(';')?;

                Some(Statement::Declarative(Declarative::VariableDeclaration(
                    VariableDeclaration {
                        variable_type_binding_specifier:
                            VariableTypeBindingSpecifier::TypeBindingSpecifier(
                                TypeBindingSpecifier {
                                    mutable_keyword: None,
                                    type_specifier,
                                },
                            ),
                        identifier: *self.expect_identifier()?,
                        equals: *equals,
                        expression,
                        semicolon: *semicolon,
                    },
                )))
            }
            _ => {
                let expression = self.parse_expression()?;

                match expression {
                    Expression::Functional(expression) => {
                        // expect semi-colon
                        let semicolon = self.expect_punctuation(';')?;

                        Some(Statement::Expressive(Expressive::Semi(Semi {
                            expression,
                            semicolon: *semicolon,
                        })))
                    }
                    Expression::Imperative(expression) => {
                        Some(Statement::Expressive(Expressive::Imperative(expression)))
                    }
                }
            }
        }
    }

    fn handle_identiier_statement(&mut self) -> Option<Statement> {
        // try to parse either a variable declaration statement or identifier expression
        // statement.
        //
        // the syntax tree that eats most tokens will be the one that is returned.
        let result = self.ambiguity_resolution(
            |this: &mut Self| -> Option<Statement> {
                let type_specifier = this.parse_type_specifier()?;
                this.parse_variable_declaration_statement(
                    VariableTypeBindingSpecifier::TypeBindingSpecifier(TypeBindingSpecifier {
                        mutable_keyword: None,
                        type_specifier,
                    }),
                )
            },
            |this: &mut Self| -> Option<Statement> {
                let expression = this.parse_expression()?;
                match expression {
                    Expression::Functional(expression) => {
                        // expect semi-colon
                        let semicolon = this.expect_punctuation(';')?;

                        Some(Statement::Expressive(Expressive::Semi(Semi {
                            expression,
                            semicolon: *semicolon,
                        })))
                    }
                    Expression::Imperative(expression) => {
                        Some(Statement::Expressive(Expressive::Imperative(expression)))
                    }
                }
            },
        );

        result.map(|x| match x {
            FirstOrSecond::First(x) | FirstOrSecond::Second(x) => x,
        })
    }

    fn parse_variable_declaration_statement(
        &mut self,
        variable_type_binding_specifier: VariableTypeBindingSpecifier,
    ) -> Option<Statement> {
        let identifier = self.expect_identifier()?;
        let equals = self.expect_punctuation('=')?;
        let expression = self.parse_expression()?;
        let semicolon = self.expect_punctuation(';')?;

        Some(Statement::Declarative(Declarative::VariableDeclaration(
            VariableDeclaration {
                variable_type_binding_specifier,
                identifier: *identifier,
                equals: *equals,
                expression,
                semicolon: *semicolon,
            },
        )))
    }
}

#[cfg(test)]
mod tests;
