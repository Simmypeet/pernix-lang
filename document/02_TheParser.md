# The Parser

Continuing from the previous chapter, the compiler now needs to identify the structure
of the source code using the tokens by building some tree that represents the 
construct of the program. This is known as the **Syntactic Analysis** phase.

There are many kinds of trees to choose from, depending on the requirements of the compiler. The one that the compiler will use is **Abstract Syntax Tree** or **AST** 
for short.

## Motivation

As mentioned earlier, in this phase, the compiler will construct an AST to represent
the structure of the source code.

This compiler goes with AST because it's easier to work with
than other kinds of trees. The AST purely contains only necessary information 
about the source code. For example, the **Syntax Tree** is another alternative
the compiler could use. It includes the tokens that construct a particular structure
of the source code.

``` rust
/// An example of a syntax tree
pub struct ReturnStatement {
    pub return_token: Token,
    pub expression: Expression,
    pub semicolon_token: Token,
}

/// An example of an AST
pub struct ReturnStatement {
    pub expression: Expression,
}
```

Obviously, each of them has its own pros and cons.

- The syntax tree contains more information about the source code, which is useful
  for error reporting. However, it's more challenging to work with than the AST.
  Furthermore, if the compiler somehow changes the language's syntax, the syntax tree  will also need to be changed.

- The abstract syntax tree is easier to work with. It only contains the necessary
  information about the source code. Nevertheless, it holds less information, making 
  it harder to report errors.

> In this phase, the compiler only concerns the syntactic elements of the source code.
Therefore, it's not expected to catch any semantic errors such as type errors, 
undefined variables, etc.

## Implementation

The `pernix_parser` crate holds the code related to the parser. The parser is
implemented as a struct that outputs one AST node at a time and advances the state
for the next AST, similar to the lexer. 

There will be three kinds of AST nodes: `Statement`, `Expression`, and `Declaration`.

- **Declaration** represents a declaration of the symbol in the source code, such as
   class, function, global variable, etc.
- **Statement** represents a construct that can be executed such as, `if`, `while`, `return`, etc.
- **Expression** represents a piece of code that can be evaluated and yields a value.

> File: [`declaration.rs`](../compiler/pernix_parser/src/abstract_syntax_tree/declaration.rs)
``` rust
/// Represent an enumeration containing all kinds of declarations.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Declaration<'a> {
    NamespaceDeclaration(NamespaceDeclaration<'a>),
    FunctionDeclaration(FunctionDeclaration<'a>),
}
```
> File: [`statement.rs`](../compiler/pernix_parser/src/abstract_syntax_tree/statement.rs)
``` rust
/// Represent an enumeration containing all kinds of statements.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement<'a> {
    ReturnStatement(ReturnStatement<'a>),
    ExpressionStatement(PositionWrapper<Expression<'a>>),
    VariableDeclarationStatement(VariableDeclarationStatement<'a>),
    IfElseStatement(IfElseStatement<'a>),
    BlockScopeStatement(BlockScopeStatement<'a>),
    WhileStatement(WhileStatement<'a>),
    BreakStatement,
    ContinueStatement,
}
```

> File: [`expression.rs`](../compiler/pernix_parser/src/abstract_syntax_tree/statement.rs)
``` rust
/// Represent an enumeration containing all possible expressions
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression<'a> {
    BinaryExpression(BinaryExpression<'a>),
    UnaryExpression(UnaryExpression<'a>),
    LiteralExpression(LiteralExpression<'a>),
    IdentifierExpression(IdentifierExpression<'a>),
    FunctionCallExpression(FunctionCallExpression<'a>),
}
```

### **The Parser**

The `Parser` struct is the main component of the Syntactic Analysis phase. It uses
the `Lexer`, the main component of the Lexical Analysis phase, to generate the token stream. The `Parser` then uses the token stream to construct the AST.

> File: [`lib.rs`](../compiler/pernix_parser/src/lib.rs)
```rust
/// Represent a state-machine data structure that is used to parse a Pernix
/// source code file.
pub struct Parser<'a> {
    // The lexer that is used to generate the token stream
    lexer: Lexer<'a>,
    // the accumulated tokens that have been tokenized by the lexer so far
    accumulated_tokens: Vec<Token<'a>>,
    // the accumulated errors that have been found during parsing so far
    accumulated_errors: Vec<Error<'a>>,
    // The current position in the source code
    current_index: usize,
    // Flag that indicates whether the parser should produce errors into the
    // list or not
    produce_errors: bool,
}
```

The parser is implemented in a recursive-descent manner. It uses the `parse_*` 
methods to parse the source code. Each of the `parse_*` methods will consume the 
tokens used to build the AST node. The `parse_*` methods can also call other `parse_*` methods to construct the AST node.

```rust
// Example of recursive-descent parsing

pub fn parse_expression(&mut self) -> Option<Expression> {
    // ...
}

pub fn parse_return_statement(&mut self) -> Option<ReturnStatement> {
    self.expect_keyword(Keyword::Return)?;

    // call the other parser_* method to construct the AST node 
    let expression = self.parse_expression()?;

    self.expect_token(TokenKind::Semicolon)?;

    Some(ReturnStatement { expression })
}

pub fn parse_function_declaration(&mut self) -> Option<FunctionDeclaration> {
    // ... expect function names, parameters, etc.

    let mut body = Vec::new();

    while self.peek_token_kind() != TokenKind::RightBrace {
        // call the other parser_* method to construct the AST node
        let statement = self.parse_statement()?;

        body.push(statement);
    }

    self.expect_token(TokenKind::RightBrace)?;

    Some(FunctionDeclaration {
        // ...
        body,
    })
}
```
### **Error Recover**
Another exciting aspect of the parser is the error-recovery strategy that it uses. In
a naive implementation, if the parser encounters an error, it will stop parsing the 
whole source code. Hence, the parser will miss out on the rest of the code, which 
could potentially contain more errors. 

To solve this problem, the parser can skip the malformed AST node and continue
parsing the next one. The parser will look for **delimiters** to determine where
a particular AST node ends. For example, every statement ends with a semicolon; if
the parser encounters an error within a statement, it will skip the rest of the 
tokens until it finds a semicolon and continues parsing the following statement.

```rust
let hello = = 5; // oh no, there is a syntax error here
// as soon as the parser encounters the second equal sign, it will skip the rest of 
// the tokens until it finds a semicolon
let next_valid_statement = 5 + 5; // this statement will be parsed
```

### **Parsing the Expression**

In most programming languages, including Pernix, the expression can perform a binary
operation. For example, `5 + 5` is an expression that performs the addition operation
on the two operands `5` and `5`.

``` rust
// Example of binary expression ast
pub struct BinaryExpression {
    let left: Expression,
    let right: Expression,
    let operator: BinaryOperator,
}
```

In a naive implementation, the parser will try to parse the expression from left to
right. Something similar to the following code:

``` rust
// Example of naive binary expression parsing
pub fn parse_expression(&mut self) -> Option<Expression> {
    let mut left = self.parse_primary_expression()?;

    while self.peek_token_kind() == TokenKind::BinaryOperator {
        let operator = self.parse_binary_operator()?;
        let right = self.parse_primary_expression()?;

        left = Expression::BinaryExpression(BinaryExpression {
            left: Box::new(left),
            right: Box::new(right),
            operator,
        });
    }

    Some(left)
}
```

If `5 + 5 * 5` is parsed, the parser will generate an AST node that looks like this:

``` json
{
    "type": "BinaryExpression",
    "left": {
        "type": "BinaryExpression",
        "left": {
            "type": "LiteralExpression",
            "value": 5
        },
        "right": {
            "type": "LiteralExpression",
            "value": 5
        },
        "operator": "+"
    },
    "right": {
        "type": "LiteralExpression",
        "value": 5
    },
    "operator": "*"
}
```

According to the above AST, the expression `5 + 5 * 5` is parsed as `(5 + 5) * 5`
and results in `
