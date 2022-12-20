# The Lexer

The lexer is the first part of the compiler, also known as the scanner. In 
compiler theories and books the lexer resides in the **Syntactic Analysis**
phase of the compiler. Despite the intimidating name, the lexer is actually
quite simple. 

## Motivation

When working with a programming language, it's quite challenging to work
directly with the string of characters that make up the source code. It's more practical to group the characters into **tokens** and work with those instead.

Tokens are like words in a sentence. And word also has its category: 
Noun, Verb, Adjective, etc. Similarly, tokens have their category as well.
Tokens can be Identifiers, Keywords, and Punctuation.

Consider the following code:

``` py
def main():
    return 0
```

The code above can be broken down into tokens:

<table>
  <tr>
    <th>Token</th>
    <td><code>def</code></td>
    <td><code>main</code></td>
    <td><code>(</code></td>
    <td><code>)</code></td>
    <td><code>:</code></td>
    <td><code>return</code></td>
    <td><code>0</code></td>
  </tr>
  <tr>
    <th>Category</th>
    <td>Keyword</td>
    <td>Identifier</td>
    <td>Punctuation</td>
    <td>Punctuation</td>
    <td>Punctuation</td>
    <td>Keyword</td>
    <td>Literal</td>
  </tr>
</table>

---

## Implementation

The `pernix_lexer` crate holds the code related to the lexer. The lexer is 
implemented as a a struct that outputs one token at a time and advances the
state for the next token, kind of like an iterator.

The lexer will output a `Token` struct that holds the token category and the
lexem (the actual string of characters that make up the token).

> File: [`token.rs`](../compiler/pernix_lexer/src/token.rs)
``` rust
/// Represent an enumeration containing all keywords.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    Return,
    Let,
    Using,
    Namespace,
    Mutable,
    If,
    Else,
    While,
    Break,
    Continue,
}

/// Represent an enumeration containing all literal constant types.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LiteralConstantToken<'a> {
    Number {
        value: &'a str,
        literal_suffix: Option<&'a str>,
        is_decimal: bool,
    },
    Boolean(bool),
}

/// Represent an enumeration containing all patterns of tokens.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind<'a> {
    Identifier,
    Space,
    Comment,
    Keyword(Keyword),
    LiteralConstant(LiteralConstantToken<'a>),
    Punctuator(char),
    EndOfFile,
}

/// Represent a single token word; containing type of the token and its lexeme.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token<'a> {
    token_kind: TokenKind<'a>,
    position_range: Range<SourcePosition>,
    lexeme: &'a str,
}
```

In the code above, the `TokenKind` is an enumeration that contains all the
possible token categories; there are seven of them:

- `Identifier` is a string of characters used to name declarations such as variables, functions.
- `Space` is a sequence of whitespace characters.
- `Comment` is a sequence of characters ignored by the compiler.
- `Keyword` is a reserved word with a special meaning in the language.
- `LiteralConstant` is a constant value that is hardcoded into the source code.
- `Punctuator`, as its name implies, is a punctuation character.
- `EndOfFile` is a special token that indicates the end of the source code.

---

As mentioned earlier, the lexer is implemented as a struct. It contains an 
iterator over the source code, the current position in the source code, and 
a reference to the `SourceCode` struct.

> File: [`lib.rs`](../compiler/pernix_lexer/src/lib.rs)
``` rust
/// Represent a state-machine that lexes the source code and outputs a token
/// one at a time.
pub struct Lexer<'a> {
    source_code: &'a SourceCode,
    chars: Peekable<CharIndices<'a>>,
    current_position: SourcePosition,
}

impl<'a> Lexer<'a> {
    pub fn lex(&mut self) -> Result<Token<'a>, Error> {
        // ...
    }
    pub fn peek(&mut self) -> Result<Token<'a>, Error> {
        // ...
    }
}
```

The struct has a `lex` function that returns the token at the current position
and advances. As you might have guessed, many possible errors can occur
during the lexing process, such as invalid characters, unterminated strings, 
etc. Therefore, the `lex` function returns a `Result` type.

The `Err` variant of the `Result` type is an `Error` enum that contains all
lexical errors.
> File: [`error.rs`](../compiler/pernix_lexer/src/error.rs)
``` rust
/// Represent an enumeration containing all lexical errors.
#[derive(Debug, Clone)]
pub enum Error {
    InvalidCharacter {
        position: SourcePosition,
        character: char,
    },
    UnterminatedMultilineComment {
        multiline_comment_position: SourcePosition,
    },
}
``` 

### **Lex Function**

Lexing occurs in the `lex` function. The function first reads the character
at its current position and then advances forward. Based from the read character,
the function now can determine what kind of token it is and will continue to 
read characters until it reaches a character that is not part of the token.

For example:

- If the character is a whitespace, the function will read all consecutive
  whitespace characters and return a `Token` with the category `Space`.
- If the character is alphabetical, the function will read all consecutive
  alphabetical characters and return a `Token` with the category `Identifier`
  or `Keyword` if the string of characters is a reserved word.
- If the character is a number, the function will read all consecutive
  numbers and return a `Token` with the category `LiteralConstant`.
- If the character is a punctuation, the function will return a `Token` with
  the category `Punctuator`.

If the character is not part of any of the above categories, the function will
return an `Error` with the variant of `InvalidCharacter`.

``` rust
let source_code = "return 0;"

let mut lexer = Lexer::new(&source_code);
assert!(matches!(lexer.lex().token_kind(), TokenKind::Keyword(Keyword::Return)));
assert!(matches!(lexer.lex().token_kind(), TokenKind::LiteralConstant(_));
assert!(matches!(lexer.lex().token_kind(), TokenKind::Punctuator(';'));
```
---
