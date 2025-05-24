# Pernix Compiler Syntax Pipeline

This document outlines how the Pernix compiler processes syntax of the source
code. Processing syntax is the first step in the compilation process, and
its steps are similar to the familiar compilation steps of tokenizing and then
parsing.

However, the Pernix compiler have an intermediate step between these two steps,
which is called "fragmentation".

## Tokenizing

Tokenizing in Pernix functions similarly to every other programming language,
where the source code is read and split into tokens. The tokens are then
classified into different types, such as keywords, identifiers, literals,
operators, and punctuation.

## Fragmentation

Fragmentation (or any other name) is an additional step that groups tokens
into a single unit where they can also be nested inside each other. With this
step, the resulting structure is a tree of fragments, which will be called
"token tree" (not yet really a syntax tree).

For example, in the Pernix programming language, the tokens inside an enclosing
delimiter (like parentheses) are grouped into a single fragment. Also, since
the language is indentation-sensitive, the indentation level is also regcognized
in this step.

For example,

```text
a b
{
    c d
    ( e f )
    g h
    [ i j ]
    k l
}
m n
```

would be tokenized into the following tree:

```text
Root
| - a
| - b
| - Delimiter {}
|   | - c
|   | - d
|   | - Delimiter ()
|   |   | - e
|   |   | - f
|   | - g
|   | - h
|   | - Delimiter []
|   |   | - i
|   |   | - j
|   | - k
|   | - l
| - m
| - n
```

## Parsing

Parsing is the final step in the syntax processing pipeline. The syntax tree
representation is based on the "Red-Green Tree" by Roslyn compiler. The
