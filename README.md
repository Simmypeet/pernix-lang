# Pernix Lang

Pernix is a system-level and statically typed programming language. It is highly
inspired by Rust and C#, aims to be a safe and fast language. This repository 
contains the compiler for the Pernix language. It is written in Rust and uses 
[LLVM](https://llvm.org/) backend for code generation. 

## Overview

The compiler is made to be modular and extensible, composed of many different crates.
Each crate is responsible for a different part of the compiler. The crates are:

- [`pernix_project`](compiler/pernix_project/): contains the project configuration of
the language. It is responsible for taking the source code inputs.

- [`pernix_lexer`](compiler/pernix_lexer/): contains the code for lexing the source 
code into tokens (Lexical analysis).

- [`pernix_parser`](compiler/pernix_parser/): contains the code for parsing the 
tokens into an AST (Syntactic analysis).

- [`pernix_analyzer`](compiler/pernix_analyzer/): contains the code for analyzing the 
AST and performing semantic checks (Semantic analysis).

- [`pernix_codegen`](compiler/pernix_codegen/): contains the code for generating LLVM
IR from the AST.
- [`pernixc`](compiler/pernixc/): combines all the crates above to make the compiler 
(Driver).

---

The repository also includes the [`document`](document/README.md) directory, which 
serves as an educational material for making a compiler. 


