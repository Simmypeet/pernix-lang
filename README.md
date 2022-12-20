# The Pernix Programming Language

Pernix is a system-level and statically typed programming language. It is highly 
inspired by Rust and C# and aims to be a safe and fast language. This repository
contains the compiler for the Pernix language. It is written in Rust and uses the
LLVM backend for code generation.

## Motivation

I've always wanted to have my programming language, combining various features from 
languages I've used before. Thus, I began implementing the idea by developing a 
compiler to achieve the goal. 

Moreover, I'll write articles explaining each component in the compiler to clarify my
thoughts and strengthen my understanding.

## The Ultimate Goal

The compiler was first created by using Rust and still is. The goal is to develop 
this compiler until it becomes self-hosted, meaning it's possible to make a compiler 
for the Pernix language using the Pernix language.

## Requirements

- **Rust** toolchain: Currently, the compiler is built using Rust 
 programming language.

 - **LLVM** toolchain (version 15.x): The compiler uses LLVM for code generation in the backend part
  and uses an LLVM linker (`ld`)  for linking object files to an executable. Please 
  look at the [llvm-sys](https://crates.io/crates/llvm-sys) crate for more 
  installation information.
