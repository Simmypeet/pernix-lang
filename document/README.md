# The Pernix Compiler

This folder serves as tutorial and documentation for the Pernix compiler.
It will describe  all the thought processes behind the compiler and how it works. It will illustrate each step of making the compiler in detail so that anyone can
follow along and make their compiler should they desire.

> Note: This is a work in progress. Everything is subject to change.

## Overview

Compilers are made of many different parts. The Pernix compiler is no other.
With modular design in mind, each compiler part will get its crate.
Finally, the compiler will be made by combining all the crates.

The compiler is powered by the [LLVM](https://llvm.org/) library similar to
many other compilers like [Rust](https://www.rust-lang.org/), [Swift](https://swift.org/), and [Kotlin](https://kotlinlang.org/). The most direct benefit of using LLVM is that it allows us to target many different platforms with the same code. This is a huge benefit for a compiler that is meant to be used on many different platforms.

---

## Table of Contents

- [The Lexer](01_TheLexer.md) - Dicussing the lexer and how it works. 