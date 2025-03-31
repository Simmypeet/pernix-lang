# Pernix Programming Language ![GitHub Release](https://img.shields.io/github/v/release/Simmypeet/pernix-lang?include_prereleases)

**Pernix** is a system-level programming language designed to provide memory
safety and a robust type system, similar to Rust.

## Example

![example](./docs/example.png)

## Try It Out

Download the latest releases from the GitHub releases page, or build it from
source.

To use the compiler, run the following command:

```bash
pernixc run <file>.pnx
```

> Currently, the compiler compiles the program to the object file, and then
> links it to the executable using `clang`. Therefore, you need to have
> `clang` installed on your system or you can manually link the object file
> using `pernixc build <file>.pnx --emit=obj -o <file>.o` and then
> link it to the executable using appropriate linker command.

## Current Status

Pernix is currently in the early stages of development. The language is largely
a clone of Rust, but with a few differences 😄.

## Planned Features

These are some of the features that are planned for Pernix to be a fully
featured system-level programming language: (sorted by priority from high to
low)

-   Fully functional constant evaluation
-   Higher-order functions
-   Effect system
-   Standard library
-   Better LSP support
-   Documentation
