//! This module is responsible for building the mid-level intermediate representation from the
//! high-level representation.
//!
//! The MIR takes the HIR and lowers it into a more lower-level representation similar to that of
//! LLVM IR, to make it easier for the code generator to generate machine code via LLVM backend.
