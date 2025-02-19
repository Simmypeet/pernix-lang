//! Contains the definition of the `Zst` type.

/// The error type for the `Zero-Sized Type` variant.
///
/// For example, when retrieving the LLVM type of Pernix's `()` type, the
/// function would return `Zst(())` as the result. This can enable niche
/// optimizations such as removing the allocation of the zero-sized type or
/// reduce instructions involving the zero-sized type to a no-op.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Zst;
