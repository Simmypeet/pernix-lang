//! Contains code related to the high-level intermediate representation of Pernix.

use getset::Getters;

/// The high-level intermediate representation of Pernix.
///
/// The high-level intermediate representation (HIR) is the first representation built by the
/// compiler. It primarily performs type checking, type inference, function resolution, and name
/// resolution. [`ControlFlowGraph`] is used to represent the control flow of a function, allowing
/// the compiler to perform data flow analysis and other optimizations.
///
/// # What HIR Does
/// - Type Binding: all the values in the HIR must have a type bound to them. This is done by
///   performing type inference and type checking.
/// - Name Resolution: all the identifiers found in the source code will be resolved to their
///   cooresponding symbols in the table and assigned with an `ID`.
/// - Function Resolution: all the function calls will be resolved to their correct overloads.
/// - Generics Monomorphization: all the generics symbols used in the source code will be
///   monomorphized to their concrete symbols.
/// - Control Flow Analysis: making sure that all the flow paths are valid such as making sure that
///   all the paths in a function returns a value.
///
/// # What HIR Does Not Do
/// - Implicit Operations: the HIR does not perform any implicit operations such as calling
///   destructors, calling copy constructors, or calling move constructors. These operations are
///   done in later stages of the compiler.
/// - Optimization: the HIR does not perform any optimizations such as constant folding, dead code
///   elimination, or inlining. These optimizations are done in later stages of the compiler.
#[derive(Debug, Copy, Clone, Getters)]
pub struct HIR {}
