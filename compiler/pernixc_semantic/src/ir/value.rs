//! Contains the definition of [`Value`] and its variants.

use enum_as_inner::EnumAsInner;

use self::{literal::Literal, register::Register};
use crate::arena::ID;

pub mod literal;
pub mod register;

/// Represents a value in the IR.
///
/// # Register vs Literal
///
/// A value can be either a register or a literal. The simple distinction
/// between the two is that a register involves in having to compose one or
/// more values together, while a literal is a single-simple value.
///
/// For example, the expression `32` is a literal, while the expression
/// `a + 32` is a register.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner,
)]
#[allow(missing_docs)]
pub enum Value {
    Literal(Literal),
    Register(ID<Register>),
}
