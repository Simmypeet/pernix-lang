//! Contains the definition of [`Instruction`] and its variants.

use std::collections::HashMap;

use enum_as_inner::EnumAsInner;

use super::{
    address::Address,
    alloca::Alloca,
    control_flow_graph::Block,
    scope::Scope,
    value::{register::Register, Value},
};
use crate::{arena::ID, type_system::model::Model};

/// An enumeration containing all the possible sources of an unconditional jump.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UnconditionalJumpSource {
    /// The jump is generated by an `express` expression.
    Express,

    /// The jump is generated by a `continue` statement.
    Continue,

    /// The jump is generated by a `break` statement.
    Break,

    /// The jump is generated implicitly by the compiler.
    ImplicitlyGenerated,
}

/// Represents a jump to another block unconditionally.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UnconditionalJump<M: Model> {
    /// The target block of the jump.
    pub target: ID<Block<M>>,
}

/// Represents a jump to another block conditionally.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConditionalJump<M: Model> {
    /// The condition of the jump.
    pub condition: Value<M>,

    /// The block to jump to if the condition is true.
    pub true_target: ID<Block<M>>,

    /// The block to jump to if the condition is false.
    pub false_target: ID<Block<M>>,
}

/// Represents a jump to another block based on a the matching of an integer
/// value.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SelectJump<M: Model> {
    /// The integer value to match.
    pub integer: Value<M>,

    /// Mapping of the integer value to the target block to jump to.
    pub branches: HashMap<i128, ID<Block<M>>>,

    /// If none of the branches match, jump to this block.
    pub otherwise: Option<ID<Block<M>>>,
}

/// An enumeration containing all kinds of jump instructions.
#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(missing_docs)]
pub enum Jump<M: Model> {
    Unconditional(UnconditionalJump<M>),
    Conditional(ConditionalJump<M>),
    Select(SelectJump<M>),
}

impl<M: Model> Jump<M> {
    /// Returns the block IDs that this jump goes to.
    pub fn jump_targets(&self) -> Vec<ID<Block<M>>> {
        match self {
            Jump::Unconditional(jump) => vec![jump.target],
            Jump::Conditional(jump) => {
                vec![jump.true_target, jump.false_target]
            }
            Jump::Select(jump) => {
                let mut targets =
                    jump.branches.values().copied().collect::<Vec<_>>();
                if let Some(otherwise) = jump.otherwise {
                    targets.push(otherwise);
                }
                targets
            }
        }
    }
}

/// Represents a return instruction.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Return<M: Model> {
    /// The value to return.
    pub value: Value<M>,
}

/// Represents an assignment of a register.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RegisterAssignment<M: Model> {
    /// The register that is being assigned.
    pub id: ID<Register<M>>,
}

/// An instruction that stores a value in memory.
///
/// This is instruction is typically translated from a `let` statement or an
/// assignment expression.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Store<M: Model> {
    /// The address where the value will be stored.
    pub address: Address<M>,

    /// The value to store.
    pub value: Value<M>,
}

/// An instruction specifying when an `alloca` is declared.
///
/// This is kind of a pseudo-instruction, as it doesn't actually do anything.
/// It is used to keep track of when an `alloca` is declared so that the ir
/// knows when to drop it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AllocaDeclaration<M: Model> { /// The ID of the `alloca` that is being declared.
    pub id: ID<Alloca<M>>,
}

/// An instructions that packs the unpacked elements of a tuple into a packed
/// element.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TuplePack<M: Model> {
    /// The address where the unpacked tuple elements will be stored.
    ///
    /// The address should have a type of tuple.
    pub store_address: Address<M>,

    /// The address to the tuple where the unpacked elements are stored.
    ///
    /// The address should have a type of tuple.
    pub tuple_address: Address<M>,

    /// The offset in the `store_address` to start storing the packed element.
    pub starting_offset: usize,

    /// The number of elements in the tuple before the packed element.
    ///
    /// This is used for calculating which elements are packed and which are
    /// not.
    pub before_packed_element_count: usize,

    /// The number of elements in the tuple after the packed element.
    ///
    /// This is used for calculating which elements are packed and which are
    /// not.
    pub after_packed_element_count: usize,
}

/// An instruction that pushes a new scope onto the scope stack.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ScopePush(pub ID<Scope>);

/// An instruction that pops a scope from the scope stack.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ScopePop(pub ID<Scope>);

/// Invokes the `Drop::drop` on the given address.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Drop<M: Model> {
    /// The address to drop.
    pub address: Address<M>,
}

/// An enumeration containig all the basic instructions.
///
/// The basic instructions are the instructions that have no effect on the
/// control flow of the program -- return instructions and jumps do change the
/// flow of the program, so they are not considered basic.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum Instruction<M: Model> {
    Store(Store<M>),
    RegisterAssignment(RegisterAssignment<M>),
    AllocaDeclaration(AllocaDeclaration<M>),
    TuplePack(TuplePack<M>),
    ScopePush(ScopePush),
    ScopePop(ScopePop),
    Drop(Drop<M>),
}

/// An enumeration containing all the possible terminators.
///
/// Terminators are instructions that change the control flow of the program.
/// Either they move to another block or they return from the function.
#[derive(Debug, Clone, PartialEq, Eq, EnumAsInner)]
#[allow(missing_docs)]
pub enum Terminator<M: Model> {
    Jump(Jump<M>),
    Return(Return<M>),

    /// Aborts the program.
    Panic,
}
