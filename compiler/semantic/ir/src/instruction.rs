//! Contains the definition of [`Instruction`] and its variants.

use std::{borrow::Cow, num::NonZero};

use enum_as_inner::EnumAsInner;
use pernixc_arena::ID;
use pernixc_hash::HashMap;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_semantic_element::variance::Variance;
use pernixc_term::r#type::Qualifier;
use pernixc_type_system::{OverflowError, normalizer::Normalizer};
use qbice::{Decode, Encode, StableHash};

use super::{
    Values,
    address::{self, Address},
    control_flow_graph::Block,
    scope::Scope,
    value::{
        Value,
        register::{Assignment, Register},
    },
};
use crate::{
    resolution_visitor::{
        self, Abort, MutableResolutionVisitor, ResolutionVisitor,
    },
    value::{Environment, TypeOf, register::subtype::Subtype},
};

macro_rules! dispatch_jump {
    ($self:expr, $visitor:expr, $method:ident) => {{
        match $self {
            Jump::Unconditional(_) => {}
            Jump::Conditional(conditional) => {
                conditional.$method($visitor).await?;
            }
            Jump::Switch(switch) => switch.$method($visitor).await?,
        }
        Ok(())
    }};
}

macro_rules! dispatch_instruction {
    ($self:expr, $visitor:expr, $method:ident) => {{
        match $self {
            Instruction::Store(store) => store.$method($visitor).await?,
            Instruction::TuplePack(tuple_pack) => {
                tuple_pack.$method($visitor).await?;
            }
            Instruction::DropUnpackTuple(drop) => {
                drop.$method($visitor).await?
            }
            Instruction::Drop(drop) => drop.$method($visitor).await?,

            Instruction::RegisterAssignment(_)
            | Instruction::RegisterDiscard(_)
            | Instruction::ScopePush(_)
            | Instruction::ScopePop(_) => {}
        }
        Ok(())
    }};
}

macro_rules! dispatch_terminator {
    ($self:expr, $visitor:expr, $method:ident) => {{
        match $self {
            Terminator::Jump(jump) => jump.$method($visitor).await?,
            Terminator::Return(ret) => ret.$method($visitor).await?,
            Terminator::Panic => {}
        }
        Ok(())
    }};
}

/// Represents a jump to another block unconditionally.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
)]
pub struct UnconditionalJump {
    /// The target block of the jump.
    pub target: ID<Block>,
}

/// Represents a jump to another block conditionally.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
)]
pub struct ConditionalJump {
    /// The condition of the jump.
    pub condition: Value,

    /// The block to jump to if the condition is true.
    pub true_target: ID<Block>,

    /// The block to jump to if the condition is false.
    pub false_target: ID<Block>,
}

impl ConditionalJump {
    async fn accept<T: ResolutionVisitor>(
        &self,
        visitor: &mut T,
    ) -> Result<(), Abort> {
        if let Value::Literal(literal) = &self.condition {
            literal.accept(visitor).await?;
        }
        Ok(())
    }

    async fn accept_mut<T: MutableResolutionVisitor>(
        &mut self,
        visitor: &mut T,
    ) -> Result<(), Abort> {
        if let Value::Literal(literal) = &mut self.condition {
            literal.accept_mut(visitor).await?;
        }
        Ok(())
    }
}

/// Switch value for the switch jump.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    EnumAsInner,
    StableHash,
    derive_more::Display,
)]
#[allow(missing_docs)]
pub enum SwitchValue {
    Positive(u64),
    Negative(NonZero<u64>),
}

/// Represents a jump to another block based on a the matching of an integer
/// value.
#[derive(Debug, Clone, PartialEq, Eq, Encode, Decode, StableHash)]
pub struct SwitchJump {
    /// The integer value to match.
    pub integer: Value,

    /// Mapping of the integer value to the target block to jump to.
    pub branches: HashMap<SwitchValue, ID<Block>>,

    /// If none of the branches match, jump to this block.
    pub otherwise: Option<ID<Block>>,
}

impl SwitchJump {
    async fn accept<T: ResolutionVisitor>(
        &self,
        visitor: &mut T,
    ) -> Result<(), Abort> {
        if let Value::Literal(literal) = &self.integer {
            literal.accept(visitor).await?;
        }
        Ok(())
    }

    async fn accept_mut<T: MutableResolutionVisitor>(
        &mut self,
        visitor: &mut T,
    ) -> Result<(), Abort> {
        if let Value::Literal(literal) = &mut self.integer {
            literal.accept_mut(visitor).await?;
        }
        Ok(())
    }
}

/// An enumeration containing all kinds of jump instructions.
#[derive(Debug, Clone, PartialEq, Eq, Encode, Decode, StableHash)]
#[allow(missing_docs)]
pub enum Jump {
    Unconditional(UnconditionalJump),
    Conditional(ConditionalJump),
    Switch(SwitchJump),
}

impl Jump {
    async fn accept<T: ResolutionVisitor>(
        &self,
        visitor: &mut T,
    ) -> Result<(), Abort> {
        dispatch_jump!(self, visitor, accept)
    }

    async fn accept_mut<T: MutableResolutionVisitor>(
        &mut self,
        visitor: &mut T,
    ) -> Result<(), Abort> {
        dispatch_jump!(self, visitor, accept_mut)
    }

    /// Returns the block IDs that this jump goes to.
    #[must_use]
    pub fn jump_targets(&self) -> Vec<ID<Block>> {
        match self {
            Self::Unconditional(jump) => vec![jump.target],
            Self::Conditional(jump) => {
                vec![jump.true_target, jump.false_target]
            }
            Self::Switch(jump) => {
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
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
)]
pub struct Return {
    /// The value to return.
    pub value: Value,

    /// The span where the return instruction is generated.
    pub span: RelativeSpan,
}

impl Return {
    /// Visits the return value using the given visitor.
    async fn accept<T: ResolutionVisitor>(
        &self,
        visitor: &mut T,
    ) -> Result<(), Abort> {
        if let Value::Literal(literal) = &self.value {
            literal.accept(visitor).await?;
        }
        Ok(())
    }

    /// Transforms the return value using the given visitor.
    async fn accept_mut<T: MutableResolutionVisitor>(
        &mut self,
        visitor: &mut T,
    ) -> Result<(), Abort> {
        if let Value::Literal(literal) = &mut self.value {
            literal.accept_mut(visitor).await?;
        }
        Ok(())
    }
}

/// Represents an assignment of a register.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
)]
pub struct RegisterAssignment {
    /// The register that is being assigned.
    pub id: ID<Register>,
}

/// An instruction that stores a value in memory.
///
/// This is instruction is typically translated from a `let` statement or an
/// assignment expression.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
)]
pub struct Store {
    /// The address where the value will be stored.
    pub address: Address,

    /// The value to store.
    pub value: Value,

    /// The span where the store instruction is generated.
    pub span: RelativeSpan,
}

impl Store {
    /// Checks subtyping for this store instruction, ensuring the value's type
    /// is a subtype of the address's type.
    pub async fn subtypes<N: Normalizer>(
        &self,
        values: &Values,
        environment: &Environment<'_, N>,
    ) -> Result<Subtype, OverflowError> {
        let mut constraints =
            pernixc_type_system::constraints::Constraints::default();

        let val_succeeded = values.type_of(&self.value, environment).await?;
        constraints.extend(val_succeeded.constraints.into_iter());
        let val_type = val_succeeded.result;

        let addr_succeeded = values.type_of(&self.address, environment).await?;
        constraints.extend(addr_succeeded.constraints.into_iter());
        let addr_type = addr_succeeded.result;

        let result = environment
            .type_environment
            .subtypes(addr_type.clone(), val_type.clone(), Variance::Covariant)
            .await?;

        let Some(succeeded) = result else {
            return Ok(Subtype::Incompatible {
                found_type: val_type,
                expected_type: addr_type,
            });
        };

        if !succeeded.result.forall_lifetime_errors.is_empty() {
            return Ok(Subtype::ForallLifetimeError {
                found_type: val_type,
                expected_type: addr_type,
            });
        }

        constraints.extend(succeeded.constraints.iter().cloned());

        Ok(Subtype::Succeeded(constraints))
    }

    async fn accept<T: ResolutionVisitor>(
        &self,
        visitor: &mut T,
    ) -> Result<(), Abort> {
        if let Value::Literal(literal) = &self.value {
            literal.accept(visitor).await?;
        }

        self.address.accept(visitor).await?;
        Ok(())
    }

    async fn accept_mut<T: MutableResolutionVisitor>(
        &mut self,
        visitor: &mut T,
    ) -> Result<(), Abort> {
        if let Value::Literal(literal) = &mut self.value {
            literal.accept_mut(visitor).await?;
        }

        self.address.accept_mut(visitor).await?;
        Ok(())
    }
}

/// An instructions that packs the unpacked elements of a tuple into a packed
/// element.
///
/// When it appears in the IR, it mostly means that the type of the tuple being
/// unpacked is not definite (unknown) at a time, so the this *special*
/// instruction is used to represent the packed tuple.
///
/// For example, consider the following code:
/// ```pnx
/// public function main[T](var: (int32, ...T, int32))
/// where
///     tuple T
/// {
///     // ...
///     let (a, ...b, c) = var;
///     // ...
/// }
/// ```
///
/// This code will generate a tuple pack instruction for the pattern matching
/// at the `...b` part.
///
/// However, in case of a known tuple type, the tuple pack instruction is not
/// generated.
///
/// ```pnx
/// public function main(var: (int32, int32, float32, int32)) {
///     // ...
///     let (a, ...b, d) = var;
///     // ...
/// }
/// ```
///
/// In this case, the tuple pack instruction is not generated; instead, normal
/// `Store` instruction are generated.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
)]
pub struct TuplePack {
    /// The address where the packed tuple elements will be stored.
    ///
    /// The address should have a type of tuple.
    pub store_address: Address,

    /// The address to the tuple where the unpacked elements are located and
    /// will be packed to a new address by this instruction.
    ///
    /// The address should have a type of tuple.
    pub tuple_address: Address,

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

    /// The span to the packed tuple pattern.
    pub packed_tuple_span: RelativeSpan,
}

impl TuplePack {
    async fn accept<T: ResolutionVisitor>(
        &self,
        visitor: &mut T,
    ) -> Result<(), Abort> {
        self.store_address.accept(visitor).await?;
        self.tuple_address.accept(visitor).await?;
        Ok(())
    }

    async fn accept_mut<T: MutableResolutionVisitor>(
        &mut self,
        visitor: &mut T,
    ) -> Result<(), Abort> {
        self.store_address.accept_mut(visitor).await?;
        self.tuple_address.accept_mut(visitor).await?;
        Ok(())
    }
}

/// An instruction that pushes a new scope onto the scope stack.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
)]
pub struct ScopePush(pub ID<Scope>);

/// An instruction that pops a scope from the scope stack.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
)]
pub struct ScopePop(pub ID<Scope>);

/// Invokes the `Drop::drop` on the given address.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
)]
pub struct Drop {
    /// The address to drop.
    pub address: Address,
}

impl Drop {
    async fn accept<T: ResolutionVisitor>(
        &self,
        visitor: &mut T,
    ) -> Result<(), Abort> {
        self.address.accept(visitor).await?;
        Ok(())
    }

    async fn accept_mut<T: MutableResolutionVisitor>(
        &mut self,
        visitor: &mut T,
    ) -> Result<(), Abort> {
        self.address.accept_mut(visitor).await?;
        Ok(())
    }
}

/// An instruction that drops the unpacked elements of a tuple.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
)]
pub struct DropUnpackTuple {
    /// The address of the tuple that contains the unpacked elements.
    pub tuple_address: Address,

    /// The number of elements in the tuple before the unpacked elements.
    pub before_unpacked_element_count: usize,

    /// The number of elements in the tuple after the unpacked elements.
    pub after_unpacked_element_count: usize,
}

impl DropUnpackTuple {
    async fn accept<T: ResolutionVisitor>(
        &self,
        visitor: &mut T,
    ) -> Result<(), Abort> {
        self.tuple_address.accept(visitor).await?;
        Ok(())
    }

    async fn accept_mut<T: MutableResolutionVisitor>(
        &mut self,
        visitor: &mut T,
    ) -> Result<(), Abort> {
        self.tuple_address.accept_mut(visitor).await?;
        Ok(())
    }
}

/// An instruction that invokes `Drop::drop` on a value in the register.
///
/// This is a pseudo-instruction.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
)]
pub struct RegisterDiscard {
    /// The register that is being dropped.
    pub id: ID<Register>,
}

/// An enumeration containig all the basic instructions.
///
/// The basic instructions are the instructions that have no effect on the
/// control flow of the program -- return instructions and jumps do change the
/// flow of the program, so they are not considered basic.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
    EnumAsInner,
)]
#[allow(missing_docs)]
pub enum Instruction {
    Store(Store),
    RegisterAssignment(RegisterAssignment),
    RegisterDiscard(RegisterDiscard),
    TuplePack(TuplePack),
    ScopePush(ScopePush),
    ScopePop(ScopePop),
    DropUnpackTuple(DropUnpackTuple),
    Drop(Drop),
}

impl resolution_visitor::MutableResolutionVisitable for Instruction {
    async fn accept_mut<T: MutableResolutionVisitor>(
        &mut self,
        visitor: &mut T,
    ) -> Result<(), Abort> {
        dispatch_instruction!(self, visitor, accept_mut)
    }
}

impl resolution_visitor::ResolutionVisitable for Instruction {
    async fn accept<T: ResolutionVisitor>(
        &self,
        visitor: &mut T,
    ) -> Result<(), Abort> {
        dispatch_instruction!(self, visitor, accept)
    }
}

/// An enumeration containing all the possible terminators.
///
/// Terminators are instructions that change the control flow of the program.
/// Either they move to another block or they return from the function.
#[derive(
    Debug, Clone, PartialEq, Eq, Encode, Decode, EnumAsInner, StableHash,
)]
#[allow(missing_docs)]
pub enum Terminator {
    Jump(Jump),
    Return(Return),

    /// Aborts the program.
    Panic,
}

impl resolution_visitor::MutableResolutionVisitable for Terminator {
    async fn accept_mut<T: MutableResolutionVisitor>(
        &mut self,
        visitor: &mut T,
    ) -> Result<(), Abort> {
        dispatch_terminator!(self, visitor, accept_mut)
    }
}

impl resolution_visitor::ResolutionVisitable for Terminator {
    async fn accept<T: ResolutionVisitor>(
        &self,
        visitor: &mut T,
    ) -> Result<(), Abort> {
        dispatch_terminator!(self, visitor, accept)
    }
}

/// Represents a read access to a value. The instruction that reads the value
/// are `Load`, `Borrow`, `VariantNumber`, and etc.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, StableHash,
)]
pub struct Read {
    /// The qualifier that the value is read with. Mostly, this will be
    /// [`Qualifier::Immutable`], however, it can be [`Qualifier::Mutable`] if
    /// the value is borrowed mutably.
    pub qualifier: Qualifier,

    /// The span where the read access is made.
    pub span: RelativeSpan,
}

/// Represents how a particular address is accessed in an instruction.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, StableHash,
)]
#[allow(missing_docs)]
pub enum AccessMode {
    Read(Read),
    /// The value is loaded/moved from the address.
    Load(RelativeSpan),

    /// The address is written to.
    Write(RelativeSpan),
}

impl AccessMode {
    /// Gets the span where the access is made.
    #[must_use]
    pub const fn span(&self) -> &RelativeSpan {
        match self {
            Self::Read(read) => &read.span,
            Self::Load(span) | Self::Write(span) => span,
        }
    }

    /// Converts the access mode to a span.
    #[must_use]
    pub const fn into_span(self) -> RelativeSpan {
        match self {
            Self::Read(read) => read.span,
            Self::Load(span) | Self::Write(span) => span,
        }
    }
}

/// Represents how a particular address is accessed in an instruction.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumAsInner,
    StableHash,
)]
pub enum AccessKind {
    /// The access is made to any instructions other than `Drop` instructions.
    Normal(AccessMode),

    /// The access is made in the drop instruction.
    Drop,
}

impl Instruction {
    /// Gets the address that was used during the execution of this instruction.
    ///
    /// This does not include the address that might be used during the drop.
    ///
    /// Returns `None` if the instruction was a register assignment and the
    /// register ID could not be found in the given `values`.
    #[allow(clippy::type_complexity, clippy::missing_errors_doc)]
    #[must_use]
    pub fn get_access_address<'a>(
        &'a self,
        values: &'a Values,
    ) -> Vec<(Cow<'a, Address>, AccessKind)> {
        match self {
            Self::Store(store) => vec![(
                Cow::Borrowed(&store.address),
                AccessKind::Normal(AccessMode::Write(store.span)),
            )],
            Self::RegisterAssignment(register_assignment) => {
                let register = &values.registers[register_assignment.id];

                match &register.assignment {
                    Assignment::Load(load) => vec![(
                        Cow::Borrowed(load.address()),
                        AccessKind::Normal(AccessMode::Load(register.span)),
                    )],
                    Assignment::Borrow(borrow) => vec![(
                        Cow::Borrowed(&borrow.address),
                        AccessKind::Normal(AccessMode::Read(Read {
                            qualifier: borrow.qualifier,
                            span: register.span,
                        })),
                    )],
                    Assignment::VariantNumber(variant_number) => vec![(
                        Cow::Borrowed(&variant_number.address),
                        AccessKind::Normal(AccessMode::Read(Read {
                            qualifier: Qualifier::Immutable,
                            span: register.span,
                        })),
                    )],

                    Assignment::Prefix(_)
                    | Assignment::Struct(_)
                    | Assignment::Variant(_)
                    | Assignment::FunctionCall(_)
                    | Assignment::Binary(_)
                    | Assignment::Array(_)
                    | Assignment::Phi(_)
                    | Assignment::Cast(_)
                    | Assignment::Do(_)
                    | Assignment::ResumeCall(_)
                    | Assignment::Tuple(_) => Vec::new(),
                }
            }

            Self::TuplePack(tuple_pack) => {
                let vec = vec![
                    (
                        Cow::Owned(Address::Tuple(address::Tuple {
                            tuple_address: Box::new(
                                tuple_pack.tuple_address.clone(),
                            ),
                            offset: address::Offset::Unpacked,
                        })),
                        AccessKind::Normal(AccessMode::Load(
                            tuple_pack.packed_tuple_span,
                        )),
                    ),
                    (
                        Cow::Borrowed(&tuple_pack.store_address),
                        AccessKind::Normal(AccessMode::Write(
                            tuple_pack.packed_tuple_span,
                        )),
                    ),
                ];
                vec
            }

            Self::Drop(drop) => {
                vec![(Cow::Borrowed(&drop.address), AccessKind::Drop)]
            }

            Self::DropUnpackTuple(drop) => vec![(
                Cow::Owned(Address::Tuple(address::Tuple {
                    tuple_address: Box::new(drop.tuple_address.clone()),
                    offset: address::Offset::Unpacked,
                })),
                AccessKind::Drop,
            )],

            Self::ScopePush(_)
            | Self::ScopePop(_)
            | Self::RegisterDiscard(_) => Vec::new(),
        }
    }
}
