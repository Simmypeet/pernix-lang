//! Contains the definition of [`Instruction`] and its variants.

use std::{borrow::Cow, collections::HashMap, num::NonZero};

use enum_as_inner::EnumAsInner;
use pernixc_arena::{Key, ID};
use pernixc_semantic::term::{
    self,
    r#type::{Qualifier, Type},
};
use pernixc_source_file::Span;
use serde::{Deserialize, Serialize};

use super::{
    address::{self, Address},
    control_flow_graph::Block,
    scope::Scope,
    value::{
        register::{Assignment, Register},
        Value,
    },
    Values,
};
use crate::model::Transform;

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
    Serialize,
    Deserialize,
)]
pub struct UnconditionalJump<M: term::Model> {
    /// The target block of the jump.
    pub target: ID<Block<M>>,
}

/// Represents a jump to another block conditionally.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct ConditionalJump<M: term::Model> {
    /// The condition of the jump.
    pub condition: Value<M>,

    /// The block to jump to if the condition is true.
    pub true_target: ID<Block<M>>,

    /// The block to jump to if the condition is false.
    pub false_target: ID<Block<M>>,
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
    Serialize,
    Deserialize,
    EnumAsInner,
    derive_more::Display,
)]
#[allow(missing_docs)]
pub enum SwitchValue {
    Positive(u64),
    Negative(NonZero<u64>),
}

/// Represents a jump to another block based on a the matching of an integer
/// value.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SwitchJump<M: term::Model> {
    /// The integer value to match.
    pub integer: Value<M>,

    /// Mapping of the integer value to the target block to jump to.
    pub branches: HashMap<SwitchValue, ID<Block<M>>>,

    /// If none of the branches match, jump to this block.
    pub otherwise: Option<ID<Block<M>>>,
}

/// An enumeration containing all kinds of jump instructions.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[allow(missing_docs)]
pub enum Jump<M: term::Model> {
    Unconditional(UnconditionalJump<M>),
    Conditional(ConditionalJump<M>),
    Switch(SwitchJump<M>),
}

impl<M: term::Model> Jump<M> {
    /// Transforms the [`Jump`] to another model using the given transformer.
    #[allow(clippy::missing_errors_doc)]
    pub fn transform_model<T: Transform<Type<M>>>(
        self,
        transformer: &mut T,
    ) -> Result<Jump<T::Target>, T::Error> {
        Ok(match self {
            Self::Unconditional(jump) => {
                Jump::Unconditional(UnconditionalJump {
                    target: ID::from_index(jump.target.into_index()),
                })
            }

            Self::Conditional(jump) => Jump::Conditional(ConditionalJump {
                condition: jump.condition.transform_model(transformer)?,
                true_target: ID::from_index(jump.true_target.into_index()),
                false_target: ID::from_index(jump.false_target.into_index()),
            }),

            Self::Switch(jump) => Jump::Switch(SwitchJump {
                integer: jump.integer.transform_model(transformer)?,
                branches: jump
                    .branches
                    .into_iter()
                    .map(|(integer, target)| {
                        (integer, ID::from_index(target.into_index()))
                    })
                    .collect(),
                otherwise: jump
                    .otherwise
                    .map(|id| ID::from_index(id.into_index())),
            }),
        })
    }

    /// Returns the block IDs that this jump goes to.
    pub fn jump_targets(&self) -> Vec<ID<Block<M>>> {
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
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct Return<M: term::Model> {
    /// The value to return.
    pub value: Value<M>,

    /// The span where the return instruction is generated.
    #[serde(skip)]
    pub span: Option<Span>,
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
    Serialize,
    Deserialize,
)]
pub struct RegisterAssignment<M: term::Model> {
    /// The register that is being assigned.
    pub id: ID<Register<M>>,
}

/// An instruction that stores a value in memory.
///
/// This is instruction is typically translated from a `let` statement or an
/// assignment expression.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct Store<M: term::Model> {
    /// The address where the value will be stored.
    pub address: Address<M>,

    /// The value to store.
    pub value: Value<M>,

    /// The span where the store instruction is generated.
    #[serde(skip)]
    pub span: Option<Span>,
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
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct TuplePack<M: term::Model> {
    /// The address where the packed tuple elements will be stored.
    ///
    /// The address should have a type of tuple.
    pub store_address: Address<M>,

    /// The address to the tuple where the unpacked elements are located and
    /// will be packed to a new address by this instruction.
    ///
    /// The address should have a type of tuple.
    pub tuple_address: Address<M>,

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
    #[serde(skip)]
    pub packed_tuple_span: Option<Span>,
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
    Serialize,
    Deserialize,
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
    Serialize,
    Deserialize,
)]
pub struct ScopePop(pub ID<Scope>);

/// Invokes the `Drop::drop` on the given address.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct Drop<M: term::Model> {
    /// The address to drop.
    pub address: Address<M>,
}

/// An instruction that drops the unpacked elements of a tuple.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct DropUnpackTuple<M: term::Model> {
    /// The address of the tuple that contains the unpacked elements.
    pub tuple_address: Address<M>,

    /// The number of elements in the tuple before the unpacked elements.
    pub before_unpacked_element_count: usize,

    /// The number of elements in the tuple after the unpacked elements.
    pub after_unpacked_element_count: usize,
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
    Serialize,
    Deserialize,
)]
pub struct RegisterDiscard<M: term::Model> {
    /// The register that is being dropped.
    pub id: ID<Register<M>>,
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
    Serialize,
    Deserialize,
    EnumAsInner,
)]
#[allow(missing_docs)]
pub enum Instruction<M: term::Model> {
    Store(Store<M>),
    RegisterAssignment(RegisterAssignment<M>),
    RegisterDiscard(RegisterDiscard<M>),
    TuplePack(TuplePack<M>),
    ScopePush(ScopePush),
    ScopePop(ScopePop),
    DropUnpackTuple(DropUnpackTuple<M>),
    Drop(Drop<M>),
}

impl<M: term::Model> Instruction<M> {
    /// Transforms the [`Instruction`] to another model using the given
    /// transformer.
    #[allow(clippy::missing_errors_doc)]
    pub fn transform_model<T: Transform<Type<M>>>(
        self,
        transformer: &mut T,
    ) -> Result<Instruction<T::Target>, T::Error> {
        Ok(match self {
            Self::Store(store) => Instruction::Store(Store {
                address: store.address.transform_model(transformer)?,
                value: store.value.transform_model(transformer)?,
                span: store.span,
            }),
            Self::RegisterAssignment(register_assignment) => {
                Instruction::RegisterAssignment(RegisterAssignment {
                    id: ID::from_index(register_assignment.id.into_index()),
                })
            }
            Self::RegisterDiscard(register_discard) => {
                Instruction::RegisterDiscard(RegisterDiscard {
                    id: ID::from_index(register_discard.id.into_index()),
                })
            }
            Self::TuplePack(tuple_pack) => Instruction::TuplePack(TuplePack {
                store_address: tuple_pack
                    .store_address
                    .transform_model(transformer)?,
                tuple_address: tuple_pack
                    .tuple_address
                    .transform_model(transformer)?,
                before_packed_element_count: tuple_pack
                    .before_packed_element_count,
                after_packed_element_count: tuple_pack
                    .after_packed_element_count,
                packed_tuple_span: tuple_pack.packed_tuple_span,
            }),
            Self::ScopePush(scope_push) => {
                Instruction::ScopePush(ScopePush(scope_push.0))
            }
            Self::ScopePop(scope_pop) => {
                Instruction::ScopePop(ScopePop(scope_pop.0))
            }
            Self::Drop(drop) => Instruction::Drop(Drop {
                address: drop.address.transform_model(transformer)?,
            }),
            Self::DropUnpackTuple(drop_unpack_tuple) => {
                Instruction::DropUnpackTuple(DropUnpackTuple {
                    tuple_address: drop_unpack_tuple
                        .tuple_address
                        .transform_model(transformer)?,
                    before_unpacked_element_count: drop_unpack_tuple
                        .before_unpacked_element_count,
                    after_unpacked_element_count: drop_unpack_tuple
                        .after_unpacked_element_count,
                })
            }
        })
    }
}

/// An enumeration containing all the possible terminators.
///
/// Terminators are instructions that change the control flow of the program.
/// Either they move to another block or they return from the function.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, EnumAsInner)]
#[allow(missing_docs)]
pub enum Terminator<M: term::Model> {
    Jump(Jump<M>),
    Return(Return<M>),

    /// Aborts the program.
    Panic,
}

impl<M: term::Model> Terminator<M> {
    /// Transforms the [`Terminator`] to another model using the given
    /// transformer.
    #[allow(clippy::missing_errors_doc)]
    pub fn transform_model<T: Transform<Type<M>>>(
        self,
        transformer: &mut T,
    ) -> Result<Terminator<T::Target>, T::Error> {
        Ok(match self {
            Self::Jump(jump) => {
                Terminator::Jump(jump.transform_model(transformer)?)
            }
            Self::Return(r#return) => Terminator::Return(Return {
                value: r#return.value.transform_model(transformer)?,
                span: r#return.span,
            }),
            Self::Panic => Terminator::Panic,
        })
    }
}

/// Represents a read access to a value. The instruction that reads the value
/// are `Load`, `Borrow`, `VariantNumber`, and etc.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Read {
    /// The qualifier that the value is read with. Mostly, this will be
    /// [`Qualifier::Immutable`], however, it can be [`Qualifier::Mutable`] if
    /// the value is borrowed mutably.
    pub qualifier: Qualifier,

    /// The span where the read access is made.
    pub span: Option<Span>,
}

/// Represents how a particular address is accessed in an instruction.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum AccessMode {
    Read(Read),

    /// The address is written to.
    Write(Option<Span>),
}

impl AccessMode {
    /// Gets the span where the access is made.
    #[must_use]
    pub const fn span(&self) -> Option<&Span> {
        match self {
            Self::Read(read) => read.span.as_ref(),
            Self::Write(span) => span.as_ref(),
        }
    }

    /// Converts the access mode to a span.
    #[must_use]
    pub fn into_span(self) -> Option<Span> {
        match self {
            Self::Read(read) => read.span,
            Self::Write(span) => span,
        }
    }
}

/// Represents how a particular address is accessed in an instruction.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum AccessKind {
    /// The access is made to any instructions other than `Drop` instructions.
    Normal(AccessMode),

    /// The access is made in the drop instruction.
    Drop,
}

impl<M: term::Model> Instruction<M> {
    /// Gets the address that was used during the execution of this instruction.
    ///
    /// This does not include the address that might be used during the drop.
    ///
    /// Returns `None` if the instruction was a register assignment and the
    /// register ID could not be found in the given `values`.
    #[allow(clippy::type_complexity, clippy::missing_errors_doc)]
    pub fn get_access_address<'a>(
        &'a self,
        values: &'a Values<M>,
    ) -> Vec<(Cow<'a, Address<M>>, AccessKind)> {
        match self {
            Self::Store(store) => vec![(
                Cow::Borrowed(&store.address),
                AccessKind::Normal(AccessMode::Write(store.span.clone())),
            )],
            Self::RegisterAssignment(register_assignment) => {
                let register = &values.registers[register_assignment.id];

                match &register.assignment {
                    Assignment::Load(load) => vec![(
                        Cow::Borrowed(&load.address),
                        AccessKind::Normal(AccessMode::Read(Read {
                            qualifier: Qualifier::Immutable,
                            span: register.span.clone(),
                        })),
                    )],
                    Assignment::Borrow(borrow) => vec![(
                        Cow::Borrowed(&borrow.address),
                        AccessKind::Normal(AccessMode::Read(Read {
                            qualifier: borrow.qualifier,
                            span: register.span.clone(),
                        })),
                    )],
                    Assignment::VariantNumber(variant_number) => vec![(
                        Cow::Borrowed(&variant_number.address),
                        AccessKind::Normal(AccessMode::Read(Read {
                            qualifier: Qualifier::Immutable,
                            span: register.span.clone(),
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
                        AccessKind::Normal(AccessMode::Read(Read {
                            qualifier: Qualifier::Immutable,
                            span: tuple_pack.packed_tuple_span.clone(),
                        })),
                    ),
                    (
                        Cow::Borrowed(&tuple_pack.store_address),
                        AccessKind::Normal(AccessMode::Write(
                            tuple_pack.packed_tuple_span.clone(),
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
