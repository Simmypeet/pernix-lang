use std::{collections::hash_map::Entry, ops::Deref};

use enum_as_inner::EnumAsInner;
use getset::{CopyGetters, Getters};
use pernixc_arena::ID;
use pernixc_handler::Handler;
use pernixc_hash::HashMap;
use pernixc_ir::{
    address::{self, Address, Offset},
    instruction::{Drop, DropUnpackTuple, Instruction},
    scope,
    value::register::load,
};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::{runtime::executor::CyclicError, TrackedEngine};
use pernixc_semantic_element::{
    fields::{self, get_fields},
    variant::get_variant_associated_type,
};
use pernixc_symbol::kind::{get_kind, Kind};
use pernixc_target::Global;
use pernixc_term::{
    generic_arguments::Symbol,
    instantiation::get_instantiation,
    r#type::{self, Qualifier, Type},
    tuple,
};
use pernixc_type_system::{
    environment::Environment, normalizer::Normalizer, UnrecoverableError,
};

use crate::diagnostic::Diagnostic;

/// Contains the state of each field in the struct.
#[derive(Debug, Clone, PartialEq, Eq, Getters, CopyGetters)]
pub struct Struct {
    /// The state of each field in the struct.
    #[get = "pub"]
    states_by_field_id: HashMap<ID<fields::Field>, State>,

    #[get_copy = "pub"]
    struct_id: Global<pernixc_symbol::ID>,
}

#[derive(Debug, Clone, PartialEq, Eq, Getters, CopyGetters)]
pub struct TupleElement {
    #[get = "pub"]
    state: State,

    #[get_copy = "pub"]
    is_unpacked: bool,
}

/// Contains the state of each element in the tuple.
#[derive(Debug, Clone, PartialEq, Eq, Getters)]
pub struct Tuple {
    /// The state of each element in the tuple.
    #[get = "pub"]
    elements: Vec<TupleElement>,
}

/// Contains the state of the current active variant in the enum.
#[derive(Debug, Clone, PartialEq, Eq, Getters, CopyGetters)]
pub struct Enum {
    /// The state of the variant.
    #[get = "pub"]
    state: Box<State>,

    #[get_copy = "pub"]
    variant_id: Global<pernixc_symbol::ID>,
}

/// The state of value behind a mutable reference.
#[derive(Debug, Clone, PartialEq, Eq, Getters)]
pub struct MutableReference {
    #[get = "pub"]
    state: Box<State>,
}

/// Represents the state of the value in memory that is partially initialized
/// (e.g. a struct with some fields initialized).
#[derive(Debug, Clone, PartialEq, Eq, EnumAsInner)]
#[allow(missing_docs)]
pub enum Projection {
    Struct(Struct),
    Tuple(Tuple),
    Enum(Enum),
    MutableReference(MutableReference),
}

/// Represents a move operation that moved the value from memory.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Getters,
    CopyGetters,
)]
pub struct Move {
    /// The span to the source code that move the value.
    #[get = "pub"]
    span: RelativeSpan,

    /// The reason why the value was moved from the memory.
    #[get_copy = "pub"]
    purpose: load::Purpose,
}

/// Represents the state of the value in memory that is uninitialized.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Uninitialized {
    /// The span that last moved the value.
    latest_move: Option<Move>,

    /// Used to determine the order of moves. The higher the version, the
    /// later/newer the move.
    version: usize,
}

impl Uninitialized {
    /// Returns the latest accessor that moved the value.
    pub const fn latest_move(&self) -> Option<&Move> {
        self.latest_move.as_ref()
    }
}

/// Represents the state of the value in memory.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Initialized {
    /// The memory is initialized.
    True,

    /// The memory is not initialized, the associated value is the register
    /// that last moved the value.
    False(Uninitialized),
}

/// Represents the state of the value in the memory.
#[derive(Debug, Clone, PartialEq, Eq, EnumAsInner)]
#[allow(missing_docs)]
pub enum State {
    Total(Initialized),
    Projection(Projection),
}

fn try_simplify_to_uninitialized<'a>(
    iterator: impl IntoIterator<Item = &'a State>,
) -> Option<(&'a Move, usize)> {
    let mut iterator = iterator.into_iter();

    let mut current = iterator
        .next()
        .expect("should have at least one element")
        .as_total()
        .and_then(|x| x.as_false())
        .map(|x| (x.latest_move.as_ref().unwrap(), x.version))?;

    for state in iterator {
        let Some(next) = state
            .as_total()
            .and_then(|x| x.as_false())
            .map(|x| (x.latest_move.as_ref().unwrap(), x.version))
        else {
            // it's a mix of initialized and uninitialized, leave
            // it as is
            return None;
        };

        if next.1 < current.1 {
            current = next;
        }
    }

    Some(current)
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
enum LatestLoad<'a> {
    Uninitialized,
    Moved(&'a Move, usize),
}

/// Summary of the state.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Summary {
    /// The whole invariant is intact
    Initialized,

    /// The value has never been initialized.
    Uninitialized,

    /// The value has been moved out by some instructions
    Moved(Move),
}

/// Two memory contains the projection that aren't copmatible.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    thiserror::Error,
    displaydoc::Display,
)]
pub struct MemoryMergeError;

impl Memory {
    pub fn min_merge(mut self, other: &Self) -> Result<Self, MemoryMergeError> {
        if Self::min_internal(&mut self.state, &other.state, &mut self.version)
            .is_none()
        {
            return Err(MemoryMergeError);
        }

        Ok(self)
    }

    // returns `Some(true)` if there's some changes made
    #[allow(clippy::too_many_lines)]
    fn min_internal(
        self_state: &mut State,
        other_state: &State,
        version: &mut usize,
    ) -> Option<bool> {
        match (&mut *self_state, other_state) {
            (
                State::Total(Initialized::True),
                State::Projection(projection),
            ) => {
                if let Some(LatestLoad::Moved(_, other_version)) =
                    other_state.get_latest_accessor_internal()
                {
                    *version = std::cmp::max(*version, other_version);
                }

                *self_state = State::Projection(projection.clone());

                Some(true)
            }

            (
                State::Total(Initialized::True) | State::Projection(_),
                State::Total(Initialized::True),
            )
            | (State::Total(Initialized::False(_)), _) => Some(false),

            (self_state, State::Total(Initialized::False(falsed))) => {
                let new_version = *version;
                *version += 1;

                *self_state = State::Total(Initialized::False(Uninitialized {
                    latest_move: falsed.latest_move,
                    version: new_version,
                }));

                Some(true)
            }

            (
                State::Projection(Projection::Struct(self_proj)),
                State::Projection(Projection::Struct(other_proj)),
            ) => {
                // check if the struct is the same
                if self_proj.struct_id != other_proj.struct_id
                    || self_proj.states_by_field_id.len()
                        != other_proj.states_by_field_id.len()
                    || self_proj
                        .states_by_field_id
                        .keys()
                        .any(|x| !other_proj.states_by_field_id.contains_key(x))
                {
                    return None;
                }

                let mut changed = false;

                for (field_id, lhs_state) in &mut self_proj.states_by_field_id {
                    let rhs_state =
                        other_proj.states_by_field_id.get(field_id)?;

                    changed |=
                        Self::min_internal(lhs_state, rhs_state, version)?;
                }

                if changed {
                    self_state.simplify();
                }

                Some(changed)
            }

            (
                State::Projection(Projection::Tuple(self_proj)),
                State::Projection(Projection::Tuple(other_proj)),
            ) => {
                if self_proj.elements.len() != other_proj.elements.len() {
                    return None;
                }

                let mut changed = false;

                for (lhs_element, rhs_element) in self_proj
                    .elements
                    .iter_mut()
                    .zip(other_proj.elements.iter())
                {
                    changed |= Self::min_internal(
                        &mut lhs_element.state,
                        &rhs_element.state,
                        version,
                    )?;
                }

                if changed {
                    self_state.simplify();
                }

                Some(changed)
            }

            (
                State::Projection(Projection::Enum(self_proj)),
                State::Projection(Projection::Enum(other_proj)),
            ) => {
                if self_proj.variant_id != other_proj.variant_id {
                    return None;
                }

                let changed = Self::min_internal(
                    self_proj.state.as_mut(),
                    other_proj.state.as_ref(),
                    version,
                )?;

                if changed {
                    self_state.simplify();
                }

                Some(changed)
            }

            (
                State::Projection(Projection::MutableReference(self_proj)),
                State::Projection(Projection::MutableReference(other_proj)),
            ) => {
                let changed = Self::min_internal(
                    &mut self_proj.state,
                    &other_proj.state,
                    version,
                )?;

                if changed {
                    self_state.simplify();
                }

                Some(changed)
            }

            (State::Projection(_), State::Projection(_)) => None,
        }
    }
}

impl State {
    pub fn get_uninitialized_diff(&self, other: &Self) -> Option<Vec<Self>> {
        match (self, other) {
            (Self::Total(Initialized::False(_)), _)
            | (_, Self::Total(Initialized::True)) => Some(Vec::new()),

            (Self::Total(Initialized::True), another) => {
                Some(vec![another.clone()])
            }

            (Self::Projection(_), Self::Total(Initialized::False(_))) => {
                Some(vec![other.clone()])
            }

            (
                Self::Projection(Projection::Enum(this)),
                Self::Projection(Projection::Enum(other)),
            ) => {
                if this.variant_id != other.variant_id {
                    return None;
                }

                this.state.get_uninitialized_diff(&other.state)
            }

            (
                Self::Projection(Projection::Struct(this)),
                Self::Projection(Projection::Struct(other)),
            ) => {
                if this.struct_id != other.struct_id
                    || this.states_by_field_id.len()
                        != other.states_by_field_id.len()
                {
                    return None;
                }

                let mut diff = Vec::new();

                for field_id in this.states_by_field_id.keys() {
                    let this_field_state =
                        this.states_by_field_id.get(field_id)?;
                    let other_field_state =
                        other.states_by_field_id.get(field_id)?;

                    diff.extend(
                        this_field_state
                            .get_uninitialized_diff(other_field_state)?,
                    );
                }

                Some(diff)
            }

            (
                Self::Projection(Projection::Tuple(this)),
                Self::Projection(Projection::Tuple(other)),
            ) => {
                if this.elements.len() != other.elements.len() {
                    return None;
                }

                let mut diff = Vec::new();

                for (this_element, other_element) in
                    this.elements.iter().zip(other.elements.iter())
                {
                    diff.extend(
                        this_element
                            .state
                            .get_uninitialized_diff(&other_element.state)?,
                    );
                }

                Some(diff)
            }

            (
                Self::Projection(Projection::MutableReference(this)),
                Self::Projection(Projection::MutableReference(other)),
            ) => this.state.get_uninitialized_diff(&other.state),

            (Self::Projection(_), Self::Projection(_)) => None,
        }
    }

    /// Returns a list of drop instructions that would make the `alternate`
    /// state the same as the current state.
    #[allow(clippy::too_many_lines)]
    pub async fn get_alternate_drop_instructions(
        &self,
        alternate: &Self,
        address: &Address,
        engine: &TrackedEngine,
    ) -> Result<Vec<Instruction>, CyclicError> {
        match (self, alternate) {
            (Self::Total(Initialized::False(_)), other) => {
                other.get_drop_instructions(address, engine).await
            }

            (Self::Projection(_), Self::Total(Initialized::False(_)))
            | (Self::Total(Initialized::True), _) => Ok(Vec::new()),

            (this, Self::Total(Initialized::True)) => {
                this.get_drop_instructions_internal(address, engine, true, true)
                    .await
            }

            (
                Self::Projection(Projection::Struct(this)),
                Self::Projection(Projection::Struct(other)),
            ) => {
                let mut instructions = Vec::new();

                assert_eq!(this.struct_id, other.struct_id);
                assert_eq!(
                    this.states_by_field_id.len(),
                    other.states_by_field_id.len()
                );

                let fields = engine.get_fields(this.struct_id).await?;

                assert_eq!(this.states_by_field_id.len(), fields.fields.len());

                for field_id in fields.field_declaration_order.iter().copied() {
                    let this_field_state = &this.states_by_field_id[&field_id];
                    let other_field_state =
                        &other.states_by_field_id[&field_id];

                    instructions.extend(
                        Box::pin(
                            this_field_state.get_alternate_drop_instructions(
                                other_field_state,
                                &Address::Field(address::Field {
                                    struct_address: Box::new(address.clone()),
                                    id: field_id,
                                }),
                                engine,
                            ),
                        )
                        .await?,
                    );
                }

                Ok(instructions)
            }

            (
                Self::Projection(Projection::Tuple(this)),
                Self::Projection(Projection::Tuple(other)),
            ) => {
                let mut instructions = Vec::new();

                assert_eq!(this.elements.len(), other.elements.len());

                for (index, (this_element, other_element)) in
                    this.elements.iter().zip(other.elements.iter()).enumerate()
                {
                    assert_eq!(
                        this_element.is_unpacked,
                        other_element.is_unpacked
                    );

                    instructions.extend(
                        Box::pin(
                            this_element.state.get_alternate_drop_instructions(
                                &other_element.state,
                                &Address::Tuple(address::Tuple {
                                    tuple_address: Box::new(address.clone()),
                                    offset: Offset::FromStart(index),
                                }),
                                engine,
                            ),
                        )
                        .await?,
                    );
                }

                Ok(instructions)
            }

            (
                Self::Projection(Projection::Enum(this)),
                Self::Projection(Projection::Enum(other)),
            ) => {
                assert_eq!(this.variant_id, other.variant_id);

                Box::pin(this.state.get_alternate_drop_instructions(
                    &other.state,
                    &Address::Variant(address::Variant {
                        enum_address: Box::new(address.clone()),
                        id: this.variant_id,
                    }),
                    engine,
                ))
                .await
            }

            (
                Self::Projection(Projection::MutableReference(this)),
                Self::Projection(Projection::MutableReference(other)),
            ) => {
                Box::pin(this.state.get_alternate_drop_instructions(
                    &other.state,
                    &Address::Reference(address::Reference {
                        reference_address: Box::new(address.clone()),
                        qualifier: Qualifier::Mutable,
                    }),
                    engine,
                ))
                .await
            }

            (Self::Projection(_), Self::Projection(_)) => {
                panic!("invalid projection state")
            }
        }
    }

    #[allow(clippy::too_many_lines)]
    async fn get_drop_instructions_internal(
        &self,
        address: &Address,
        engine: &TrackedEngine,
        negative: bool,
        should_drop_mutable_reference: bool,
    ) -> Result<Vec<Instruction>, CyclicError> {
        match self {
            Self::Total(Initialized::True) => Ok(if negative {
                Vec::new()
            } else {
                vec![Instruction::Drop(Drop { address: address.clone() })]
            }),

            Self::Total(Initialized::False(_)) => Ok(if negative {
                vec![Instruction::Drop(Drop { address: address.clone() })]
            } else {
                Vec::new()
            }),

            Self::Projection(Projection::Struct(st)) => {
                let mut instructions = Vec::new();
                let fields = engine.get_fields(st.struct_id).await?;

                for field_id in fields.field_declaration_order.iter().copied() {
                    let field_state = &st.states_by_field_id[&field_id];

                    instructions.extend(
                        Box::pin(field_state.get_drop_instructions_internal(
                            &Address::Field(address::Field {
                                struct_address: Box::new(address.clone()),
                                id: field_id,
                            }),
                            engine,
                            negative,
                            should_drop_mutable_reference,
                        ))
                        .await?,
                    );
                }

                Ok(instructions)
            }

            Self::Projection(Projection::Tuple(tuple)) => {
                let mut instructions = Vec::new();
                assert!(
                    tuple.elements.iter().filter(|x| x.is_unpacked).count()
                        <= 1
                );
                let unpacked_position =
                    tuple.elements.iter().position(|x| x.is_unpacked);

                for (index, element) in tuple.elements.iter().enumerate() {
                    if Some(index) == unpacked_position {
                        assert!(element.is_unpacked);

                        let should_drop = match &element.state {
                            Self::Total(Initialized::True) => !negative,

                            Self::Total(Initialized::False(_)) => negative,

                            Self::Projection(_) => unreachable!(
                                "unpacked tuple element should not have \
                                 projection"
                            ),
                        };

                        instructions.extend(should_drop.then(|| {
                            Instruction::DropUnpackTuple(DropUnpackTuple {
                                tuple_address: address.clone(),
                                before_unpacked_element_count: index,
                                after_unpacked_element_count: tuple
                                    .elements
                                    .len()
                                    - index
                                    - 1,
                            })
                        }));
                    } else {
                        instructions.extend(
                            Box::pin(
                                element.state.get_drop_instructions_internal(
                                    &Address::Tuple(address::Tuple {
                                        tuple_address: Box::new(
                                            address.clone(),
                                        ),
                                        offset: unpacked_position.map_or(
                                            Offset::FromStart(index),
                                            |x| {
                                                if index < x {
                                                    Offset::FromStart(index)
                                                } else {
                                                    Offset::FromEnd(
                                                        tuple.elements.len()
                                                            - index
                                                            - 1,
                                                    )
                                                }
                                            },
                                        ),
                                    }),
                                    engine,
                                    negative,
                                    should_drop_mutable_reference,
                                ),
                            )
                            .await?,
                        );
                    }
                }

                Ok(instructions)
            }

            Self::Projection(Projection::Enum(en)) => {
                Box::pin(en.state.get_drop_instructions_internal(
                    &Address::Variant(address::Variant {
                        enum_address: Box::new(address.clone()),
                        id: en.variant_id,
                    }),
                    engine,
                    negative,
                    should_drop_mutable_reference,
                ))
                .await
            }

            Self::Projection(Projection::MutableReference(state)) => {
                if should_drop_mutable_reference {
                    Box::pin(state.state.get_drop_instructions_internal(
                        &Address::Reference(address::Reference {
                            reference_address: Box::new(address.clone()),
                            qualifier: Qualifier::Mutable,
                        }),
                        engine,
                        negative,
                        should_drop_mutable_reference,
                    ))
                    .await
                } else {
                    Ok(Vec::new())
                }
            }
        }
    }

    /// Gets a list of drop instructions that will drop all the initialized
    /// values in the memory.
    pub async fn get_drop_instructions(
        &self,
        address: &Address,
        table: &TrackedEngine,
    ) -> Result<Vec<Instruction>, CyclicError> {
        self.get_drop_instructions_internal(address, table, false, false).await
    }

    /// Returns `Some` with the register that move, if there's a moved out value
    /// in a mutable reference.
    pub fn get_moved_out_mutable_reference(&self) -> Option<&Move> {
        self.get_moved_out_mutable_reference_internal().map(|x| x.0)
    }

    fn get_moved_out_mutable_reference_internal(
        &self,
    ) -> Option<(&Move, usize)> {
        match self {
            Self::Total(_) => None,

            Self::Projection(projection) => match projection {
                Projection::Struct(st) => st
                    .states_by_field_id
                    .values()
                    .filter_map(Self::get_moved_out_mutable_reference_internal)
                    .max_by_key(|x| x.1),

                Projection::Tuple(tuple) => tuple
                    .elements
                    .iter()
                    .map(|x| &x.state)
                    .filter_map(Self::get_moved_out_mutable_reference_internal)
                    .max_by_key(|x| x.1),

                Projection::Enum(en) => {
                    en.state.get_moved_out_mutable_reference_internal()
                }

                Projection::MutableReference(mutable_reference) => {
                    match mutable_reference.state.get_latest_accessor_internal()
                    {
                        Some(LatestLoad::Uninitialized) => unreachable!(),
                        Some(LatestLoad::Moved(move_info, version)) => {
                            Some((move_info, version))
                        }
                        None => None,
                    }
                }
            },
        }
    }
    /// Returns the latest register ID that moved the value.
    pub fn get_state_summary(&self) -> Summary {
        self.get_latest_accessor_internal().map_or(Summary::Initialized, |x| {
            match x {
                LatestLoad::Uninitialized => Summary::Uninitialized,
                LatestLoad::Moved(move_info, _) => Summary::Moved(*move_info),
            }
        })
    }

    fn get_latest_accessor_internal(&self) -> Option<LatestLoad<'_>> {
        match self {
            Self::Total(Initialized::False(Uninitialized {
                latest_move,
                version,
            })) => Some(
                latest_move.as_ref().map_or(LatestLoad::Uninitialized, |x| {
                    LatestLoad::Moved(x, *version)
                }),
            ),
            Self::Projection(Projection::Struct(Struct {
                states_by_field_id,
                ..
            })) => {
                let mut iter = states_by_field_id
                    .values()
                    .filter_map(Self::get_latest_accessor_internal);

                let first = iter.next()?;

                Some(
                    std::iter::once(first)
                        .chain(iter)
                        .filter_map(|x| x.into_moved().ok())
                        .max_by_key(|x| x.1)
                        .map_or(LatestLoad::Uninitialized, |x| {
                            LatestLoad::Moved(x.0, x.1)
                        }),
                )
            }
            Self::Projection(Projection::Tuple(Tuple { elements })) => {
                let mut iter = elements
                    .iter()
                    .map(|x| &x.state)
                    .filter_map(Self::get_latest_accessor_internal);

                let first = iter.next()?;

                Some(
                    std::iter::once(first)
                        .chain(iter)
                        .filter_map(|x| x.into_moved().ok())
                        .max_by_key(|x| x.1)
                        .map_or(LatestLoad::Uninitialized, |x| {
                            LatestLoad::Moved(x.0, x.1)
                        }),
                )
            }
            Self::Projection(Projection::Enum(Enum { state, .. })) => {
                state.get_latest_accessor_internal()
            }
            Self::Projection(Projection::MutableReference(
                MutableReference { state },
            )) => state.get_latest_accessor_internal(),

            Self::Total(Initialized::True) => None,
        }
    }

    /// Simplifies the state to the [`State::Total`] if possible.
    pub fn simplify(&mut self) {
        match self {
            Self::Total(_) => {}
            Self::Projection(Projection::Struct(Struct {
                states_by_field_id,
                ..
            })) => {
                for state in states_by_field_id.values_mut() {
                    state.simplify();
                }

                if states_by_field_id
                    .values()
                    .all(|x| x.as_total().is_some_and(Initialized::is_true))
                {
                    *self = Self::Total(Initialized::True);
                    // empty struct should always be initialized
                    return;
                }

                if let Some((latest_accessor, version)) =
                    try_simplify_to_uninitialized(states_by_field_id.values())
                {
                    *self = Self::Total(Initialized::False(Uninitialized {
                        latest_move: Some(*latest_accessor),
                        version,
                    }));
                }
            }
            Self::Projection(Projection::Tuple(Tuple { elements })) => {
                for state in elements.iter_mut().map(|x| &mut x.state) {
                    state.simplify();
                }

                if elements.iter().all(|x| {
                    x.state.as_total().is_some_and(Initialized::is_true)
                }) {
                    *self = Self::Total(Initialized::True);
                    // empty tuple should always be initialized
                    return;
                }

                if let Some((latest_accessor, version)) =
                    try_simplify_to_uninitialized(
                        elements.iter().map(|x| &x.state),
                    )
                {
                    *self = Self::Total(Initialized::False(Uninitialized {
                        latest_move: Some(*latest_accessor),
                        version,
                    }));
                }
            }
            Self::Projection(Projection::MutableReference(
                MutableReference { state },
            )) => {
                state.simplify();

                if **state == Self::Total(Initialized::True) {
                    *self = Self::Total(Initialized::True);
                }
            }

            Self::Projection(Projection::Enum(Enum { state, .. })) => {
                state.simplify();

                if let Some(initialized) = state.as_total() {
                    *self = Self::Total(initialized.clone());
                }
            }
        }
    }
}

/// Represents the state of the stack.
#[derive(Debug, Clone, PartialEq, Eq, Getters, CopyGetters)]
pub struct Scope {
    #[get_copy = "pub"]
    scope_id: ID<scope::Scope>,

    #[get = "pub"]
    memories_by_address: HashMap<address::Memory, Memory>,
}

#[derive(Debug, Clone, PartialEq, Eq, Getters, CopyGetters)]
pub struct Memory {
    #[get = "pub"]
    state: State,
    #[get = "pub"]
    r#type: Type,

    version: usize,
}

impl Default for Memory {
    fn default() -> Self {
        Self {
            state: State::Total(Initialized::True),
            r#type: Type::Tuple(tuple::Tuple { elements: Vec::new() }),
            version: 0,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
#[allow(clippy::large_enum_variant)]
enum SetStateResultInternal<'a> {
    Unchanged(Initialized, Address),
    Done(State),
    Continue { state: &'a mut State, ty: Type, version: &'a mut usize },
}

/// Represents the result of setting the state.
#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(clippy::large_enum_variant)]
pub enum SetStateSucceeded {
    /// The state was not changed. The state was already satisfied.
    Unchanged(Initialized, Address),

    /// The state was changed. The variant holds the state that was replaced.
    Updated(State),
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    thiserror::Error,
    displaydoc::Display,
)]
pub enum ScopeMergeError {
    /// The scopes have different IDs.
    DifferentScope,

    /// The scopes have different memory counts.
    DifferentMemoryCount,

    /// The memory address of one scope is not found in the other scope.
    MemoryNotFound(address::Memory),

    /// The memory state is not compatible.
    #[error(transparent)]
    MemoryMerge(#[from] MemoryMergeError),
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner,
)]
enum SetState {
    Initialized,
    Move(load::Purpose),
}

impl Scope {
    pub fn new(scope_id: ID<scope::Scope>) -> Self {
        Self { scope_id, memories_by_address: HashMap::default() }
    }

    /// Merges the state of the memory with the other memory.
    ///
    /// # Returns
    ///
    /// Returns `true` if the merge is successful. Returns `false` if the merge
    /// is not successful and the state might be partially updated.
    pub fn min_merge(mut self, other: &Self) -> Result<Self, ScopeMergeError> {
        if self.scope_id != other.scope_id {
            return Err(ScopeMergeError::DifferentScope);
        }

        if self.memories_by_address.len() != other.memories_by_address.len()
            || self
                .memories_by_address
                .keys()
                .any(|x| !other.memories_by_address.contains_key(x))
        {
            return Err(ScopeMergeError::DifferentMemoryCount);
        }

        for (address, memory) in &mut self.memories_by_address {
            let other_memory = other
                .memories_by_address
                .get(address)
                .ok_or(ScopeMergeError::MemoryNotFound(*address))?;

            let temp_memory = std::mem::take(memory);
            *memory = temp_memory.min_merge(other_memory)?;
        }

        Ok(self)
    }

    /// Adds a new uninitialized state to the stack.
    ///
    /// If there's already a state associated with the stack, this method does
    /// nothing and returns `false`.
    #[must_use]
    pub fn new_state(
        &mut self,
        memory: address::Memory,
        initialized: bool,
        ty: Type,
    ) -> bool {
        match self.memories_by_address.entry(memory) {
            Entry::Occupied(_) => false,
            Entry::Vacant(entry) => {
                entry.insert(Memory {
                    state: if initialized {
                        State::Total(Initialized::True)
                    } else {
                        State::Total(Initialized::False(Uninitialized {
                            latest_move: None,
                            version: 0,
                        }))
                    },
                    r#type: ty,
                    version: 1,
                });

                true
            }
        }
    }

    /// Checks if the memory state is contained in this scope.
    pub fn contains(&self, memory: &address::Memory) -> bool {
        self.memories_by_address.contains_key(memory)
    }

    /// Sets the state of the value in memory as uninitialized (moved out).
    pub async fn set_uninitialized<N: Normalizer>(
        &mut self,
        address: &Address,
        load_span: RelativeSpan,
        load_purpose: load::Purpose,
        environment: &Environment<'_, N>,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<SetStateSucceeded, UnrecoverableError> {
        let result = self
            .set_state_internal(
                address,
                load_span,
                SetState::Move(load_purpose),
                true,
                environment,
                handler,
            )
            .await
            .map(|x| match x {
                SetStateResultInternal::Unchanged(a, b) => {
                    SetStateSucceeded::Unchanged(a, b)
                }
                SetStateResultInternal::Done(state) => {
                    SetStateSucceeded::Updated(state)
                }
                SetStateResultInternal::Continue { .. } => {
                    unreachable!()
                }
            });

        if result.is_ok() {
            let state = self
                .memories_by_address
                .get_mut(address.get_root_memory())
                .unwrap();

            state.state.simplify();
        }

        result
    }

    /// Sets the state of the value in memory as initialized.
    pub async fn set_initialized<N: Normalizer>(
        &mut self,
        address: &Address,
        set_span: RelativeSpan,
        environment: &Environment<'_, N>,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<SetStateSucceeded, UnrecoverableError> {
        let result = self
            .set_state_internal(
                address,
                set_span,
                SetState::Initialized,
                true,
                environment,
                handler,
            )
            .await
            .map(|x| match x {
                SetStateResultInternal::Unchanged(a, b) => {
                    SetStateSucceeded::Unchanged(a, b)
                }
                SetStateResultInternal::Done(state) => {
                    SetStateSucceeded::Updated(state)
                }
                SetStateResultInternal::Continue { .. } => {
                    unreachable!()
                }
            });

        if result.is_ok() {
            let state = self
                .memories_by_address
                .get_mut(address.get_root_memory())
                .unwrap();

            state.state.simplify();
        }

        result
    }

    /// Gets the state of the value in memory with the given address.
    pub fn get_state(&self, address: &Address) -> Option<&State> {
        match address {
            Address::Memory(memory) => {
                self.memories_by_address.get(memory).map(|x| &x.state)
            }

            Address::Field(field) => {
                let state = self.get_state(&field.struct_address)?;

                state.as_projection().map_or(Some(state), |x| {
                    x.as_struct()
                        .and_then(|x| x.states_by_field_id.get(&field.id))
                })
            }

            Address::Tuple(tuple) => {
                let state = self.get_state(&tuple.tuple_address)?;

                state.as_projection().map_or(Some(state), |x| {
                    x.as_tuple().and_then(|x| {
                        x.elements
                            .get(match tuple.offset {
                                Offset::FromStart(index) => index,
                                Offset::FromEnd(index) => {
                                    x.elements.len() - index
                                }
                                Offset::Unpacked => {
                                    return x.elements.iter().find_map(|x| {
                                        x.is_unpacked.then_some(&x.state)
                                    })
                                }
                            })
                            .map(|x| &x.state)
                    })
                })
            }

            Address::Index(index) => self.get_state(&index.array_address),

            Address::Variant(variant) => {
                let state = self.get_state(&variant.enum_address)?;

                state
                    .as_projection()
                    .map_or(Some(state), |x| x.as_enum().map(|x| &*x.state))
            }

            Address::Reference(reference) => {
                let state = self.get_state(&reference.reference_address)?;

                state.as_projection().map_or(Some(state), |x| {
                    x.as_mutable_reference().map(|x| &*x.state)
                })
            }
        }
    }

    // returns `Ok(None)` if the state is already satisfied
    #[allow(clippy::too_many_lines, clippy::cognitive_complexity)]
    async fn set_state_internal<N: Normalizer>(
        &mut self,
        address: &Address,
        set_span: RelativeSpan,
        set_initialized: SetState,
        root: bool,
        environment: &Environment<'_, N>,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<SetStateResultInternal<'_>, UnrecoverableError> {
        let (target_state, ty, version) = match address {
            Address::Memory(memory) => self
                .memories_by_address
                .get_mut(memory)
                .map(|memory| {
                    (
                        &mut memory.state,
                        memory.r#type.clone(),
                        &mut memory.version,
                    )
                })
                .unwrap(),

            Address::Field(field) => {
                let (state, ty, version) = match Box::pin(
                    self.set_state_internal(
                        &field.struct_address,
                        set_span,
                        set_initialized,
                        false,
                        environment,
                        handler,
                    ),
                )
                .await?
                {
                    SetStateResultInternal::Continue { state, ty, version } => {
                        (state, ty, version)
                    }
                    rest => return Ok(rest),
                };

                let Type::Symbol(Symbol { id: struct_id, generic_arguments }) =
                    ty
                else {
                    panic!("expected struct type");
                };

                assert_eq!(
                    environment.tracked_engine().get_kind(struct_id).await,
                    Kind::Struct
                );

                let fields =
                    environment.tracked_engine().get_fields(struct_id).await?;

                let instantiation = environment
                    .tracked_engine()
                    .get_instantiation(struct_id, generic_arguments)
                    .await?
                    .unwrap();

                if let State::Total(current) = state {
                    // already satisfied
                    if current.is_true() == set_initialized.is_initialized() {
                        return Ok(SetStateResultInternal::Unchanged(
                            current.clone(),
                            (*field.struct_address).clone(),
                        ));
                    }

                    *state = State::Projection(Projection::Struct(Struct {
                        struct_id,
                        states_by_field_id: fields
                            .field_declaration_order
                            .iter()
                            .copied()
                            .map(|x| (x, State::Total(current.clone())))
                            .collect(),
                    }));
                }

                (
                    state
                        .as_projection_mut()
                        .and_then(|x| x.as_struct_mut())
                        .filter(|x| x.struct_id == struct_id)
                        .unwrap()
                        .states_by_field_id
                        .get_mut(&field.id)
                        .unwrap(),
                    {
                        let mut ty = fields.fields[field.id].r#type.clone();

                        instantiation.instantiate(&mut ty);

                        environment
                            .simplify(ty)
                            .await
                            .map_err(|x| {
                                x.report_as_type_calculating_overflow(
                                    set_span, &handler,
                                )
                            })?
                            .result
                            .clone()
                    },
                    version,
                )
            }

            Address::Tuple(tuple) => {
                let (state, ty, version) = match Box::pin(
                    self.set_state_internal(
                        &tuple.tuple_address,
                        set_span,
                        set_initialized,
                        false,
                        environment,
                        handler,
                    ),
                )
                .await?
                {
                    SetStateResultInternal::Continue { state, ty, version } => {
                        (state, ty, version)
                    }
                    rest => return Ok(rest),
                };

                let Type::Tuple(tuple::Tuple { elements }) = ty else {
                    panic!("expected tuple type");
                };

                if let State::Total(current) = state {
                    // already satisfied
                    if current.is_true() == set_initialized.is_initialized() {
                        return Ok(SetStateResultInternal::Unchanged(
                            current.clone(),
                            (*tuple.tuple_address).clone(),
                        ));
                    }

                    *state = State::Projection(Projection::Tuple(Tuple {
                        elements: elements
                            .iter()
                            .map(|x| TupleElement {
                                state: State::Total(current.clone()),
                                is_unpacked: x.is_unpacked,
                            })
                            .collect(),
                    }));
                }

                (
                    'state: {
                        let tuple_proj = state
                            .as_projection_mut()
                            .and_then(|x| x.as_tuple_mut())
                            .unwrap();

                        let tuple_index = match tuple.offset {
                            Offset::FromStart(index) => index,
                            Offset::FromEnd(index) => elements.len() - index,
                            Offset::Unpacked => {
                                break 'state tuple_proj
                                    .elements
                                    .iter_mut()
                                    .find_map(|x| {
                                        x.is_unpacked.then_some(&mut x.state)
                                    })
                                    .unwrap();
                            }
                        };

                        &mut tuple_proj.elements[tuple_index].state
                    },
                    'type_result: {
                        let index = match tuple.offset {
                            Offset::FromStart(index) => index,
                            Offset::FromEnd(index) => elements.len() - index,
                            Offset::Unpacked => {
                                break 'type_result elements
                                    .iter()
                                    .find_map(|x| {
                                        x.is_unpacked.then(|| x.term.clone())
                                    })
                                    .unwrap()
                            }
                        };

                        elements[index].term.clone()
                    },
                    version,
                )
            }

            Address::Index(_) => {
                panic!("setting state for index is not valid");
            }

            Address::Variant(variant) => {
                let (state, ty, version) = match Box::pin(
                    self.set_state_internal(
                        &variant.enum_address,
                        set_span,
                        set_initialized,
                        false,
                        environment,
                        handler,
                    ),
                )
                .await?
                {
                    SetStateResultInternal::Continue { state, ty, version } => {
                        (state, ty, version)
                    }
                    rest => return Ok(rest),
                };

                let Type::Symbol(Symbol { id: enum_id, generic_arguments }) =
                    ty
                else {
                    panic!("expected enum type");
                };

                assert_eq!(
                    environment.tracked_engine().get_kind(enum_id).await,
                    Kind::Enum
                );
                assert_eq!(
                    environment.tracked_engine().get_kind(variant.id).await,
                    Kind::Variant
                );

                let assocated_type = environment
                    .tracked_engine()
                    .get_variant_associated_type(variant.id)
                    .await?
                    .unwrap()
                    .deref()
                    .clone();

                let instantiation = environment
                    .tracked_engine()
                    .get_instantiation(enum_id, generic_arguments)
                    .await?
                    .unwrap();

                if let State::Total(current) = state {
                    // already satisfied
                    if current.is_true() == set_initialized.is_initialized() {
                        return Ok(SetStateResultInternal::Unchanged(
                            current.clone(),
                            (*variant.enum_address).clone(),
                        ));
                    }

                    *state = State::Projection(Projection::Enum(Enum {
                        variant_id: variant.id,
                        state: Box::new(State::Total(current.clone())),
                    }));
                }

                (
                    state
                        .as_projection_mut()
                        .and_then(|x| x.as_enum_mut())
                        .filter(|x| x.variant_id == variant.id)
                        .unwrap()
                        .state
                        .as_mut(),
                    {
                        instantiation.instantiate(&mut assocated_type.clone());

                        environment
                            .simplify(assocated_type)
                            .await
                            .map_err(|x| {
                                x.report_as_type_calculating_overflow(
                                    set_span, &handler,
                                )
                            })?
                            .result
                            .clone()
                    },
                    version,
                )
            }

            Address::Reference(reference) => {
                // must be a mutable reference
                let (state, ty, version) = match Box::pin(
                    self.set_state_internal(
                        &reference.reference_address,
                        set_span,
                        set_initialized,
                        false,
                        environment,
                        handler,
                    ),
                )
                .await?
                {
                    SetStateResultInternal::Continue { state, ty, version } => {
                        (state, ty, version)
                    }
                    rest => return Ok(rest),
                };

                let Type::Reference(r#type::Reference {
                    pointee,
                    qualifier,
                    ..
                }) = ty
                else {
                    panic!("expected reference type")
                };

                // must be mutable
                assert_eq!(
                    address.get_reference_qualifier(),
                    Some(r#type::Qualifier::Mutable)
                );
                assert_eq!(qualifier, r#type::Qualifier::Mutable);

                if let State::Total(current) = state {
                    // already satisfied
                    if current.is_true() == set_initialized.is_initialized() {
                        return Ok(SetStateResultInternal::Unchanged(
                            current.clone(),
                            (*reference.reference_address).clone(),
                        ));
                    }

                    *state = State::Projection(Projection::MutableReference(
                        MutableReference {
                            state: Box::new(State::Total(current.clone())),
                        },
                    ));
                }

                (
                    state
                        .as_projection_mut()
                        .and_then(|x| x.as_mutable_reference_mut())
                        .unwrap()
                        .state
                        .as_mut(),
                    *pointee,
                    version,
                )
            }
        };

        if root {
            let replaced = match set_initialized {
                SetState::Initialized => std::mem::replace(
                    target_state,
                    State::Total(Initialized::True),
                ),
                SetState::Move(load_purpose) => {
                    let current_version = *version;
                    *version += 1;

                    std::mem::replace(
                        target_state,
                        State::Total(Initialized::False(Uninitialized {
                            latest_move: Some(Move {
                                purpose: load_purpose,
                                span: set_span,
                            }),
                            version: current_version,
                        })),
                    )
                }
            };

            Ok(SetStateResultInternal::Done(replaced))
        } else {
            Ok(SetStateResultInternal::Continue {
                state: target_state,
                ty,
                version,
            })
        }
    }
}

/// Represents the stack of the state.
///
/// This is used to keep track the state of the memory e.g. whether the value
/// is moved out or not.
#[derive(Debug, Clone, PartialEq, Eq, Getters)]
pub struct Stack {
    #[get = "pub"]
    scopes: Vec<Scope>,
}

impl Stack {
    pub const fn new() -> Self { Self { scopes: Vec::new() } }

    #[allow(unused)]
    pub fn new_scope(&mut self, scope_id: ID<scope::Scope>) {
        self.scopes.push(Scope::new(scope_id));
    }

    pub fn pop_scope(&mut self) -> Option<Scope> { self.scopes.pop() }

    pub async fn set_initialized<N: Normalizer>(
        &mut self,
        address: &Address,
        span: RelativeSpan,
        environment: &Environment<'_, N>,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<SetStateSucceeded, UnrecoverableError> {
        let root = address.get_root_memory();
        for scope in self.scopes.iter_mut().rev() {
            if scope.contains(root) {
                return scope
                    .set_initialized(address, span, environment, handler)
                    .await;
            }
        }

        // not found
        panic!("Invalid address");
    }

    pub fn get_state(&self, address: &Address) -> Option<&State> {
        let root = address.get_root_memory();

        for scope in self.scopes.iter().rev() {
            if scope.contains(root) {
                return Some(scope.get_state(address).unwrap());
            }
        }

        // not found
        None
    }

    pub async fn set_uninitialized<N: Normalizer>(
        &mut self,
        address: &Address,
        move_span: RelativeSpan,
        load_purpose: load::Purpose,
        environment: &Environment<'_, N>,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<SetStateSucceeded, UnrecoverableError> {
        let root = address.get_root_memory();
        for scope in self.scopes.iter_mut().rev() {
            if scope.contains(root) {
                return scope
                    .set_uninitialized(
                        address,
                        move_span,
                        load_purpose,
                        environment,
                        handler,
                    )
                    .await;
            }
        }

        // not found
        panic!("Invalid address {root:#?} {:#?}", self.scopes);
    }

    pub fn current_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    pub fn min_merge(&mut self, other: &Self) {
        assert_eq!(self.scopes.len(), other.scopes.len());

        for (lhs, rhs) in self.scopes.iter_mut().zip(other.scopes.iter()) {
            take_mut::take(lhs, |lhs| lhs.min_merge(rhs).unwrap());
        }
    }
}
