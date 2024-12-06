use std::collections::{hash_map::Entry, HashMap};

use enum_as_inner::EnumAsInner;
use getset::{CopyGetters, Getters};
use pernixc_base::source_file::Span;

use crate::{
    arena::ID,
    ir::{
        self,
        address::{self, Address, Offset},
        instruction::{Drop, DropUnpackTuple, Instruction},
        scope,
    },
    symbol::{
        self,
        table::{self, representation::Index as _, Table},
        AdtID, GlobalID,
    },
    type_system::{
        environment::Environment,
        instantiation::{
            self, Instantiation, MismatchedGenericArgumentCountError,
        },
        normalizer::Normalizer,
        observer::Observer,
        simplify,
        term::{
            self,
            r#type::{self, Qualifier, SymbolID, Type},
            Symbol, Term,
        },
        OverflowError,
    },
};

/// Contains the state of each field in the struct.
#[derive(Debug, Clone, PartialEq, Eq, Getters, CopyGetters)]
pub struct Struct {
    /// The state of each field in the struct.
    #[get = "pub"]
    states_by_field_id: HashMap<ID<symbol::Field>, State>,

    #[get_copy = "pub"]
    struct_id: ID<symbol::Struct>,
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
    variant_id: ID<symbol::Variant>,
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

/// Represents the state of the value in memory that is uninitialized.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Uninitialized {
    /// The span that last moved the value.
    #[get = "pub"]
    latest_accessor: Option<Span>,

    /// Used to determine the order of moves. The higher the version, the
    /// later/newer the move.
    version: usize,
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
) -> Option<(&'a Span, usize)> {
    let mut iterator = iterator.into_iter();

    let mut current = iterator
        .next()
        .expect("should have at least one element")
        .as_total()
        .and_then(|x| x.as_false())
        .map(|x| (x.latest_accessor.as_ref().unwrap(), x.version))?;

    for state in iterator {
        let Some(next) = state
            .as_total()
            .and_then(|x| x.as_false())
            .map(|x| (x.latest_accessor.as_ref().unwrap(), x.version))
        else {
            // it's a mix of initialized and uninitialized, leave
            // it as is
            return None;
        };

        if next.1 > current.1 {
            current = next;
        }
    }

    Some(current)
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
enum LatestLoad<'a> {
    Uninitialized,
    Moved(&'a Span, usize),
}

/// Summary of the state.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Summary {
    /// The whole invariant is intact
    Initialized,

    /// The value has never been initialized.
    Uninitialized,

    /// The value has been moved out by some instructions
    Moved(Span),
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
                    latest_accessor: falsed.latest_accessor.clone(),
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
    pub fn get_alternate_drop_instructions(
        &self,
        alternate: &Self,
        address: &Address<ir::Model>,
        table: &Table<impl table::State>,
    ) -> Option<Vec<Instruction<ir::Model>>> {
        match (self, alternate) {
            (Self::Total(Initialized::False(_)), other) => {
                other.get_drop_instructions(address, table)
            }

            (Self::Projection(_), Self::Total(Initialized::False(_)))
            | (Self::Total(Initialized::True), _) => Some(Vec::new()),

            (this, Self::Total(Initialized::True)) => {
                this.get_drop_instructions_interanl(address, table, true, true)
            }

            (
                Self::Projection(Projection::Struct(this)),
                Self::Projection(Projection::Struct(other)),
            ) => {
                let mut instructions = Vec::new();

                if this.struct_id != other.struct_id
                    || this.states_by_field_id.len()
                        != other.states_by_field_id.len()
                {
                    return None;
                }

                let struct_sym = table.get(this.struct_id)?;

                if this.states_by_field_id.len() != struct_sym.fields().len() {
                    return None;
                }

                for (field_id, _) in struct_sym.fields_as_order() {
                    let this_field_state =
                        this.states_by_field_id.get(&field_id)?;
                    let other_field_state =
                        other.states_by_field_id.get(&field_id)?;

                    instructions.extend(
                        this_field_state.get_alternate_drop_instructions(
                            other_field_state,
                            &Address::Field(address::Field {
                                struct_address: Box::new(address.clone()),
                                id: field_id,
                            }),
                            table,
                        )?,
                    );
                }

                Some(instructions)
            }

            (
                Self::Projection(Projection::Tuple(this)),
                Self::Projection(Projection::Tuple(other)),
            ) => {
                let mut instructions = Vec::new();

                if this.elements.len() != other.elements.len() {
                    return None;
                }

                for (index, (this_element, other_element)) in
                    this.elements.iter().zip(other.elements.iter()).enumerate()
                {
                    if this_element.is_unpacked != other_element.is_unpacked {
                        return None;
                    }

                    instructions.extend(
                        this_element.state.get_alternate_drop_instructions(
                            &other_element.state,
                            &Address::Tuple(address::Tuple {
                                tuple_address: Box::new(address.clone()),
                                offset: Offset::FromStart(index),
                            }),
                            table,
                        )?,
                    );
                }

                Some(instructions)
            }

            (
                Self::Projection(Projection::Enum(this)),
                Self::Projection(Projection::Enum(other)),
            ) => {
                if this.variant_id != other.variant_id {
                    return None;
                }

                this.state.get_alternate_drop_instructions(
                    &other.state,
                    &Address::Variant(address::Variant {
                        enum_address: Box::new(address.clone()),
                        id: this.variant_id,
                    }),
                    table,
                )
            }

            (
                Self::Projection(Projection::MutableReference(this)),
                Self::Projection(Projection::MutableReference(other)),
            ) => this.state.get_alternate_drop_instructions(
                &other.state,
                &Address::Reference(address::Reference {
                    reference_address: Box::new(address.clone()),
                    qualifier: Qualifier::Mutable,
                }),
                table,
            ),

            (Self::Projection(_), Self::Projection(_)) => None,
        }
    }

    #[allow(clippy::too_many_lines)]
    fn get_drop_instructions_interanl(
        &self,
        address: &Address<ir::Model>,
        table: &Table<impl table::State>,
        negative: bool,
        should_drop_mutable_reference: bool,
    ) -> Option<Vec<Instruction<ir::Model>>> {
        match self {
            Self::Total(Initialized::True) => Some(if negative {
                Vec::new()
            } else {
                vec![Instruction::Drop(Drop { address: address.clone() })]
            }),

            Self::Total(Initialized::False(_)) => Some(if negative {
                vec![Instruction::Drop(Drop { address: address.clone() })]
            } else {
                Vec::new()
            }),

            Self::Projection(Projection::Struct(st)) => {
                let mut instructions = Vec::new();
                for (field_id, _) in table.get(st.struct_id)?.fields_as_order()
                {
                    let field_state = st.states_by_field_id.get(&field_id)?;
                    instructions.extend(
                        field_state.get_drop_instructions_interanl(
                            &Address::Field(address::Field {
                                struct_address: Box::new(address.clone()),
                                id: field_id,
                            }),
                            table,
                            negative,
                            should_drop_mutable_reference,
                        )?,
                    );
                }

                Some(instructions)
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
                            element.state.get_drop_instructions_interanl(
                                &Address::Tuple(address::Tuple {
                                    tuple_address: Box::new(address.clone()),
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
                                table,
                                negative,
                                should_drop_mutable_reference,
                            )?,
                        );
                    }
                }

                Some(instructions)
            }

            Self::Projection(Projection::Enum(en)) => {
                en.state.get_drop_instructions_interanl(
                    &Address::Variant(address::Variant {
                        enum_address: Box::new(address.clone()),
                        id: en.variant_id,
                    }),
                    table,
                    negative,
                    should_drop_mutable_reference,
                )
            }

            Self::Projection(Projection::MutableReference(state)) => {
                if should_drop_mutable_reference {
                    state.state.get_drop_instructions_interanl(
                        &Address::Reference(address::Reference {
                            reference_address: Box::new(address.clone()),
                            qualifier: Qualifier::Mutable,
                        }),
                        table,
                        negative,
                        should_drop_mutable_reference,
                    )
                } else {
                    Some(Vec::new())
                }
            }
        }
    }

    /// Gets a list of drop instructions that will drop all the initialized
    /// values in the memory.
    pub fn get_drop_instructions(
        &self,
        address: &Address<ir::Model>,
        table: &Table<impl table::State>,
    ) -> Option<Vec<Instruction<ir::Model>>> {
        self.get_drop_instructions_interanl(address, table, false, false)
    }

    /// Returns `Some` with the register that move, if there's a moved out value
    /// in a mutable reference.
    pub fn get_moved_out_mutable_reference(&self) -> Option<&Span> {
        self.get_moved_out_mutable_reference_internal().map(|x| x.0)
    }

    fn get_moved_out_mutable_reference_internal(
        &self,
    ) -> Option<(&Span, usize)> {
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
                        Some(LatestLoad::Moved(id, version)) => {
                            Some((id, version))
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
                LatestLoad::Moved(id, _) => Summary::Moved(id.clone()),
            }
        })
    }

    fn get_latest_accessor_internal(&self) -> Option<LatestLoad> {
        match self {
            Self::Total(Initialized::False(Uninitialized {
                latest_accessor,
                version,
            })) => Some(
                latest_accessor
                    .as_ref()
                    .map_or(LatestLoad::Uninitialized, |x| {
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
                    .all(|x| x.as_total().map_or(false, Initialized::is_true))
                {
                    *self = Self::Total(Initialized::True);
                    // empty struct should always be initialized
                    return;
                }

                if let Some((latest_accessor, version)) =
                    try_simplify_to_uninitialized(states_by_field_id.values())
                {
                    *self = Self::Total(Initialized::False(Uninitialized {
                        latest_accessor: Some(latest_accessor.clone()),
                        version,
                    }));
                }
            }
            Self::Projection(Projection::Tuple(Tuple { elements })) => {
                for state in elements.iter_mut().map(|x| &mut x.state) {
                    state.simplify();
                }

                if elements.iter().all(|x| {
                    x.state.as_total().map_or(false, Initialized::is_true)
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
                        latest_accessor: Some(latest_accessor.clone()),
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
    memories_by_address: HashMap<address::Memory<ir::Model>, Memory>,
}

#[derive(Debug, Clone, PartialEq, Eq, Getters, CopyGetters)]
pub struct Memory {
    #[get = "pub"]
    state: State,
    #[get = "pub"]
    r#type: Type<ir::Model>,

    version: usize,
}

impl Default for Memory {
    fn default() -> Self {
        Self {
            state: State::Total(Initialized::True),
            r#type: Type::default(),
            version: 0,
        }
    }
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
    EnumAsInner,
)]
pub enum SetStateError {
    /**
    An error that occurs when giving an invalid address when setting the
    state.
    */
    InvalidAddress,

    /**
    An error that occurs when trying to access a global ID that is not
    found in the table.
     */
    GlobalIDNotFound(GlobalID),

    /// An error that occurs when isntantiating the struct field's type.
    MismatchedArgumentCount(
        #[from] MismatchedGenericArgumentCountError<ir::Model>,
    ),

    /// Encountered an overflow error obtaining the type of the field/variant.
    Overflow(#[from] OverflowError),
}

#[derive(Debug, PartialEq, Eq)]
enum SetStateResultInternal<'a> {
    Unchanged(Initialized, Address<ir::Model>),
    Done(State),
    Continue {
        state: &'a mut State,
        ty: Type<ir::Model>,
        version: &'a mut usize,
    },
}

/// Represents the result of setting the state.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SetStateSucceeded {
    /// The state was not changed. The state was already satisfied.
    Unchanged(Initialized, Address<ir::Model>),

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
    MemoryNotFound(address::Memory<ir::Model>),

    /// The memory state is not compatible.
    #[error(transparent)]
    MemoryMerge(#[from] MemoryMergeError),
}

impl Scope {
    pub fn new(scope_id: ID<scope::Scope>) -> Self {
        Self { scope_id, memories_by_address: HashMap::new() }
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
        memory: address::Memory<ir::Model>,
        initialized: bool,
        ty: Type<ir::Model>,
    ) -> bool {
        match self.memories_by_address.entry(memory) {
            Entry::Occupied(_) => false,
            Entry::Vacant(entry) => {
                entry.insert(Memory {
                    state: if initialized {
                        State::Total(Initialized::True)
                    } else {
                        State::Total(Initialized::False(Uninitialized {
                            latest_accessor: None,
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
    pub fn contains(&self, memory: &address::Memory<ir::Model>) -> bool {
        self.memories_by_address.contains_key(memory)
    }

    /// Sets the state of the value in memory as uninitialized (moved out).
    pub fn set_uninitialized<S: table::State>(
        &mut self,
        address: &Address<ir::Model>,
        load_span: Span,
        environment: &Environment<
            ir::Model,
            S,
            impl Normalizer<ir::Model, S>,
            impl Observer<ir::Model, S>,
        >,
    ) -> Result<SetStateSucceeded, SetStateError> {
        let result = self
            .set_state_internal(address, Some(load_span), true, environment)
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
    pub fn set_initialized<S: table::State>(
        &mut self,
        address: &Address<ir::Model>,
        environment: &Environment<
            ir::Model,
            S,
            impl Normalizer<ir::Model, S>,
            impl Observer<ir::Model, S>,
        >,
    ) -> Result<SetStateSucceeded, SetStateError> {
        let result = self
            .set_state_internal(address, None, true, environment)
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
    pub fn get_state(&self, address: &Address<ir::Model>) -> Option<&State> {
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
    #[allow(clippy::too_many_lines)]
    fn set_state_internal<S: table::State>(
        &mut self,
        address: &Address<ir::Model>,
        // None, if set to initialize, Some if set to uninitialized
        initialized: Option<Span>,
        root: bool,
        environment: &Environment<
            ir::Model,
            S,
            impl Normalizer<ir::Model, S>,
            impl Observer<ir::Model, S>,
        >,
    ) -> Result<SetStateResultInternal, SetStateError> {
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
                .ok_or(SetStateError::InvalidAddress)?,

            Address::Field(field) => {
                let (state, ty, version) = match self.set_state_internal(
                    &field.struct_address,
                    initialized.clone(),
                    false,
                    environment,
                )? {
                    SetStateResultInternal::Continue { state, ty, version } => {
                        (state, ty, version)
                    }
                    rest => return Ok(rest),
                };

                let Type::Symbol(Symbol {
                    id: SymbolID::Adt(AdtID::Struct(struct_id)),
                    generic_arguments,
                }) = ty
                else {
                    return Err(SetStateError::InvalidAddress);
                };

                let struct_symbol = environment
                    .table()
                    .get(struct_id)
                    .ok_or(SetStateError::GlobalIDNotFound(struct_id.into()))?;

                if !struct_symbol.field_declaration_order().contains(&field.id)
                {
                    return Err(SetStateError::InvalidAddress);
                }

                let instantiation = Instantiation::from_generic_arguments(
                    generic_arguments,
                    struct_id.into(),
                    &struct_symbol.generic_declaration.parameters,
                )?;

                if let State::Total(current) = state {
                    // already satisfied
                    if current.is_true() == initialized.is_none() {
                        return Ok(SetStateResultInternal::Unchanged(
                            current.clone(),
                            (*field.struct_address).clone(),
                        ));
                    }

                    *state = State::Projection(Projection::Struct(Struct {
                        struct_id,
                        states_by_field_id: struct_symbol
                            .field_declaration_order()
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
                        .ok_or(SetStateError::InvalidAddress)?
                        .states_by_field_id
                        .get_mut(&field.id)
                        .ok_or(SetStateError::InvalidAddress)?,
                    {
                        let mut ty = Type::from_default_model(
                            struct_symbol
                                .fields()
                                .get(field.id)
                                .unwrap()
                                .r#type
                                .clone(),
                        );

                        instantiation::instantiate(&mut ty, &instantiation);

                        simplify::simplify(&ty, environment)?.result
                    },
                    version,
                )
            }

            Address::Tuple(tuple) => {
                let (state, ty, version) = match self.set_state_internal(
                    &tuple.tuple_address,
                    initialized.clone(),
                    false,
                    environment,
                )? {
                    SetStateResultInternal::Continue { state, ty, version } => {
                        (state, ty, version)
                    }
                    rest => return Ok(rest),
                };

                let Type::Tuple(term::Tuple { elements }) = ty else {
                    return Err(SetStateError::InvalidAddress);
                };

                if let State::Total(current) = state {
                    // already satisfied
                    if current.is_true() == initialized.is_none() {
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
                    state
                        .as_projection_mut()
                        .and_then(|x| x.as_tuple_mut())
                        .ok_or(SetStateError::InvalidAddress)?
                        .elements
                        .get_mut(match tuple.offset {
                            Offset::FromStart(index) => index,
                            Offset::FromEnd(index) => elements.len() - index,
                        })
                        .map(|x| &mut x.state)
                        .ok_or(SetStateError::InvalidAddress)?,
                    {
                        elements
                            .get(match tuple.offset {
                                Offset::FromStart(index) => index,
                                Offset::FromEnd(index) => {
                                    elements.len() - index
                                }
                            })
                            .unwrap()
                            .term
                            .clone()
                    },
                    version,
                )
            }

            Address::Index(_) => {
                return Err(SetStateError::InvalidAddress);
            }

            Address::Variant(variant) => {
                let (state, ty, version) = match self.set_state_internal(
                    &variant.enum_address,
                    initialized.clone(),
                    false,
                    environment,
                )? {
                    SetStateResultInternal::Continue { state, ty, version } => {
                        (state, ty, version)
                    }
                    rest => return Ok(rest),
                };

                let Type::Symbol(Symbol {
                    id: SymbolID::Adt(AdtID::Enum(enum_id)),
                    generic_arguments,
                }) = ty
                else {
                    return Err(SetStateError::InvalidAddress);
                };

                let enum_symbol = environment
                    .table()
                    .get(enum_id)
                    .ok_or(SetStateError::GlobalIDNotFound(enum_id.into()))?;

                let variant_symbol = environment
                    .table()
                    .get(variant.id)
                    .ok_or(SetStateError::InvalidAddress)?;

                let assocated_type = variant_symbol
                    .associated_type
                    .clone()
                    .ok_or(SetStateError::InvalidAddress)?;

                let instantiation = Instantiation::from_generic_arguments(
                    generic_arguments,
                    enum_id.into(),
                    &enum_symbol.generic_declaration.parameters,
                )?;

                if let State::Total(current) = state {
                    // already satisfied
                    if current.is_true() == initialized.is_none() {
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
                        .ok_or(SetStateError::InvalidAddress)?
                        .state
                        .as_mut(),
                    {
                        let mut ty = Type::from_default_model(assocated_type);

                        instantiation::instantiate(&mut ty, &instantiation);

                        simplify::simplify(&ty, environment)?.result
                    },
                    version,
                )
            }

            Address::Reference(reference) => {
                // must be a mutable reference
                let (state, ty, version) = match self.set_state_internal(
                    &reference.reference_address,
                    initialized.clone(),
                    false,
                    environment,
                )? {
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
                    return Err(SetStateError::InvalidAddress);
                };

                // must be mutable
                if address.get_reference_qualifier()
                    != Some(r#type::Qualifier::Mutable)
                    || qualifier != r#type::Qualifier::Mutable
                {
                    return Err(SetStateError::InvalidAddress);
                }

                if let State::Total(current) = state {
                    // already satisfied
                    if current.is_true() == initialized.is_none() {
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
                        .ok_or(SetStateError::InvalidAddress)?
                        .state
                        .as_mut(),
                    *pointee,
                    version,
                )
            }
        };

        if root {
            let repalced = match initialized {
                Some(id) => {
                    let current_version = *version;
                    *version += 1;

                    std::mem::replace(
                        target_state,
                        State::Total(Initialized::False(Uninitialized {
                            latest_accessor: Some(id),
                            version: current_version,
                        })),
                    )
                }
                None => std::mem::replace(
                    target_state,
                    State::Total(Initialized::True),
                ),
            };

            Ok(SetStateResultInternal::Done(repalced))
        } else {
            Ok(SetStateResultInternal::Continue {
                state: target_state,
                ty,
                version,
            })
        }
    }
}
