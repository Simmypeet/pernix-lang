use std::collections::{hash_map::Entry, HashMap};

use enum_as_inner::EnumAsInner;
use getset::{CopyGetters, Getters};

use crate::{
    arena::ID,
    ir::{
        self,
        address::{self, Address},
        scope,
        value::register::Register,
    },
    symbol::{
        self,
        table::{self, representation::Index as _, Table},
        AdtID, GlobalID,
    },
    type_system::{
        instantiation::{
            self, Instantiation, MismatchedGenericArgumentCountError,
        },
        term::{
            self,
            r#type::{self, SymbolID, Type},
            Symbol, Term,
        },
    },
};

/// Contains the state of each field in the struct.
#[derive(Debug, Clone, PartialEq, Eq, Getters, CopyGetters)]
pub struct Struct {
    /// The state of each field in the struct.
    #[get = "pub"]
    states_by_field_id: HashMap<ID<symbol::Field>, State>,
}

/// Contains the state of each element in the tuple.
#[derive(Debug, Clone, PartialEq, Eq, Getters)]
pub struct Tuple {
    /// The state of each element in the tuple.
    #[get = "pub"]
    elements: Vec<State>,
}

/// Contains the state of the current active variant in the enum.
#[derive(Debug, Clone, PartialEq, Eq, Getters, CopyGetters)]
pub struct Enum {
    /// The state of the variant.
    #[get = "pub"]
    state: Box<State>,
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
pub struct Uninitialized {
    /// The register that last moved the value. If `None`, then the memory was
    /// just declared and has never been initialized.
    #[get_copy = "pub"]
    latest_accessor: Option<ID<Register<ir::Model>>>,

    /// Used to determine the order of moves. The higher the version, the
    /// later/newer the move.
    version: usize,
}

/// Represents the state of the value in memory.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner,
)]
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
) -> Option<(ID<Register<ir::Model>>, usize)> {
    let mut iterator = iterator.into_iter();

    let mut current = iterator
        .next()
        .expect("should have at least one element")
        .as_total()
        .and_then(|x| x.as_false())
        .map(|x| (x.latest_accessor.unwrap(), x.version))?;

    for state in iterator {
        let Some(next) = state
            .as_total()
            .and_then(|x| x.as_false())
            .map(|x| (x.latest_accessor.unwrap(), x.version))
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

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner,
)]
enum LatestLoadInternal {
    Uninitialized,
    Moved(ID<Register<ir::Model>>, usize),
}

/// Represents the latest register that moved the value.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner,
)]
pub enum LatestLoad {
    Uninitialized,
    Moved(ID<Register<ir::Model>>),
}

impl State {
    /// Returns the latest register ID that moved the value.
    pub fn get_latest_load_register_id(&self) -> Option<LatestLoad> {
        self.get_latest_accessor_internal().map(|x| match x {
            LatestLoadInternal::Uninitialized => LatestLoad::Uninitialized,
            LatestLoadInternal::Moved(id, _) => LatestLoad::Moved(id),
        })
    }

    fn get_latest_accessor_internal(&self) -> Option<LatestLoadInternal> {
        match self {
            Self::Total(Initialized::False(Uninitialized {
                latest_accessor,
                version,
            })) => Some(
                latest_accessor
                    .map_or(LatestLoadInternal::Uninitialized, |x| {
                        LatestLoadInternal::Moved(x, *version)
                    }),
            ),
            Self::Projection(Projection::Struct(Struct {
                states_by_field_id,
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
                        .map_or(LatestLoadInternal::Uninitialized, |x| {
                            LatestLoadInternal::Moved(x.0, x.1)
                        }),
                )
            }
            Self::Projection(Projection::Tuple(Tuple { elements })) => {
                let mut iter = elements
                    .iter()
                    .filter_map(Self::get_latest_accessor_internal);

                let first = iter.next()?;

                Some(
                    std::iter::once(first)
                        .chain(iter)
                        .filter_map(|x| x.into_moved().ok())
                        .max_by_key(|x| x.1)
                        .map_or(LatestLoadInternal::Uninitialized, |x| {
                            LatestLoadInternal::Moved(x.0, x.1)
                        }),
                )
            }
            Self::Projection(Projection::Enum(Enum { state })) => {
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
                        latest_accessor: Some(latest_accessor),
                        version,
                    }));
                }
            }
            Self::Projection(Projection::Tuple(Tuple { elements })) => {
                for state in elements.iter_mut() {
                    state.simplify();
                }

                if elements
                    .iter()
                    .all(|x| x.as_total().map_or(false, Initialized::is_true))
                {
                    *self = Self::Total(Initialized::True);
                    // empty tuple should always be initialized
                    return;
                }

                if let Some((latest_accessor, version)) =
                    try_simplify_to_uninitialized(elements.iter())
                {
                    *self = Self::Total(Initialized::False(Uninitialized {
                        latest_accessor: Some(latest_accessor),
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
            Self::Projection(Projection::Enum(Enum { state })) => {
                state.simplify();

                if let Some(initialized) = state.as_total() {
                    *self = Self::Total(*initialized);
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
}

#[derive(Debug, PartialEq, Eq)]
enum SetStateResultInternal<'a> {
    Unchanged(Initialized),
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
    Unchanged(Initialized),

    /// The state was changed. The variant holds the state that was replaced.
    Updated(State),
}

impl Scope {
    pub fn new(scope_id: ID<scope::Scope>) -> Self {
        Self { scope_id, memories_by_address: HashMap::new() }
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
    pub fn set_uninitialized(
        &mut self,
        address: &Address<ir::Model>,
        load_register_id: ID<Register<ir::Model>>,
        table: &Table<impl table::State>,
    ) -> Result<SetStateSucceeded, SetStateError> {
        self.set_state_internal(address, Some(load_register_id), true, table)
            .map(|x| match x {
                SetStateResultInternal::Unchanged(a) => {
                    SetStateSucceeded::Unchanged(a)
                }
                SetStateResultInternal::Done(state) => {
                    SetStateSucceeded::Updated(state)
                }
                SetStateResultInternal::Continue { .. } => {
                    unreachable!()
                }
            })
    }

    /// Sets the state of the value in memory as initialized.
    pub fn set_initialized(
        &mut self,
        address: &Address<ir::Model>,
        table: &Table<impl table::State>,
    ) -> Result<SetStateSucceeded, SetStateError> {
        self.set_state_internal(address, None, true, table).map(|x| match x {
            SetStateResultInternal::Unchanged(a) => {
                SetStateSucceeded::Unchanged(a)
            }
            SetStateResultInternal::Done(state) => {
                SetStateSucceeded::Updated(state)
            }
            SetStateResultInternal::Continue { .. } => {
                unreachable!()
            }
        })
    }

    // returns `Ok(None)` if the state is already satisfied
    #[allow(clippy::too_many_lines)]
    fn set_state_internal(
        &mut self,
        address: &Address<ir::Model>,
        // None, if set to initialize, Some if set to uninitialized
        initialized: Option<ID<Register<ir::Model>>>,
        root: bool,
        table: &Table<impl table::State>,
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
                    initialized,
                    false,
                    table,
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

                let struct_symbol = table
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
                        return Ok(SetStateResultInternal::Unchanged(*current));
                    }

                    *state = State::Projection(Projection::Struct(Struct {
                        states_by_field_id: struct_symbol
                            .field_declaration_order()
                            .iter()
                            .copied()
                            .map(|x| (x, State::Total(*current)))
                            .collect(),
                    }));
                }

                (
                    state
                        .as_projection_mut()
                        .and_then(|x| x.as_struct_mut())
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

                        ty
                    },
                    version,
                )
            }

            Address::Tuple(tuple) => {
                let (state, ty, version) = match self.set_state_internal(
                    &tuple.tuple_address,
                    initialized,
                    false,
                    table,
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
                        return Ok(SetStateResultInternal::Unchanged(*current));
                    }

                    *state = State::Projection(Projection::Tuple(Tuple {
                        elements: elements
                            .iter()
                            .map(|_| State::Total(*current))
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
                            address::Offset::FromStart(index) => index,
                            address::Offset::FromEnd(index) => {
                                elements.len() - index
                            }
                        })
                        .ok_or(SetStateError::InvalidAddress)?,
                    {
                        elements
                            .get(match tuple.offset {
                                address::Offset::FromStart(index) => index,
                                address::Offset::FromEnd(index) => {
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
                    initialized,
                    false,
                    table,
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

                let enum_symbol = table
                    .get(enum_id)
                    .ok_or(SetStateError::GlobalIDNotFound(enum_id.into()))?;

                let variant_symbol = table
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
                        return Ok(SetStateResultInternal::Unchanged(*current));
                    }

                    *state = State::Projection(Projection::Enum(Enum {
                        state: Box::new(State::Total(*current)),
                    }));
                }

                (
                    state
                        .as_projection_mut()
                        .and_then(|x| x.as_enum_mut())
                        .ok_or(SetStateError::InvalidAddress)?
                        .state
                        .as_mut(),
                    {
                        let mut ty = Type::from_default_model(assocated_type);

                        instantiation::instantiate(&mut ty, &instantiation);

                        ty
                    },
                    version,
                )
            }

            Address::Reference(reference) => {
                // must be a mutable reference
                let (state, ty, version) = match self.set_state_internal(
                    &reference.reference_address,
                    initialized,
                    false,
                    table,
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
                        return Ok(SetStateResultInternal::Unchanged(*current));
                    }

                    *state = State::Projection(Projection::MutableReference(
                        MutableReference {
                            state: Box::new(State::Total(*current)),
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

            target_state.simplify();

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
