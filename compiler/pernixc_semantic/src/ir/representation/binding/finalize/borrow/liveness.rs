use std::collections::{HashMap, HashSet};

use enum_as_inner::EnumAsInner;

use crate::{
    arena::{Key, ID},
    error::{OverflowOperation, TypeSystemOverflow, Usage},
    ir::{
        self,
        address::{self, Address, Memory},
        control_flow_graph::{Block, ControlFlowGraph, Point},
        instruction::{AccessKind, AccessMode, Instruction, Jump, Terminator},
        representation::borrow::{Model as BorrowModel, Region},
        value::register::Register,
    },
    symbol::{
        self,
        table::{self, representation::Index as _},
        AdtID, Field, GlobalID,
    },
    type_system::{
        environment::Environment,
        instantiation::{self, Instantiation},
        model::Model,
        normalizer::Normalizer,
        observer::Observer,
        simplify,
        term::{
            r#type::{SymbolID, Type},
            Symbol, Term,
        },
        OverflowError,
    },
};

/// An error occurs when trying to set the accessed address.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumAsInner,
    thiserror::Error,
    displaydoc::Display,
)]
pub enum SetAccessedError {
    /**
    An error occurs because of the invalid input given to the function.
     */
    Internal(String),

    /**
    Encountered an overflow error when trying to instantiate a type.
     */
    Overflow(#[from] OverflowError),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TupleElement {
    projection: Assigned,
    is_unpacked: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Tuple {
    elements: Vec<TupleElement>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Struct {
    projections_by_field_id: HashMap<ID<Field>, Assigned>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Variant {
    variant_projection: Box<Assigned>,
    variant_id: ID<symbol::Variant>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Dereference {
    projection: Box<Assigned>,
}

/// An enumeration representing the state whether a particular address has been
/// assigned or not.
#[derive(Debug, Clone, PartialEq, Eq, EnumAsInner)]
pub enum Assigned {
    Struct(Struct),
    Tuple(Tuple),
    Variant(Variant),
    Dereference(Dereference),

    /// `true` means has been assigned, `false` otherwise.
    Whole(bool),
}

impl Assigned {
    /// Returns `true` if every address in the projection has been accessed.
    pub fn is_assigned(&self) -> bool {
        match self {
            Self::Struct(struct_proj) => struct_proj
                .projections_by_field_id
                .values()
                .all(Self::is_assigned),

            Self::Tuple(tuple_proj) => tuple_proj
                .elements
                .iter()
                .all(|element| element.projection.is_assigned()),

            Self::Variant(variant_proj) => {
                variant_proj.variant_projection.is_assigned()
            }

            Self::Dereference(_) => false,

            Self::Whole(assigned) => *assigned,
        }
    }

    /// Returns assignment state for the given address.
    pub fn from_address(&self, address: &Address<impl Model>) -> &Self {
        match address {
            Address::Memory(_) => self,

            Address::Field(field) => match self {
                Self::Struct(struct_proj) => struct_proj
                    .projections_by_field_id
                    .get(&field.id)
                    .unwrap()
                    .from_address(&field.struct_address),

                Self::Whole(_) => self,

                _ => panic!(
                    "expected a struct/whole; found address: {address:?}, \
                     found assigned: {self:?}"
                ),
            },

            Address::Tuple(tuple) => match self {
                Self::Tuple(tuple_proj) => {
                    let index = match tuple.offset {
                        address::Offset::FromStart(index) => index,
                        address::Offset::FromEnd(index) => {
                            tuple_proj.elements.len() - index
                        }
                        address::Offset::Unpacked => {
                            for element in &tuple_proj.elements {
                                if element.is_unpacked {
                                    return element
                                        .projection
                                        .from_address(&tuple.tuple_address);
                                }
                            }

                            panic!("unpacked tuple type not found");
                        }
                    };

                    tuple_proj.elements[index]
                        .projection
                        .from_address(&tuple.tuple_address)
                }

                Self::Whole(_) => self,

                _ => panic!(
                    "expected a tuple/whole; found address: {address:?}, \
                     found assigned: {self:?}"
                ),
            },

            Address::Index(_) => {
                assert!(self.is_whole());

                self
            }

            Address::Variant(variant) => match self {
                Self::Variant(variant_proj) => variant_proj
                    .variant_projection
                    .from_address(&variant.enum_address),

                Self::Whole(_) => self,

                _ => panic!(
                    "expected a variant/whole; found address: {address:?}, \
                     found assigned: {self:?}"
                ),
            },

            Address::Reference(reference) => match self {
                Self::Dereference(deref_proj) => deref_proj
                    .projection
                    .from_address(&reference.reference_address),

                Self::Whole(_) => self,

                _ => panic!(
                    "expected a dereference/whole; found address: \
                     {address:?}, found assigned: {self:?}"
                ),
            },
        }
    }
}

#[derive(Debug, EnumAsInner)]
enum SetAssignedResultInternal<'a, M: Model> {
    Done(bool),
    Continue { projection: &'a mut Assigned, ty: Type<M> },
}

impl Assigned {
    /// Remembers that the given `address` has been accessed. Returns `true`
    /// if the address is accessed for the first time.
    pub fn set_assigned<M: Model, S: table::State>(
        &mut self,
        address: &Address<M>,
        root_ty: Type<M>,
        environment: &Environment<
            M,
            S,
            impl Normalizer<M, S>,
            impl Observer<M, S>,
        >,
    ) -> Result<bool, SetAccessedError> {
        Ok(self
            .set_accessed_internal(address, root_ty, true, environment)?
            .into_done()
            .unwrap())
    }

    #[allow(clippy::too_many_lines)]
    fn set_accessed_internal<M: Model, S: table::State>(
        &mut self,
        address: &Address<M>,
        root_ty: Type<M>,
        root: bool,
        environment: &Environment<
            M,
            S,
            impl Normalizer<M, S>,
            impl Observer<M, S>,
        >,
    ) -> Result<SetAssignedResultInternal<M>, SetAccessedError> {
        // no more work to do
        if self.is_assigned() {
            return Ok(SetAssignedResultInternal::Done(false));
        }

        let (target_projection, ty) = match address {
            Address::Memory(_) => (self, root_ty),

            Address::Field(field) => {
                let (proj, ty) = match self.set_accessed_internal(
                    &field.struct_address,
                    root_ty,
                    false,
                    environment,
                )? {
                    done @ SetAssignedResultInternal::Done(_) => {
                        return Ok(done);
                    }

                    SetAssignedResultInternal::Continue { projection, ty } => {
                        (projection, ty)
                    }
                };

                let (struct_id, generic_arguments) = match ty {
                    Type::Symbol(Symbol {
                        id: SymbolID::Adt(AdtID::Struct(struct_id)),
                        generic_arguments,
                    }) => (struct_id, generic_arguments),

                    ty => {
                        return Err(SetAccessedError::Internal(format!(
                            "expected a struct type, found `{ty:?}`",
                        )));
                    }
                };

                let struct_symbol =
                    environment.table().get(struct_id).ok_or_else(|| {
                        SetAccessedError::Internal(format!(
                            "symbol ID: {struct_id:?} not found",
                        ))
                    })?;

                let inst = Instantiation::from_generic_arguments(
                    generic_arguments,
                    struct_id.into(),
                    &struct_symbol.generic_declaration.parameters,
                )
                .map_err(|x| {
                    SetAccessedError::Internal(format!(
                        "failed to create instantiation {x:?}"
                    ))
                })?;

                if let Self::Whole(whole_proj) = proj {
                    // if have concluded, we should've returned earlier
                    assert!(!*whole_proj);

                    *proj = Self::Struct(Struct {
                        projections_by_field_id: struct_symbol
                            .fields_as_order()
                            .map(|(field_id, _)| (field_id, Self::Whole(false)))
                            .collect(),
                    });
                }

                (
                    proj.as_struct_mut()
                        .ok_or_else(|| {
                            SetAccessedError::Internal(
                                "expected a struct type".to_string(),
                            )
                        })?
                        .projections_by_field_id
                        .get_mut(&field.id)
                        .ok_or_else(|| {
                            SetAccessedError::Internal(format!(
                                "field ID: {:?} not found in struct ID: {:?}",
                                field.id, struct_id
                            ))
                        })?,
                    {
                        let mut field_ty = Type::from_default_model(
                            struct_symbol
                                .fields()
                                .get(field.id)
                                .ok_or_else(|| {
                                    SetAccessedError::Internal(format!(
                                        "field ID: {:?} not found in struct \
                                         ID: {:?}",
                                        field.id, struct_id
                                    ))
                                })?
                                .r#type
                                .clone(),
                        );

                        instantiation::instantiate(&mut field_ty, &inst);

                        simplify::simplify(&field_ty, environment)?.result
                    },
                )
            }
            Address::Tuple(tuple) => {
                let (proj, ty) = match self.set_accessed_internal(
                    &tuple.tuple_address,
                    root_ty,
                    false,
                    environment,
                )? {
                    done @ SetAssignedResultInternal::Done(_) => {
                        return Ok(done);
                    }

                    SetAssignedResultInternal::Continue { projection, ty } => {
                        (projection, ty)
                    }
                };

                let mut tuple_ty = ty.into_tuple().map_err(|x| {
                    SetAccessedError::Internal(format!(
                        "expected a tuple type, found `{x:?}`",
                    ))
                })?;

                if let Self::Whole(whole_proj) = proj {
                    // if have concluded, we should've returned earlier
                    assert!(!*whole_proj);

                    *proj = Self::Tuple(Tuple {
                        elements: tuple_ty
                            .elements
                            .iter()
                            .map(|term| TupleElement {
                                projection: Self::Whole(false),
                                is_unpacked: term.is_unpacked,
                            })
                            .collect(),
                    });
                }

                (
                    'proj: {
                        let tuple_proj =
                            proj.as_tuple_mut().ok_or_else(|| {
                                SetAccessedError::Internal(
                                    "expected a tuple type".to_string(),
                                )
                            })?;

                        let index = match tuple.offset {
                            address::Offset::FromStart(index) => index,
                            address::Offset::FromEnd(index) => {
                                tuple_ty.elements.len() - index
                            }
                            address::Offset::Unpacked => {
                                break 'proj tuple_proj
                                    .elements
                                    .iter_mut()
                                    .find_map(|element| {
                                        element
                                            .is_unpacked
                                            .then_some(&mut element.projection)
                                    })
                                    .ok_or_else(|| {
                                        SetAccessedError::Internal(
                                            "unpacked tuple type not found"
                                                .to_string(),
                                        )
                                    })?;
                            }
                        };

                        &mut tuple_proj.elements[index].projection
                    },
                    'ty: {
                        let index = match tuple.offset {
                            address::Offset::FromStart(index) => index,
                            address::Offset::FromEnd(index) => {
                                tuple_ty.elements.len() - index
                            }
                            address::Offset::Unpacked => {
                                for element in tuple_ty.elements {
                                    if element.is_unpacked {
                                        break 'ty element.term;
                                    }
                                }

                                return Err(SetAccessedError::Internal(
                                    "unpacked tuple type not found".to_string(),
                                ));
                            }
                        };

                        if index >= tuple_ty.elements.len() {
                            return Err(SetAccessedError::Internal(format!(
                                "index out of bounds: {} >= {}",
                                index,
                                tuple_ty.elements.len()
                            )));
                        }

                        tuple_ty.elements.swap_remove(index).term
                    },
                )
            }

            Address::Index(_) => {
                return Ok(SetAssignedResultInternal::Done(false));
            }

            Address::Variant(variant) => {
                let (proj, ty) = match self.set_accessed_internal(
                    &variant.enum_address,
                    root_ty,
                    false,
                    environment,
                )? {
                    done @ SetAssignedResultInternal::Done(_) => {
                        return Ok(done);
                    }

                    SetAssignedResultInternal::Continue { projection, ty } => {
                        (projection, ty)
                    }
                };

                let (enum_id, generic_arguments) = match ty {
                    Type::Symbol(Symbol {
                        id: SymbolID::Adt(AdtID::Enum(enum_id)),
                        generic_arguments,
                    }) => (enum_id, generic_arguments),

                    ty => {
                        return Err(SetAccessedError::Internal(format!(
                            "expected an enum type, found `{ty:?}`",
                        )));
                    }
                };

                let enum_symbol =
                    environment.table().get(enum_id).ok_or_else(|| {
                        SetAccessedError::Internal(format!(
                            "symbol ID: {enum_id:?} not found",
                        ))
                    })?;

                let variant_symbol =
                    environment.table().get(variant.id).ok_or_else(|| {
                        SetAccessedError::Internal(format!(
                            "variant ID: {:?} not found",
                            variant.id
                        ))
                    })?;

                let instantiation = Instantiation::from_generic_arguments(
                    generic_arguments,
                    enum_id.into(),
                    &enum_symbol.generic_declaration.parameters,
                )
                .map_err(|x| {
                    SetAccessedError::Internal(format!(
                        "failed to create instantiation {x:?}"
                    ))
                })?;

                if let Self::Whole(whole_proj) = proj {
                    // if have concluded, we should've returned earlier
                    assert!(!*whole_proj);

                    *proj = Self::Variant(Variant {
                        variant_projection: Box::new(Self::Whole(false)),
                        variant_id: variant.id,
                    });
                }

                (
                    proj.as_variant_mut()
                        .ok_or_else(|| {
                            SetAccessedError::Internal(
                                "expected a variant type".to_string(),
                            )
                        })?
                        .variant_projection
                        .as_mut(),
                    {
                        let mut variant_ty = Type::from_default_model(
                            variant_symbol
                                .associated_type
                                .as_ref()
                                .ok_or_else(|| {
                                    SetAccessedError::Internal(format!(
                                        "variant ID: {:?} has no associated \
                                         type",
                                        variant.id
                                    ))
                                })?
                                .clone(),
                        );

                        instantiation::instantiate(
                            &mut variant_ty,
                            &instantiation,
                        );

                        simplify::simplify(&variant_ty, environment)?.result
                    },
                )
            }

            Address::Reference(reference) => {
                let (proj, ty) = match self.set_accessed_internal(
                    &reference.reference_address,
                    root_ty,
                    false,
                    environment,
                )? {
                    done @ SetAssignedResultInternal::Done(_) => {
                        return Ok(done);
                    }

                    SetAssignedResultInternal::Continue { projection, ty } => {
                        (projection, ty)
                    }
                };

                let ty = ty.into_reference().map_err(|x| {
                    SetAccessedError::Internal(format!(
                        "expected a reference type, found `{x:?}`",
                    ))
                })?;

                if let Self::Whole(whole_proj) = proj {
                    // if have concluded, we should've returned earlier
                    assert!(!*whole_proj);

                    *proj = Self::Dereference(Dereference {
                        projection: Box::new(Self::Whole(false)),
                    });
                }

                (
                    proj.as_dereference_mut()
                        .ok_or_else(|| {
                            SetAccessedError::Internal(
                                "expected a dereference type".to_string(),
                            )
                        })?
                        .projection
                        .as_mut(),
                    *ty.pointee,
                )
            }
        };

        Ok(if root {
            *target_projection = Self::Whole(true);

            SetAssignedResultInternal::Done(true)
        } else {
            SetAssignedResultInternal::Continue {
                projection: target_projection,
                ty,
            }
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ControlFlow<T> {
    Continue,
    Break(T),
}

/// A trait for the state machine that traverses the control flow graph.
///
/// # Note
///
/// Cloning is used when encountering a branch in the control flow graph.
trait Traverser<M: Model>: Clone {
    type Error;
    type Result: Default;

    fn on_instruction(
        &mut self,
        point: Point<M>,
        instruction: &Instruction<M>,
    ) -> Result<ControlFlow<Self::Result>, Self::Error>;

    fn on_terminator(
        &mut self,
        block_id: ID<Block<M>>,
        terminator: Option<&Terminator<M>>,
    ) -> Result<ControlFlow<Self::Result>, Self::Error>;

    fn fold_result(
        previous: Self::Result,
        next: Self::Result,
    ) -> (Self::Result, bool);
}

fn traverse_block<M: Model, T: Traverser<M>>(
    traverser_state: &mut T,
    control_flow_graph: &ControlFlowGraph<M>,
    point: Point<M>,
    exit: &mut impl FnMut(&Instruction<M>, Point<M>) -> bool,
) -> Result<T::Result, T::Error> {
    traverse_block_internal(
        traverser_state,
        control_flow_graph,
        point.block_id,
        Some(point.instruction_index),
        exit,
        &mut HashSet::new(),
    )
}

fn traverse_block_internal<M: Model, T: Traverser<M>>(
    traverser_state: &mut T,
    control_flow_graph: &ControlFlowGraph<M>,
    block_id: ID<Block<M>>,
    starting_instruction_index: Option<usize>,
    exit: &mut impl FnMut(&Instruction<M>, Point<M>) -> bool,
    visited: &mut HashSet<(usize, Option<usize>)>,
) -> Result<T::Result, T::Error> {
    if starting_instruction_index.is_none()
        && !visited.insert((block_id.into_index(), None))
    {
        return Ok(T::Result::default());
    }

    let block = control_flow_graph.blocks().get(block_id).unwrap();

    for (index, instruction) in block
        .instructions()
        .iter()
        .enumerate()
        .skip(starting_instruction_index.map_or(0, |x| x + 1))
    {
        // skip to the starting instruction index
        if visited.contains(&(block_id.into_index(), Some(index))) {
            return Ok(T::Result::default());
        }

        // if the next instruction is the starting instruction index, we
        // should mark the current instruction as visited
        if starting_instruction_index.map_or(false, |x| x + 1 == index) {
            assert!(visited.insert((block_id.into_index(), Some(index))));
        }

        // explicitly exit the traversal
        if exit(instruction, Point { block_id, instruction_index: index }) {
            return Ok(T::Result::default());
        }

        match traverser_state.on_instruction(
            Point { block_id, instruction_index: index },
            instruction,
        )? {
            ControlFlow::Continue => {}
            ControlFlow::Break(result) => return Ok(result),
        }
    }

    if let ControlFlow::Break(result) =
        traverser_state.on_terminator(block_id, block.terminator().as_ref())?
    {
        return Ok(result);
    }

    match block.terminator() {
        Some(Terminator::Jump(jump)) => match jump {
            Jump::Unconditional(unconditional_jump) => traverse_block_internal(
                traverser_state,
                control_flow_graph,
                unconditional_jump.target,
                None,
                exit,
                visited,
            ),
            Jump::Conditional(conditional_jump) => {
                let true_usages = traverse_block_internal(
                    &mut traverser_state.clone(),
                    control_flow_graph,
                    conditional_jump.true_target,
                    None,
                    exit,
                    &mut visited.clone(),
                )?;
                let false_usages = traverse_block_internal(
                    traverser_state,
                    control_flow_graph,
                    conditional_jump.false_target,
                    None,
                    exit,
                    &mut visited.clone(),
                )?;

                Ok(T::fold_result(true_usages, false_usages).0)
            }
            Jump::Select(select_jump) => {
                let mut blocks = select_jump
                    .branches
                    .values()
                    .copied()
                    .chain(select_jump.otherwise);

                let Some(first_block) = blocks.next() else {
                    return Ok(T::Result::default());
                };
                let mut first_result = traverse_block_internal(
                    &mut traverser_state.clone(),
                    control_flow_graph,
                    first_block,
                    None,
                    exit,
                    &mut visited.clone(),
                )?;

                for block in select_jump.branches.values().copied() {
                    let new_result = traverse_block_internal(
                        &mut traverser_state.clone(),
                        control_flow_graph,
                        block,
                        None,
                        exit,
                        &mut visited.clone(),
                    )?;

                    let (folded, return_now) =
                        T::fold_result(first_result, new_result);

                    if return_now {
                        return Ok(folded);
                    }

                    first_result = folded;
                }

                Ok(first_result)
            }
        },

        Some(Terminator::Return(_)) | Some(Terminator::Panic) | None => {
            Ok(T::Result::default())
        }
    }
}

/// Returns the *live* addresses in the given `check_address`.
///
/// For example,
///
/// ```pnx
/// let a = (true, true, true);
/// // <-- start liveness checking here
/// a.0 = false;
/// a.2 = false;
/// print(a.1);
/// // <-- stop liveness checking here
/// ```
///
/// the live addresses is only `a.1`.
pub fn get_live_usages<S: table::State>(
    memories: impl IntoIterator<Item = Memory<BorrowModel>>,
    checking_registers: HashSet<ID<Register<BorrowModel>>>,
    invalidated_regions: &HashSet<Region>,
    invalidated_borrow_register_id: ID<Register<BorrowModel>>,
    point: Point<BorrowModel>,
    mut exit: &mut impl FnMut(&Instruction<BorrowModel>, Point<BorrowModel>) -> bool,
    representation: &ir::Representation<BorrowModel>,
    current_site: GlobalID,
    environment: &Environment<
        BorrowModel,
        S,
        impl Normalizer<BorrowModel, S>,
        impl Observer<BorrowModel, S>,
    >,
) -> Result<HashSet<Usage>, TypeSystemOverflow<ir::Model>> {
    let mut state = LiveBorrowTraverser {
        assigned_states_by_memory: memories
            .into_iter()
            .map(|memory| (memory, Assigned::Whole(false)))
            .collect(),
        invalidated_borrow_register_id,
        checking_registers,
        invalidated_regions,
        representation,
        environment,
        current_site,
    };

    traverse_block(
        &mut state,
        &representation.control_flow_graph,
        point,
        &mut exit,
    )
}

/// The state struct used for keep tracking the liveness of the addresses.
#[derive(Debug)]
struct LiveBorrowTraverser<
    'a,
    S: table::State,
    N: Normalizer<BorrowModel, S>,
    O: Observer<BorrowModel, S>,
> {
    assigned_states_by_memory: HashMap<Memory<BorrowModel>, Assigned>,
    checking_registers: HashSet<ID<Register<BorrowModel>>>,
    invalidated_regions: &'a HashSet<Region>,
    invalidated_borrow_register_id: ID<Register<BorrowModel>>,

    representation: &'a ir::Representation<BorrowModel>,
    environment: &'a Environment<'a, BorrowModel, S, N, O>,
    current_site: GlobalID,
}

impl<
        'a,
        S: table::State,
        N: Normalizer<BorrowModel, S>,
        O: Observer<BorrowModel, S>,
    > Clone for LiveBorrowTraverser<'a, S, N, O>
{
    fn clone(&self) -> Self {
        Self {
            assigned_states_by_memory: self.assigned_states_by_memory.clone(),
            checking_registers: self.checking_registers.clone(),
            invalidated_borrow_register_id: self.invalidated_borrow_register_id,

            invalidated_regions: self.invalidated_regions,
            representation: self.representation,
            environment: self.environment,
            current_site: self.current_site,
        }
    }
}

impl<
        'a,
        S: table::State,
        N: Normalizer<BorrowModel, S>,
        O: Observer<BorrowModel, S>,
    > Traverser<BorrowModel> for LiveBorrowTraverser<'a, S, N, O>
{
    type Error = TypeSystemOverflow<ir::Model>;

    type Result = HashSet<Usage>;

    fn on_instruction(
        &mut self,
        _: Point<BorrowModel>,
        instruction: &Instruction<BorrowModel>,
    ) -> Result<ControlFlow<Self::Result>, Self::Error> {
        let accesses = instruction
            .get_access_address(&self.representation.values)
            .unwrap();

        // check each access
        for (address, kind) in accesses {
            let memory_root = address.get_root_memory();
            let memory_root_ty = match *memory_root {
                Memory::Parameter(id) => Type::from_default_model(
                    self.environment
                        .table()
                        .get_callable(self.current_site.try_into().unwrap())
                        .unwrap()
                        .parameters()
                        .get(id)
                        .unwrap()
                        .r#type
                        .clone(),
                ),
                Memory::Alloca(id) => self
                    .representation
                    .values
                    .allocas
                    .get(id)
                    .unwrap()
                    .r#type
                    .clone(),
            };

            let Some(assigned_state) =
                self.assigned_states_by_memory.get_mut(&memory_root)
            else {
                // skip irrelevant addresses
                continue;
            };

            // check if the address is accessed
            if assigned_state.from_address(&address).is_assigned() {
                // has already assigned, dead address
                continue;
            }

            let (read_span, is_drop) = match kind {
                AccessKind::Normal(AccessMode::Write(write_span)) => {
                    // check if the write dereference the regions that are
                    // invalidated
                    let dereferenced_regions = self
                        .representation
                        .values
                        .get_dereferenced_regions_in_address(
                            &address,
                            &write_span,
                            self.current_site,
                            self.environment,
                        )?;

                    if dereferenced_regions
                        .iter()
                        .any(|x| self.invalidated_regions.contains(x))
                    {
                        return Ok(ControlFlow::Break(
                            std::iter::once(Usage::Local(write_span)).collect(),
                        ));
                    }

                    match assigned_state.set_assigned(
                        &address,
                        memory_root_ty,
                        self.environment,
                    ) {
                        Ok(_) => {
                            continue;
                        }

                        Err(SetAccessedError::Internal(reason)) => {
                            panic!("{reason}")
                        }

                        Err(SetAccessedError::Overflow(overflow_error)) => {
                            return Err(TypeSystemOverflow {
                                operation: OverflowOperation::TypeOf,
                                overflow_span: write_span,
                                overflow_error,
                            });
                        }
                    }
                }

                AccessKind::Normal(AccessMode::Read(read)) => {
                    (read.span, false)
                }
                AccessKind::Drop => (
                    match *memory_root {
                        Memory::Parameter(id) => self
                            .environment
                            .table()
                            .get_callable(self.current_site.try_into().unwrap())
                            .unwrap()
                            .parameters()
                            .get(id)
                            .unwrap()
                            .span
                            .clone()
                            .unwrap(),
                        Memory::Alloca(id) => self
                            .representation
                            .values
                            .allocas
                            .get(id)
                            .unwrap()
                            .span
                            .clone(),
                    },
                    true,
                ),
            };

            let read_lifetimes =
                self.representation.values.get_regions_in_address(
                    &address,
                    &read_span,
                    true,
                    self.current_site,
                    self.environment,
                )?;

            // check if the lifetime appears in the invalidated regions
            let has_invalidated_region = read_lifetimes
                .iter()
                .any(|x| self.invalidated_regions.contains(x));

            // found usage of invalidated regions
            if has_invalidated_region {
                return Ok(ControlFlow::Break(
                    std::iter::once(if is_drop {
                        Usage::Drop
                    } else {
                        Usage::Local(read_span)
                    })
                    .collect(),
                ));
            }
        }

        let invalidated_use_register = match instruction {
            Instruction::Store(store) => {
                store.value.as_register().and_then(|x| {
                    self.checking_registers.contains(x).then_some(*x)
                })
            }

            Instruction::RegisterAssignment(register_assignment) => {
                // if self.invalidated_borrow_register_id ==
                // register_assignment.id {
                //     return Ok(ControlFlow::Break(HashSet::new()));
                // }

                self.checking_registers.remove(&register_assignment.id);

                let register = self
                    .representation
                    .values
                    .registers
                    .get(register_assignment.id)
                    .unwrap();

                let used_registers = register.assignment.get_used_registers();

                used_registers
                    .iter()
                    .any(|x| self.checking_registers.contains(x))
                    .then_some(register_assignment.id)
            }

            Instruction::RegisterDiscard(register_discard) => {
                self.checking_registers.remove(&register_discard.id);

                None
            }

            Instruction::TuplePack(_)
            | Instruction::ScopePush(_)
            | Instruction::ScopePop(_)
            | Instruction::DropUnpackTuple(_)
            | Instruction::Drop(_) => None,
        };

        if let Some(register_id) = invalidated_use_register {
            return Ok(ControlFlow::Break(
                std::iter::once(Usage::Local(
                    self.representation
                        .values
                        .registers
                        .get(register_id)
                        .unwrap()
                        .span
                        .clone(),
                ))
                .collect(),
            ));
        }

        Ok(ControlFlow::Continue)
    }

    fn on_terminator(
        &mut self,
        _: ID<Block<BorrowModel>>,
        _: Option<&Terminator<BorrowModel>>,
    ) -> Result<ControlFlow<Self::Result>, Self::Error> {
        Ok(ControlFlow::Continue)
    }

    fn fold_result(
        mut previous: Self::Result,
        next: Self::Result,
    ) -> (Self::Result, bool) {
        (
            {
                previous.extend(next.into_iter());
                previous
            },
            false,
        )
    }
}

pub fn is_live<S: table::State>(
    checking_address: &Address<BorrowModel>,
    root_address_type: &Type<BorrowModel>,
    from: Point<BorrowModel>,
    to: Point<BorrowModel>,
    representation: &ir::Representation<BorrowModel>,
    environment: &Environment<
        BorrowModel,
        S,
        impl Normalizer<BorrowModel, S>,
        impl Observer<BorrowModel, S>,
    >,
) -> Result<bool, TypeSystemOverflow<ir::Model>> {
    let mut traverser = LiveLenderTraverser {
        root_memory: *checking_address.get_root_memory(),
        assigned_states: Assigned::Whole(false),
        checking_address,
        representation,
        to_point: to,
        root_type: root_address_type,
        environment,
    };

    traverse_block(
        &mut traverser,
        &representation.control_flow_graph,
        from,
        &mut |_, _| false,
    )
}

#[derive(Debug)]
struct LiveLenderTraverser<
    'a,
    S: table::State,
    N: Normalizer<BorrowModel, S>,
    O: Observer<BorrowModel, S>,
> {
    root_memory: Memory<BorrowModel>,
    assigned_states: Assigned,
    checking_address: &'a Address<BorrowModel>,
    root_type: &'a Type<BorrowModel>,
    to_point: Point<BorrowModel>,

    environment: &'a Environment<'a, BorrowModel, S, N, O>,
    representation: &'a ir::Representation<BorrowModel>,
}

impl<
        'a,
        S: table::State,
        N: Normalizer<BorrowModel, S>,
        O: Observer<BorrowModel, S>,
    > Clone for LiveLenderTraverser<'a, S, N, O>
{
    fn clone(&self) -> Self {
        Self {
            root_memory: self.root_memory,
            assigned_states: self.assigned_states.clone(),
            checking_address: self.checking_address,
            root_type: self.root_type,
            representation: self.representation,
            to_point: self.to_point,

            environment: self.environment,
        }
    }
}

impl<
        'a,
        S: table::State,
        N: Normalizer<BorrowModel, S>,
        O: Observer<BorrowModel, S>,
    > Traverser<BorrowModel> for LiveLenderTraverser<'a, S, N, O>
{
    type Error = TypeSystemOverflow<ir::Model>;

    /// True if the address is reachable to the `to_point` without the address
    /// be reassigned or go you of scope.
    type Result = bool;

    fn on_instruction(
        &mut self,
        point: Point<BorrowModel>,
        instruction: &Instruction<BorrowModel>,
    ) -> Result<ControlFlow<Self::Result>, Self::Error> {
        if point == self.to_point {
            return Ok(ControlFlow::Break(true));
        }

        match instruction {
            Instruction::Store(store) => {
                let overlapping_addresses =
                    store.address.is_child_of(&self.checking_address)
                        || self.checking_address.is_child_of(&store.address);

                // non-related address
                if !overlapping_addresses {
                    return Ok(ControlFlow::Continue);
                }

                match self.assigned_states.set_assigned(
                    &store.address,
                    self.root_type.clone(),
                    self.environment,
                ) {
                    Ok(_) => {}

                    Err(SetAccessedError::Internal(reason)) => {
                        panic!("{reason}")
                    }

                    Err(SetAccessedError::Overflow(overflow_error)) => {
                        return Err(TypeSystemOverflow {
                            operation: OverflowOperation::TypeOf,
                            overflow_span: store.span.clone(),
                            overflow_error,
                        });
                    }
                }

                if self
                    .assigned_states
                    .from_address(&self.checking_address)
                    .is_assigned()
                {
                    Ok(ControlFlow::Break(false))
                } else {
                    Ok(ControlFlow::Continue)
                }
            }

            Instruction::ScopePop(scope_pop) => {
                let scope_of_memory = match self.root_memory {
                    Memory::Parameter(_) => {
                        self.representation.scope_tree.root_scope_id()
                    }
                    Memory::Alloca(id) => {
                        self.representation
                            .values
                            .allocas
                            .get(id)
                            .unwrap()
                            .declared_in_scope_id
                    }
                };

                if scope_of_memory == scope_pop.0 {
                    Ok(ControlFlow::Break(false))
                } else {
                    Ok(ControlFlow::Continue)
                }
            }

            _ => return Ok(ControlFlow::Continue),
        }
    }

    fn on_terminator(
        &mut self,
        _: ID<Block<BorrowModel>>,
        _: Option<&Terminator<BorrowModel>>,
    ) -> Result<ControlFlow<Self::Result>, Self::Error> {
        Ok(ControlFlow::Continue)
    }

    fn fold_result(
        previous: Self::Result,
        next: Self::Result,
    ) -> (Self::Result, bool) {
        let result = previous || next;

        (result, result)
    }
}
