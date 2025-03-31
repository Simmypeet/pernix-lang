use std::collections::{HashMap, HashSet};

use enum_as_inner::EnumAsInner;
use pernixc_abort::Abort;
use pernixc_arena::{Key, ID};
use pernixc_handler::Handler;
use pernixc_ir::{
    address::{self, Address, Memory},
    control_flow_graph::{Block, ControlFlowGraph, Point},
    instruction::{AccessKind, AccessMode, Instruction, Jump, Terminator},
    value::register::Register,
    Representation,
};
use pernixc_semantic::{
    component::{
        derived::{
            fields::{Field, Fields},
            function_signature::FunctionSignature,
            generic_parameters::GenericParameters,
            variant,
        },
        input::SymbolKind,
    },
    diagnostic::Diagnostic,
    table::GlobalID,
    term::{
        self,
        instantiation::{self, Instantiation},
        r#type::Type,
        Model as _, Symbol,
    },
};
use pernixc_source_file::Span;
use pernixc_type_system::{environment::Environment, normalizer::Normalizer};

use crate::{
    diagnostic::Usage, get_dereferenced_regions_in_address,
    get_regions_in_address, Model as BorrowModel, Region,
};

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
    variant_id: GlobalID,
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
    pub fn at_address(&self, address: &Address<impl term::Model>) -> &Self {
        match address {
            Address::Memory(_) => self,

            Address::Field(field) => match self {
                Self::Struct(struct_proj) => struct_proj
                    .projections_by_field_id
                    .get(&field.id)
                    .unwrap()
                    .at_address(&field.struct_address),

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
                                        .at_address(&tuple.tuple_address);
                                }
                            }

                            panic!("unpacked tuple type not found");
                        }
                    };

                    tuple_proj.elements[index]
                        .projection
                        .at_address(&tuple.tuple_address)
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
                    .at_address(&variant.enum_address),

                Self::Whole(_) => self,

                _ => panic!(
                    "expected a variant/whole; found address: {address:?}, \
                     found assigned: {self:?}"
                ),
            },

            Address::Reference(reference) => match self {
                Self::Dereference(deref_proj) => deref_proj
                    .projection
                    .at_address(&reference.reference_address),

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
enum SetAssignedResultInternal<'a, M: term::Model> {
    Done(bool),
    Continue { projection: &'a mut Assigned, ty: Type<M> },
}

impl Assigned {
    /// Remembers that the given `address` has been accessed. Returns `true`
    /// if the address is accessed for the first time.
    pub fn set_assigned<M: term::Model>(
        &mut self,
        address: &Address<M>,
        root_ty: Type<M>,
        environment: &Environment<M, impl Normalizer<M>>,
        access_span: Span,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<bool, Abort> {
        Ok(self
            .set_accessed_internal(
                address,
                root_ty,
                true,
                environment,
                access_span,
                handler,
            )?
            .into_done()
            .unwrap())
    }

    #[allow(clippy::too_many_lines)]
    fn set_accessed_internal<M: term::Model>(
        &mut self,
        address: &Address<M>,
        root_ty: Type<M>,
        root: bool,
        environment: &Environment<M, impl Normalizer<M>>,
        access_span: Span,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<SetAssignedResultInternal<M>, Abort> {
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
                    access_span.clone(),
                    handler,
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
                        id: struct_id,
                        generic_arguments,
                    }) => (struct_id, generic_arguments),

                    ty => {
                        panic!("expected a struct type, found `{ty:?}`");
                    }
                };

                assert_eq!(
                    *environment.table().get::<SymbolKind>(struct_id),
                    SymbolKind::Struct
                );

                let struct_generic_params =
                    environment
                        .table()
                        .query::<GenericParameters>(struct_id)?;

                let inst = Instantiation::from_generic_arguments(
                    generic_arguments,
                    struct_id,
                    &struct_generic_params,
                )
                .unwrap();

                let fields = environment.table().query::<Fields>(struct_id)?;

                if let Self::Whole(whole_proj) = proj {
                    // if have concluded, we should've returned earlier
                    assert!(!*whole_proj);

                    *proj = Self::Struct(Struct {
                        projections_by_field_id: fields
                            .field_declaration_order
                            .iter()
                            .copied()
                            .map(|field_id| (field_id, Self::Whole(false)))
                            .collect(),
                    });
                }

                (
                    proj.as_struct_mut()
                        .unwrap()
                        .projections_by_field_id
                        .get_mut(&field.id)
                        .unwrap(),
                    {
                        let mut field_ty = M::from_default_type(
                            fields.fields.get(field.id).unwrap().r#type.clone(),
                        );

                        instantiation::instantiate(&mut field_ty, &inst);

                        environment
                            .simplify(field_ty)
                            .map_err(|x| {
                                x.report_overflow(|x| {
                                    x.report_as_type_calculating_overflow(
                                        access_span,
                                        handler,
                                    )
                                })
                            })?
                            .result
                            .clone()
                    },
                )
            }
            Address::Tuple(tuple) => {
                let (proj, ty) = match self.set_accessed_internal(
                    &tuple.tuple_address,
                    root_ty,
                    false,
                    environment,
                    access_span,
                    handler,
                )? {
                    done @ SetAssignedResultInternal::Done(_) => {
                        return Ok(done);
                    }

                    SetAssignedResultInternal::Continue { projection, ty } => {
                        (projection, ty)
                    }
                };

                let mut tuple_ty = ty.into_tuple().unwrap();

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
                        let tuple_proj = proj.as_tuple_mut().unwrap();

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
                                    .unwrap();
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

                                panic!("unpacked tuple element not found");
                            }
                        };

                        assert!(index < tuple_ty.elements.len());

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
                    access_span.clone(),
                    handler,
                )? {
                    done @ SetAssignedResultInternal::Done(_) => {
                        return Ok(done);
                    }

                    SetAssignedResultInternal::Continue { projection, ty } => {
                        (projection, ty)
                    }
                };

                let (enum_id, generic_arguments) = match ty {
                    Type::Symbol(Symbol { id: enum_id, generic_arguments }) => {
                        (enum_id, generic_arguments)
                    }

                    ty => {
                        panic!("expected an enum type, found `{ty:?}`");
                    }
                };

                let enum_generic_params =
                    environment.table().query::<GenericParameters>(enum_id)?;

                let instantiation = Instantiation::from_generic_arguments(
                    generic_arguments,
                    enum_id,
                    &enum_generic_params,
                )
                .unwrap();

                if let Self::Whole(whole_proj) = proj {
                    // if have concluded, we should've returned earlier
                    assert!(!*whole_proj);

                    *proj = Self::Variant(Variant {
                        variant_projection: Box::new(Self::Whole(false)),
                        variant_id: variant.id,
                    });
                }

                let variant_comp = environment
                    .table()
                    .query::<variant::Variant>(variant.id)?;

                (proj.as_variant_mut().unwrap().variant_projection.as_mut(), {
                    let mut variant_ty = M::from_default_type(
                        variant_comp.associated_type.clone().unwrap(),
                    );

                    instantiation::instantiate(&mut variant_ty, &instantiation);

                    environment
                        .simplify(variant_ty)
                        .map_err(|x| {
                            x.report_overflow(|x| {
                                x.report_as_type_calculating_overflow(
                                    access_span,
                                    handler,
                                )
                            })
                        })?
                        .result
                        .clone()
                })
            }

            Address::Reference(reference) => {
                let (proj, ty) = match self.set_accessed_internal(
                    &reference.reference_address,
                    root_ty,
                    false,
                    environment,
                    access_span,
                    handler,
                )? {
                    done @ SetAssignedResultInternal::Done(_) => {
                        return Ok(done);
                    }

                    SetAssignedResultInternal::Continue { projection, ty } => {
                        (projection, ty)
                    }
                };

                let ty = ty.into_reference().unwrap();

                if let Self::Whole(whole_proj) = proj {
                    // if have concluded, we should've returned earlier
                    assert!(!*whole_proj);

                    *proj = Self::Dereference(Dereference {
                        projection: Box::new(Self::Whole(false)),
                    });
                }

                (
                    proj.as_dereference_mut().unwrap().projection.as_mut(),
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
trait Traverser<M: term::Model>: Clone {
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

fn traverse_block<M: term::Model, T: Traverser<M>>(
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

#[allow(clippy::too_many_lines)]
fn traverse_block_internal<M: term::Model, T: Traverser<M>>(
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
        if starting_instruction_index.is_some_and(|x| x + 1 == index) {
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
            Jump::Switch(select_jump) => {
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

        Some(Terminator::Return(_) | Terminator::Panic) | None => {
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
#[allow(clippy::too_many_arguments)]
pub fn get_live_usages(
    memories: impl IntoIterator<Item = Memory<BorrowModel>>,
    checking_registers: HashSet<ID<Register<BorrowModel>>>,
    invalidated_regions: &HashSet<Region>,
    invalidated_borrow_register_id: ID<Register<BorrowModel>>,
    point: Point<BorrowModel>,
    mut exit: &mut impl FnMut(&Instruction<BorrowModel>, Point<BorrowModel>) -> bool,
    representation: &Representation<BorrowModel>,
    current_site: GlobalID,
    environment: &Environment<BorrowModel, impl Normalizer<BorrowModel>>,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) -> Result<HashSet<Usage>, Abort> {
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
        handler,
    };

    traverse_block(
        &mut state,
        &representation.control_flow_graph,
        point,
        &mut exit,
    )
}

/// The state struct used for keep tracking the liveness of the addresses.
struct LiveBorrowTraverser<'a, N: Normalizer<BorrowModel>> {
    assigned_states_by_memory: HashMap<Memory<BorrowModel>, Assigned>,
    checking_registers: HashSet<ID<Register<BorrowModel>>>,
    invalidated_regions: &'a HashSet<Region>,
    invalidated_borrow_register_id: ID<Register<BorrowModel>>,

    representation: &'a Representation<BorrowModel>,
    environment: &'a Environment<'a, BorrowModel, N>,
    handler: &'a dyn Handler<Box<dyn Diagnostic>>,
    current_site: GlobalID,
}

impl<N: Normalizer<BorrowModel>> Clone for LiveBorrowTraverser<'_, N> {
    fn clone(&self) -> Self {
        Self {
            assigned_states_by_memory: self.assigned_states_by_memory.clone(),
            checking_registers: self.checking_registers.clone(),
            invalidated_borrow_register_id: self.invalidated_borrow_register_id,

            invalidated_regions: self.invalidated_regions,
            representation: self.representation,
            environment: self.environment,
            current_site: self.current_site,
            handler: self.handler,
        }
    }
}

impl<N: Normalizer<BorrowModel>> Traverser<BorrowModel>
    for LiveBorrowTraverser<'_, N>
{
    type Error = Abort;

    type Result = HashSet<Usage>;

    #[allow(clippy::too_many_lines)]
    fn on_instruction(
        &mut self,
        _: Point<BorrowModel>,
        instruction: &Instruction<BorrowModel>,
    ) -> Result<ControlFlow<Self::Result>, Self::Error> {
        let accesses =
            instruction.get_access_address(&self.representation.values);
        // check each access
        for (address, kind) in accesses {
            let memory_root = address.get_root_memory();
            let memory_root_ty = match *memory_root {
                Memory::Parameter(id) => BorrowModel::from_default_type(
                    self.environment
                        .table()
                        .query::<FunctionSignature>(self.current_site)?
                        .parameters
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
                self.assigned_states_by_memory.get_mut(memory_root)
            else {
                // skip irrelevant addresses
                continue;
            };

            // check if the address is accessed
            if assigned_state.at_address(&address).is_assigned() {
                // has already assigned, dead address
                continue;
            }

            let (read_span, is_drop) = match kind {
                AccessKind::Normal(AccessMode::Write(write_span)) => {
                    // check if the write dereference the regions that are
                    // invalidated
                    let dereferenced_regions =
                        get_dereferenced_regions_in_address(
                            &self.representation.values,
                            &address,
                            write_span.as_ref().unwrap(),
                            self.current_site,
                            self.environment,
                            self.handler,
                        )?;

                    if dereferenced_regions
                        .iter()
                        .any(|x| self.invalidated_regions.contains(x))
                    {
                        return Ok(ControlFlow::Break(
                            std::iter::once(Usage::Local(write_span.unwrap()))
                                .collect(),
                        ));
                    }

                    assigned_state.set_assigned(
                        &address,
                        memory_root_ty,
                        self.environment,
                        write_span.clone().unwrap(),
                        self.handler,
                    )?;

                    continue;
                }

                AccessKind::Normal(AccessMode::Read(read)) => {
                    (read.span.clone().unwrap(), false)
                }
                AccessKind::Drop => (
                    match *memory_root {
                        Memory::Parameter(id) => self
                            .environment
                            .table()
                            .query::<FunctionSignature>(self.current_site)?
                            .parameters
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
                            .clone()
                            .unwrap(),
                    },
                    true,
                ),
            };

            let read_lifetimes = get_regions_in_address(
                &self.representation.values,
                &address,
                &read_span,
                true,
                self.current_site,
                self.environment,
                self.handler,
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
                        .clone()
                        .unwrap(),
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
                previous.extend(next);
                previous
            },
            false,
        )
    }
}

pub fn is_live(
    checking_address: &Address<BorrowModel>,
    root_address_type: &Type<BorrowModel>,
    from: Point<BorrowModel>,
    to: Point<BorrowModel>,
    representation: &Representation<BorrowModel>,
    environment: &Environment<BorrowModel, impl Normalizer<BorrowModel>>,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) -> Result<bool, Abort> {
    let mut traverser = LiveLenderTraverser {
        root_memory: *checking_address.get_root_memory(),
        assigned_states: Assigned::Whole(false),
        checking_address,
        representation,
        to_point: to,
        root_type: root_address_type,
        environment,
        handler,
    };

    traverse_block(
        &mut traverser,
        &representation.control_flow_graph,
        from,
        &mut |_, _| false,
    )
}

struct LiveLenderTraverser<'a, N: Normalizer<BorrowModel>> {
    root_memory: Memory<BorrowModel>,
    assigned_states: Assigned,
    checking_address: &'a Address<BorrowModel>,
    root_type: &'a Type<BorrowModel>,
    to_point: Point<BorrowModel>,

    environment: &'a Environment<'a, BorrowModel, N>,
    representation: &'a Representation<BorrowModel>,
    handler: &'a dyn Handler<Box<dyn Diagnostic>>,
}

impl<N: Normalizer<BorrowModel>> Clone for LiveLenderTraverser<'_, N> {
    fn clone(&self) -> Self {
        Self {
            root_memory: self.root_memory,
            assigned_states: self.assigned_states.clone(),
            checking_address: self.checking_address,
            root_type: self.root_type,
            representation: self.representation,
            to_point: self.to_point,

            environment: self.environment,
            handler: self.handler,
        }
    }
}

impl<N: Normalizer<BorrowModel>> Traverser<BorrowModel>
    for LiveLenderTraverser<'_, N>
{
    type Error = Abort;

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
                    store.address.is_child_of(self.checking_address)
                        || self.checking_address.is_child_of(&store.address);

                // non-related address
                if !overlapping_addresses {
                    return Ok(ControlFlow::Continue);
                }

                self.assigned_states.set_assigned(
                    &store.address,
                    self.root_type.clone(),
                    self.environment,
                    store.span.clone().unwrap(),
                    self.handler,
                )?;

                if self
                    .assigned_states
                    .at_address(self.checking_address)
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

            _ => Ok(ControlFlow::Continue),
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
