use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
};

use enum_as_inner::EnumAsInner;
use pernixc_base::source_file::Span;

use crate::{
    arena::{Key, ID},
    ir::{
        self,
        address::{self, Address, Memory},
        control_flow_graph::{Block, Point},
        instruction::{AccessKind, Instruction, Jump, Terminator},
        value::register::Register,
    },
    symbol::{
        self,
        table::{self, representation::Index as _},
        AdtID, Field,
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
    projection: Accessed,
    is_unpacked: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Tuple {
    elements: Vec<TupleElement>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Struct {
    projections_by_field_id: HashMap<ID<Field>, Accessed>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Variant {
    variant_projection: Box<Accessed>,
    variant_id: ID<symbol::Variant>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Dereference {
    projection: Box<Accessed>,
}

/// An enumeration representing the state whether a particular address has been
/// accessed or not.
#[derive(Debug, Clone, PartialEq, Eq, EnumAsInner)]
pub enum Accessed {
    Struct(Struct),
    Tuple(Tuple),
    Variant(Variant),
    Dereference(Dereference),

    Whole(bool),
}

impl Accessed {
    /// Returns `true` if every address in the projection has been accessed.
    pub fn is_accessed(&self) -> bool {
        match self {
            Self::Struct(struct_) => struct_
                .projections_by_field_id
                .values()
                .all(Self::is_accessed),

            Self::Tuple(tuple) => tuple
                .elements
                .iter()
                .all(|element| element.projection.is_accessed()),

            Self::Whole(accessed) => *accessed,

            Self::Variant(_) | Self::Dereference(_) => false,
        }
    }
}

#[derive(Debug, EnumAsInner)]
enum SetAccessedResultInternal<'a, M: Model> {
    Done(bool),
    Continue { projection: &'a mut Accessed, ty: Type<M> },
}

impl Accessed {
    /// Remembers that the given `address` has been accessed. Returns `true`
    /// if the address is accessed for the first time.
    pub fn set_accessed<M: Model, S: table::State>(
        &mut self,
        address: &Address<M>,
        root_ty: Type<M>,
        write: bool,
        environment: &Environment<
            M,
            S,
            impl Normalizer<M, S>,
            impl Observer<M, S>,
        >,
    ) -> Result<bool, SetAccessedError> {
        Ok(self
            .set_accessed_internal(address, root_ty, true, write, environment)?
            .into_done()
            .unwrap())
    }

    #[allow(clippy::too_many_lines)]
    fn set_accessed_internal<M: Model, S: table::State>(
        &mut self,
        address: &Address<M>,
        root_ty: Type<M>,
        root: bool,
        write: bool,
        environment: &Environment<
            M,
            S,
            impl Normalizer<M, S>,
            impl Observer<M, S>,
        >,
    ) -> Result<SetAccessedResultInternal<M>, SetAccessedError> {
        // no more work to do
        if self.is_accessed() {
            return Ok(SetAccessedResultInternal::Done(false));
        }

        let (target_projection, ty) = match address {
            Address::Memory(_) => (self, root_ty),

            Address::Field(field) => {
                let (proj, ty) = match self.set_accessed_internal(
                    &field.struct_address,
                    root_ty,
                    false,
                    write,
                    environment,
                )? {
                    done @ SetAccessedResultInternal::Done(_) => {
                        return Ok(done);
                    }

                    SetAccessedResultInternal::Continue { projection, ty } => {
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
                    write,
                    environment,
                )? {
                    done @ SetAccessedResultInternal::Done(_) => {
                        return Ok(done);
                    }

                    SetAccessedResultInternal::Continue { projection, ty } => {
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
                if !write {
                    *self = Self::Whole(true);
                }

                return Ok(SetAccessedResultInternal::Done(false));
            }

            Address::Variant(variant) => {
                let (proj, ty) = match self.set_accessed_internal(
                    &variant.enum_address,
                    root_ty,
                    false,
                    write,
                    environment,
                )? {
                    done @ SetAccessedResultInternal::Done(_) => {
                        return Ok(done);
                    }

                    SetAccessedResultInternal::Continue { projection, ty } => {
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
                    write,
                    environment,
                )? {
                    done @ SetAccessedResultInternal::Done(_) => {
                        return Ok(done);
                    }

                    SetAccessedResultInternal::Continue { projection, ty } => {
                        (projection, ty)
                    }
                };

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
                    ty,
                )
            }
        };

        Ok(if root {
            *target_projection = Self::Whole(true);

            SetAccessedResultInternal::Done(true)
        } else {
            SetAccessedResultInternal::Continue {
                projection: target_projection,
                ty,
            }
        })
    }
}

pub fn live_register_spans<M: Model>(
    register_id: ID<Register<M>>,
    point: Point<M>,
    representation: &ir::Representation<M>,
) -> Vec<Span> {
    let traverser =
        RegisterTraverser { target_register_id: register_id, representation };

    traverser.traverse_block(
        point.block_id,
        Some(point.instruction_index),
        &mut HashSet::new(),
    )
}

/// The state struct used for keep tracking the liveness of the addresses.
#[derive(Debug, Clone)]
struct RegisterTraverser<'a, M: Model> {
    target_register_id: ID<Register<M>>,
    representation: &'a ir::Representation<M>,
}

impl<M: Model> RegisterTraverser<'_, M> {
    fn traverse_block(
        &self,
        block_id: ID<Block<M>>,
        starting_instruction_index: Option<usize>,
        visited: &mut HashSet<(usize, Option<usize>)>,
    ) -> Vec<Span> {
        if starting_instruction_index.is_none()
            && !visited.insert((block_id.into_index(), None))
        {
            return Vec::new();
        }

        let block = self
            .representation
            .control_flow_graph
            .blocks()
            .get(block_id)
            .unwrap();

        for (index, instruction) in block
            .instructions()
            .iter()
            .enumerate()
            .skip(starting_instruction_index.map_or(0, |x| x + 1))
        {
            // skip to the starting instruction index
            if visited.contains(&(block_id.into_index(), Some(index))) {
                return Vec::new();
            }

            // if the next instruction is the starting instruction index, we
            // should mark the current instruction as visited
            if starting_instruction_index.map_or(false, |x| x + 1 == index) {
                assert!(visited.insert((block_id.into_index(), Some(index))));
            }

            match instruction {
                Instruction::RegisterAssignment(register_assignment) => {
                    if register_assignment.id == self.target_register_id {
                        return Vec::new();
                    }

                    let register = self
                        .representation
                        .values
                        .registers
                        .get(register_assignment.id)
                        .unwrap();
                    let used_register =
                        register.assignment.get_used_registers();

                    if used_register.contains(&self.target_register_id) {
                        return vec![register.span.clone()];
                    }
                }
                Instruction::RegisterDiscard(register_discard) => {
                    if register_discard.id == self.target_register_id {
                        return Vec::new();
                    }
                }
                Instruction::Store(_)
                | Instruction::TuplePack(_)
                | Instruction::ScopePush(_)
                | Instruction::ScopePop(_)
                | Instruction::DropUnpackTuple(_)
                | Instruction::Drop(_) => {}
            }
        }

        match block.terminator() {
            Some(Terminator::Jump(jump)) => match jump {
                Jump::Unconditional(unconditional_jump) => self.traverse_block(
                    unconditional_jump.target,
                    None,
                    visited,
                ),
                Jump::Conditional(conditional_jump) => {
                    let mut true_spans = self.traverse_block(
                        conditional_jump.true_target,
                        None,
                        &mut visited.clone(),
                    );

                    let false_span = self.traverse_block(
                        conditional_jump.false_target,
                        None,
                        &mut visited.clone(),
                    );

                    true_spans.extend(false_span);

                    true_spans
                }
                Jump::Select(select_jump) => {
                    let mut spans = Vec::new();
                    for block in select_jump
                        .branches
                        .values()
                        .copied()
                        .chain(select_jump.otherwise)
                    {
                        spans.extend(self.traverse_block(
                            block,
                            None,
                            &mut visited.clone(),
                        ));
                    }

                    spans
                }
            },

            Some(Terminator::Return(_)) => {
                // NOTE: should we consider the return value?
                Vec::new()
            }

            Some(Terminator::Panic) | None => Vec::new(),
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
pub fn get_live_addresses<S: table::State, M: Model>(
    root_memory_address: Memory<M>,
    root_type: &Type<M>,
    point: Point<M>,
    exit: &mut impl FnMut(&Instruction<M>, Point<M>) -> bool,
    representation: &ir::Representation<M>,
    environment: &Environment<M, S, impl Normalizer<M, S>, impl Observer<M, S>>,
) -> Result<HashSet<(Address<M>, AccessKind)>, OverflowError> {
    let mut state = MemoryTraverser {
        root_memory_address,
        root_type,
        representation,
        environment,
        projection: Accessed::Whole(false),
    };

    state.traverse_block(
        point.block_id,
        Some(point.instruction_index),
        exit,
        &mut HashSet::new(),
    )
}

/// The state struct used for keep tracking the liveness of the addresses.
#[derive(Debug)]
struct MemoryTraverser<
    'a,
    S: table::State,
    M: Model,
    N: Normalizer<M, S>,
    O: Observer<M, S>,
> {
    root_memory_address: Memory<M>,

    root_type: &'a Type<M>,
    representation: &'a ir::Representation<M>,
    environment: &'a Environment<'a, M, S, N, O>,

    projection: Accessed,
}

impl<'a, S: table::State, M: Model, N: Normalizer<M, S>, O: Observer<M, S>>
    Clone for MemoryTraverser<'a, S, M, N, O>
{
    fn clone(&self) -> Self {
        Self {
            root_memory_address: self.root_memory_address,
            root_type: self.root_type,
            representation: self.representation,
            environment: self.environment,
            projection: self.projection.clone(),
        }
    }
}

impl<'a, S: table::State, M: Model, N: Normalizer<M, S>, O: Observer<M, S>>
    MemoryTraverser<'a, S, M, N, O>
{
    #[allow(clippy::too_many_lines)]
    fn traverse_block(
        &mut self,
        block_id: ID<Block<M>>,
        starting_instruction_index: Option<usize>,
        exit: &mut impl FnMut(&Instruction<M>, Point<M>) -> bool,
        visited: &mut HashSet<(usize, Option<usize>)>,
    ) -> Result<HashSet<(Address<M>, AccessKind)>, OverflowError> {
        if starting_instruction_index.is_none()
            && !visited.insert((block_id.into_index(), None))
        {
            return Ok(HashSet::new());
        }

        let block = self
            .representation
            .control_flow_graph
            .blocks()
            .get(block_id)
            .unwrap();
        let mut live_addresses = HashSet::new();

        for (index, instruction) in block
            .instructions()
            .iter()
            .enumerate()
            .skip(starting_instruction_index.map_or(0, |x| x + 1))
        {
            // skip to the starting instruction index
            if visited.contains(&(block_id.into_index(), Some(index))) {
                return Ok(live_addresses);
            }

            // if the next instruction is the starting instruction index, we
            // should mark the current instruction as visited
            if starting_instruction_index.map_or(false, |x| x + 1 == index) {
                assert!(visited.insert((block_id.into_index(), Some(index))));
            }

            // explicitly exit the traversal
            if exit(instruction, Point { block_id, instruction_index: index }) {
                return Ok(live_addresses);
            }

            let accesses = instruction
                .get_access_address(&self.representation.values)
                .unwrap();

            for (address, kind) in accesses {
                if !address
                    .is_child_of(&Address::Memory(self.root_memory_address))
                {
                    // not relevant, skip
                    continue;
                }

                let write =
                    matches!(&kind, AccessKind::Normal { write: true, .. });

                match self.projection.set_accessed(
                    &address,
                    self.root_type.clone(),
                    write,
                    self.environment,
                ) {
                    Ok(true) => {
                        if !write {
                            live_addresses.insert((
                                match address {
                                    Cow::Borrowed(x) => x.clone(),
                                    Cow::Owned(x) => x,
                                },
                                kind,
                            ));
                        }
                    }

                    Ok(false) => {}

                    Err(err) => match err {
                        SetAccessedError::Overflow(err) => return Err(err),

                        err @ SetAccessedError::Internal(_) => {
                            panic!("{err}")
                        }
                    },
                }
            }

            // if the whole thing is used or reassigned, we can skip the
            // rest of the instructions
            if self.projection.is_accessed() {
                break;
            }
        }

        match block.terminator() {
            Some(Terminator::Jump(jump)) => match jump {
                Jump::Unconditional(unconditional_jump) => {
                    let accesses = self.traverse_block(
                        unconditional_jump.target,
                        None,
                        exit,
                        visited,
                    )?;

                    live_addresses.extend(accesses);

                    Ok(live_addresses)
                }
                Jump::Conditional(conditional_jump) => {
                    let true_accesses = self.clone().traverse_block(
                        conditional_jump.true_target,
                        None,
                        exit,
                        &mut visited.clone(),
                    )?;

                    let false_accesses = self.traverse_block(
                        conditional_jump.false_target,
                        None,
                        exit,
                        &mut visited.clone(),
                    )?;

                    live_addresses.extend(true_accesses);
                    live_addresses.extend(false_accesses);

                    Ok(live_addresses)
                }
                Jump::Select(select_jump) => {
                    for block in select_jump
                        .branches
                        .values()
                        .copied()
                        .chain(select_jump.otherwise)
                    {
                        live_addresses.extend(self.clone().traverse_block(
                            block,
                            None,
                            exit,
                            &mut visited.clone(),
                        )?);
                    }

                    Ok(live_addresses)
                }
            },

            Some(Terminator::Return(_) | Terminator::Panic) | None => {
                Ok(live_addresses)
            }
        }
    }
}
