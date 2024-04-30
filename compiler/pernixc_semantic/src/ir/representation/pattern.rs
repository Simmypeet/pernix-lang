use std::collections::HashMap;

use pernixc_base::{
    diagnostic::Handler,
    source_file::{SourceElement, Span},
};
use pernixc_syntax::syntax_tree::{self, ConnectedList};

use super::Representation;
use crate::{
    arena::{Reserve, ID},
    error::{
        AlreadyBoundFieldPattern, Error, ExpectPackedTuplePattern,
        FieldNotFound, MismatchedPatternBindingType,
        MismatchedTuplePatternLength, MoreThanOneUnpackedInTuplePattern,
        PatternBindingType,
    },
    ir::{
        address::{self, Address, Field},
        alloca::Alloca,
        control_flow_graph::{Block, Scope},
        instruction::{
            self, AllocaAllocation, Basic, RegisterAssignment, Store,
        },
        value::{
            register::{Load, LoadKind, ReferenceOf, Register},
            Value,
        },
    },
    pattern::{
        self, Irrefutable, Named, RegularTupleBinding, Structural, Tuple,
        Wildcard,
    },
    semantic::{
        self,
        instantiation::{self, Instantiation, MismatchedGenericParameterCount},
        term::{
            self,
            lifetime::{Lifetime, Local},
            r#type::{Qualifier, Reference, SymbolID, Type},
            Symbol, TupleElement,
        },
    },
    symbol::{GlobalID, Struct},
    table::{Index, State, Table},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum TypeBinding<'a> {
    Value(&'a Type),
    Reference { lifetime: Lifetime, qualifier: Qualifier, r#type: &'a Type },
}

/// The error that can occur when creating a pattern.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
pub enum CreatePatternError {
    #[error("the given `block_id` is invalid")]
    InvalidBlockID,

    #[error(
        "the given block is unreachable and cannot be used to create a pattern"
    )]
    UnreachableBlock,

    #[error(
        "the type contains an invalid struct ID: {0:?} which does not exist \
         in the symbol table"
    )]
    InvalidStructID(ID<Struct>),

    #[error(
        "the type contains a term containing generic arguments that do not \
         match the generic parameters of the instantiated symbol"
    )]
    MismatchedGenericParameterCount(#[from] MismatchedGenericParameterCount),
}

struct SideEffect<'a> {
    current_scope_id: ID<Scope>,
    new_instructions: Vec<Basic>,
    register_reserved: Reserve<'a, Register>,
    alloca_reserved: Reserve<'a, Alloca>,
}

impl<'a> SideEffect<'a> {
    fn create_reference_bound_named_pattern(
        &mut self,
        reference_ty: Reference,
        span: Span,
        address: &Address,
        name: String,
        mutable: bool,
    ) -> Named {
        let alloca_id = self.alloca_reserved.reserve(Alloca {
            r#type: Type::Reference(reference_ty),
            span: Some(span),
        });

        // create a register that holds the reference of the
        // value
        let register_id = self.register_reserved.reserve(
            Register::ReferenceOf(ReferenceOf { address: address.clone() }),
        );

        self.new_instructions.push(instruction::Basic::AllocaAllocation(
            AllocaAllocation { id: alloca_id },
        ));
        self.new_instructions.push(instruction::Basic::RegisterAssignment(
            RegisterAssignment { id: register_id },
        ));
        self.new_instructions.push(instruction::Basic::Store(Store {
            address: Address::Alloca(alloca_id),
            value: Value::Register(register_id),
        }));

        Named { name, load_address: Address::Alloca(alloca_id), mutable }
    }

    fn reduce_reference<'b>(
        &mut self,
        type_binding: &TypeBinding<'b>,
        mut address: Address,
    ) -> (&'b Type, Address, Option<(Lifetime, Qualifier)>) {
        let (mut current_ty, mut reference_binding_info) = match type_binding {
            TypeBinding::Value(ty) => (*ty, None),
            TypeBinding::Reference { lifetime, qualifier, r#type } => {
                (*r#type, Some((*lifetime, *qualifier)))
            }
        };

        loop {
            match current_ty {
                Type::Reference(reference) => {
                    let register =
                        self.register_reserved.reserve(Register::Load(Load {
                            address: address.clone(),
                            kind: LoadKind::Copy,
                        }));
                    self.new_instructions.push(Basic::RegisterAssignment(
                        RegisterAssignment { id: register },
                    ));

                    // update the address, reference binding
                    // info, and binding ty
                    address = Address::Value(Value::Register(register));
                    reference_binding_info =
                        Some((reference.lifetime, reference.qualifier));
                    current_ty = reference.pointee.as_ref();
                }

                _ => break (current_ty, address, reference_binding_info),
            }
        }
    }

    #[allow(
        clippy::only_used_in_recursion,
        clippy::too_many_arguments,
        clippy::too_many_lines
    )]
    fn create_irrefutable_interanl(
        &mut self,
        table: &Table<impl State>,
        syntax_tree: &syntax_tree::pattern::Irrefutable,
        type_binding: TypeBinding,
        address: &Address,
        referring_site: GlobalID,
        handler: &dyn Handler<Box<dyn Error>>,
    ) -> Result<Irrefutable, CreatePatternError> {
        Ok(match syntax_tree {
            syntax_tree::pattern::Irrefutable::Structural(structrual) => {
                let (ty, address, reference_binding_info) =
                    self.reduce_reference(&type_binding, address.clone());

                // must be a struct type
                let Type::Symbol(Symbol {
                    id: SymbolID::Struct(struct_id),
                    generic_arguments,
                }) = ty
                else {
                    handler.receive(Box::new(MismatchedPatternBindingType {
                        expected_bindnig_type: PatternBindingType::Struct,
                        found_type: ty.clone(),
                        pattern_span: structrual.span(),
                    }));
                    return Ok(Irrefutable::Wildcard(Wildcard));
                };

                let struct_id = *struct_id;

                let struct_symbol = table
                    .get(struct_id)
                    .ok_or(CreatePatternError::InvalidStructID(struct_id))?;

                let instantiation = Instantiation::from_generic_arguments(
                    generic_arguments.clone(),
                    struct_id.into(),
                    &struct_symbol.generic_declaration.parameters,
                )?;

                let mut patterns_by_field_id = HashMap::new();
                // iterate to each field
                for field in
                    structrual.fields().iter().flat_map(ConnectedList::elements)
                {
                    let field_name = match field {
                        syntax_tree::pattern::Field::Association(
                            association,
                        ) => association.identifier().span.str(),
                        syntax_tree::pattern::Field::Named(named) => {
                            named.identifier().span.str()
                        }
                    };

                    // get the field id
                    let Some((field_sym, field_id)) = struct_symbol
                        .fields
                        .get_id(field_name)
                        .map(|x| (struct_symbol.fields.get(x).unwrap(), x))
                    else {
                        // field not found error
                        handler.receive(Box::new(FieldNotFound {
                            identifier_span: match field {
                                syntax_tree::pattern::Field::Association(
                                    pat,
                                ) => pat.identifier().span.clone(),
                                syntax_tree::pattern::Field::Named(pat) => {
                                    pat.identifier().span.clone()
                                }
                            },
                            struct_id,
                        }));

                        continue;
                    };

                    let field_address = Address::Field(Field {
                        struct_address: Box::new(address.clone()),
                        id: field_id,
                    });

                    let entry = match patterns_by_field_id.entry(field_id) {
                        std::collections::hash_map::Entry::Occupied(_) => {
                            handler.receive(Box::new(
                                AlreadyBoundFieldPattern {
                                    pattern_span: field.span(),
                                    struct_id,
                                    field_id,
                                },
                            ));
                            continue;
                        }
                        std::collections::hash_map::Entry::Vacant(entry) => {
                            entry
                        }
                    };

                    // instantiation the type
                    let mut field_ty = field_sym.r#type.clone();
                    instantiation::instantiate(&mut field_ty, &instantiation);

                    let type_binding = reference_binding_info.map_or_else(
                        || TypeBinding::Value(&field_ty),
                        |(lifetime, qualifier)| TypeBinding::Reference {
                            lifetime,
                            qualifier,
                            r#type: &field_ty,
                        },
                    );

                    // the pattern for the field
                    let pattern = match field {
                        syntax_tree::pattern::Field::Association(assoc) => self
                            .create_irrefutable_interanl(
                                table,
                                assoc.pattern(),
                                type_binding,
                                &field_address,
                                referring_site,
                                handler,
                            ),
                        syntax_tree::pattern::Field::Named(named) => self
                            .create_irrefutable_interanl(
                                table,
                                &syntax_tree::pattern::Irrefutable::Named(
                                    named.clone(),
                                ),
                                type_binding,
                                &field_address,
                                referring_site,
                                handler,
                            ),
                    }?;

                    // TODO: check if the field is accessible

                    entry.insert(pattern);
                }

                Irrefutable::Structural(Structural {
                    struct_id,
                    patterns_by_field_id,
                })
            }

            syntax_tree::pattern::Irrefutable::Named(named) => {
                match (named.binding(), type_binding) {
                    // obtains the reference of the value.
                    (
                        syntax_tree::pattern::Binding::Ref(ref_binding),
                        TypeBinding::Value(ty),
                    ) => Irrefutable::Named(
                        self.create_reference_bound_named_pattern(
                            Reference {
                                qualifier: ref_binding
                                    .qualifier()
                                    .as_ref()
                                    .map_or(
                                        Qualifier::Immutable,
                                        |q| match q {
                                            syntax_tree::Qualifier::Mutable(
                                                _,
                                            ) => Qualifier::Mutable,
                                            syntax_tree::Qualifier::Unique(
                                                _,
                                            ) => Qualifier::Unique,
                                        },
                                    ),

                                lifetime: Lifetime::Local(Local(
                                    self.current_scope_id,
                                )),

                                #[allow(unreachable_code)]
                                pointee: Box::new(ty.clone()),
                            },
                            named.span(),
                            address,
                            named.identifier().span.str().to_owned(),
                            false,
                        ),
                    ),

                    // normal value binding
                    (
                        syntax_tree::pattern::Binding::Value {
                            mutable_keyword,
                        },
                        TypeBinding::Value(_),
                    ) => Irrefutable::Named(Named {
                        name: named.identifier().span.str().to_owned(),
                        load_address: address.clone(),
                        mutable: mutable_keyword.is_some(),
                    }),

                    (
                        binding,
                        TypeBinding::Reference { lifetime, qualifier, r#type },
                    ) => Irrefutable::Named(
                        self.create_reference_bound_named_pattern(
                            Reference {
                                qualifier,
                                lifetime,
                                pointee: Box::new(r#type.clone()),
                            },
                            named.span(),
                            address,
                            named.identifier().span.str().to_owned(),
                            match binding {
                                syntax_tree::pattern::Binding::Ref(_) => false,
                                syntax_tree::pattern::Binding::Value {
                                    mutable_keyword,
                                } => mutable_keyword.is_some(),
                            },
                        ),
                    ),
                }
            }

            syntax_tree::pattern::Irrefutable::Tuple(tuple_pat) => {
                let (ty, address, reference_binding_info) =
                    self.reduce_reference(&type_binding, address.clone());

                let Type::Tuple(tuple_ty) = ty else {
                    handler.receive(Box::new(MismatchedPatternBindingType {
                        expected_bindnig_type: PatternBindingType::Tuple,
                        found_type: ty.clone(),
                        pattern_span: tuple_pat.span(),
                    }));
                    return Ok(Irrefutable::Wildcard(Wildcard));
                };

                // if the tuple has unpacking, then the it must not be bound as
                // a reference
                let tuple_element_patterns = tuple_pat
                    .patterns()
                    .iter()
                    .flat_map(ConnectedList::elements)
                    .collect::<Vec<_>>();

                let unpacked_position = {
                    let unpacked_count = tuple_element_patterns
                        .iter()
                        .filter(|x| x.is_packed())
                        .count();

                    match unpacked_count {
                        0 => None,
                        1 => Some(
                            tuple_element_patterns
                                .iter()
                                .position(|x| x.is_packed())
                                .unwrap(),
                        ),

                        _ => {
                            handler.receive(Box::new(
                                MoreThanOneUnpackedInTuplePattern {
                                    illegal_tuple_pattern_span: tuple_pat
                                        .span(),
                                },
                            ));
                            return Ok(Irrefutable::Wildcard(Wildcard));
                        }
                    }
                };

                // normal tuple pattern, the number of pattern must exactly
                // match
                if tuple_element_patterns.len() != tuple_ty.elements.len() {
                    handler.receive(Box::new(MismatchedTuplePatternLength {
                        pattern_span: tuple_pat.span(),
                        pattern_element_count: tuple_element_patterns.len(),
                        type_element_count: tuple_ty.elements.len(),
                    }));
                    return Ok(Irrefutable::Wildcard(Wildcard));
                }

                if let Some(unpacked_position) = unpacked_position {
                    // match with unpacked tuple pattern

                    // move the matched tuple elements in the unpacked range to
                    // a new tuple value and bind it to the unpacked pattern

                    if reference_binding_info.is_some() {
                        // TODO: pattern with unpacked tuple element must not be
                        // bound as a reference
                        return Ok(Irrefutable::Wildcard(Wildcard));
                    }

                    let before_packed_elements = {
                        let before_packed = 0..unpacked_position;

                        tuple_ty.elements[before_packed.clone()]
                            .iter()
                            .zip(
                                tuple_element_patterns[before_packed]
                                    .iter()
                                    .copied()
                                    .map(|x| x.as_regular().unwrap()),
                            )
                            .enumerate()
                            .map(|(idx, (ty, pat))| {
                                let TupleElement::Regular(ty) = ty else {
                                    handler.receive(Box::new(
                                        ExpectPackedTuplePattern {
                                            regular_tuple_pattern_span: pat
                                                .span(),
                                        },
                                    ));
                                    return Ok(Irrefutable::Wildcard(Wildcard));
                                };

                                let element_address =
                                    Address::Tuple(crate::ir::address::Tuple {
                                        tuple_address: Box::new(
                                            address.clone(),
                                        ),
                                        offset: address::Offset::FromStart(idx),
                                    });

                                self.create_irrefutable_interanl(
                                    table,
                                    syntax_tree,
                                    TypeBinding::Value(ty), // can only be value binding
                                    &element_address,
                                    referring_site,
                                    handler,
                                )
                            })
                            .collect::<Result<Vec<_>, _>>()
                    }?;

                    let after_packed_elements = {
                        let after_packed = (unpacked_position + 1)
                            ..tuple_element_patterns.len();

                        tuple_ty.elements[after_packed.clone()]
                            .iter()
                            .zip(
                                tuple_element_patterns[after_packed]
                                    .iter()
                                    .copied()
                                    .map(|x| x.as_regular().unwrap()),
                            )
                            .enumerate()
                            .map(|(idx, (ty, pat))| {
                                let TupleElement::Regular(ty) = ty else {
                                    handler.receive(Box::new(
                                        ExpectPackedTuplePattern {
                                            regular_tuple_pattern_span: pat
                                                .span(),
                                        },
                                    ));
                                    return Ok(Irrefutable::Wildcard(Wildcard));
                                };

                                let element_address =
                                    Address::Tuple(crate::ir::address::Tuple {
                                        tuple_address: Box::new(
                                            address.clone(),
                                        ),
                                        offset: address::Offset::FromStart(
                                            idx + unpacked_position,
                                        ),
                                    });

                                self.create_irrefutable_interanl(
                                    table,
                                    syntax_tree,
                                    TypeBinding::Value(ty), // can only be value binding
                                    &element_address,
                                    referring_site,
                                    handler,
                                )
                            })
                            .collect::<Result<Vec<_>, _>>()
                    }?;

                    let packed_element = 'packed_element: {
                        let term::TupleElement::Unpacked(ty) =
                            tuple_ty.elements.get(unpacked_position).unwrap()
                        else {
                            // TODO: report mismatched error
                            break 'packed_element Irrefutable::Wildcard(
                                Wildcard,
                            );
                        };

                        // create a new alloca where all the unpacked elements
                        // will be stoered.
                        let alloca_id = self.alloca_reserved.reserve(Alloca {
                            r#type: ty.clone(),
                            span: Some(
                                tuple_element_patterns
                                    .get(unpacked_position)
                                    .unwrap()
                                    .span(),
                            ),
                        });

                        // variable declaration instruction and packing
                        self.new_instructions.push(Basic::AllocaAllocation(
                            AllocaAllocation { id: alloca_id },
                        ));
                        self.new_instructions.push(Basic::TuplePack(
                            instruction::TuplePack {
                                store_address: Address::Alloca(alloca_id),
                                tuple_address: address,
                                before_packed_element_count:
                                    before_packed_elements.len(),
                                after_packed_element_count:
                                    after_packed_elements.len(),
                            },
                        ));

                        self.create_irrefutable_interanl(
                            table,
                            tuple_element_patterns
                                .get(unpacked_position)
                                .unwrap()
                                .as_packed()
                                .unwrap()
                                .pattern(),
                            TypeBinding::Value(ty),
                            &Address::Alloca(alloca_id),
                            referring_site,
                            handler,
                        )?
                    };

                    Irrefutable::Tuple(Tuple::Packed(
                        pattern::PackedTupleBinding {
                            before_packed_elements,
                            after_packed_elements,
                            packed_element: Box::new(packed_element),
                        },
                    ))
                } else {
                    // check if every tuple_ty elements are regular
                    if tuple_ty
                        .elements
                        .iter()
                        .any(semantic::term::TupleElement::is_unpacked)
                    {
                        // TODO: report unpacked tuple element exists
                        return Ok(Irrefutable::Wildcard(Wildcard));
                    }

                    let mut elements = Vec::new();
                    for (index, (tuple_ty, tuple_pat)) in tuple_ty
                        .elements
                        .iter()
                        .map(|x| x.as_regular().unwrap())
                        .zip(
                            tuple_element_patterns
                                .iter()
                                .map(|x| x.as_regular().unwrap()),
                        )
                        .enumerate()
                    {
                        let element_address =
                            Address::Tuple(crate::ir::address::Tuple {
                                tuple_address: Box::new(address.clone()),
                                offset: address::Offset::FromStart(index),
                            });
                        let pattern = self.create_irrefutable_interanl(
                            table,
                            tuple_pat,
                            reference_binding_info.map_or_else(
                                || TypeBinding::Value(tuple_ty),
                                |(lifetime, qualifier)| {
                                    TypeBinding::Reference {
                                        lifetime,
                                        qualifier,
                                        r#type: tuple_ty,
                                    }
                                },
                            ),
                            &element_address,
                            referring_site,
                            handler,
                        )?;

                        elements.push(pattern);
                    }

                    Irrefutable::Tuple(Tuple::Regular(RegularTupleBinding {
                        elements,
                    }))
                }
            }

            syntax_tree::pattern::Irrefutable::Wildcard(_) => {
                Irrefutable::Wildcard(Wildcard)
            }
        })
    }
}

impl<T> Representation<T> {
    /// Creates an irrefutable pattern from the given syntax tree.
    ///
    /// # Errors
    ///
    /// See [`CreatePatternError`] for the possible errors that can occur.
    #[allow(clippy::too_many_arguments)]
    pub fn create_irrefutable(
        &mut self,
        table: &Table<impl State>,
        syntax_tree: &syntax_tree::pattern::Irrefutable,
        simplified_type: &Type,
        address: &Address,
        block_id: ID<Block>,
        referring_site: GlobalID,
        handler: &dyn Handler<Box<dyn Error>>,
    ) -> Result<Irrefutable, CreatePatternError> {
        // check if the `block_id` is valid
        let block = self
            .control_flow_graph
            .get_block(block_id)
            .ok_or(CreatePatternError::InvalidBlockID)?;

        // check if the `block` is reachable
        if block.is_unreachable() {
            return Err(CreatePatternError::UnreachableBlock);
        }

        let mut side_effect = SideEffect {
            new_instructions: Vec::new(),
            register_reserved: Reserve::new(&self.registers),
            alloca_reserved: Reserve::new(&self.allocas),
            current_scope_id: block.in_scope_id(),
        };

        let pattern = side_effect.create_irrefutable_interanl(
            table,
            syntax_tree,
            TypeBinding::Value(simplified_type),
            address,
            referring_site,
            handler,
        )?;

        // insert new allocas
        let SideEffect {
            new_instructions,
            register_reserved,
            alloca_reserved,
            ..
        } = side_effect;

        let register_reserved = register_reserved.into_reserved();
        let alloca_reserved = alloca_reserved.into_reserved();

        // TODO: eliminate the unused instructions, allocas, and registers

        // insert the new instructions
        let block = self.control_flow_graph.get_block_mut(block_id).unwrap();
        for instruction in new_instructions {
            block.insert_basic(instruction);
        }

        for (id, alloca) in alloca_reserved {
            self.allocas.insert_with_id(id, alloca).unwrap();
        }

        // insert new registers
        for (id, register) in register_reserved {
            self.registers.insert_with_id(id, register).unwrap();
        }

        Ok(pattern)
    }
}

#[cfg(test)]
mod tests;
