//! Contains the code to bind a pattern syntax tree to the IR.

use std::collections::HashMap;

use pernixc_base::{
    diagnostic::{Handler, Storage},
    source_file::{SourceElement, Span},
};
use pernixc_lexical::token::Identifier;
use pernixc_syntax::syntax_tree::{self, ConnectedList};

use super::{
    infer::{self, Erased},
    Binder,
};
use crate::{
    arena::{Reserve, ID},
    error::{
        AlreadyBoundFieldPattern, Error, FieldIsNotAccessible, FieldNotFound,
        FoundUnpackedElementInReferenceBoundTupleType,
        MismatchedPatternBindingType, MismatchedTuplePatternLength,
        PatternBindingType,
    },
    ir::{
        address::{self, Address, Field, Memory, Stack},
        alloca::Alloca,
        instruction::{
            self, AllocaAllocation, Instruction, RegisterAssignment, Store,
        },
        pattern::{
            self, Irrefutable, Named, RegularTupleBinding, Structural, Wildcard,
        },
        register::{Assignment, Load, LoadKind, ReferenceOf, Register},
    },
    symbol::{
        table::{
            self, representation::Index, resolution::Observer, State, Table,
        },
        GlobalID, Struct,
    },
    type_system::{
        fresh,
        instantiation::{
            self, Instantiation, MismatchedGenericArgumentCountError,
        },
        model::Model,
        term::{
            self,
            lifetime::Lifetime,
            r#type::{self, Qualifier, Reference, SymbolID, Type},
            Symbol,
        },
    },
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum TypeBinding<'a, M: Model> {
    Value(&'a Type<M>),
    Reference { qualifier: Qualifier, r#type: &'a Type<M> },
}

/// The error that can occur when creating a pattern.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
pub enum CreatePatternError {
    #[error(
        "the type contains an invalid struct ID: {0:?} which does not exist \
         in the symbol table"
    )]
    InvalidStructID(ID<Struct>),

    #[error(
        "the type contains a term containing generic arguments that do not \
         match the generic parameters of the instantiated symbol"
    )]
    MismatchedGenericParameterCount(
        #[from] MismatchedGenericArgumentCountError<infer::Model>,
    ),

    /// The type of the pattern binding contains an ill-formed tuple type. This
    /// is considered as a compiler's error.
    #[error(
        "the type contains a tuple type having more than one unpacked element"
    )]
    MoreThanOneUnpackedInTupleType(term::Tuple<r#type::Type<infer::Model>>),
}

impl<'t, S: table::State, O: Observer<S, infer::Model>> Binder<'t, S, O> {
    /// Bind the given irrefutable pattern syntax tree to the IR and return the
    /// [`Irrefutable`] pattern.
    pub fn create_irrefutable(
        &mut self,
        syntax_tree: &syntax_tree::pattern::Irrefutable,
        simplified_type: &Type<infer::Model>,
        address: &Address<Memory<infer::Model>>,
        handler: &dyn Handler<Box<dyn Error>>,
    ) -> Result<Irrefutable<infer::Model>, CreatePatternError> {
        let storage = Storage::<Box<dyn Error>>::default();
        let mut side_effect = SideEffect {
            new_instructions: Vec::new(),
            register_reserved: Reserve::new(
                &self.intermediate_representation.registers,
            ),
            alloca_reserved: Reserve::new(
                &self.intermediate_representation.allocas,
            ),
        };

        let pattern = side_effect.create_irrefutable_interanl(
            self.table,
            syntax_tree,
            TypeBinding::Value(simplified_type),
            address,
            self.current_site,
            &storage,
        )?;

        // have no ICEs, propagate the diagnostic
        storage.propagate(handler);

        // insert new allocas
        let SideEffect {
            new_instructions,
            register_reserved,
            alloca_reserved,
            ..
        } = side_effect;

        let register_reserved = register_reserved.into_reserved();
        let alloca_reserved = alloca_reserved.into_reserved();

        // insert the new instructions
        let block = self.current_block_mut();
        for instruction in new_instructions {
            block.insert_basic(instruction);
        }

        for (id, alloca) in alloca_reserved {
            self.intermediate_representation
                .allocas
                .insert_with_id(id, alloca)
                .unwrap();
        }

        // insert new registers
        for (id, register) in register_reserved {
            self.intermediate_representation
                .registers
                .insert_with_id(id, register)
                .unwrap();
        }

        Ok(pattern)
    }
}

struct SideEffect<'a> {
    new_instructions: Vec<Instruction<infer::Model>>,
    register_reserved: Reserve<'a, Register<infer::Model>>,
    alloca_reserved: Reserve<'a, Alloca<infer::Model>>,
}

impl<'a> SideEffect<'a> {
    fn create_reference_bound_named_pattern(
        &mut self,
        mut address_type: Type<infer::Model>,
        qualifier: Qualifier,
        span: Span,
        address: &Address<Memory<infer::Model>>,
        identifier: &Identifier,
        mutable: bool,
    ) -> Named<infer::Model> {
        // address_type <= alloca_type <= named_type
        let register_id = self.register_reserved.reserve(Register {
            assignment: Assignment::ReferenceOf(ReferenceOf {
                is_local: false,
                address: address.clone(),
                qualifier,
                lifetime: Lifetime::Inference(Erased),
            }),
            span: None,
        });

        // freshen the lifetime of the address type
        fresh::replace_with_fresh_lifeteime_inference(&mut address_type);

        let alloca_ty = Type::Reference(Reference {
            qualifier,
            lifetime: Lifetime::Inference(Erased),
            pointee: Box::new(address_type),
        });

        let alloca_id = self
            .alloca_reserved
            .reserve(Alloca { r#type: alloca_ty.clone(), span: Some(span) });

        // create a register that holds the reference of the
        // value
        self.new_instructions.push(instruction::Instruction::AllocaAllocation(
            AllocaAllocation { id: alloca_id },
        ));
        self.new_instructions.push(
            instruction::Instruction::RegisterAssignment(RegisterAssignment {
                id: register_id,
            }),
        );
        self.new_instructions.push(instruction::Instruction::Store(Store {
            address: Address::Base(Memory::Alloca(alloca_id)),
            value: register_id,
            is_initializattion: true,
        }));

        Named {
            name: identifier.span.str().to_owned(),
            load_address: Stack::Alloca(alloca_id),
            mutable,
            span: Some(identifier.span.clone()),
        }
    }

    fn reduce_reference<'b>(
        &mut self,
        type_binding: &TypeBinding<'b, infer::Model>,
        mut address: Address<Memory<infer::Model>>,
    ) -> (
        &'b Type<infer::Model>,
        Address<Memory<infer::Model>>,
        Option<Qualifier>,
    ) {
        let (mut current_ty, mut reference_binding_info) = match type_binding {
            TypeBinding::Value(ty) => (*ty, None),
            TypeBinding::Reference { qualifier, r#type } => {
                (*r#type, Some(*qualifier))
            }
        };

        loop {
            match current_ty {
                Type::Reference(reference) => {
                    let register = self.register_reserved.reserve(Register {
                        assignment: Assignment::Load(Load {
                            address,
                            kind: LoadKind::Copy,
                        }),
                        span: None,
                    });
                    self.new_instructions.push(
                        Instruction::RegisterAssignment(RegisterAssignment {
                            id: register,
                        }),
                    );

                    // update the address, reference binding
                    // info, and binding ty
                    address = Address::Base(Memory::ReferenceValue(register));
                    reference_binding_info = Some(reference.qualifier);
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
        type_binding: TypeBinding<infer::Model>,
        address: &Address<Memory<infer::Model>>,
        referring_site: GlobalID,
        handler: &dyn Handler<Box<dyn Error>>,
    ) -> Result<Irrefutable<infer::Model>, CreatePatternError> {
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
                        .fields()
                        .get_id(field_name)
                        .map(|x| (struct_symbol.fields().get(x).unwrap(), x))
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
                    let mut field_ty = infer::Model::from_default_type(
                        field_sym.r#type.clone(),
                    );
                    instantiation::instantiate(&mut field_ty, &instantiation);

                    let type_binding = reference_binding_info.map_or_else(
                        || TypeBinding::Value(&field_ty),
                        |qualifier| TypeBinding::Reference {
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

                    if !table
                        .is_accessible_from(
                            referring_site,
                            field_sym.accessibility,
                        )
                        .unwrap()
                    {
                        // soft error, no need to stop the process
                        handler.receive(Box::new(FieldIsNotAccessible {
                            field_id,
                            struct_id,
                            referring_site,
                            referring_identifier_span: match field {
                                syntax_tree::pattern::Field::Association(
                                    pat,
                                ) => pat.identifier().span.clone(),
                                syntax_tree::pattern::Field::Named(pat) => {
                                    pat.identifier().span.clone()
                                }
                            },
                        }));
                    }

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
                    ) => {
                        let qualifier = ref_binding
                            .qualifier()
                            .as_ref()
                            .map_or(Qualifier::Immutable, |q| match q {
                                syntax_tree::Qualifier::Mutable(_) => {
                                    Qualifier::Mutable
                                }
                                syntax_tree::Qualifier::Unique(_) => {
                                    Qualifier::Unique
                                }
                            });

                        Irrefutable::Named(
                            self.create_reference_bound_named_pattern(
                                ty.clone(),
                                qualifier,
                                named.span(),
                                address,
                                named.identifier(),
                                false,
                            ),
                        )
                    }

                    // normal value binding
                    (
                        syntax_tree::pattern::Binding::Value {
                            mutable_keyword,
                        },
                        TypeBinding::Value(ty),
                    ) => {
                        // if the address is not alloca or parameter, then
                        // create a new alloca and move
                        // the value to the alloca

                        let stack = match address {
                            Address::Base(Memory::Alloca(id)) => {
                                Some(Stack::Alloca(*id))
                            }
                            Address::Base(Memory::Parameter(id)) => {
                                Some(Stack::Parameter(*id))
                            }
                            _ => None,
                        };

                        if let Some(stack) = stack {
                            Irrefutable::Named(Named {
                                name: named.identifier().span.str().to_owned(),
                                load_address: stack,
                                mutable: mutable_keyword.is_some(),
                                span: Some(named.identifier().span.clone()),
                            })
                        } else {
                            let alloca_id =
                                self.alloca_reserved.reserve(Alloca {
                                    r#type: {
                                        let mut ty = ty.clone();
                                        fresh::replace_with_fresh_lifeteime_inference(
                                            &mut ty,
                                        );

                                        ty
                                    },
                                    span: Some(named.identifier().span.clone()),
                                });

                            let id = self.register_reserved.reserve(Register {
                                assignment: Assignment::Load(Load {
                                    address: address.clone(),
                                    kind: LoadKind::Move,
                                }),
                                span: None,
                            });

                            self.new_instructions.push(
                                Instruction::AllocaAllocation(
                                    AllocaAllocation { id: alloca_id },
                                ),
                            );
                            self.new_instructions.push(
                                Instruction::RegisterAssignment(
                                    RegisterAssignment { id },
                                ),
                            );
                            self.new_instructions.push(Instruction::Store(
                                Store {
                                    address: Address::Base(Memory::Alloca(
                                        alloca_id,
                                    )),
                                    value: id,
                                    is_initializattion: true,
                                },
                            ));

                            Irrefutable::Named(Named {
                                name: named.identifier().span.str().to_owned(),
                                load_address: Stack::Alloca(alloca_id),
                                mutable: mutable_keyword.is_some(),
                                span: Some(named.identifier().span.clone()),
                            })
                        }
                    }

                    (binding, TypeBinding::Reference { qualifier, r#type }) => {
                        Irrefutable::Named(
                            self.create_reference_bound_named_pattern(
                                r#type.clone(),
                                qualifier,
                                named.span(),
                                address,
                                named.identifier(),
                                match binding {
                                    syntax_tree::pattern::Binding::Ref(_) => {
                                        false
                                    }
                                    syntax_tree::pattern::Binding::Value {
                                        mutable_keyword,
                                    } => mutable_keyword.is_some(),
                                },
                            ),
                        )
                    }
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

                // find the position of the unpacked element
                let unpacked_position = {
                    let unpacked_count = tuple_ty
                        .elements
                        .iter()
                        .filter(|x| x.is_unpacked)
                        .count();

                    match unpacked_count {
                        0 => None,
                        1 => Some(
                            tuple_ty
                                .elements
                                .iter()
                                .position(|x| x.is_unpacked)
                                .unwrap(),
                        ),

                        _ => return Err(
                            CreatePatternError::MoreThanOneUnpackedInTupleType(
                                tuple_ty.clone(),
                            ),
                        ),
                    }
                };

                // normal tuple pattern, the number of pattern must exactly
                // match
                let tuple_element_patterns = tuple_pat
                    .patterns()
                    .iter()
                    .flat_map(ConnectedList::elements)
                    .map(|x| &**x)
                    .collect::<Vec<_>>();

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
                        handler.receive(Box::new(
                            FoundUnpackedElementInReferenceBoundTupleType {
                                pattern_span: tuple_pat.span(),
                            },
                        ));
                        return Ok(Irrefutable::Wildcard(Wildcard));
                    }

                    let before_packed_elements = {
                        let before_packed = 0..unpacked_position;

                        tuple_ty.elements[before_packed.clone()]
                            .iter()
                            .map(|x| &x.term)
                            .zip(
                                tuple_element_patterns[before_packed]
                                    .iter()
                                    .copied(),
                            )
                            .enumerate()
                            .map(|(idx, (ty, pat))| {
                                let element_address =
                                    Address::Tuple(crate::ir::address::Tuple {
                                        tuple_address: Box::new(
                                            address.clone(),
                                        ),
                                        offset: address::Offset::FromStart(idx),
                                    });

                                self.create_irrefutable_interanl(
                                    table,
                                    pat,
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

                        let after_packed_count = after_packed.clone().count();

                        tuple_ty.elements[after_packed.clone()]
                            .iter()
                            .map(|x| &x.term)
                            .zip(
                                tuple_element_patterns[after_packed]
                                    .iter()
                                    .copied(),
                            )
                            .enumerate()
                            .map(|(idx, (ty, pat))| {
                                let element_address =
                                    Address::Tuple(crate::ir::address::Tuple {
                                        tuple_address: Box::new(
                                            address.clone(),
                                        ),
                                        offset: address::Offset::FromEnd(
                                            after_packed_count - idx - 1,
                                        ),
                                    });

                                self.create_irrefutable_interanl(
                                    table,
                                    pat,
                                    TypeBinding::Value(ty), // can only be value binding
                                    &element_address,
                                    referring_site,
                                    handler,
                                )
                            })
                            .collect::<Result<Vec<_>, _>>()
                    }?;

                    let packed_element = {
                        let mut ty = tuple_ty
                            .elements
                            .get(unpacked_position)
                            .unwrap()
                            .term
                            .clone();
                        fresh::replace_with_fresh_lifeteime_inference(&mut ty);

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
                        self.new_instructions.push(
                            Instruction::AllocaAllocation(AllocaAllocation {
                                id: alloca_id,
                            }),
                        );
                        self.new_instructions.push(Instruction::TuplePack(
                            instruction::TuplePack {
                                store_address: Address::Base(Memory::Alloca(
                                    alloca_id,
                                )),
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
                                .unwrap(),
                            TypeBinding::Value(&ty),
                            &Address::Base(Memory::Alloca(alloca_id)),
                            referring_site,
                            handler,
                        )?
                    };

                    Irrefutable::Tuple(pattern::Tuple::Packed(
                        pattern::PackedTupleBinding {
                            before_packed_elements,
                            after_packed_elements,
                            packed_element: Box::new(packed_element),
                        },
                    ))
                } else {
                    let mut elements = Vec::new();
                    for (index, (tuple_ty, tuple_pat)) in tuple_ty
                        .elements
                        .iter()
                        .map(|x| &x.term)
                        .zip(tuple_element_patterns.iter().copied())
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
                                |qualifier| TypeBinding::Reference {
                                    qualifier,
                                    r#type: tuple_ty,
                                },
                            ),
                            &element_address,
                            referring_site,
                            handler,
                        )?;

                        elements.push(pattern);
                    }

                    Irrefutable::Tuple(pattern::Tuple::Regular(
                        RegularTupleBinding { elements },
                    ))
                }
            }

            syntax_tree::pattern::Irrefutable::Wildcard(_) => {
                Irrefutable::Wildcard(Wildcard)
            }
        })
    }
}

#[cfg(test)]
mod tests;
