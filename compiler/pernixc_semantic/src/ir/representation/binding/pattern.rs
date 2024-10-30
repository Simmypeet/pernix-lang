//! Contains the code to bind a pattern syntax tree to the IR.

use pernixc_base::{handler::Handler, source_file::Span};

use super::{
    infer::{self, Erased},
    Binder,
};
use crate::{
    arena::ID,
    error::{
        Error, FoundPackTuplePatternInReferenceBoundTupleType,
        MismatchedQualifierForReferenceOf,
    },
    ir::{
        address::{self, Address, Memory, ReferenceAddress},
        instruction::{self, Instruction, Store, TuplePack},
        pattern::{
            Irrefutable, NameBinding, NameBindingPoint, Pattern, Structural,
            Tuple,
        },
        value::{
            register::{Assignment, Load, ReferenceOf},
            Value,
        },
    },
    symbol::{
        table::{self, representation::Index, resolution},
        AdtID, Field, Variant,
    },
    type_system::{
        self,
        instantiation::{self, Instantiation},
        model::Model,
        simplify,
        term::{
            self,
            lifetime::Lifetime,
            r#type::{Qualifier, Reference, Type},
            Symbol,
        },
    },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum BindingKind {
    Value,
    Reference,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Binding<'a> {
    kind: BindingKind,
    r#type: &'a Type<infer::Model>,
    address: Address<infer::Model>,
    qualifier: Qualifier,
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>,
    > Binder<'t, S, RO, TO>
{
    /// Finds the [`Named`] pattern and adds it to the [`NameBindingPoint`].
    ///
    /// # Assumptions
    ///
    /// - The irrefutable pattern has been bound with the given
    ///   `simplified_type`.
    /// - `address` is where the value is stored.
    /// - All the pattern must have a span.
    ///
    /// Any violation of the assumptions will result in a panic.
    pub(super) fn insert_named_binding_point(
        &mut self,
        name_binding_point: &mut NameBindingPoint<infer::Model>,
        irreftuable: &Irrefutable,
        simplified_type: &Type<infer::Model>,
        address: Address<infer::Model>,
        qualifier: Qualifier,
        must_copy: bool,
        handler: &dyn Handler<Box<dyn Error>>,
    ) {
        self.insert_named_binding_point_internal(
            name_binding_point,
            irreftuable,
            Binding {
                kind: BindingKind::Value,
                r#type: simplified_type,
                qualifier,
                address,
            },
            must_copy,
            handler,
        );
    }

    #[allow(clippy::too_many_arguments)]
    fn create_reference_bound_named_pattern(
        &mut self,
        name_binding_point: &mut NameBindingPoint<infer::Model>,
        address_type: Type<infer::Model>,
        qualifier: Qualifier,
        address: Address<infer::Model>,
        span: Span,
        name: String,
        mutable: bool,
        handler: &dyn Handler<Box<dyn Error>>,
    ) {
        // address_type <= alloca_type <= named_type
        let register_id = self.create_register_assignmnet(
            Assignment::ReferenceOf(ReferenceOf {
                address,
                qualifier,
                lifetime: Lifetime::Inference(Erased),
            }),
            Some(span.clone()),
        );

        let alloca_ty = Type::Reference(Reference {
            qualifier,
            lifetime: Lifetime::Inference(Erased),
            pointee: Box::new(address_type),
        });

        let alloca_id = self.create_alloca(alloca_ty, Some(span.clone()));

        let _ = self.current_block_mut().insert_instruction(
            instruction::Instruction::Store(Store {
                address: Address::Memory(Memory::Alloca(alloca_id)),
                value: Value::Register(register_id),
            }),
        );

        let _ = name_binding_point.insert(
            name,
            NameBinding {
                mutable,
                load_address: Address::Memory(Memory::Alloca(alloca_id)),
                span: Some(span),
            },
            handler,
        );
    }

    fn reduce_reference<'a>(&self, mut binding: Binding<'a>) -> Binding<'a> {
        loop {
            match binding.r#type {
                Type::Reference(reference) => {
                    // update the address, reference binding
                    // info, and binding ty
                    binding = Binding {
                        kind: BindingKind::Reference,
                        r#type: &*reference.pointee,
                        qualifier: reference.qualifier.min(
                            if self.is_behind_reference(&binding.address) {
                                binding.qualifier
                            } else {
                                Qualifier::Mutable
                            },
                        ),
                        address: Address::ReferenceAddress(ReferenceAddress {
                            reference_address: Box::new(binding.address),
                        }),
                    };
                }

                _ => break binding,
            }
        }
    }

    const fn convert_from_start_index_to_end_index(
        start_index: usize,
        total_length: usize,
    ) -> usize {
        total_length - start_index - 1
    }

    #[allow(clippy::too_many_lines)]
    fn insert_named_binding_point_tuple(
        &mut self,
        name_binding_point: &mut NameBindingPoint<infer::Model>,
        tuple_pat: &Tuple<Irrefutable>,
        mut binding: Binding,
        must_copy: bool,
        handler: &dyn Handler<Box<dyn Error>>,
    ) {
        binding = self.reduce_reference(binding);

        let Type::Tuple(tuple_ty) = binding.r#type else {
            panic!("unexpected type!");
        };

        assert!(tuple_pat.elements.iter().filter(|x| x.is_packed).count() <= 1);
        let packed_position =
            tuple_pat.elements.iter().position(|x| x.is_packed);

        if let Some(packed_position) = packed_position {
            // find the position of the unpacked element in type
            let unpacked_position_in_type = {
                let unpacked_count =
                    tuple_ty.elements.iter().filter(|x| x.is_unpacked).count();

                match unpacked_count {
                    0 => None,
                    1 => Some(
                        tuple_ty
                            .elements
                            .iter()
                            .position(|x| x.is_unpacked)
                            .unwrap(),
                    ),

                    count => panic!("unexpected unpacked count: {count}"),
                }
            };

            // can't be reference bound
            if matches!(binding.kind, BindingKind::Reference) {
                handler.receive(Box::new(
                    FoundPackTuplePatternInReferenceBoundTupleType {
                        pattern_span: tuple_pat.span.clone().unwrap(),
                    },
                ));
                return;
            }

            let start_range = 0..packed_position;
            let tuple_end_range = packed_position + 1..tuple_pat.elements.len();
            let type_end_range = (tuple_ty.elements.len()
                - tuple_end_range.len())
                ..tuple_ty.elements.len();
            let type_pack_range = packed_position..type_end_range.start;

            // match the start
            for (index, (tuple_ty, tuple_pat)) in tuple_ty.elements
                [start_range.clone()]
            .iter()
            .zip(&tuple_pat.elements[start_range])
            .enumerate()
            {
                assert!(!tuple_ty.is_unpacked);

                let element_address = Address::Tuple(address::Tuple {
                    tuple_address: Box::new(binding.address.clone()),
                    offset: address::Offset::FromStart(index),
                });

                self.insert_named_binding_point_internal(
                    name_binding_point,
                    &tuple_pat.pattern,
                    Binding {
                        kind: binding.kind,
                        r#type: &tuple_ty.term,
                        address: element_address,
                        qualifier: binding.qualifier,
                    },
                    must_copy,
                    handler,
                );
            }

            // create a new alloca where all the elements will be stoered.
            let packed_type = Type::Tuple(term::Tuple {
                elements: tuple_ty.elements[type_pack_range.clone()].to_vec(),
            });
            let packed_alloca = self.create_alloca(
                packed_type.clone(),
                Some(
                    tuple_pat
                        .elements
                        .get(packed_position)
                        .unwrap()
                        .pattern
                        .span()
                        .cloned()
                        .unwrap(),
                ),
            );

            if let Some(unpacked_position_in_type) = unpacked_position_in_type {
                assert!(type_pack_range.contains(&unpacked_position_in_type));
                // pack from starting_pack to unpacked_position
                let before_unpacked_range =
                    type_pack_range.start..unpacked_position_in_type;

                for (offset, index) in before_unpacked_range.clone().enumerate()
                {
                    let element_address =
                        Address::Tuple(crate::ir::address::Tuple {
                            tuple_address: Box::new(binding.address.clone()),
                            offset: address::Offset::FromStart(index),
                        });

                    let moved_reg = self.create_register_assignmnet(
                        Assignment::Load(Load { address: element_address }),
                        Some(
                            tuple_pat
                                .elements
                                .get(packed_position)
                                .unwrap()
                                .pattern
                                .span()
                                .cloned()
                                .unwrap(),
                        ),
                    );

                    let _ = self.current_block_mut().insert_instruction(
                        Instruction::Store(Store {
                            address: Address::Tuple(address::Tuple {
                                tuple_address: Box::new(Address::Memory(
                                    Memory::Alloca(packed_alloca),
                                )),
                                offset: address::Offset::FromStart(offset),
                            }),
                            value: Value::Register(moved_reg),
                        }),
                    );
                }

                // use dedicated instruction to pack the unpacked element
                let _ = self.current_block_mut().insert_instruction(
                    Instruction::TuplePack(TuplePack {
                        store_address: Address::Memory(Memory::Alloca(
                            packed_alloca,
                        )),
                        tuple_address: binding.address.clone(),
                        starting_offset: before_unpacked_range.count(),
                        before_packed_element_count: unpacked_position_in_type,
                        after_packed_element_count: tuple_ty.elements.len()
                            - unpacked_position_in_type
                            - 1,
                    }),
                );

                // pack from unpacked_position to end
                let after_unpacked_range =
                    unpacked_position_in_type + 1..type_pack_range.end;
                for (enumerate, index) in
                    after_unpacked_range.clone().enumerate()
                {
                    let element_address =
                        Address::Tuple(crate::ir::address::Tuple {
                            tuple_address: Box::new(binding.address.clone()),
                            offset: address::Offset::FromEnd(
                                Self::convert_from_start_index_to_end_index(
                                    index,
                                    tuple_ty.elements.len(),
                                ),
                            ),
                        });

                    let moved_reg = self.create_register_assignmnet(
                        Assignment::Load(Load { address: element_address }),
                        Some(
                            tuple_pat
                                .elements
                                .get(packed_position)
                                .unwrap()
                                .pattern
                                .span()
                                .cloned()
                                .unwrap(),
                        ),
                    );

                    let _ = self.current_block_mut().insert_instruction(
                        Instruction::Store(Store {
                            address: Address::Tuple(address::Tuple {
                                tuple_address: Box::new(Address::Memory(
                                    Memory::Alloca(packed_alloca),
                                )),
                                offset: address::Offset::FromEnd(
                                    after_unpacked_range.clone().count()
                                        - enumerate
                                        - 1,
                                ),
                            }),
                            value: Value::Register(moved_reg),
                        }),
                    );
                }
            } else {
                for (offset, index) in type_pack_range.enumerate() {
                    let element_address =
                        Address::Tuple(crate::ir::address::Tuple {
                            tuple_address: Box::new(binding.address.clone()),
                            offset: address::Offset::FromStart(index),
                        });

                    let moved_reg = self.create_register_assignmnet(
                        Assignment::Load(Load { address: element_address }),
                        Some(
                            tuple_pat
                                .elements
                                .get(packed_position)
                                .unwrap()
                                .pattern
                                .span()
                                .cloned()
                                .unwrap(),
                        ),
                    );

                    let _ = self.current_block_mut().insert_instruction(
                        Instruction::Store(Store {
                            address: Address::Tuple(address::Tuple {
                                tuple_address: Box::new(Address::Memory(
                                    Memory::Alloca(packed_alloca),
                                )),
                                offset: address::Offset::FromStart(offset),
                            }),
                            value: Value::Register(moved_reg),
                        }),
                    );
                }
            }

            self.insert_named_binding_point_internal(
                name_binding_point,
                &tuple_pat.elements.get(packed_position).unwrap().pattern,
                Binding {
                    kind: binding.kind,
                    r#type: &packed_type,
                    address: Address::Memory(Memory::Alloca(packed_alloca)),
                    qualifier: Qualifier::Mutable,
                },
                false,
                handler,
            );

            // match the end
            for ((ty_elem, pat_elem), ty_index) in tuple_ty.elements
                [type_end_range.clone()]
            .iter()
            .zip(&tuple_pat.elements[tuple_end_range])
            .zip(type_end_range)
            {
                assert!(!ty_elem.is_unpacked);
                let element_address = Address::Tuple(address::Tuple {
                    tuple_address: Box::new(binding.address.clone()),
                    offset: if unpacked_position_in_type.is_some() {
                        address::Offset::FromEnd(
                            Self::convert_from_start_index_to_end_index(
                                ty_index,
                                tuple_ty.elements.len(),
                            ),
                        )
                    } else {
                        address::Offset::FromStart(ty_index)
                    },
                });

                self.insert_named_binding_point_internal(
                    name_binding_point,
                    &pat_elem.pattern,
                    Binding {
                        kind: binding.kind,
                        r#type: &ty_elem.term,
                        address: element_address,
                        qualifier: binding.qualifier,
                    },
                    must_copy,
                    handler,
                );
            }
        } else {
            assert_eq!(
                tuple_ty.elements.iter().filter(|x| x.is_unpacked).count(),
                0
            );

            for (index, (tuple_ty, tuple_pat)) in tuple_ty
                .elements
                .iter()
                .map(|x| &x.term)
                .zip(tuple_pat.elements.iter())
                .enumerate()
            {
                let element_address = Address::Tuple(address::Tuple {
                    tuple_address: Box::new(binding.address.clone()),
                    offset: address::Offset::FromStart(index),
                });

                self.insert_named_binding_point_internal(
                    name_binding_point,
                    &tuple_pat.pattern,
                    Binding {
                        kind: binding.kind,
                        r#type: tuple_ty,
                        address: element_address,
                        qualifier: binding.qualifier,
                    },
                    must_copy,
                    handler,
                );
            }
        }
    }

    fn insert_named_binding_point_structural(
        &mut self,
        name_binding_point: &mut NameBindingPoint<infer::Model>,
        structural: &Structural<Irrefutable>,
        mut binding: Binding,
        must_copy: bool,
        handler: &dyn Handler<Box<dyn Error>>,
    ) {
        binding = self.reduce_reference(binding);

        // must be a struct type
        let Type::Symbol(Symbol {
            id: AdtID::Struct(struct_id),
            generic_arguments,
        }) = binding.r#type
        else {
            panic!("unexpected type!");
        };

        let struct_id = *struct_id;
        let struct_symbol = self.table.get(struct_id).unwrap();

        let instantiation = Instantiation::from_generic_arguments(
            generic_arguments.clone(),
            struct_id.into(),
            &struct_symbol.generic_declaration.parameters,
        )
        .unwrap();

        assert_eq!(
            struct_symbol.field_declaration_order().len(),
            structural.patterns_by_field_id.len()
        );

        for field_id in struct_symbol.field_declaration_order().iter().copied()
        {
            let mut binding_cloned = binding.clone();
            let mut field_ty = infer::Model::from_default_type(
                struct_symbol.fields().get(field_id).unwrap().r#type.clone(),
            );

            instantiation::instantiate(&mut field_ty, &instantiation);
            field_ty =
                simplify::simplify(&field_ty, &self.create_environment())
                    .result;

            binding_cloned.r#type = &field_ty;
            binding_cloned.address = Address::Field(address::Field {
                struct_address: Box::new(binding_cloned.address),
                id: field_id,
            });

            self.insert_named_binding_point_internal(
                name_binding_point,
                structural.patterns_by_field_id.get(&field_id).unwrap(),
                binding_cloned,
                must_copy,
                handler,
            );
        }
    }

    fn insert_named_binding_point_internal(
        &mut self,
        name_binding_point: &mut NameBindingPoint<infer::Model>,
        irreftuable: &Irrefutable,
        binding: Binding,
        must_copy: bool,
        handler: &dyn Handler<Box<dyn Error>>,
    ) {
        match irreftuable {
            Irrefutable::Named(pat) => {
                match (pat.reference_binding, binding.kind) {
                    // obtains the reference of the value.
                    (
                        Some(qualifier),
                        BindingKind::Value | BindingKind::Reference,
                    ) => {
                        if qualifier > binding.qualifier {
                            handler.receive(Box::new(
                                MismatchedQualifierForReferenceOf {
                                    reference_of_span: pat
                                        .span
                                        .clone()
                                        .unwrap(),
                                    found_qualifier: binding.qualifier,
                                    expected_qualifier: qualifier,
                                    is_behind_reference: self
                                        .is_behind_reference(&binding.address),
                                },
                            ));
                        }

                        self.create_reference_bound_named_pattern(
                            name_binding_point,
                            binding.r#type.clone(),
                            qualifier,
                            binding.address,
                            pat.span.clone().unwrap(),
                            pat.name.clone(),
                            pat.is_mutable,
                            handler,
                        )
                    }

                    // normal value binding
                    (None, BindingKind::Value) => {
                        let load_address = if must_copy {
                            let alloca_id = self.create_alloca(
                                binding.r#type.clone(),
                                Some(pat.span.clone().unwrap()),
                            );

                            // copy/move the value from the given address
                            let load_value = Value::Register(
                                self.create_register_assignmnet(
                                    Assignment::Load(Load {
                                        address: binding.address,
                                    }),
                                    Some(pat.span.clone().unwrap()),
                                ),
                            );

                            // store the value to the alloca
                            let _ =
                                self.current_block_mut().insert_instruction(
                                    Instruction::Store(Store {
                                        address: Address::Memory(
                                            Memory::Alloca(alloca_id),
                                        ),
                                        value: load_value,
                                    }),
                                );

                            Address::Memory(Memory::Alloca(alloca_id))
                        } else {
                            binding.address
                        };

                        let _ = name_binding_point.insert(
                            pat.name.clone(),
                            NameBinding {
                                mutable: pat.is_mutable,
                                load_address,
                                span: Some(pat.span.clone().unwrap()),
                            },
                            handler,
                        );
                    }

                    (None, BindingKind::Reference) => {
                        self.create_reference_bound_named_pattern(
                            name_binding_point,
                            binding.r#type.clone(),
                            binding.qualifier,
                            binding.address,
                            pat.span.clone().unwrap(),
                            pat.name.clone(),
                            pat.is_mutable,
                            handler,
                        );
                    }
                }
            }

            Irrefutable::Tuple(pat) => {
                self.insert_named_binding_point_tuple(
                    name_binding_point,
                    pat,
                    binding,
                    must_copy,
                    handler,
                );
            }

            Irrefutable::Structural(pat) => {
                self.insert_named_binding_point_structural(
                    name_binding_point,
                    pat,
                    binding,
                    must_copy,
                    handler,
                );
            }

            Irrefutable::Wildcard(_) => {}
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(super) struct FieldPath {
    pub(super) field_id: ID<Field>,
    pub(super) struct_path: Box<Path>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(super) struct TupleElementPath {
    pub(super) index: usize,
    pub(super) tuple_path: Box<Path>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(super) struct VariantPath {
    pub(super) variant_id: ID<Variant>,
    pub(super) enum_path: Box<Path>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(super) enum Path {
    Base,
    Field(FieldPath),
    TupleElement(TupleElementPath),
    Variant(VariantPath),
}

#[cfg(test)]
mod test;
