//! Contains the logic for collecting name bindings from patterns

use flexstr::SharedStr;
use pernixc_arena::ID;
use pernixc_handler::Handler;
use pernixc_ir::{
    address::{self, Address, Memory, Variant},
    instruction::{Instruction, Store, TuplePack},
    pattern::{
        Enum, Irrefutable, NameBinding, NameBindingPoint, Named, Refutable,
        Structural, Tuple,
    },
    scope::Scope,
    value::{
        Value,
        register::{Assignment, Borrow, load::Load},
    },
};
use pernixc_lexical::tree::{RelativeLocation, RelativeSpan};
use pernixc_semantic_element::{
    fields::get_fields, variant::get_variant_associated_type,
};
use pernixc_source_file::SourceElement;
use pernixc_term::{
    generic_arguments::Symbol,
    generic_parameters::get_generic_parameters,
    instantiation::Instantiation,
    lifetime::Lifetime,
    tuple,
    r#type::{Qualifier, Reference, Type},
};

use crate::{
    binder::{Binder, UnrecoverableError},
    pattern::insert_name_binding::diagnostic::{
        Diagnostic, FoundPackTuplePatternInReferenceBoundTupleType,
        MismatchedQualifierForReferenceOf,
    },
};

pub mod diagnostic;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum BindingKind {
    Value,
    Reference,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct Binding<'a> {
    kind: BindingKind,
    r#type: &'a Type,
    address: Address,
    qualifier: Qualifier,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct Config {
    pub must_copy: bool,
    pub scope_id: ID<Scope>,
    pub address_span: Option<RelativeSpan>,
}

#[allow(missing_docs)]
pub trait InsertNameBinding:
    SourceElement<Location = RelativeLocation>
{
    #[allow(async_fn_in_trait)]
    async fn insert_name_binding(
        binder: &mut Binder<'_>,
        name_binding_point: &mut NameBindingPoint,
        pattern: &Self,
        binding: Binding,
        config: &Config,
        handler: &dyn Handler<crate::diagnostic::Diagnostic>,
    ) -> Result<(), UnrecoverableError>;
}

impl InsertNameBinding for Refutable {
    async fn insert_name_binding(
        binder: &mut Binder<'_>,
        name_binding_point: &mut NameBindingPoint,
        pattern: &Self,
        binding: Binding<'_>,
        config: &Config,
        handler: &dyn Handler<crate::diagnostic::Diagnostic>,
    ) -> Result<(), UnrecoverableError> {
        match pattern {
            Self::Named(named) => {
                binder.insert_named_name_binding(
                    name_binding_point,
                    named,
                    binding,
                    config,
                    handler,
                );
                Ok(())
            }
            Self::Enum(en) => {
                binder
                    .insert_named_binding_point_enum(
                        name_binding_point,
                        en,
                        binding,
                        config,
                        handler,
                    )
                    .await
            }
            Self::Tuple(tuple) => {
                Box::pin(binder.insert_named_binding_point_tuple(
                    name_binding_point,
                    tuple,
                    binding,
                    config,
                    handler,
                ))
                .await
            }
            Self::Structural(structural) => {
                Box::pin(binder.insert_named_binding_point_struct(
                    name_binding_point,
                    structural,
                    binding,
                    config,
                    handler,
                ))
                .await
            }

            Self::Wildcard(_) | Self::Boolean(_) | Self::Integer(_) => Ok(()),
        }
    }
}

impl InsertNameBinding for Irrefutable {
    async fn insert_name_binding(
        binder: &mut Binder<'_>,
        name_binding_point: &mut NameBindingPoint,
        pattern: &Self,
        binding: Binding<'_>,
        config: &Config,
        handler: &dyn Handler<crate::diagnostic::Diagnostic>,
    ) -> Result<(), UnrecoverableError> {
        match pattern {
            Self::Named(named) => {
                binder.insert_named_name_binding(
                    name_binding_point,
                    named,
                    binding,
                    config,
                    handler,
                );
                Ok(())
            }
            Self::Structural(structural) => {
                Box::pin(binder.insert_named_binding_point_struct(
                    name_binding_point,
                    structural,
                    binding,
                    config,
                    handler,
                ))
                .await
            }

            Self::Tuple(tuple) => {
                Box::pin(binder.insert_named_binding_point_tuple(
                    name_binding_point,
                    tuple,
                    binding,
                    config,
                    handler,
                ))
                .await
            }

            Self::Wildcard(_) => Ok(()),
        }
    }
}

impl Binder<'_> {
    fn insert_named_name_binding(
        &mut self,
        name_binding_point: &mut NameBindingPoint,
        pattern: &Named,
        binding: Binding<'_>,
        config: &Config,
        handler: &dyn Handler<crate::diagnostic::Diagnostic>,
    ) {
        match (pattern.reference_binding, binding.kind) {
            // obtains the reference of the value.
            (Some(qualifier), BindingKind::Value | BindingKind::Reference) => {
                if qualifier > binding.qualifier {
                    handler.receive(
                        Diagnostic::MismatchedQualifierForReferenceOf(
                            MismatchedQualifierForReferenceOf {
                                reference_of_span: pattern.span,
                                found_qualifier: binding.qualifier,
                                expected_qualifier: qualifier,
                                is_behind_reference: binding
                                    .address
                                    .is_behind_reference(),
                            },
                        )
                        .into(),
                    );
                }

                self.create_reference_bound_named_pattern(
                    name_binding_point,
                    binding.r#type.clone(),
                    qualifier,
                    binding.address,
                    config.address_span,
                    pattern.span,
                    pattern.name.clone(),
                    pattern.is_mutable,
                    config.scope_id,
                    handler,
                );
            }

            // normal value binding
            (None, BindingKind::Value) => {
                let load_address = if config.must_copy {
                    let alloca_id = self.create_alloca_with_scope_id(
                        binding.r#type.clone(),
                        config.scope_id,
                        pattern.span,
                    );

                    // copy/move the value from the given address
                    let load_value =
                        Value::Register(self.create_register_assignment(
                            Assignment::Load(Load::new(binding.address)),
                            config.address_span.unwrap_or(pattern.span),
                        ));

                    // store the value to the alloca
                    self.push_instruction(Instruction::Store(Store {
                        address: Address::Memory(Memory::Alloca(alloca_id)),
                        value: load_value,
                        span: pattern.span,
                    }));

                    Address::Memory(Memory::Alloca(alloca_id))
                } else {
                    binding.address
                };

                let _ = name_binding_point.insert(
                    pattern.name.clone(),
                    NameBinding {
                        mutable: pattern.is_mutable,
                        load_address,
                        span: pattern.span,
                    },
                    &handler,
                );
            }

            (None, BindingKind::Reference) => {
                self.create_reference_bound_named_pattern(
                    name_binding_point,
                    binding.r#type.clone(),
                    binding.qualifier,
                    binding.address,
                    config.address_span,
                    pattern.span,
                    pattern.name.clone(),
                    pattern.is_mutable,
                    config.scope_id,
                    handler,
                );
            }
        }
    }

    async fn insert_named_binding_point_struct<T: InsertNameBinding>(
        &mut self,
        name_binding_point: &mut NameBindingPoint,
        structural: &Structural<T>,
        mut binding: Binding<'_>,
        config: &Config,
        handler: &dyn Handler<crate::diagnostic::Diagnostic>,
    ) -> Result<(), UnrecoverableError> {
        binding = reduce_reference(binding);

        // must be a struct type
        let Type::Symbol(Symbol { id: struct_id, generic_arguments }) =
            binding.r#type
        else {
            panic!("unexpected type!");
        };

        let struct_generic_parameters =
            self.engine().get_generic_parameters(*struct_id).await?;

        let instantiation = Instantiation::from_generic_arguments(
            generic_arguments.clone(),
            *struct_id,
            &struct_generic_parameters,
        )
        .unwrap();

        let fields = self.engine().get_fields(*struct_id).await?;

        assert_eq!(
            fields.field_declaration_order.len(),
            structural.patterns_by_field_id.len()
        );

        for field_id in fields.field_declaration_order.iter().copied() {
            let mut binding_cloned = binding.clone();
            let mut field_ty =
                fields.fields.get(field_id).unwrap().r#type.clone();

            instantiation.instantiate(&mut field_ty);

            field_ty = self
                .simplify_type(
                    field_ty,
                    structural
                        .patterns_by_field_id
                        .get(&field_id)
                        .unwrap()
                        .span(),
                    handler,
                )
                .await?
                .result
                .clone();

            binding_cloned.r#type = &field_ty;
            binding_cloned.address = Address::Field(address::Field {
                struct_address: Box::new(binding_cloned.address),
                id: field_id,
            });

            T::insert_name_binding(
                self,
                name_binding_point,
                structural.patterns_by_field_id.get(&field_id).unwrap(),
                binding_cloned,
                config,
                handler,
            )
            .await?;
        }

        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    fn create_reference_bound_named_pattern(
        &mut self,
        name_binding_point: &mut NameBindingPoint,
        address_type: Type,
        qualifier: Qualifier,
        address: Address,
        address_span: Option<RelativeSpan>,
        pattern_span: RelativeSpan,
        name: SharedStr,
        mutable: bool,
        scope_id: ID<Scope>,
        handler: &dyn Handler<crate::diagnostic::Diagnostic>,
    ) {
        // address_type <= alloca_type <= named_type
        let register_id = self.create_register_assignment(
            Assignment::Borrow(Borrow {
                address,
                qualifier,
                lifetime: Lifetime::Erased,
            }),
            address_span.unwrap_or(pattern_span),
        );

        let alloca_ty = Type::Reference(Reference {
            qualifier,
            lifetime: Lifetime::Erased,
            pointee: Box::new(address_type),
        });

        let alloca_id =
            self.create_alloca_with_scope_id(alloca_ty, scope_id, pattern_span);

        self.push_instruction(Instruction::Store(Store {
            address: Address::Memory(Memory::Alloca(alloca_id)),
            value: Value::Register(register_id),
            span: pattern_span,
        }));

        let _ = name_binding_point.insert(
            name,
            NameBinding {
                mutable,
                load_address: Address::Memory(Memory::Alloca(alloca_id)),
                span: pattern_span,
            },
            &handler,
        );
    }

    async fn insert_named_binding_point_enum(
        &mut self,
        name_binding_point: &mut NameBindingPoint,
        enum_pat: &Enum,
        mut binding: Binding<'_>,
        config: &Config,
        handler: &dyn Handler<crate::diagnostic::Diagnostic>,
    ) -> Result<(), UnrecoverableError> {
        let Some(inner) = &enum_pat.pattern else { return Ok(()) };

        binding = reduce_reference(binding);

        // must be an enum type
        let Type::Symbol(Symbol { id: enum_id, generic_arguments }) =
            binding.r#type
        else {
            panic!("unexpected type!");
        };

        let enum_generic_parameters =
            self.engine().get_generic_parameters(*enum_id).await?;

        let instantiation = Instantiation::from_generic_arguments(
            generic_arguments.clone(),
            *enum_id,
            &enum_generic_parameters,
        )
        .unwrap();

        let mut variant_ty = self
            .engine()
            .get_variant_associated_type(enum_pat.variant_id)
            .await?
            .as_deref()
            .cloned()
            .unwrap();

        instantiation.instantiate(&mut variant_ty);

        Box::pin(Refutable::insert_name_binding(
            self,
            name_binding_point,
            &**inner,
            Binding {
                kind: binding.kind,
                r#type: &variant_ty,
                address: Address::Variant(Variant {
                    enum_address: Box::new(binding.address),
                    id: enum_pat.variant_id,
                }),
                qualifier: binding.qualifier,
            },
            config,
            handler,
        ))
        .await
    }

    #[allow(clippy::too_many_lines, clippy::too_many_arguments)]
    async fn insert_named_binding_point_tuple<T: InsertNameBinding>(
        &mut self,
        name_binding_point: &mut NameBindingPoint,
        tuple_pat: &Tuple<T>,
        mut binding: Binding<'_>,
        config: &Config,
        handler: &dyn Handler<crate::diagnostic::Diagnostic>,
    ) -> Result<(), UnrecoverableError> {
        binding = reduce_reference(binding);

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
                handler.receive(
                    Diagnostic::FoundPackTuplePatternInReferenceBoundTupleType(
                        FoundPackTuplePatternInReferenceBoundTupleType {
                            pattern_span: tuple_pat.span,
                        },
                    )
                    .into(),
                );
                return Ok(());
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

                T::insert_name_binding(
                    self,
                    name_binding_point,
                    &tuple_pat.pattern,
                    Binding {
                        kind: binding.kind,
                        r#type: &tuple_ty.term,
                        address: element_address,
                        qualifier: binding.qualifier,
                    },
                    config,
                    handler,
                )
                .await?;
            }

            // create a new alloca where all the elements will be stoered.
            let packed_type = Type::Tuple(tuple::Tuple {
                elements: tuple_ty.elements[type_pack_range.clone()].to_vec(),
            });
            let packed_alloca = self.create_alloca_with_scope_id(
                packed_type.clone(),
                config.scope_id,
                tuple_pat.elements.get(packed_position).unwrap().pattern.span(),
            );

            if let Some(unpacked_position_in_type) = unpacked_position_in_type {
                assert!(type_pack_range.contains(&unpacked_position_in_type));
                // pack from starting_pack to unpacked_position
                let before_unpacked_range =
                    type_pack_range.start..unpacked_position_in_type;

                for (offset, index) in before_unpacked_range.enumerate() {
                    let element_address = Address::Tuple(address::Tuple {
                        tuple_address: Box::new(binding.address.clone()),
                        offset: address::Offset::FromStart(index),
                    });
                    let span = config.address_span.unwrap_or_else(|| {
                        tuple_pat
                            .elements
                            .get(packed_position)
                            .unwrap()
                            .pattern
                            .span()
                    });

                    let moved_reg = self.create_register_assignment(
                        Assignment::Load(Load::new(element_address)),
                        span,
                    );

                    self.push_instruction(Instruction::Store(Store {
                        address: Address::Tuple(address::Tuple {
                            tuple_address: Box::new(Address::Memory(
                                Memory::Alloca(packed_alloca),
                            )),
                            offset: address::Offset::FromStart(offset),
                        }),
                        value: Value::Register(moved_reg),
                        span: tuple_pat
                            .elements
                            .get(packed_position)
                            .unwrap()
                            .pattern
                            .span(),
                    }));
                }

                // use dedicated instruction to pack the unpacked element
                self.push_instruction(Instruction::TuplePack(TuplePack {
                    packed_tuple_span: tuple_pat
                        .elements
                        .get(packed_position)
                        .unwrap()
                        .pattern
                        .span(),
                    store_address: Address::Memory(Memory::Alloca(
                        packed_alloca,
                    )),
                    tuple_address: binding.address.clone(),
                    before_packed_element_count: unpacked_position_in_type,
                    after_packed_element_count: tuple_ty.elements.len()
                        - unpacked_position_in_type
                        - 1,
                }));

                // pack from unpacked_position to end
                let after_unpacked_range =
                    unpacked_position_in_type + 1..type_pack_range.end;
                for (enumerate, index) in
                    after_unpacked_range.clone().enumerate()
                {
                    let element_address = Address::Tuple(address::Tuple {
                        tuple_address: Box::new(binding.address.clone()),
                        offset: address::Offset::FromEnd(
                            Self::convert_from_start_index_to_end_index(
                                index,
                                tuple_ty.elements.len(),
                            ),
                        ),
                    });

                    let moved_reg = self.create_register_assignment(
                        Assignment::Load(Load::new(element_address)),
                        config.address_span.unwrap_or_else(|| {
                            tuple_pat
                                .elements
                                .get(packed_position)
                                .unwrap()
                                .pattern
                                .span()
                        }),
                    );

                    self.push_instruction(Instruction::Store(Store {
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
                        span: tuple_pat
                            .elements
                            .get(packed_position)
                            .unwrap()
                            .pattern
                            .span(),
                    }));
                }
            } else {
                for (offset, index) in type_pack_range.enumerate() {
                    let element_address = Address::Tuple(address::Tuple {
                        tuple_address: Box::new(binding.address.clone()),
                        offset: address::Offset::FromStart(index),
                    });

                    let moved_reg = self.create_register_assignment(
                        Assignment::Load(Load::new(element_address)),
                        config.address_span.unwrap_or_else(|| {
                            tuple_pat
                                .elements
                                .get(packed_position)
                                .unwrap()
                                .pattern
                                .span()
                        }),
                    );

                    self.push_instruction(Instruction::Store(Store {
                        address: Address::Tuple(address::Tuple {
                            tuple_address: Box::new(Address::Memory(
                                Memory::Alloca(packed_alloca),
                            )),
                            offset: address::Offset::FromStart(offset),
                        }),
                        value: Value::Register(moved_reg),
                        span: tuple_pat
                            .elements
                            .get(packed_position)
                            .unwrap()
                            .pattern
                            .span(),
                    }));
                }
            }

            T::insert_name_binding(
                self,
                name_binding_point,
                &tuple_pat.elements.get(packed_position).unwrap().pattern,
                Binding {
                    kind: binding.kind,
                    r#type: &packed_type,
                    address: Address::Memory(Memory::Alloca(packed_alloca)),
                    qualifier: Qualifier::Mutable,
                },
                &Config {
                    must_copy: false,
                    scope_id: config.scope_id,
                    address_span: config.address_span,
                },
                handler,
            )
            .await?;

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

                T::insert_name_binding(
                    self,
                    name_binding_point,
                    &pat_elem.pattern,
                    Binding {
                        kind: binding.kind,
                        r#type: &ty_elem.term,
                        address: element_address,
                        qualifier: binding.qualifier,
                    },
                    config,
                    handler,
                )
                .await?;
            }
        } else {
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

                T::insert_name_binding(
                    self,
                    name_binding_point,
                    &tuple_pat.pattern,
                    Binding {
                        kind: binding.kind,
                        r#type: tuple_ty,
                        address: element_address,
                        qualifier: binding.qualifier,
                    },
                    config,
                    handler,
                )
                .await?;
            }
        }
        Ok(())
    }

    const fn convert_from_start_index_to_end_index(
        start_index: usize,
        total_length: usize,
    ) -> usize {
        total_length - start_index - 1
    }

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
    #[allow(clippy::too_many_arguments)]
    pub async fn insert_name_binding_point<T: InsertNameBinding>(
        &mut self,
        name_binding_point: &mut NameBindingPoint,
        pattern: &T,
        simplified_type: &Type,
        address: Address,
        qualifier: Qualifier,
        config: &Config,
        handler: &dyn Handler<crate::diagnostic::Diagnostic>,
    ) -> Result<(), UnrecoverableError> {
        T::insert_name_binding(
            self,
            name_binding_point,
            pattern,
            Binding {
                kind: BindingKind::Value,
                r#type: simplified_type,
                qualifier,
                address,
            },
            config,
            handler,
        )
        .await
    }
}

fn reduce_reference(mut binding: Binding) -> Binding {
    loop {
        match binding.r#type {
            Type::Reference(reference) => {
                // update the address, reference binding
                // info, and binding ty
                binding = Binding {
                    kind: BindingKind::Reference,
                    r#type: &reference.pointee,
                    qualifier: reference.qualifier.min(
                        if binding.address.is_behind_reference() {
                            binding.qualifier
                        } else {
                            Qualifier::Mutable
                        },
                    ),
                    address: Address::Reference(address::Reference {
                        qualifier: reference.qualifier,
                        reference_address: Box::new(binding.address),
                    }),
                };
            }

            _ => break binding,
        }
    }
}
