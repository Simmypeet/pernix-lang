use std::collections::BTreeSet;

use inkwell::{
    module::Linkage,
    types::BasicType,
    values::{FunctionValue, PointerValue},
    AddressSpace,
};
use pernixc_component::{
    fields::Fields, implementation::Implementation, variant::Variant,
};
use pernixc_ir::model::Erased;
use pernixc_table::{
    component::{Implemented, Member, SymbolKind, VariantDeclarationOrder},
    DisplayObject, GlobalID,
};
use pernixc_term::{
    constant::Constant,
    elided_lifetimes::{ElidedLifetimeID, ElidedLifetimes},
    generic_arguments::GenericArguments,
    generic_parameter::{GenericParameters, LifetimeParameterID},
    instantiation::Instantiation,
    lifetime::Lifetime,
    r#type::{Array, Type},
    Model as _, Symbol, Tuple,
};
use pernixc_type_system::{
    environment::{Environment, Premise},
    normalizer,
};

use crate::{
    context::Context, function::Builder, r#type::LlvmEnumSignature, zst::Zst,
    Model,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum AdtDropKind {
    MembersHaveDrop(Instantiation<Model>),
    DropImplementation(GlobalID),
}

impl<'ctx> Context<'_, 'ctx> {
    /// Gets the drop function for the given type.
    pub fn get_drop(&mut self, ty: Type<Model>) -> Option<FunctionValue<'ctx>> {
        match ty {
            Type::Symbol(symbol) => self.get_adt_drop(symbol),

            Type::Array(array) => self.get_array_drop(array),

            Type::Tuple(tuple) => self.get_tuple_drop(tuple),

            Type::Inference(inference) => match inference {},

            Type::Pointer(_)
            | Type::Reference(_)
            | Type::Primitive(_)
            | Type::Phantom(_) => None,

            Type::MemberSymbol(_) | Type::TraitMember(_) => {
                panic!("unsupported type {ty:?}")
            }

            Type::FunctionSignature(function_signature) => {
                panic!("should not be here {function_signature:?}")
            }

            Type::Error(error) => {
                panic!("error type found {error:?}")
            }

            Type::Parameter(member_id) => {
                panic!("non-monomorphized type found {member_id:?}")
            }
        }
    }

    #[allow(clippy::too_many_lines)]
    fn build_member_drop_adt(
        &mut self,
        inst: &Instantiation<Model>,
        symbol_ty: Symbol<Model>,
    ) -> FunctionValue<'ctx> {
        {
            let symbol_kind = *self.table().get::<SymbolKind>(symbol_ty.id);
            let drop_func_sig = self.context().void_type().fn_type(
                &[self.context().ptr_type(AddressSpace::default()).into()],
                false,
            );

            let function_value = self.module().add_function(
                &format!("memberDrop({})", DisplayObject {
                    table: self.table(),
                    display: &symbol_ty
                }),
                drop_func_sig,
                Some(Linkage::Private),
            );

            let entry_block =
                self.context().append_basic_block(function_value, "entry");
            let builder = self.context().create_builder();
            builder.position_at_end(entry_block);

            let ptr_address =
                function_value.get_first_param().unwrap().into_pointer_value();

            match symbol_kind {
                SymbolKind::Struct => {
                    let fields =
                        self.table().query::<Fields>(symbol_ty.id).unwrap();
                    let llvm_struct_signature = self.get_struct_type(symbol_ty);

                    for field_id in
                        fields.field_declaration_order.iter().copied()
                    {
                        let field_type = self.monomorphize_term(
                            Model::from_default_type(
                                fields.fields[field_id].r#type.clone(),
                            ),
                            inst,
                        );

                        let Some(drop_function) = self.get_drop(field_type)
                        else {
                            // has no drop function
                            continue;
                        };

                        let field_index =
                            llvm_struct_signature.as_ref().ok().map(|x| {
                                x.llvm_field_indices_by_field_id[&field_id]
                            });

                        let elem_ptr = field_index.map_or_else(
                            || {
                                let non_null_dangling_ptr = self
                                    .context()
                                    .ptr_sized_int_type(
                                        self.target_data(),
                                        Some(AddressSpace::default()),
                                    )
                                    .const_int(1, false);

                                builder
                                    .build_int_to_ptr(
                                        non_null_dangling_ptr,
                                        self.context()
                                            .ptr_type(AddressSpace::default()),
                                        &format!("field_{field_id:?}"),
                                    )
                                    .unwrap()
                            },
                            |field_index| {
                                builder
                                    .build_struct_gep(
                                        llvm_struct_signature
                                            .as_ref()
                                            .unwrap()
                                            .llvm_struct_type,
                                        ptr_address,
                                        field_index.try_into().unwrap(),
                                        &format!("field_{field_id:?}"),
                                    )
                                    .unwrap()
                            },
                        );

                        builder
                            .build_call(
                                drop_function,
                                &[elem_ptr.into()],
                                &format!("drop_field_{field_id:?}"),
                            )
                            .unwrap();
                    }

                    builder.build_return(None).unwrap();
                }

                SymbolKind::Enum => {
                    let variants = self.table().get::<Member>(symbol_ty.id);
                    let llvm_enum_signature =
                        self.get_enum_type(symbol_ty.clone());
                    let mut blocks_by_variant_order = Vec::new();

                    for variant_id in variants
                        .values()
                        .copied()
                        .map(|x| GlobalID::new(symbol_ty.id.target_id, x))
                    {
                        let declaration_order = self
                            .table()
                            .get::<VariantDeclarationOrder>(variant_id)
                            .order;

                        let variant =
                            self.table().query::<Variant>(variant_id).unwrap();

                        let Some(ty) = &variant.associated_type else {
                            continue;
                        };

                        let ty = self.monomorphize_term(
                            Model::from_default_type(ty.clone()),
                            inst,
                        );

                        let Some(drop_function) = self.get_drop(ty.clone())
                        else {
                            // has no drop function
                            continue;
                        };

                        let llvm_ty = self.get_type(ty);
                        let block = self.context().append_basic_block(
                            function_value,
                            &format!("variant_{variant_id:?}"),
                        );
                        builder.position_at_end(block);

                        let pointer =
                            match (&*llvm_enum_signature, llvm_ty.is_err()) {
                                (LlvmEnumSignature::TaggedUnion(_), true)
                                | (
                                    LlvmEnumSignature::NullablePointer(_)
                                    | LlvmEnumSignature::Numeric(_)
                                    | LlvmEnumSignature::Zst,
                                    _,
                                ) => {
                                    let non_null_dangling_ptr = self
                                        .context()
                                        .ptr_sized_int_type(
                                            self.target_data(),
                                            Some(AddressSpace::default()),
                                        )
                                        .const_int(1, false);

                                    builder
                                        .build_int_to_ptr(
                                            non_null_dangling_ptr,
                                            self.context().ptr_type(
                                                AddressSpace::default(),
                                            ),
                                            &format!("variant_{variant_id:?}"),
                                        )
                                        .unwrap()
                                }

                                (LlvmEnumSignature::Transparent(_), _) => {
                                    ptr_address
                                }

                                (
                                    LlvmEnumSignature::TaggedUnion(
                                        tagged_union,
                                    ),
                                    false,
                                ) => builder
                                    .build_struct_gep(
                                        tagged_union.llvm_variant_types
                                            [&variant_id],
                                        ptr_address,
                                        1,
                                        &format!("variant_{variant_id:?}"),
                                    )
                                    .unwrap(),
                            };

                        builder
                            .build_call(
                                drop_function,
                                &[pointer.into()],
                                &format!("drop_variant_{variant_id:?}"),
                            )
                            .unwrap();
                        builder.build_return(None).unwrap();

                        blocks_by_variant_order
                            .push((declaration_order, block));
                    }

                    builder.position_at_end(entry_block);

                    match &*llvm_enum_signature {
                        LlvmEnumSignature::Zst => {
                            blocks_by_variant_order.first().map_or_else(
                                || builder.build_return(None).unwrap(),
                                |x| {
                                    builder
                                        .build_unconditional_branch(x.1)
                                        .unwrap()
                                },
                            );
                        }

                        LlvmEnumSignature::NullablePointer(_) => {
                            let is_null = builder
                                .build_is_null(ptr_address, "is_null")
                                .unwrap();

                            let null_return_block =
                                self.context().append_basic_block(
                                    function_value,
                                    "null_return",
                                );

                            builder
                                .build_conditional_branch(
                                    is_null,
                                    null_return_block,
                                    blocks_by_variant_order.first().unwrap().1,
                                )
                                .unwrap();

                            builder.position_at_end(null_return_block);
                            builder.build_return(None).unwrap();
                        }

                        LlvmEnumSignature::Transparent(_) => {
                            builder
                                .build_unconditional_branch(
                                    blocks_by_variant_order.first().unwrap().1,
                                )
                                .unwrap();
                        }

                        llvm_enum_sig @ (LlvmEnumSignature::Numeric(_)
                        | LlvmEnumSignature::TaggedUnion(_)) => {
                            let (numeric_tag_ptr, numeric_ty) =
                                match llvm_enum_sig {
                                    LlvmEnumSignature::Numeric(ty) => {
                                        (ptr_address, *ty)
                                    }
                                    LlvmEnumSignature::TaggedUnion(
                                        tagged_union,
                                    ) => (
                                        builder
                                            .build_struct_gep(
                                                tagged_union
                                                    .most_alignment_type,
                                                ptr_address,
                                                0,
                                                "variant_tag_gep",
                                            )
                                            .unwrap(),
                                        tagged_union.llvm_tag_type,
                                    ),

                                    _ => unreachable!(),
                                };

                            let default_block = self
                                .context()
                                .append_basic_block(function_value, "default");
                            builder.position_at_end(default_block);
                            builder.build_return(None).unwrap();

                            builder.position_at_end(entry_block);

                            builder
                                .build_switch(
                                    builder
                                        .build_load(
                                            numeric_ty,
                                            numeric_tag_ptr,
                                            "load_tag",
                                        )
                                        .unwrap()
                                        .into_int_value(),
                                    default_block,
                                    &blocks_by_variant_order
                                        .into_iter()
                                        .map(|x| {
                                            (
                                                numeric_ty.const_int(
                                                    x.0 as u64, false,
                                                ),
                                                x.1,
                                            )
                                        })
                                        .collect::<Vec<_>>(),
                                )
                                .unwrap();
                        }
                    }
                }

                kind => panic!("unsupported symbol kind {kind:?}"),
            }

            function_value
        }
    }

    /// Gets the drop function for the given ADT symbol.
    #[allow(clippy::too_many_lines)]
    pub fn get_adt_drop(
        &mut self,
        symbol_ty: Symbol<Model>,
    ) -> Option<FunctionValue<'ctx>> {
        if let Some(drop_func_sig) =
            self.function_map_mut().adt_drop.get(&symbol_ty).copied()
        {
            return drop_func_sig;
        }

        let Some(drop_kind) = self.get_adt_drop_kind(&symbol_ty) else {
            self.function_map_mut().adt_drop.insert(symbol_ty, None);
            return None;
        };

        match drop_kind {
            AdtDropKind::MembersHaveDrop(inst) => {
                let function_value =
                    self.build_member_drop_adt(&inst, symbol_ty.clone());

                assert!(self
                    .function_map_mut()
                    .adt_drop
                    .insert(symbol_ty, Some(function_value))
                    .is_none());

                Some(function_value)
            }

            AdtDropKind::DropImplementation(impl_id) => {
                let implementation = GenericArguments::from_default_model(
                    self.table()
                        .query::<Implementation>(impl_id)
                        .unwrap()
                        .generic_arguments
                        .types[0]
                        .as_symbol()
                        .unwrap()
                        .generic_arguments
                        .clone(),
                );

                let impl_func_id = GlobalID::new(
                    impl_id.target_id,
                    *self.table().get::<Member>(impl_id).get("drop").unwrap(),
                );
                let impl_func_generic_params = self
                    .table()
                    .query::<GenericParameters>(impl_func_id)
                    .unwrap();
                let impl_funcs_elided_params = self
                    .table()
                    .query::<ElidedLifetimes>(impl_func_id)
                    .unwrap();

                let env = Environment::new(
                    std::borrow::Cow::Owned(Premise::<Model> {
                        predicates: BTreeSet::new(),
                        query_site: None,
                    }),
                    self.table(),
                    normalizer::NO_OP,
                );

                let mut inst = env
                    .deduce(&implementation, &symbol_ty.generic_arguments)
                    .unwrap()
                    .result
                    .instantiation;

                inst.lifetimes.extend(
                    impl_func_generic_params
                        .lifetime_parameters_as_order()
                        .map(|x| {
                            Lifetime::Parameter(LifetimeParameterID::new(
                                impl_func_id,
                                x.0,
                            ))
                        })
                        .chain(
                            impl_funcs_elided_params
                                .elided_lifetimes
                                .ids()
                                .map(|x| {
                                    Lifetime::Elided(ElidedLifetimeID::new(
                                        impl_func_id,
                                        x,
                                    ))
                                }),
                        )
                        .map(|x| (x, Lifetime::Inference(Erased))),
                );

                let member_drop =
                    self.build_member_drop_adt(&inst, symbol_ty.clone());
                let impl_func = self.get_function(&super::Call {
                    callable_id: impl_func_id,
                    instantiation: inst,
                });

                let func_ty = self.context().void_type().fn_type(
                    &[self.context().ptr_type(AddressSpace::default()).into()],
                    false,
                );
                let function_value = self.module().add_function(
                    &format!("drop({})", DisplayObject {
                        table: self.table(),
                        display: &symbol_ty
                    }),
                    func_ty,
                    Some(Linkage::Private),
                );

                let entry_block =
                    self.context().append_basic_block(function_value, "entry");
                let builder = self.context().create_builder();

                builder.position_at_end(entry_block);

                let ptr_address = function_value
                    .get_first_param()
                    .unwrap()
                    .into_pointer_value();

                builder
                    .build_call(
                        impl_func.llvm_function_value,
                        &[ptr_address.into()],
                        "call_impl",
                    )
                    .unwrap();

                builder
                    .build_call(
                        member_drop,
                        &[ptr_address.into()],
                        "call_member_drop",
                    )
                    .unwrap();
                builder.build_return(None).unwrap();

                assert!(self
                    .function_map_mut()
                    .adt_drop
                    .insert(symbol_ty, Some(function_value))
                    .is_none());

                Some(function_value)
            }
        }
    }

    /// Gets the drop function for the given array value.
    #[allow(clippy::too_many_lines)]
    pub fn get_array_drop(
        &mut self,
        array_ty: Array<Model>,
    ) -> Option<FunctionValue<'ctx>> {
        let array_tup_ty = (
            *array_ty.r#type,
            array_ty.length.into_primitive().unwrap().into_usize().unwrap(),
        );

        if let Some(drop_func_sig) =
            self.function_map_mut().array_drop.get(&array_tup_ty).copied()
        {
            return drop_func_sig;
        }

        let has_drop = self.has_drop(&array_tup_ty.0);

        if !has_drop {
            self.function_map_mut().array_drop.insert(array_tup_ty, None);
            return None;
        }

        let drop_func_sig = self.context().void_type().fn_type(
            &[self.context().ptr_type(AddressSpace::default()).into()],
            false,
        );

        let function_value = self.module().add_function(
            &format!("drop({})", DisplayObject {
                table: self.table(),
                display: &Array {
                    length: Constant::Primitive(
                        pernixc_term::constant::Primitive::Usize(
                            array_tup_ty.1
                        )
                    ),
                    r#type: Box::new(array_tup_ty.0.clone())
                }
            }),
            drop_func_sig,
            Some(Linkage::Private),
        );

        assert!(self
            .function_map_mut()
            .array_drop
            .insert(array_tup_ty.clone(), Some(function_value))
            .is_none());

        let element_llvm_ty = self.get_type(array_tup_ty.0.clone());
        let array_llvm_ty = element_llvm_ty
            .map(|x| x.array_type(array_tup_ty.1.try_into().unwrap()));

        let entry_block =
            self.context().append_basic_block(function_value, "entry");
        let builder = self.context().create_builder();

        builder.position_at_end(entry_block);

        let counter_ty = self.context().ptr_sized_int_type(
            self.target_data(),
            Some(AddressSpace::default()),
        );
        let counter = builder.build_alloca(counter_ty, "counter").unwrap();

        let body_block =
            self.context().append_basic_block(function_value, "body");
        let header_block =
            self.context().append_basic_block(function_value, "header");
        let exit_block =
            self.context().append_basic_block(function_value, "exit");

        builder.build_store(counter, counter_ty.const_zero()).unwrap();
        builder.build_unconditional_branch(header_block).unwrap();

        builder.position_at_end(header_block);

        let counter_value = builder
            .build_load(counter_ty, counter, "load_counter_value")
            .unwrap()
            .into_int_value();
        let is_end = builder
            .build_int_compare(
                inkwell::IntPredicate::ULT,
                counter_value,
                counter_ty.const_int(array_tup_ty.1, false),
                "is_end",
            )
            .unwrap();

        builder
            .build_conditional_branch(is_end, body_block, exit_block)
            .unwrap();

        builder.position_at_end(body_block);

        let element_ptr = array_llvm_ty.map_or_else(
            |Zst| {
                let non_null_dangling_ptr = self.context().ptr_sized_int_type(
                    self.target_data(),
                    Some(AddressSpace::default()),
                );

                builder
                    .build_int_to_ptr(
                        non_null_dangling_ptr.const_int(1, false),
                        self.context().ptr_type(AddressSpace::default()),
                        "element_ptr",
                    )
                    .unwrap()
            },
            |array_llvm_ty| unsafe {
                builder
                    .build_gep(
                        array_llvm_ty,
                        function_value
                            .get_first_param()
                            .unwrap()
                            .into_pointer_value(),
                        &[counter_ty.const_zero(), counter_value],
                        "element_ptr",
                    )
                    .unwrap()
            },
        );

        let drop_function =
            self.get_drop(array_tup_ty.0).expect("has drop already");

        builder
            .build_call(drop_function, &[element_ptr.into()], "call_drop")
            .unwrap();

        let next_counter_value = builder
            .build_int_add(
                counter_value,
                counter_ty.const_int(1, false),
                "next_counter_value",
            )
            .unwrap();

        builder.build_store(counter, next_counter_value).unwrap();

        builder.build_unconditional_branch(header_block).unwrap();

        builder.position_at_end(exit_block);

        builder.build_return(None).unwrap();

        Some(function_value)
    }

    /// Gets the drop function for the tuple type.
    pub fn get_tuple_drop(
        &mut self,
        tuple_ty: Tuple<Type<Model>>,
    ) -> Option<FunctionValue<'ctx>> {
        if let Some(drop_func_sig) =
            self.function_map_mut().tuple_drop.get(&tuple_ty).copied()
        {
            return drop_func_sig;
        }

        let has_drop = tuple_ty
            .elements
            .iter()
            .any(|element| self.has_drop(&element.term));

        if !has_drop {
            self.function_map_mut().tuple_drop.insert(tuple_ty, None);
            return None;
        }

        let drop_func_sig = self.context().void_type().fn_type(
            &[self.context().ptr_type(AddressSpace::default()).into()],
            false,
        );

        let function_value = self.module().add_function(
            &format!("drop({})", DisplayObject {
                table: self.table(),
                display: &tuple_ty
            }),
            drop_func_sig,
            Some(Linkage::Private),
        );

        self.function_map_mut()
            .tuple_drop
            .insert(tuple_ty.clone(), Some(function_value));

        let entry_block =
            self.context().append_basic_block(function_value, "entry");
        let builder = self.context().create_builder();
        builder.position_at_end(entry_block);

        let llvm_tuple_ty = self.get_tuple_type(tuple_ty.clone());
        let ptr_address =
            function_value.get_first_param().unwrap().into_pointer_value();

        for (index, elem) in tuple_ty.elements.into_iter().enumerate() {
            let Some(drop_function) = self.get_drop(elem.term) else {
                // has no drop function
                continue;
            };

            let field_index = llvm_tuple_ty
                .as_ref()
                .ok()
                .map(|x| x.llvm_field_indices_by_tuple_idnex[&index]);

            let elem_ptr = field_index.map_or_else(
                || {
                    let non_null_dangling_ptr = self
                        .context()
                        .ptr_sized_int_type(
                            self.target_data(),
                            Some(AddressSpace::default()),
                        )
                        .const_int(1, false);

                    builder
                        .build_int_to_ptr(
                            non_null_dangling_ptr,
                            self.context().ptr_type(AddressSpace::default()),
                            &format!("field_{index}"),
                        )
                        .unwrap()
                },
                |field_index| {
                    builder
                        .build_struct_gep(
                            llvm_tuple_ty.as_ref().unwrap().llvm_tuple_type,
                            ptr_address,
                            field_index.try_into().unwrap(),
                            &format!("field_{index}"),
                        )
                        .unwrap()
                },
            );

            builder
                .build_call(
                    drop_function,
                    &[elem_ptr.into()],
                    &format!("drop_elem_{index}"),
                )
                .unwrap();
        }

        builder.build_return(None).unwrap();

        Some(function_value)
    }

    fn get_adt_drop_kind(&self, symbol: &Symbol<Model>) -> Option<AdtDropKind> {
        let current_symbol_id = symbol.id;

        // check for the explict drop implementation
        let drop_trait_id =
            self.table().get_by_qualified_name(["core", "Drop"]).unwrap();

        let implemented = self.table().get::<Implemented>(drop_trait_id);

        let implementation_id = implemented.iter().copied().find(|x| {
            let implementation =
                self.table().query::<Implementation>(*x).unwrap();

            // drop only allows struct/enum to be implemented.
            let symbol =
                implementation.generic_arguments.types[0].as_symbol().unwrap();

            symbol.id == current_symbol_id
        });

        if let Some(impl_id) = implementation_id {
            return Some(AdtDropKind::DropImplementation(impl_id));
        }

        let inst = Instantiation::from_generic_arguments(
            symbol.generic_arguments.clone(),
            current_symbol_id,
            &self
                .table()
                .query::<GenericParameters>(current_symbol_id)
                .unwrap(),
        )
        .unwrap();

        let symbol_kind = *self.table().get::<SymbolKind>(current_symbol_id);

        match symbol_kind {
            SymbolKind::Struct => {
                for term in self
                    .table()
                    .query::<Fields>(current_symbol_id)
                    .unwrap()
                    .fields
                    .items()
                    .map(|x| {
                        self.monomorphize_term(
                            Model::from_default_type(x.r#type.clone()),
                            &inst,
                        )
                    })
                {
                    if self.has_drop(&term) {
                        return Some(AdtDropKind::MembersHaveDrop(inst));
                    }
                }

                None
            }

            SymbolKind::Enum => {
                for variant in self
                    .table()
                    .get::<Member>(current_symbol_id)
                    .values()
                    .copied()
                    .map(|x| GlobalID::new(current_symbol_id.target_id, x))
                    .filter_map(|x| {
                        let variant = self.table().query::<Variant>(x).unwrap();

                        let Some(ty) = &variant.associated_type else {
                            return None;
                        };

                        Some(self.monomorphize_term(
                            Model::from_default_type(ty.clone()),
                            &inst,
                        ))
                    })
                {
                    if self.has_drop(&variant) {
                        return Some(AdtDropKind::MembersHaveDrop(inst));
                    }
                }

                None
            }
            kind => panic!("unsupported symbol kind {kind:?}"),
        }
    }

    /// Checks if the type has a drop implementation.
    #[allow(clippy::too_many_lines)]
    pub fn has_drop(&self, ty: &Type<Model>) -> bool {
        match ty {
            Type::Symbol(symbol) => self.get_adt_drop_kind(symbol).is_some(),

            Type::Array(array) => self.has_drop(&array.r#type),

            Type::Tuple(tuple) => {
                for element in &tuple.elements {
                    assert!(!element.is_unpacked, "found unpacked tuple");

                    if self.has_drop(&element.term) {
                        return true;
                    }
                }

                false
            }

            Type::Inference(inference) => match *inference {},

            Type::Pointer(_)
            | Type::Reference(_)
            | Type::Primitive(_)
            | Type::Phantom(_) => false,

            Type::MemberSymbol(_) | Type::TraitMember(_) => {
                panic!("unsupported type {ty:?}")
            }

            Type::FunctionSignature(function_signature) => {
                panic!("should not be here {function_signature:?}")
            }

            Type::Error(error) => {
                panic!("error type found {error:?}")
            }

            Type::Parameter(member_id) => {
                panic!("non-monomorphized type found {member_id:?}")
            }
        }
    }
}

impl<'ctx> Builder<'_, 'ctx, '_, '_> {
    pub fn build_drop(&mut self, address: PointerValue<'ctx>, ty: Type<Model>) {
        let Some(drop) = self.context.get_drop(ty) else {
            return;
        };

        self.inkwell_builder
            .build_call(drop, &[address.into()], "call_drop")
            .unwrap();
    }
}
