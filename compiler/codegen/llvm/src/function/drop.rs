use std::collections::BTreeSet;

use inkwell::{
    module::Linkage,
    types::BasicType,
    values::{FunctionValue, PointerValue},
    AddressSpace,
};
use pernixc_semantic_element::{
    elided_lifetime::get_elided_lifetimes, fields::get_fields,
    implemented::get_implemented,
    implements_arguments::get_implements_argument,
    variant::get_variant_associated_type,
};
use pernixc_symbol::{
    kind::{get_kind, Kind},
    member::get_members,
    name::get_by_qualified_name,
    variant_declaration_order::get_variant_declaration_order,
};
use pernixc_target::{Global, TargetID};
use pernixc_term::{
    constant::Constant,
    display::Display,
    generic_arguments::Symbol,
    generic_parameters::{get_generic_parameters, LifetimeParameterID},
    instantiation::Instantiation,
    lifetime::{ElidedLifetimeID, Lifetime},
    r#type::{Array, Type},
    tuple::Tuple,
};
use pernixc_type_system::{
    environment::{Environment, Premise},
    normalizer,
};

use crate::{
    context::Context, function::Builder, r#type::LlvmEnumSignature, zst::Zst,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum AdtDropKind {
    MembersHaveDrop(Instantiation),
    DropImplementation(Global<pernixc_symbol::ID>),
}

impl<'ctx> Context<'_, 'ctx> {
    /// Gets the drop function for the given type.
    pub async fn get_drop(&mut self, ty: Type) -> Option<FunctionValue<'ctx>> {
        match ty {
            Type::Symbol(symbol) => self.get_adt_drop(symbol).await,

            Type::Array(array) => self.get_array_drop(array).await,

            Type::Tuple(tuple) => self.get_tuple_drop(tuple).await,

            Type::Inference(inference) => {
                unreachable!("should not be here {inference:?}")
            }

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

    fn create_drop_function_value(&self, name: &str) -> FunctionValue<'ctx> {
        let drop_func_sig = self.context().void_type().fn_type(
            &[self.context().ptr_type(AddressSpace::default()).into()],
            false,
        );

        self.module().add_function(name, drop_func_sig, Some(Linkage::Private))
    }

    #[allow(clippy::too_many_lines)]
    async fn build_member_drop_adt(
        &mut self,
        inst: &Instantiation,
        symbol_ty: Symbol,
        function_value: FunctionValue<'ctx>,
    ) -> FunctionValue<'ctx> {
        let symbol_kind = self.engine().get_kind(symbol_ty.id).await;

        let entry_block =
            self.context().append_basic_block(function_value, "entry");
        let builder = self.context().create_builder();
        builder.position_at_end(entry_block);

        let ptr_address =
            function_value.get_first_param().unwrap().into_pointer_value();

        match symbol_kind {
            Kind::Struct => {
                let fields =
                    self.engine().get_fields(symbol_ty.id).await.unwrap();
                let llvm_struct_signature =
                    self.get_struct_type(symbol_ty).await;

                for field_id in fields.field_declaration_order.iter().copied() {
                    let field_type = self
                        .monomorphize_term(
                            fields.fields[field_id].r#type.clone(),
                            inst,
                        )
                        .await;

                    let Some(drop_function) = self.get_drop(field_type).await
                    else {
                        // has no drop function
                        continue;
                    };

                    let field_index = llvm_struct_signature
                        .as_ref()
                        .ok()
                        .map(|x| x.llvm_field_indices_by_field_id[&field_id]);

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

            Kind::Enum => {
                let variants = self.engine().get_members(symbol_ty.id).await;
                let llvm_enum_signature =
                    self.get_enum_type(symbol_ty.clone()).await;
                let mut blocks_by_variant_order = Vec::new();

                for variant_id in variants
                    .member_ids_by_name
                    .values()
                    .copied()
                    .map(|x| Global::new(symbol_ty.id.target_id, x))
                {
                    let declaration_order = self
                        .engine()
                        .get_variant_declaration_order(variant_id)
                        .await;

                    let variant = self
                        .engine()
                        .get_variant_associated_type(variant_id)
                        .await
                        .unwrap();

                    let Some(ty) = variant else {
                        continue;
                    };

                    let ty = self.monomorphize_term((*ty).clone(), inst).await;

                    let Some(drop_function) = self.get_drop(ty.clone()).await
                    else {
                        // has no drop function
                        continue;
                    };

                    let llvm_ty = self.get_type(ty).await;
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
                                        self.context()
                                            .ptr_type(AddressSpace::default()),
                                        &format!("variant_{variant_id:?}"),
                                    )
                                    .unwrap()
                            }

                            (LlvmEnumSignature::Transparent(_), _) => {
                                ptr_address
                            }

                            (
                                LlvmEnumSignature::TaggedUnion(tagged_union),
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

                    blocks_by_variant_order.push((declaration_order, block));
                }

                builder.position_at_end(entry_block);

                match &*llvm_enum_signature {
                    LlvmEnumSignature::Zst => {
                        blocks_by_variant_order.first().map_or_else(
                            || builder.build_return(None).unwrap(),
                            |x| {
                                builder.build_unconditional_branch(x.1).unwrap()
                            },
                        );
                    }

                    LlvmEnumSignature::NullablePointer(_) => {
                        let is_null = builder
                            .build_is_null(ptr_address, "is_null")
                            .unwrap();

                        let null_return_block = self
                            .context()
                            .append_basic_block(function_value, "null_return");

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
                        let (numeric_tag_ptr, numeric_ty) = match llvm_enum_sig
                        {
                            LlvmEnumSignature::Numeric(ty) => {
                                (ptr_address, *ty)
                            }
                            LlvmEnumSignature::TaggedUnion(tagged_union) => (
                                builder
                                    .build_struct_gep(
                                        tagged_union.most_alignment_type,
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
                                            numeric_ty
                                                .const_int(x.0 as u64, false),
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

    /// Gets the drop function for the given ADT symbol.
    #[allow(clippy::too_many_lines)]
    pub async fn get_adt_drop(
        &mut self,
        symbol_ty: Symbol,
    ) -> Option<FunctionValue<'ctx>> {
        if let Some(drop_func_sig) =
            self.function_map_mut().adt_drop.get(&symbol_ty).copied()
        {
            return drop_func_sig;
        }

        let Some(drop_kind) =
            Box::pin(self.get_adt_drop_kind(&symbol_ty)).await
        else {
            self.function_map_mut().adt_drop.insert(symbol_ty, None);
            return None;
        };

        match drop_kind {
            AdtDropKind::MembersHaveDrop(inst) => {
                let function_value = self.create_drop_function_value(&format!(
                    "memberDrop({})",
                    symbol_ty.write_to_string(self.engine()).await.unwrap()
                ));

                assert!(self
                    .function_map_mut()
                    .adt_drop
                    .insert(symbol_ty.clone(), Some(function_value))
                    .is_none());

                Some(
                    Box::pin(self.build_member_drop_adt(
                        &inst,
                        symbol_ty,
                        function_value,
                    ))
                    .await,
                )
            }

            AdtDropKind::DropImplementation(impl_id) => {
                let implementation = self
                    .engine()
                    .get_implements_argument(impl_id)
                    .await
                    .unwrap()
                    .unwrap()
                    .types[0]
                    .as_symbol()
                    .unwrap()
                    .generic_arguments
                    .clone();

                let impl_func_id = Global::new(
                    impl_id.target_id,
                    self.engine()
                        .get_members(impl_id)
                        .await
                        .member_ids_by_name
                        .get("drop")
                        .copied()
                        .unwrap(),
                );
                let impl_func_generic_params = self
                    .engine()
                    .get_generic_parameters(impl_func_id)
                    .await
                    .unwrap();
                let impl_funcs_elided_params = self
                    .engine()
                    .get_elided_lifetimes(impl_func_id)
                    .await
                    .unwrap();

                let env = Environment::new(
                    std::borrow::Cow::Owned(Premise {
                        predicates: BTreeSet::new(),
                        query_site: None,
                    }),
                    std::borrow::Cow::Borrowed(self.engine()),
                    normalizer::NO_OP,
                );

                let mut inst = env
                    .deduce(&implementation, &symbol_ty.generic_arguments)
                    .await
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
                        .chain(impl_funcs_elided_params.ids().map(|x| {
                            Lifetime::Elided(ElidedLifetimeID::new(
                                impl_func_id,
                                x,
                            ))
                        }))
                        .map(|x| (x, Lifetime::Erased)),
                );

                // the drop function that includes both custom implements drop
                // and automatically generated adt member drop
                let mut symbol_ty_string = String::new();

                symbol_ty
                    .write_async(self.engine(), &mut symbol_ty_string)
                    .await
                    .unwrap();

                let main_drop_function_value = self.create_drop_function_value(
                    &format!("drop({})", symbol_ty_string),
                );

                let member_drop_function_value = self
                    .create_drop_function_value(&format!(
                        "memberDrop({})",
                        symbol_ty_string
                    ));

                assert!(self
                    .function_map_mut()
                    .adt_drop
                    .insert(symbol_ty.clone(), Some(main_drop_function_value))
                    .is_none());

                let member_drop = Box::pin(
                    self.build_member_drop_adt(
                        &Instantiation::from_generic_arguments(
                            symbol_ty.generic_arguments.clone(),
                            symbol_ty.id,
                            &self
                                .engine()
                                .get_generic_parameters(symbol_ty.id)
                                .await
                                .unwrap(),
                        )
                        .unwrap(),
                        symbol_ty,
                        member_drop_function_value,
                    ),
                )
                .await;

                let impl_func = self
                    .get_function(&super::Call {
                        callable_id: impl_func_id,
                        instantiation: inst,
                    })
                    .await;

                let entry_block = self
                    .context()
                    .append_basic_block(main_drop_function_value, "entry");
                let builder = self.context().create_builder();

                builder.position_at_end(entry_block);

                let ptr_address = main_drop_function_value
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

                Some(main_drop_function_value)
            }
        }
    }

    /// Gets the drop function for the given array value.
    #[allow(clippy::too_many_lines)]
    pub async fn get_array_drop(
        &mut self,
        array_ty: Array,
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

        let has_drop = Box::pin(self.has_drop(&array_tup_ty.0)).await;

        if !has_drop {
            self.function_map_mut().array_drop.insert(array_tup_ty, None);
            return None;
        }

        let drop_func_sig = self.context().void_type().fn_type(
            &[self.context().ptr_type(AddressSpace::default()).into()],
            false,
        );

        let mut array_string = String::new();
        Type::Array(Array {
            length: Constant::Primitive(
                pernixc_term::constant::Primitive::Usize(array_tup_ty.1),
            ),
            r#type: Box::new(array_tup_ty.0.clone()),
        })
        .write_async(self.engine(), &mut array_string)
        .await
        .unwrap();

        let function_value = self.module().add_function(
            &format!("drop({})", array_string),
            drop_func_sig,
            Some(Linkage::Private),
        );

        assert!(self
            .function_map_mut()
            .array_drop
            .insert(array_tup_ty.clone(), Some(function_value))
            .is_none());

        let element_llvm_ty = self.get_type(array_tup_ty.0.clone()).await;
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

        let drop_function = Box::pin(self.get_drop(array_tup_ty.0).await)
            .expect("has drop already");

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
    pub async fn get_tuple_drop(
        &mut self,
        tuple_ty: Tuple<Type>,
    ) -> Option<FunctionValue<'ctx>> {
        if let Some(drop_func_sig) =
            self.function_map_mut().tuple_drop.get(&tuple_ty).copied()
        {
            return drop_func_sig;
        }

        let has_drop = 'result: {
            for element in &tuple_ty.elements {
                assert!(!element.is_unpacked, "found unpacked tuple");

                if Box::pin(self.has_drop(&element.term)).await {
                    break 'result true;
                }
            }

            false
        };

        if !has_drop {
            self.function_map_mut().tuple_drop.insert(tuple_ty, None);
            return None;
        }

        let drop_func_sig = self.context().void_type().fn_type(
            &[self.context().ptr_type(AddressSpace::default()).into()],
            false,
        );

        let function_value = self.module().add_function(
            &format!(
                "drop({})",
                tuple_ty.write_to_string(self.engine()).await.unwrap()
            ),
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

        let llvm_tuple_ty = self.get_tuple_type(tuple_ty.clone()).await;
        let ptr_address =
            function_value.get_first_param().unwrap().into_pointer_value();

        for (index, elem) in tuple_ty.elements.into_iter().enumerate() {
            let Some(drop_function) = Box::pin(self.get_drop(elem.term)).await
            else {
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

    async fn get_adt_drop_kind(&self, symbol: &Symbol) -> Option<AdtDropKind> {
        // check if the symbol is a NoDrop struct
        if symbol.id.target_id == TargetID::CORE
            && Some(symbol.id)
                == self.engine().get_by_qualified_name(["core", "NoDrop"]).await
        {
            return None;
        }

        let current_symbol_id = symbol.id;

        // check for the explict drop implementation
        let drop_trait_id = self
            .engine()
            .get_by_qualified_name(["core", "Drop"])
            .await
            .unwrap();

        let implemented =
            self.engine().get_implemented(drop_trait_id).await.unwrap();

        let implementation_id = 'result: {
            for impl_id in implemented.iter().copied() {
                let Some(implementation) = self
                    .engine()
                    .get_implements_argument(impl_id)
                    .await
                    .unwrap()
                else {
                    continue;
                };

                // drop only allows struct/enum to be implemented.
                let symbol = implementation.types[0].as_symbol().unwrap();

                if symbol.id == current_symbol_id {
                    break 'result Some(impl_id);
                }
            }

            None
        };

        if let Some(impl_id) = implementation_id {
            return Some(AdtDropKind::DropImplementation(impl_id));
        }

        let inst = Instantiation::from_generic_arguments(
            symbol.generic_arguments.clone(),
            current_symbol_id,
            &self
                .engine()
                .get_generic_parameters(current_symbol_id)
                .await
                .unwrap(),
        )
        .unwrap();

        let symbol_kind = self.engine().get_kind(current_symbol_id).await;

        match symbol_kind {
            Kind::Struct => {
                for field in self
                    .engine()
                    .get_fields(current_symbol_id)
                    .await
                    .unwrap()
                    .fields
                    .items()
                {
                    let ty = self
                        .monomorphize_term(field.r#type.clone(), &inst)
                        .await;

                    if Box::pin(self.has_drop(&ty)).await {
                        return Some(AdtDropKind::MembersHaveDrop(inst));
                    }
                }

                None
            }

            Kind::Enum => {
                for variant in self
                    .engine()
                    .get_members(current_symbol_id)
                    .await
                    .member_ids_by_name
                    .values()
                    .copied()
                    .map(|x| Global::new(current_symbol_id.target_id, x))
                {
                    let variant = self
                        .engine()
                        .get_variant_associated_type(variant)
                        .await
                        .unwrap();

                    let Some(ty) = variant else {
                        continue;
                    };

                    let ty = self.monomorphize_term((*ty).clone(), &inst).await;

                    if Box::pin(self.has_drop(&ty)).await {
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
    pub async fn has_drop(&self, ty: &Type) -> bool {
        match ty {
            Type::Symbol(symbol) => {
                self.get_adt_drop_kind(symbol).await.is_some()
            }

            Type::Array(array) => Box::pin(self.has_drop(&array.r#type)).await,

            Type::Tuple(tuple) => {
                for element in &tuple.elements {
                    assert!(!element.is_unpacked, "found unpacked tuple");

                    if Box::pin(self.has_drop(&element.term)).await {
                        return true;
                    }
                }

                false
            }

            Type::Inference(inference) => {
                unreachable!("should not be here: {inference:?}")
            }

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
    pub async fn build_drop(&mut self, address: PointerValue<'ctx>, ty: Type) {
        let Some(drop) = self.context.get_drop(ty).await else {
            return;
        };

        self.inkwell_builder
            .build_call(drop, &[address.into()], "call_drop")
            .unwrap();
    }
}
