use std::{cmp::Ordering, ops::Deref};

use inkwell::{
    attributes::AttributeLoc,
    types::BasicTypeEnum,
    values::{BasicMetadataValueEnum, BasicValueEnum, PointerValue},
    AddressSpace, FloatPredicate, IntPredicate,
};
use pernixc_arena::ID;
use pernixc_component::fields::Fields;
use pernixc_ir::{
    control_flow_graph::Block,
    instruction::{
        Instruction, Jump, RegisterAssignment, SwitchValue, Terminator,
        TuplePack,
    },
    model::Erased,
    value::{
        register::{
            self, Assignment, BinaryOperator, Register, RelationalOperator,
        },
        Value,
    },
};
use pernixc_table::{
    component::{Member, Name, Parent, SymbolKind, VariantDeclarationOrder},
    GlobalID,
};
use pernixc_term::{
    constant::Constant,
    elided_lifetimes::{ElidedLifetimeID, ElidedLifetimes},
    generic_parameter::{
        ConstantParameterID, GenericParameters, LifetimeParameterID,
        TypeParameterID,
    },
    instantiation,
    lifetime::Lifetime,
    r#type::{Primitive, Type},
};

use super::{
    Builder, Call, Error, LlvmAddress, LlvmFunctionSignature, LlvmValue,
};
use crate::{
    function::ReturnType,
    r#type::{IsAggregateTypeExt, LlvmEnumSignature},
    Model,
};

impl<'ctx> Builder<'_, 'ctx, '_, '_> {
    fn build_memcpy(
        &mut self,
        dst: PointerValue<'ctx>,
        src: PointerValue<'ctx>,
        ty: BasicTypeEnum<'ctx>,
    ) {
        let alignment = self.context.target_data().get_abi_alignment(&ty);
        let size = self.context.target_data().get_abi_size(&ty);

        let int_ptr_size = self
            .context
            .context()
            .ptr_sized_int_type(self.context.target_data(), None);

        self.inkwell_builder
            .build_memcpy(
                dst,
                alignment,
                src,
                alignment,
                int_ptr_size.const_int(size, false),
            )
            .unwrap();
    }

    fn build_store(
        &mut self,
        store: &pernixc_ir::instruction::Store<Model>,
    ) -> Result<(), Error> {
        let dest_value = self.get_value(&store.value)?;
        let store_address = self.get_address(&store.address)?;

        let (Some(dest_value), Some(store_address)) =
            (dest_value, store_address)
        else {
            return Ok(());
        };

        match dest_value {
            LlvmValue::Scalar(basic_value_enum) => {
                self.inkwell_builder
                    .build_store(store_address.address, basic_value_enum)
                    .unwrap();
            }
            LlvmValue::TmpAggegate(dest_address) => {
                self.build_memcpy(
                    store_address.address,
                    dest_address.address,
                    dest_address.r#type,
                );
            }
        };

        Ok(())
    }

    fn handle_load(
        &mut self,
        load: &register::Load<Model>,
        reg_id: ID<Register<Model>>,
    ) -> Result<Option<LlvmValue<'ctx>>, Error> {
        let Some(ptr) = self.get_address(&load.address)? else {
            return Ok(None);
        };

        if ptr.r#type.is_aggregate() {
            let tmp = self.create_aggregate_temporary(ptr.r#type, reg_id);

            self.build_memcpy(tmp.address, ptr.address, ptr.r#type);

            Ok(Some(LlvmValue::TmpAggegate(LlvmAddress::new(
                tmp.address,
                ptr.r#type,
            ))))
        } else {
            Ok(Some(LlvmValue::Scalar(
                self.inkwell_builder
                    .build_load(
                        ptr.r#type,
                        ptr.address,
                        &format!("load_{reg_id:?}"),
                    )
                    .unwrap(),
            )))
        }
    }

    fn handle_struct_lit(
        &mut self,
        struct_lit: &register::Struct<Model>,
        reg_id: ID<Register<Model>>,
    ) -> Result<Option<LlvmValue<'ctx>>, Error> {
        let fields =
            self.context.table().query::<Fields>(struct_lit.struct_id).unwrap();
        let values = fields
            .field_declaration_order
            .iter()
            .copied()
            .map(|x| (x, &struct_lit.initializers_by_field_id[&x]))
            .map(|(id, val)| Ok((id, self.get_value(val)?)))
            .collect::<Result<Vec<_>, Error>>()?;

        let Ok(struct_ty) = self.type_of_register(reg_id) else {
            return Ok(None);
        };

        let tmp = self.create_aggregate_temporary(struct_ty, reg_id);

        for (index, (field_id, val)) in
            values.into_iter().filter_map(|x| x.1.map(|y| (x.0, y))).enumerate()
        {
            let pointer_value = self
                .inkwell_builder
                .build_struct_gep(
                    struct_ty,
                    tmp.address,
                    index.try_into().unwrap(),
                    &format!("init_struct_{reg_id:?}_field_{field_id:?}_gep"),
                )
                .unwrap();

            match val {
                LlvmValue::Scalar(basic_value_enum) => {
                    self.inkwell_builder
                        .build_store(pointer_value, basic_value_enum)
                        .unwrap();
                }
                LlvmValue::TmpAggegate(llvm_address) => {
                    self.build_memcpy(
                        pointer_value,
                        llvm_address.address,
                        llvm_address.r#type,
                    );
                }
            }
        }

        Ok(Some(LlvmValue::TmpAggegate(LlvmAddress::new(
            tmp.address,
            struct_ty,
        ))))
    }

    #[allow(clippy::too_many_lines)]
    fn create_function_call(
        &mut self,
        llvm_function_signature: &LlvmFunctionSignature<'ctx>,
        arguments: &[Value<Model>],
        reg_id: ID<Register<Model>>,
    ) -> Result<Option<LlvmValue<'ctx>>, Error> {
        let mut args = Vec::with_capacity(arguments.len());
        for arg in arguments {
            let Some(val) = self.get_value(arg)? else {
                continue;
            };

            args.push(val);
        }

        let mut sret = match &llvm_function_signature.return_type {
            ReturnType::Scalar(_) | ReturnType::Void => None,
            ReturnType::Sret(basic_type_enum) => {
                Some(self.create_aggregate_temporary(*basic_type_enum, reg_id))
            }
        };

        let mut llvm_arguments: Vec<BasicMetadataValueEnum> =
            Vec::with_capacity(args.len() + usize::from(sret.is_some()));

        // add `sret` to the first argument
        if let Some(sret) = sret.as_mut() {
            llvm_arguments.push(sret.address.into());
        }

        llvm_arguments.extend(args.iter().copied().map(|x| match x {
            LlvmValue::Scalar(basic_value_enum) => {
                BasicMetadataValueEnum::from(basic_value_enum)
            }
            LlvmValue::TmpAggegate(llvm_address) => llvm_address.address.into(),
        }));

        let call = self
            .inkwell_builder
            .build_call(
                llvm_function_signature.llvm_function_value,
                &llvm_arguments,
                &format!("call_{reg_id:?}"),
            )
            .unwrap();

        // add sret attribute
        if let ReturnType::Sret(sret_ty) = &llvm_function_signature.return_type
        {
            call.add_attribute(
                AttributeLoc::Param(0),
                self.context.create_type_attribute("sret", *sret_ty),
            );
        }

        // add required byval attributes
        for (mut index, arg) in args.iter().enumerate() {
            index += usize::from(sret.is_some());

            if let LlvmValue::TmpAggegate(temp_arg) = arg {
                call.add_attribute(
                    AttributeLoc::Param(index.try_into().unwrap()),
                    self.context
                        .create_type_attribute("byval", temp_arg.r#type),
                );
            }
        }

        match &llvm_function_signature.return_type {
            ReturnType::Void => Ok(None),
            ReturnType::Scalar(_) => Ok(Some(LlvmValue::Scalar(
                call.try_as_basic_value().left().unwrap(),
            ))),
            ReturnType::Sret(basic_type_enum) => {
                Ok(Some(LlvmValue::TmpAggegate(LlvmAddress::new(
                    sret.unwrap().address,
                    *basic_type_enum,
                ))))
            }
        }
    }

    #[allow(clippy::too_many_lines)]
    fn handle_function_call(
        &mut self,
        function_call: &register::FunctionCall<Model>,
        reg_id: ID<Register<Model>>,
    ) -> Result<Option<LlvmValue<'ctx>>, Error> {
        let symbol_kind =
            *self.context.table().get::<SymbolKind>(function_call.callable_id);

        match symbol_kind {
            SymbolKind::ExternFunction => {
                let llvm_function =
                    self.context.get_extern_function(function_call.callable_id);

                self.create_function_call(
                    &llvm_function,
                    &function_call.arguments,
                    reg_id,
                )
            }
            SymbolKind::Function
            | SymbolKind::AdtImplementationFunction
            | SymbolKind::TraitImplementationFunction => {
                let mut calling_inst = function_call.instantiation.clone();

                calling_inst.lifetimes.values_mut().for_each(|term| {
                    instantiation::instantiate(term, self.instantiation);
                });
                calling_inst.types.values_mut().for_each(|term| {
                    instantiation::instantiate(term, self.instantiation);
                });
                calling_inst.constants.values_mut().for_each(|term| {
                    instantiation::instantiate(term, self.instantiation);
                });

                self.context.normalize_instantiation(&mut calling_inst);

                let llvm_function = self.context.get_function(&Call {
                    callable_id: function_call.callable_id,
                    instantiation: calling_inst,
                });

                self.create_function_call(
                    &llvm_function,
                    &function_call.arguments,
                    reg_id,
                )
            }

            SymbolKind::TraitFunction => {
                let mut calling_inst = function_call.instantiation.clone();

                calling_inst.lifetimes.values_mut().for_each(|term| {
                    instantiation::instantiate(term, self.instantiation);
                });
                calling_inst.types.values_mut().for_each(|term| {
                    instantiation::instantiate(term, self.instantiation);
                });
                calling_inst.constants.values_mut().for_each(|term| {
                    instantiation::instantiate(term, self.instantiation);
                });

                self.context.normalize_instantiation(&mut calling_inst);

                // get the parent trait to search for the implc
                let parent_trait_id = GlobalID::new(
                    function_call.callable_id.target_id,
                    self.context
                        .table()
                        .get::<Parent>(function_call.callable_id)
                        .parent
                        .unwrap(),
                );

                let trait_generic_params = self
                    .context
                    .table()
                    .query::<GenericParameters>(parent_trait_id)
                    .unwrap();
                let trait_func_generic_params = self
                    .context
                    .table()
                    .query::<GenericParameters>(function_call.callable_id)
                    .unwrap();

                let trait_generic_args = calling_inst
                    .create_generic_arguments(
                        parent_trait_id,
                        &trait_generic_params,
                    )
                    .unwrap();

                // resolve for the trait impl
                let resolution = self
                    .environment
                    .resolve_implementation(
                        parent_trait_id,
                        &trait_generic_args,
                    )
                    .unwrap();

                let trait_impl_id = resolution.result.id;
                let mut new_inst = resolution.result.instantiation;

                let trait_function_name =
                    self.context.table().get::<Name>(function_call.callable_id);

                let trait_impl_fun_id = GlobalID::new(
                    trait_impl_id.target_id,
                    self.context.table().get::<Member>(trait_impl_id)
                        [&trait_function_name.0],
                );

                let trait_impl_fun_generic_params = self
                    .context
                    .table()
                    .query::<GenericParameters>(trait_impl_fun_id)
                    .unwrap();
                let trait_impl_fun_elided_lts = self
                    .context
                    .table()
                    .query::<ElidedLifetimes>(trait_impl_fun_id)
                    .unwrap();

                // populate the new_inst will the generic arguments of the trait
                // func args
                for lt in trait_impl_fun_generic_params
                    .lifetime_order()
                    .iter()
                    .copied()
                    .map(|x| {
                        Lifetime::<Model>::Parameter(LifetimeParameterID::new(
                            trait_impl_fun_id,
                            x,
                        ))
                    })
                    .chain(
                        trait_impl_fun_elided_lts.elided_lifetimes.ids().map(
                            |x| {
                                Lifetime::<Model>::Elided(
                                    ElidedLifetimeID::new(trait_impl_fun_id, x),
                                )
                            },
                        ),
                    )
                {
                    new_inst.lifetimes.insert(lt, Lifetime::Inference(Erased));
                }

                for (trait_fun_ty, trait_impl_fun_ty) in
                    trait_func_generic_params.type_order().iter().copied().zip(
                        trait_impl_fun_generic_params
                            .type_order()
                            .iter()
                            .copied(),
                    )
                {
                    new_inst.types.insert(
                        Type::Parameter(TypeParameterID::new(
                            trait_impl_fun_id,
                            trait_impl_fun_ty,
                        )),
                        calling_inst
                            .types
                            .remove(&Type::Parameter(TypeParameterID::new(
                                function_call.callable_id,
                                trait_fun_ty,
                            )))
                            .unwrap(),
                    );
                }

                for (trait_fun_const, trait_impl_fun_const) in
                    trait_func_generic_params
                        .constant_order()
                        .iter()
                        .copied()
                        .zip(
                            trait_impl_fun_generic_params
                                .constant_order()
                                .iter()
                                .copied(),
                        )
                {
                    new_inst.constants.insert(
                        Constant::Parameter(ConstantParameterID::new(
                            trait_impl_fun_id,
                            trait_impl_fun_const,
                        )),
                        calling_inst
                            .constants
                            .remove(&Constant::Parameter(
                                ConstantParameterID::new(
                                    function_call.callable_id,
                                    trait_fun_const,
                                ),
                            ))
                            .unwrap(),
                    );
                }

                self.context.normalize_instantiation(&mut new_inst);

                let llvm_function = self.context.get_function(&Call {
                    callable_id: trait_impl_fun_id,
                    instantiation: new_inst,
                });

                self.create_function_call(
                    &llvm_function,
                    &function_call.arguments,
                    reg_id,
                )
            }

            kind => panic!("unexpected symbol kind: {kind:?}"),
        }
    }

    fn handle_array(
        &mut self,
        array: &register::Array<Model>,
        reg_id: ID<Register<Model>>,
    ) -> Result<Option<LlvmValue<'ctx>>, Error> {
        let mut values = Vec::new();
        for element in &array.elements {
            let Some(value) = self.get_value(element)? else {
                continue;
            };

            values.push(value);
        }

        let Ok(array_ty) = self.type_of_register(reg_id) else {
            return Ok(None);
        };

        let tmp = self.create_aggregate_temporary(array_ty, reg_id);

        for (index, element) in values.into_iter().enumerate() {
            let pointer_value = unsafe {
                self.inkwell_builder
                    .build_gep(
                        array_ty,
                        tmp.address,
                        &[
                            self.context.context().i64_type().const_zero(),
                            self.context
                                .context()
                                .i64_type()
                                .const_int(index.try_into().unwrap(), false),
                        ],
                        &format!("init_array_{reg_id:?}_index_{index:?}_gep"),
                    )
                    .unwrap()
            };

            match element {
                LlvmValue::Scalar(basic_value_enum) => {
                    self.inkwell_builder
                        .build_store(pointer_value, basic_value_enum)
                        .unwrap();
                }
                LlvmValue::TmpAggegate(llvm_address) => {
                    self.build_memcpy(
                        pointer_value,
                        llvm_address.address,
                        llvm_address.r#type,
                    );
                }
            }
        }

        Ok(Some(LlvmValue::TmpAggegate(tmp)))
    }

    #[allow(clippy::too_many_lines)]
    fn handle_binary(
        &mut self,
        binary: &register::Binary<Model>,
        reg_id: ID<Register<Model>>,
    ) -> Result<Option<LlvmValue<'ctx>>, Error> {
        let mut lhs =
            self.get_value(&binary.lhs)?.unwrap().into_scalar().unwrap();
        let mut rhs =
            self.get_value(&binary.rhs)?.unwrap().into_scalar().unwrap();

        match binary.operator {
            BinaryOperator::Arithmetic(arithmetic_operator) => {
                enum Kind {
                    Signed,
                    Unsigned,
                    Float,
                    Pointer,
                }

                let pernix_lhs_ty = self
                    .function_ir
                    .values
                    .type_of_value(
                        &binary.lhs,
                        self.callable_id,
                        &self.environment,
                    )
                    .unwrap();

                let kind = match &pernix_lhs_ty.result {
                    Type::Primitive(
                        Primitive::Int8
                        | Primitive::Int16
                        | Primitive::Int32
                        | Primitive::Int64
                        | Primitive::Isize,
                    ) => Kind::Signed,

                    Type::Primitive(
                        Primitive::Uint8
                        | Primitive::Uint16
                        | Primitive::Uint32
                        | Primitive::Uint64
                        | Primitive::Usize,
                    ) => Kind::Unsigned,

                    Type::Primitive(
                        Primitive::Float32 | Primitive::Float64,
                    ) => Kind::Float,

                    Type::Pointer(_) => Kind::Pointer,

                    _ => unreachable!(),
                };

                let value: BasicValueEnum = match (kind, arithmetic_operator) {
                    (
                        Kind::Signed | Kind::Unsigned,
                        register::ArithmeticOperator::Add,
                    ) => self
                        .inkwell_builder
                        .build_int_add(
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            &format!("add_{reg_id:?}"),
                        )
                        .unwrap()
                        .into(),

                    (
                        Kind::Signed | Kind::Unsigned,
                        register::ArithmeticOperator::Subtract,
                    ) => self
                        .inkwell_builder
                        .build_int_sub(
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            &format!("sub_{reg_id:?}"),
                        )
                        .unwrap()
                        .into(),

                    (
                        Kind::Signed | Kind::Unsigned,
                        register::ArithmeticOperator::Multiply,
                    ) => self
                        .inkwell_builder
                        .build_int_mul(
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            &format!("mul_{reg_id:?}"),
                        )
                        .unwrap()
                        .into(),

                    (Kind::Signed, register::ArithmeticOperator::Divide) => {
                        self.inkwell_builder
                            .build_int_signed_div(
                                lhs.into_int_value(),
                                rhs.into_int_value(),
                                &format!("div_{reg_id:?}"),
                            )
                            .unwrap()
                            .into()
                    }

                    (Kind::Signed, register::ArithmeticOperator::Modulo) => {
                        self.inkwell_builder
                            .build_int_signed_rem(
                                lhs.into_int_value(),
                                rhs.into_int_value(),
                                &format!("rem_{reg_id:?}"),
                            )
                            .unwrap()
                            .into()
                    }

                    (Kind::Unsigned, register::ArithmeticOperator::Divide) => {
                        self.inkwell_builder
                            .build_int_unsigned_div(
                                lhs.into_int_value(),
                                rhs.into_int_value(),
                                &format!("div_{reg_id:?}"),
                            )
                            .unwrap()
                            .into()
                    }

                    (Kind::Unsigned, register::ArithmeticOperator::Modulo) => {
                        self.inkwell_builder
                            .build_int_unsigned_rem(
                                lhs.into_int_value(),
                                rhs.into_int_value(),
                                &format!("rem_{reg_id:?}"),
                            )
                            .unwrap()
                            .into()
                    }

                    (Kind::Float, register::ArithmeticOperator::Add) => self
                        .inkwell_builder
                        .build_float_add(
                            lhs.into_float_value(),
                            rhs.into_float_value(),
                            &format!("add_{reg_id:?}"),
                        )
                        .unwrap()
                        .into(),

                    (Kind::Float, register::ArithmeticOperator::Subtract) => {
                        self.inkwell_builder
                            .build_float_sub(
                                lhs.into_float_value(),
                                rhs.into_float_value(),
                                &format!("sub_{reg_id:?}"),
                            )
                            .unwrap()
                            .into()
                    }

                    (Kind::Float, register::ArithmeticOperator::Multiply) => {
                        self.inkwell_builder
                            .build_float_mul(
                                lhs.into_float_value(),
                                rhs.into_float_value(),
                                &format!("mul_{reg_id:?}"),
                            )
                            .unwrap()
                            .into()
                    }

                    (Kind::Float, register::ArithmeticOperator::Divide) => self
                        .inkwell_builder
                        .build_float_div(
                            lhs.into_float_value(),
                            rhs.into_float_value(),
                            &format!("div_{reg_id:?}"),
                        )
                        .unwrap()
                        .into(),
                    (Kind::Float, register::ArithmeticOperator::Modulo) => self
                        .inkwell_builder
                        .build_float_rem(
                            lhs.into_float_value(),
                            rhs.into_float_value(),
                            &format!("rem_{reg_id:?}"),
                        )
                        .unwrap()
                        .into(),

                    (
                        Kind::Pointer,
                        register::ArithmeticOperator::Add
                        | register::ArithmeticOperator::Subtract,
                    ) => unsafe {
                        let pointee_type = self.context.get_type(
                            pernix_lhs_ty
                                .result
                                .as_pointer()
                                .unwrap()
                                .pointee
                                .deref()
                                .clone(),
                        );

                        let Ok(pointee_type) = pointee_type else {
                            // pointer arithmetic on zst type is no-op
                            return Ok(Some(LlvmValue::Scalar(lhs)));
                        };

                        if arithmetic_operator
                            == register::ArithmeticOperator::Subtract
                        {
                            rhs = self
                                .inkwell_builder
                                .build_int_neg(
                                    rhs.into_int_value(),
                                    &format!("neg_ptr_arith_{reg_id:?}"),
                                )
                                .unwrap()
                                .into();
                        }

                        self.inkwell_builder
                            .build_gep(
                                pointee_type,
                                lhs.into_pointer_value(),
                                &[rhs.into_int_value()],
                                &format!("ptr_arith_{reg_id:?}"),
                            )
                            .unwrap()
                            .into()
                    },

                    _ => unreachable!(),
                };

                Ok(Some(LlvmValue::Scalar(value)))
            }

            BinaryOperator::Relational(relational_operator) => {
                enum Kind {
                    Signed,
                    Unsigned,
                    Float,
                    Bool,
                }

                let pernix_lhs_ty = self
                    .function_ir
                    .values
                    .type_of_value(
                        &binary.lhs,
                        self.callable_id,
                        &self.environment,
                    )
                    .unwrap();

                let kind = match &pernix_lhs_ty.result {
                    Type::Primitive(
                        Primitive::Int8
                        | Primitive::Int16
                        | Primitive::Int32
                        | Primitive::Int64
                        | Primitive::Isize,
                    ) => Kind::Signed,

                    Type::Primitive(
                        Primitive::Uint8
                        | Primitive::Uint16
                        | Primitive::Uint32
                        | Primitive::Uint64
                        | Primitive::Usize,
                    )
                    // pointer included here since it will be converted to
                    // usize
                    | Type::Pointer(_) => Kind::Unsigned,

                    Type::Primitive(
                        Primitive::Float32 | Primitive::Float64,
                    ) => Kind::Float,

                    Type::Primitive(Primitive::Bool) => Kind::Bool,

                    _ => unreachable!(),
                };

                if pernix_lhs_ty.result.is_pointer() {
                    lhs = self
                        .inkwell_builder
                        .build_ptr_to_int(
                            lhs.into_pointer_value(),
                            self.context.context().ptr_sized_int_type(
                                self.context.target_data(),
                                None,
                            ),
                            &format!("ptr_to_int_lhs_{reg_id:?}"),
                        )
                        .unwrap()
                        .into();

                    rhs = self
                        .inkwell_builder
                        .build_ptr_to_int(
                            rhs.into_pointer_value(),
                            self.context.context().ptr_sized_int_type(
                                self.context.target_data(),
                                None,
                            ),
                            &format!("ptr_to_int_rhs_{reg_id:?}"),
                        )
                        .unwrap()
                        .into();
                }

                match kind {
                    Kind::Bool | Kind::Signed | Kind::Unsigned => {
                        let pred = match (kind, relational_operator) {
                            (
                                Kind::Signed | Kind::Unsigned | Kind::Bool,
                                RelationalOperator::Equal,
                            ) => IntPredicate::EQ,
                            (
                                Kind::Signed | Kind::Unsigned | Kind::Bool,
                                RelationalOperator::NotEqual,
                            ) => IntPredicate::NE,
                            (Kind::Signed, RelationalOperator::LessThan) => {
                                IntPredicate::SLT
                            }
                            (
                                Kind::Signed,
                                RelationalOperator::LessThanOrEqual,
                            ) => IntPredicate::SLE,
                            (Kind::Signed, RelationalOperator::GreaterThan) => {
                                IntPredicate::SGT
                            }
                            (
                                Kind::Signed,
                                RelationalOperator::GreaterThanOrEqual,
                            ) => IntPredicate::SGE,
                            (
                                Kind::Unsigned | Kind::Bool,
                                RelationalOperator::LessThan,
                            ) => IntPredicate::ULT,
                            (
                                Kind::Unsigned | Kind::Bool,
                                RelationalOperator::LessThanOrEqual,
                            ) => IntPredicate::ULE,
                            (
                                Kind::Unsigned | Kind::Bool,
                                RelationalOperator::GreaterThan,
                            ) => IntPredicate::UGT,
                            (
                                Kind::Unsigned | Kind::Bool,
                                RelationalOperator::GreaterThanOrEqual,
                            ) => IntPredicate::UGE,

                            (Kind::Float, _) => unreachable!(),
                        };

                        Ok(Some(LlvmValue::Scalar(
                            self.inkwell_builder
                                .build_int_compare(
                                    pred,
                                    lhs.into_int_value(),
                                    rhs.into_int_value(),
                                    &format!("cmp_{reg_id:?}"),
                                )
                                .unwrap()
                                .into(),
                        )))
                    }
                    Kind::Float => {
                        let float_pred = match relational_operator {
                            RelationalOperator::LessThan => FloatPredicate::OLT,
                            RelationalOperator::LessThanOrEqual => {
                                FloatPredicate::OLE
                            }
                            RelationalOperator::GreaterThan => {
                                FloatPredicate::OGT
                            }
                            RelationalOperator::GreaterThanOrEqual => {
                                FloatPredicate::OGE
                            }
                            RelationalOperator::Equal => FloatPredicate::OEQ,
                            RelationalOperator::NotEqual => FloatPredicate::ONE,
                        };

                        Ok(Some(LlvmValue::Scalar(
                            self.inkwell_builder
                                .build_float_compare(
                                    float_pred,
                                    lhs.into_float_value(),
                                    rhs.into_float_value(),
                                    &format!("cmp_{reg_id:?}"),
                                )
                                .unwrap()
                                .into(),
                        )))
                    }
                }
            }

            BinaryOperator::Bitwise(bitwise_operator) => {
                let pernix_lhs_ty = self
                    .function_ir
                    .values
                    .type_of_value(
                        &binary.lhs,
                        self.callable_id,
                        &self.environment,
                    )
                    .unwrap()
                    .result
                    .into_primitive()
                    .unwrap();

                let value = match bitwise_operator {
                    register::BitwiseOperator::And => self
                        .inkwell_builder
                        .build_and(
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            &format!("and_{reg_id:?}"),
                        )
                        .unwrap(),
                    register::BitwiseOperator::Or => self
                        .inkwell_builder
                        .build_or(
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            &format!("or_{reg_id:?}"),
                        )
                        .unwrap(),
                    register::BitwiseOperator::Xor => self
                        .inkwell_builder
                        .build_xor(
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            &format!("xor_{reg_id:?}"),
                        )
                        .unwrap(),

                    register::BitwiseOperator::LeftShift
                    | register::BitwiseOperator::RightShift => {
                        // pernix allows the rhs to have different bit size with
                        // the lhs; therefore, we might need to extend or
                        // truncuate

                        let lhs_width =
                            lhs.into_int_value().get_type().get_bit_width();
                        let rhs_width =
                            rhs.into_int_value().get_type().get_bit_width();

                        match rhs_width.cmp(&lhs_width) {
                            Ordering::Less => {
                                // extend the rhs
                                let extended_rhs = self
                                    .inkwell_builder
                                    .build_int_z_extend(
                                        rhs.into_int_value(),
                                        lhs.into_int_value().get_type(),
                                        &format!("zext_{reg_id:?}"),
                                    )
                                    .unwrap();

                                rhs = extended_rhs.into();
                            }
                            Ordering::Equal => {} // do nothing
                            Ordering::Greater => {
                                // truncuate the rhs
                                let truncated_rhs = self
                                    .inkwell_builder
                                    .build_int_truncate(
                                        rhs.into_int_value(),
                                        lhs.into_int_value().get_type(),
                                        &format!("trunc_{reg_id:?}"),
                                    )
                                    .unwrap();

                                rhs = truncated_rhs.into();
                            }
                        }

                        match bitwise_operator {
                            register::BitwiseOperator::LeftShift => self
                                .inkwell_builder
                                .build_left_shift(
                                    lhs.into_int_value(),
                                    rhs.into_int_value(),
                                    &format!("shl_{reg_id:?}"),
                                )
                                .unwrap(),
                            register::BitwiseOperator::RightShift => self
                                .inkwell_builder
                                .build_right_shift(
                                    lhs.into_int_value(),
                                    rhs.into_int_value(),
                                    match pernix_lhs_ty {
                                        Primitive::Isize
                                        | Primitive::Int8
                                        | Primitive::Int16
                                        | Primitive::Int32
                                        | Primitive::Int64 => true,

                                        Primitive::Uint8
                                        | Primitive::Uint16
                                        | Primitive::Uint32
                                        | Primitive::Uint64
                                        | Primitive::Bool
                                        | Primitive::Usize => false,

                                        Primitive::Float32
                                        | Primitive::Float64 => {
                                            unreachable!()
                                        }
                                    },
                                    &format!("shr_{reg_id:?}"),
                                )
                                .unwrap(),

                            _ => unreachable!(),
                        }
                    }
                };

                Ok(Some(LlvmValue::Scalar(value.into())))
            }
        }
    }

    fn handle_phi(
        &mut self,
        phi: &register::Phi<Model>,
        reg_id: ID<Register<Model>>,
    ) -> Result<Option<LlvmValue<'ctx>>, Error> {
        let mut incoming_values = Vec::new();
        for (block_id, value) in &phi.incoming_values {
            let block = self.basic_block_map[block_id];
            let value = self.get_value(value)?;

            let Some(value) = value else {
                continue;
            };

            incoming_values.push((value, block));
        }

        let Ok(ty) = self.type_of_register(reg_id) else {
            return Ok(None);
        };

        let phi = self
            .inkwell_builder
            .build_phi(
                if ty.is_aggregate() {
                    self.context
                        .context()
                        .ptr_type(AddressSpace::default())
                        .into()
                } else {
                    ty
                },
                &format!("phi_{reg_id:?}"),
            )
            .unwrap();

        for (value, block) in incoming_values {
            let value = match value {
                LlvmValue::Scalar(basic_value_enum) => basic_value_enum,
                LlvmValue::TmpAggegate(llvm_address) => {
                    llvm_address.address.into()
                }
            };
            phi.add_incoming(&[(&value, block)]);
        }

        if ty.is_aggregate() {
            Ok(Some(LlvmValue::TmpAggegate(LlvmAddress::new(
                phi.as_basic_value().into_pointer_value(),
                ty,
            ))))
        } else {
            Ok(Some(LlvmValue::Scalar(phi.as_basic_value())))
        }
    }

    fn handle_tuple(
        &mut self,
        tuple: &register::Tuple<Model>,
        reg_id: ID<Register<Model>>,
    ) -> Result<Option<LlvmValue<'ctx>>, Error> {
        let mut values = Vec::new();
        for element in &tuple.elements {
            let Some(value) = self.get_value(&element.value)? else {
                continue;
            };

            if element.is_unpacked {
                // tuple is struct avlue
                let tuple_address = value.into_tmp_aggegate().unwrap();
                let tuple_as_struct = tuple_address.r#type.into_struct_type();
                let count = tuple_as_struct.count_fields();

                for i in 0..count {
                    let ty =
                        tuple_as_struct.get_field_type_at_index(i).unwrap();

                    let address = self
                        .inkwell_builder
                        .build_struct_gep(
                            tuple_as_struct,
                            tuple_address.address,
                            i,
                            &format!("extract_tuple_{reg_id:?}_index_{i:?}"),
                        )
                        .unwrap();

                    values.push(if ty.is_aggregate() {
                        LlvmValue::TmpAggegate(LlvmAddress::new(address, ty))
                    } else {
                        LlvmValue::Scalar(
                            self.inkwell_builder
                                .build_load(
                                    ty,
                                    address,
                                    &format!(
                                        "load_extract_tuple_{reg_id:?\
                                         }_index_{i:?}"
                                    ),
                                )
                                .unwrap(),
                        )
                    });
                }
            } else {
                values.push(value);
            }
        }

        let Ok(ty) = self.type_of_register(reg_id) else {
            return Ok(None);
        };

        let tmp = self.create_aggregate_temporary(ty, reg_id);

        for (index, element) in values.into_iter().enumerate() {
            let pointer_value = self
                .inkwell_builder
                .build_struct_gep(
                    ty,
                    tmp.address,
                    index.try_into().unwrap(),
                    &format!("init_tuple_{reg_id:?}_index_{index:?}_gep"),
                )
                .unwrap();

            match element {
                LlvmValue::Scalar(basic_value_enum) => {
                    self.inkwell_builder
                        .build_store(pointer_value, basic_value_enum)
                        .unwrap();
                }
                LlvmValue::TmpAggegate(llvm_address) => {
                    self.build_memcpy(
                        pointer_value,
                        llvm_address.address,
                        llvm_address.r#type,
                    );
                }
            }
        }

        Ok(Some(LlvmValue::TmpAggegate(tmp)))
    }

    #[allow(clippy::too_many_lines)]
    fn handle_variant(
        &mut self,
        variant: &register::Variant<Model>,
        reg_id: ID<Register<Model>>,
    ) -> Result<Option<LlvmValue<'ctx>>, Error> {
        let value = variant
            .associated_value
            .as_ref()
            .map(|variant| self.get_value(variant))
            .transpose()?;

        let ty = self.type_of_register_pnx(reg_id).into_symbol().unwrap();
        let llvm_enum_sig = self.context.get_enum_type(ty);

        match &*llvm_enum_sig {
            LlvmEnumSignature::Zst => Ok(None),
            LlvmEnumSignature::NullablePointer(nullable_pointer) => {
                if nullable_pointer.null_variant_index as usize
                    == self
                        .context
                        .table()
                        .get::<VariantDeclarationOrder>(variant.variant_id)
                        .order
                {
                    Ok(Some(LlvmValue::Scalar(
                        self.context
                            .context()
                            .ptr_type(AddressSpace::default())
                            .const_null()
                            .into(),
                    )))
                } else {
                    Ok(Some(LlvmValue::Scalar(
                        value
                            .expect("should have associated value")
                            .expect("pointer is not zst")
                            .into_scalar()
                            .unwrap(),
                    )))
                }
            }
            LlvmEnumSignature::Transparent(_) => Ok(value.unwrap()),
            LlvmEnumSignature::Numeric(int_type) => {
                Ok(Some(LlvmValue::Scalar(
                    int_type
                        .const_int(
                            self.context
                                .table()
                                .get::<VariantDeclarationOrder>(
                                    variant.variant_id,
                                )
                                .order
                                .try_into()
                                .unwrap(),
                            false,
                        )
                        .into(),
                )))
            }
            LlvmEnumSignature::TaggedUnion(tagged_union) => {
                // create the alloca as the given enum repr
                let tmp = self.create_aggregate_temporary(
                    tagged_union.llvm_struct_type.into(),
                    reg_id,
                );

                let variant_repr =
                    tagged_union.llvm_variant_types[&variant.variant_id];

                let variant_index = self
                    .context
                    .table()
                    .get::<VariantDeclarationOrder>(variant.variant_id)
                    .order;

                let tag_pointer = self
                    .inkwell_builder
                    .build_struct_gep(
                        variant_repr,
                        tmp.address,
                        0,
                        &format!("tag_{reg_id:?}_gep"),
                    )
                    .unwrap();

                self.inkwell_builder
                    .build_store(
                        tag_pointer,
                        tagged_union.llvm_tag_type.const_int(
                            variant_index.try_into().unwrap(),
                            false,
                        ),
                    )
                    .unwrap();

                match value {
                    Some(Some(LlvmValue::Scalar(value))) => {
                        let payload_pointer = self
                            .inkwell_builder
                            .build_struct_gep(
                                variant_repr,
                                tmp.address,
                                1,
                                &format!("payload_{reg_id:?}_gep"),
                            )
                            .unwrap();

                        self.inkwell_builder
                            .build_store(payload_pointer, value)
                            .unwrap();
                    }

                    Some(Some(LlvmValue::TmpAggegate(value))) => {
                        let payload_pointer = self
                            .inkwell_builder
                            .build_struct_gep(
                                variant_repr,
                                tmp.address,
                                1,
                                &format!("payload_{reg_id:?}_gep"),
                            )
                            .unwrap();

                        self.build_memcpy(
                            payload_pointer,
                            value.address,
                            value.r#type,
                        );
                    }

                    Some(None) | None => {}
                }

                Ok(Some(LlvmValue::TmpAggegate(tmp)))
            }
        }
    }

    #[allow(clippy::too_many_lines)]
    fn handle_prefix(
        &mut self,
        prefix: &register::Prefix<Model>,
        reg_id: ID<Register<Model>>,
    ) -> Result<Option<LlvmValue<'ctx>>, Error> {
        let operand = self
            .get_value(&prefix.operand)?
            .expect("prefix operators operate on scalar values")
            .into_scalar()
            .unwrap();

        let value = match prefix.operator {
            register::PrefixOperator::Negate => match operand.get_type() {
                BasicTypeEnum::FloatType(_) => self
                    .inkwell_builder
                    .build_float_neg(
                        operand.into_float_value(),
                        &format!("neg_{reg_id:?}"),
                    )
                    .unwrap()
                    .into(),
                BasicTypeEnum::IntType(_) => self
                    .inkwell_builder
                    .build_int_neg(
                        operand.into_int_value(),
                        &format!("neg_{reg_id:?}"),
                    )
                    .unwrap()
                    .into(),

                unexpected => {
                    panic!("unexpected type for negation: {unexpected:?}")
                }
            },

            register::PrefixOperator::LogicalNot => {
                let zero = self.context.context().bool_type().const_zero();
                self.inkwell_builder
                    .build_int_compare(
                        IntPredicate::EQ,
                        operand.into_int_value(),
                        zero,
                        &format!("not_{reg_id:?}"),
                    )
                    .unwrap()
                    .into()
            }
            register::PrefixOperator::BitwiseNot => self
                .inkwell_builder
                .build_not(operand.into_int_value(), &format!("not_{reg_id:?}"))
                .unwrap()
                .into(),
        };

        Ok(Some(LlvmValue::Scalar(value)))
    }

    fn handle_variant_number(
        &mut self,
        variant_number: &register::VariantNumber<Model>,
        reg_id: ID<Register<Model>>,
    ) -> Result<Option<LlvmValue<'ctx>>, Error> {
        let address = self.get_address(&variant_number.address)?;
        let enum_type = self
            .type_of_address_pnx(&variant_number.address)
            .into_symbol()
            .unwrap();

        let llvm_enum_sig = self.context.get_enum_type(enum_type);

        match &*llvm_enum_sig {
            LlvmEnumSignature::Transparent(_) | LlvmEnumSignature::Zst => {
                Ok(Some(LlvmValue::Scalar(
                    self.context
                        .context()
                        .bool_type()
                        .const_int(0, false)
                        .into(),
                )))
            }

            LlvmEnumSignature::NullablePointer(nullable_pointer) => {
                let pointer_val = self
                    .inkwell_builder
                    .build_load(
                        self.context
                            .context()
                            .ptr_type(AddressSpace::default()),
                        address.unwrap().address,
                        &format!("load_{reg_id:?}_ptr"),
                    )
                    .unwrap();

                let is_null = self
                    .inkwell_builder
                    .build_int_compare(
                        IntPredicate::EQ,
                        pointer_val.into_pointer_value(),
                        self.context
                            .context()
                            .ptr_type(AddressSpace::default())
                            .const_null(),
                        &format!("cmp_{reg_id:?}_is_null"),
                    )
                    .unwrap();

                let (mut then, mut else_) = (
                    self.context.context().bool_type().const_zero(),
                    self.context.context().bool_type().const_all_ones(),
                );

                if nullable_pointer.null_variant_index != 0 {
                    std::mem::swap(&mut then, &mut else_);
                }

                Ok(Some(LlvmValue::Scalar(
                    self.inkwell_builder
                        .build_select(
                            is_null,
                            then,
                            else_,
                            &format!("select_{reg_id:?}"),
                        )
                        .unwrap(),
                )))
            }

            LlvmEnumSignature::Numeric(int_type) => {
                // directly load the variant number
                Ok(Some(LlvmValue::Scalar(
                    self.inkwell_builder
                        .build_load(
                            *int_type,
                            address.unwrap().address,
                            &format!("load_{reg_id:?}_variant_number"),
                        )
                        .unwrap(),
                )))
            }

            LlvmEnumSignature::TaggedUnion(tagged_union) => {
                let tag_gep = self
                    .inkwell_builder
                    .build_struct_gep(
                        tagged_union.llvm_struct_type,
                        address.unwrap().address,
                        0,
                        &format!("gep_{reg_id:?}_tag"),
                    )
                    .unwrap();

                Ok(Some(LlvmValue::Scalar(
                    self.inkwell_builder
                        .build_load(
                            tagged_union.llvm_tag_type,
                            tag_gep,
                            &format!("load_{reg_id:?}_tag"),
                        )
                        .unwrap(),
                )))
            }
        }
    }

    #[allow(clippy::too_many_lines)]
    fn handle_cast(
        &mut self,
        cast: &register::Cast<Model>,
        reg_id: ID<Register<Model>>,
    ) -> Result<Option<LlvmValue<'ctx>>, Error> {
        // NOTE: in the future, the pointer casting will be here.
        #[derive(Debug, Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash)]
        enum PrimitiveKind {
            Signed,
            Unsigned,
            Float,
            ReferenceOrPointer,
        }

        let operand = self
            .get_value(&cast.value)?
            .expect("scalar is not zst")
            .into_scalar()
            .expect("cast only available on scalar values");

        let operand_pnx_type = self.type_of_value_pnx(&cast.value);
        let cast_to_pnx_type = &cast.r#type;

        let get_kind = |ty: &Type<Model>| match ty {
            Type::Primitive(
                Primitive::Isize
                | Primitive::Int8
                | Primitive::Int16
                | Primitive::Int32
                | Primitive::Int64,
            ) => PrimitiveKind::Signed,

            Type::Primitive(
                Primitive::Bool
                | Primitive::Usize
                | Primitive::Uint8
                | Primitive::Uint16
                | Primitive::Uint32
                | Primitive::Uint64,
            ) => PrimitiveKind::Unsigned,

            Type::Primitive(Primitive::Float32 | Primitive::Float64) => {
                PrimitiveKind::Float
            }

            Type::Reference(_) | Type::Pointer(_) => {
                PrimitiveKind::ReferenceOrPointer
            }

            _ => unreachable!(),
        };

        let cast_to_kind = get_kind(cast_to_pnx_type);
        let operand_kind = get_kind(&operand_pnx_type);

        let operand_type = self
            .context
            .get_type(operand_pnx_type.clone())
            .expect("should be no zst");
        let cast_type = self
            .context
            .get_type(cast_to_pnx_type.clone())
            .expect("should be no zst");

        let float_bit_width = |ty| match ty {
            Primitive::Float32 => 32,
            Primitive::Float64 => 64,
            _ => unreachable!(),
        };

        match (operand_kind, cast_to_kind) {
            (
                PrimitiveKind::Unsigned | PrimitiveKind::Signed,
                PrimitiveKind::Unsigned | PrimitiveKind::Signed,
            ) => {
                let value_bit_width =
                    operand_type.into_int_type().get_bit_width();
                let cast_to_bit_width =
                    cast_type.into_int_type().get_bit_width();

                match value_bit_width.cmp(&cast_to_bit_width) {
                    // sext or zext, smaller to bigger
                    Ordering::Less => {
                        let is_signed = operand_kind == PrimitiveKind::Signed;

                        if is_signed {
                            Ok(Some(LlvmValue::Scalar(
                                self.inkwell_builder
                                    .build_int_s_extend(
                                        operand.into_int_value(),
                                        cast_type.into_int_type(),
                                        &format!("sext_{reg_id:?}"),
                                    )
                                    .unwrap()
                                    .into(),
                            )))
                        } else {
                            Ok(Some(LlvmValue::Scalar(
                                self.inkwell_builder
                                    .build_int_z_extend(
                                        operand.into_int_value(),
                                        cast_type.into_int_type(),
                                        &format!("zext_{reg_id:?}"),
                                    )
                                    .unwrap()
                                    .into(),
                            )))
                        }
                    }

                    // trunc, bigger to smaller
                    Ordering::Greater => Ok(Some(LlvmValue::Scalar(
                        self.inkwell_builder
                            .build_int_truncate(
                                operand.into_int_value(),
                                cast_type.into_int_type(),
                                &format!("trunc_{reg_id:?}"),
                            )
                            .unwrap()
                            .into(),
                    ))),

                    // no-op
                    Ordering::Equal => Ok(Some(LlvmValue::Scalar(operand))),
                }
            }

            // int to float
            (
                PrimitiveKind::Signed | PrimitiveKind::Unsigned,
                PrimitiveKind::Float,
            ) => {
                let operand_is_signed = operand_kind == PrimitiveKind::Signed;

                if operand_is_signed {
                    Ok(Some(LlvmValue::Scalar(
                        self.inkwell_builder
                            .build_signed_int_to_float(
                                operand.into_int_value(),
                                cast_type.into_float_type(),
                                &format!("sitofp_{reg_id:?}"),
                            )
                            .unwrap()
                            .into(),
                    )))
                } else {
                    Ok(Some(LlvmValue::Scalar(
                        self.inkwell_builder
                            .build_unsigned_int_to_float(
                                operand.into_int_value(),
                                cast_type.into_float_type(),
                                &format!("uitofp_{reg_id:?}"),
                            )
                            .unwrap()
                            .into(),
                    )))
                }
            }

            (
                PrimitiveKind::Float,
                PrimitiveKind::Signed | PrimitiveKind::Unsigned,
            ) => {
                let cast_to_is_signed = cast_to_kind == PrimitiveKind::Signed;

                if cast_to_is_signed {
                    Ok(Some(LlvmValue::Scalar(
                        self.inkwell_builder
                            .build_float_to_signed_int(
                                operand.into_float_value(),
                                cast_type.into_int_type(),
                                &format!("fptosi_{reg_id:?}"),
                            )
                            .unwrap()
                            .into(),
                    )))
                } else {
                    Ok(Some(LlvmValue::Scalar(
                        self.inkwell_builder
                            .build_float_to_unsigned_int(
                                operand.into_float_value(),
                                cast_type.into_int_type(),
                                &format!("fptoui_{reg_id:?}"),
                            )
                            .unwrap()
                            .into(),
                    )))
                }
            }

            (PrimitiveKind::Float, PrimitiveKind::Float) => {
                let value_bit_width =
                    float_bit_width(*operand_pnx_type.as_primitive().unwrap());
                let cast_to_bit_width =
                    float_bit_width(*cast_to_pnx_type.as_primitive().unwrap());

                match value_bit_width.cmp(&cast_to_bit_width) {
                    // smaller to bigger
                    Ordering::Less => Ok(Some(LlvmValue::Scalar(
                        self.inkwell_builder
                            .build_float_ext(
                                operand.into_float_value(),
                                cast_type.into_float_type(),
                                &format!("fpext_{reg_id:?}"),
                            )
                            .unwrap()
                            .into(),
                    ))),

                    // bigger to smaller
                    Ordering::Greater => Ok(Some(LlvmValue::Scalar(
                        self.inkwell_builder
                            .build_float_trunc(
                                operand.into_float_value(),
                                cast_type.into_float_type(),
                                &format!("fptrunc_{reg_id:?}"),
                            )
                            .unwrap()
                            .into(),
                    ))),

                    // no-op instruction
                    Ordering::Equal => Ok(Some(LlvmValue::Scalar(operand))),
                }
            }

            // reference/pointer
            (
                PrimitiveKind::ReferenceOrPointer,
                PrimitiveKind::ReferenceOrPointer,
            ) => Ok(Some(LlvmValue::Scalar(operand))),

            // reference/pointer -> usize
            (PrimitiveKind::ReferenceOrPointer, PrimitiveKind::Unsigned) => {
                Ok(Some(LlvmValue::Scalar(
                    self.inkwell_builder
                        .build_ptr_to_int(
                            operand.into_pointer_value(),
                            cast_type.into_int_type(),
                            &format!("ptrtoint_{reg_id:?}"),
                        )
                        .unwrap()
                        .into(),
                )))
            }

            // usize -> reference/pointer
            (PrimitiveKind::Unsigned, PrimitiveKind::ReferenceOrPointer) => {
                Ok(Some(LlvmValue::Scalar(
                    self.inkwell_builder
                        .build_int_to_ptr(
                            operand.into_int_value(),
                            cast_type.into_pointer_type(),
                            &format!("inttoptr_{reg_id:?}"),
                        )
                        .unwrap()
                        .into(),
                )))
            }

            _ => unreachable!(),
        }
    }

    fn get_register_assignment_value(
        &mut self,
        register_assignment: &Assignment<Model>,
        reg_id: ID<Register<Model>>,
    ) -> Result<Option<LlvmValue<'ctx>>, Error> {
        match register_assignment {
            Assignment::Tuple(tuple) => self.handle_tuple(tuple, reg_id),
            Assignment::Load(load) => self.handle_load(load, reg_id),
            Assignment::Borrow(borrow) => (self
                .get_address(&borrow.address)?)
            .map_or_else(
                || {
                    Ok(Some(LlvmValue::Scalar(
                        self.build_non_null_dangling().into(),
                    )))
                },
                |basic_value_enum| {
                    Ok(Some(LlvmValue::Scalar(basic_value_enum.address.into())))
                },
            ),
            Assignment::Prefix(prefix) => self.handle_prefix(prefix, reg_id),
            Assignment::Struct(struct_lit) => {
                self.handle_struct_lit(struct_lit, reg_id)
            }
            Assignment::Variant(variant) => {
                self.handle_variant(variant, reg_id)
            }
            Assignment::FunctionCall(function_call) => {
                self.handle_function_call(function_call, reg_id)
            }
            Assignment::Binary(binary) => self.handle_binary(binary, reg_id),
            Assignment::Array(array) => self.handle_array(array, reg_id),
            Assignment::Phi(phi) => self.handle_phi(phi, reg_id),
            Assignment::Cast(cast) => self.handle_cast(cast, reg_id),
            Assignment::VariantNumber(variant_number) => {
                self.handle_variant_number(variant_number, reg_id)
            }
        }
    }

    #[allow(clippy::too_many_lines)]
    fn build_register_assignment(
        &mut self,
        register_assignment: RegisterAssignment<Model>,
    ) -> Result<(), Error> {
        let reg = &self.function_ir.values.registers[register_assignment.id];

        let value = self.get_register_assignment_value(
            &reg.assignment,
            register_assignment.id,
        )?;

        assert!(self
            .register_map
            .insert(register_assignment.id, value)
            .is_none());

        Ok(())
    }

    fn build_tuple_pack(
        &mut self,
        tuple_pack: &TuplePack<Model>,
    ) -> Result<(), Error> {
        let source_address_type = self
            .type_of_address_pnx(&tuple_pack.tuple_address)
            .into_tuple()
            .unwrap();
        let store_address_type = self
            .type_of_address_pnx(&tuple_pack.store_address)
            .into_tuple()
            .unwrap();

        assert!(source_address_type.elements.iter().all(|x| !x.is_unpacked));

        let pack_element_count = source_address_type.elements.len()
            - tuple_pack.after_packed_element_count
            - tuple_pack.before_packed_element_count;

        let (Some(source_address), Some(store_address)) = (
            self.get_address(&tuple_pack.tuple_address)?,
            self.get_address(&tuple_pack.store_address)?,
        ) else {
            return Ok(());
        };

        let (Ok(source_tuple_sig), Ok(store_tuple_sig)) = (
            self.context.get_tuple_type(source_address_type),
            self.context.get_tuple_type(store_address_type),
        ) else {
            return Ok(());
        };

        for i in 0..pack_element_count {
            let (Some(store_index), Some(source_index)) = (
                store_tuple_sig
                    .llvm_field_indices_by_tuple_idnex
                    .get(&i)
                    .copied(),
                source_tuple_sig
                    .llvm_field_indices_by_tuple_idnex
                    .get(&(i + tuple_pack.before_packed_element_count))
                    .copied(),
            ) else {
                continue;
            };

            let ty = source_tuple_sig.llvm_element_types[i];
            let source_pointer = self
                .inkwell_builder
                .build_struct_gep(
                    source_tuple_sig.llvm_tuple_type,
                    source_address.address,
                    source_index.try_into().unwrap(),
                    &format!("tuple_pack_source_{i:?}_gep"),
                )
                .unwrap();

            let load = self
                .inkwell_builder
                .build_load(
                    source_tuple_sig.llvm_element_types[i],
                    source_pointer,
                    &format!("tuple_pack_load_{i:?}"),
                )
                .unwrap();

            let store_pointer = self
                .inkwell_builder
                .build_struct_gep(
                    store_tuple_sig.llvm_tuple_type,
                    store_address.address,
                    store_index.try_into().unwrap(),
                    &format!("tuple_pack_store_{i:?}_gep"),
                )
                .unwrap();

            self.build_memcpy(store_pointer, source_pointer, ty);

            self.inkwell_builder.build_store(store_pointer, load).unwrap();
        }

        Ok(())
    }

    pub fn build_non_null_dangling(&mut self) -> PointerValue<'ctx> {
        let ptr_sized_int = self.context.context().ptr_sized_int_type(
            self.context.target_data(),
            Some(AddressSpace::default()),
        );

        self.inkwell_builder
            .build_int_to_ptr(
                ptr_sized_int.const_int(1, false),
                self.context.context().ptr_type(AddressSpace::default()),
                "non_null_dangling",
            )
            .unwrap()
    }

    /// Translates the Pernix's basic block to LLVM's basic block if haven't
    #[allow(clippy::too_many_lines)]
    pub fn build_basic_block(&mut self, block_id: ID<Block<Model>>) {
        // already built, or being built currently.
        if !self.built.insert(block_id) {
            return;
        }

        let current_block = self.basic_block_map[&block_id];

        // build the predecessors, bottom-up
        for predecessor in self.function_ir.control_flow_graph.blocks()
            [block_id]
            .predecessors()
            .iter()
            .copied()
        {
            self.build_basic_block(predecessor);
        }

        self.inkwell_builder.position_at_end(current_block);

        let block = &self.function_ir.control_flow_graph.blocks()[block_id];

        for instruction in block.instructions() {
            let result = match instruction {
                Instruction::Store(store) => self.build_store(store),
                Instruction::RegisterAssignment(register_assignment) => {
                    self.build_register_assignment(*register_assignment)
                }
                Instruction::TuplePack(tuple_pack) => {
                    self.build_tuple_pack(tuple_pack)
                }

                Instruction::DropUnpackTuple(drop_unpack_tuple) => 'ext: {
                    let tuple_ty_pnx = self
                        .type_of_address_pnx(&drop_unpack_tuple.tuple_address)
                        .into_tuple()
                        .unwrap();

                    let drop_element_count = tuple_ty_pnx.elements.len()
                        - drop_unpack_tuple.before_unpacked_element_count
                        - drop_unpack_tuple.after_unpacked_element_count;

                    let tuple_ty =
                        self.context.get_tuple_type(tuple_ty_pnx.clone()).ok();
                    let tuple_address = match self
                        .get_address(&drop_unpack_tuple.tuple_address)
                    {
                        Ok(address) => address,
                        Err(error) => {
                            break 'ext Err(error);
                        }
                    };

                    let non_null_dangling = self.build_non_null_dangling();

                    for (i, eleme) in tuple_ty_pnx.elements[drop_unpack_tuple
                        .before_unpacked_element_count
                        ..(drop_unpack_tuple.before_unpacked_element_count
                            + drop_element_count)]
                        .iter()
                        .enumerate()
                    {
                        let i =
                            i + drop_unpack_tuple.before_unpacked_element_count;

                        let ptr = tuple_ty
                            .as_ref()
                            .and_then(|x| {
                                x.llvm_field_indices_by_tuple_idnex
                                    .get(&i)
                                    .copied()
                            })
                            .map_or_else(
                                || non_null_dangling,
                                |x| {
                                    self.inkwell_builder
                                        .build_struct_gep(
                                            tuple_ty
                                                .as_ref()
                                                .unwrap()
                                                .llvm_tuple_type,
                                            tuple_address.unwrap().address,
                                            x.try_into().unwrap(),
                                            &format!(
                                                "gep_{block_id:?}_index_{i:?}"
                                            ),
                                        )
                                        .unwrap()
                                },
                            );

                        self.build_drop(ptr, eleme.term.clone());
                    }

                    Ok(())
                }

                Instruction::Drop(drop) => 'ext: {
                    let address = match self.get_address(&drop.address) {
                        Ok(address) => address,
                        Err(error) => break 'ext Err(error),
                    };
                    let adress_pnx_type =
                        self.type_of_address_pnx(&drop.address);

                    let pointer = address.map_or_else(
                        || self.build_non_null_dangling(),
                        |x| x.address,
                    );

                    self.build_drop(pointer, adress_pnx_type);

                    Ok(())
                }

                Instruction::RegisterDiscard(reg_dis) => {
                    let value = self.register_map[&reg_dis.id];

                    match value {
                        // scalar values (primitives) never need to be dropped
                        Some(LlvmValue::Scalar(_)) => {}

                        Some(LlvmValue::TmpAggegate(_)) | None => {
                            let pnx_type =
                                self.type_of_register_pnx(reg_dis.id);

                            let ptr_value = value.map_or_else(
                                || self.build_non_null_dangling(),
                                |x| {
                                    x.into_tmp_aggegate()
                                        .map(|x| x.address)
                                        .unwrap()
                                },
                            );

                            self.build_drop(ptr_value, pnx_type);
                        }
                    }

                    Ok(())
                }

                Instruction::ScopePush(_) | Instruction::ScopePop(_) => Ok(()),
            };

            if let Err(err) = result {
                match err {
                    Error::Unreachable => {
                        self.inkwell_builder.build_unreachable().unwrap();
                        return;
                    }
                }
            }
        }

        // build the terminator
        match block.terminator() {
            Some(Terminator::Panic) => {
                self.inkwell_builder
                    .build_call(
                        self.context.panic_function(),
                        &[],
                        &format!("panic_{block_id:?}"),
                    )
                    .unwrap();
                self.inkwell_builder.build_unreachable().unwrap();
            }

            Some(Terminator::Return(ret)) => {
                let val = match self.get_value(&ret.value) {
                    Ok(val) => val,
                    Err(Error::Unreachable) => {
                        self.inkwell_builder.build_unreachable().unwrap();
                        return;
                    }
                };

                match &self.llvm_function_signature.return_type {
                    ReturnType::Void => {
                        self.inkwell_builder.build_return(None).unwrap();
                    }
                    ReturnType::Scalar(_) => {
                        self.inkwell_builder
                            .build_return(Some(
                                &val.unwrap().into_scalar().unwrap(),
                            ))
                            .unwrap();
                    }
                    ReturnType::Sret(_) => {
                        let aggregate =
                            val.unwrap().into_tmp_aggegate().unwrap();

                        self.build_memcpy(
                            self.llvm_function_signature
                                .llvm_function_value
                                .get_first_param()
                                .unwrap()
                                .into_pointer_value(),
                            aggregate.address,
                            aggregate.r#type,
                        );

                        self.inkwell_builder.build_return(None).unwrap();
                    }
                }
            }

            Some(Terminator::Jump(Jump::Unconditional(jump))) => {
                self.inkwell_builder
                    .build_unconditional_branch(
                        self.basic_block_map[&jump.target],
                    )
                    .unwrap();
            }

            Some(Terminator::Jump(Jump::Conditional(jump))) => {
                let condition = match self.get_value(&jump.condition) {
                    Ok(val) => {
                        val.expect("boolean is not zst").into_scalar().unwrap()
                    }
                    Err(Error::Unreachable) => {
                        self.inkwell_builder.build_unreachable().unwrap();
                        return;
                    }
                };

                self.inkwell_builder
                    .build_conditional_branch(
                        condition.into_int_value(),
                        self.basic_block_map[&jump.true_target],
                        self.basic_block_map[&jump.false_target],
                    )
                    .unwrap();
            }

            Some(Terminator::Jump(Jump::Switch(select))) => {
                let int_value = match self.get_value(&select.integer) {
                    Ok(val) => val
                        .expect("integer is not zst")
                        .into_scalar()
                        .unwrap()
                        .into_int_value(),
                    Err(Error::Unreachable) => {
                        self.inkwell_builder.build_unreachable().unwrap();
                        return;
                    }
                };

                let int_ty = int_value.get_type();

                // the switches value and block to jump
                let mut switches = Vec::with_capacity(select.branches.len());

                for (value, target) in &select.branches {
                    let value = *value;

                    let const_int_value = match value {
                        SwitchValue::Positive(positive) => {
                            int_ty.const_int(positive, false)
                        }
                        SwitchValue::Negative(non_zero) => {
                            // do the two's complement
                            let mut non_zero = non_zero.get();
                            non_zero -= 1;
                            non_zero = !non_zero;

                            int_ty.const_int(non_zero, true)
                        }
                    };

                    switches
                        .push((const_int_value, self.basic_block_map[target]));
                }

                let else_block = if let Some(else_block) = select.otherwise {
                    self.basic_block_map[&else_block]
                } else {
                    let else_block = self.context.context().append_basic_block(
                        self.llvm_function_signature.llvm_function_value,
                        &format!("select_else_{block_id:?}"),
                    );

                    // build unreachable at the else block
                    self.inkwell_builder.position_at_end(else_block);
                    self.inkwell_builder.build_unreachable().unwrap();

                    self.inkwell_builder.position_at_end(current_block);

                    else_block
                };

                self.inkwell_builder
                    .build_switch(int_value, else_block, &switches)
                    .unwrap();
            }

            None => {
                self.inkwell_builder.build_return(None).unwrap();
            }
        }
    }
}
