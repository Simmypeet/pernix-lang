use std::cmp::Ordering;

use inkwell::{values::BasicValueEnum, AddressSpace, IntPredicate};
use pernixc_arena::ID;
use pernixc_component::fields::Fields;
use pernixc_ir::{
    control_flow_graph::Block,
    instruction::{
        Instruction, Jump, RegisterAssignment, Terminator, TuplePack,
    },
    model::Erased,
    value::register::{
        self, Assignment, BinaryOperator, Register, RelationalOperator,
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

use super::{Builder, Call, Error, LlvmValue};
use crate::{into_basic, r#type::LlvmEnumSignature, Model};

impl<'ctx> Builder<'_, 'ctx, '_, '_> {
    fn build_store(
        &mut self,
        store: &pernixc_ir::instruction::Store<Model>,
    ) -> Result<(), Error> {
        let value = self.get_value(&store.value)?;
        let address = self.get_address_value(&store.address)?;

        let (LlvmValue::Basic(value), LlvmValue::Basic(address)) =
            (value, address)
        else {
            return Ok(());
        };

        self.inkwell_builder
            .build_store(address.into_pointer_value(), value)
            .unwrap();

        Ok(())
    }

    fn handle_load(
        &mut self,
        load: &register::Load<Model>,
        reg_id: ID<Register<Model>>,
    ) -> Result<LlvmValue<'ctx>, Error> {
        let ptr = into_basic!(self.get_address_value(&load.address)?);
        let Ok(pointee_ty) = self.type_of_address(&load.address) else {
            return Ok(LlvmValue::Zst);
        };

        Ok(LlvmValue::Basic(
            self.inkwell_builder
                .build_load(
                    pointee_ty,
                    ptr.into_pointer_value(),
                    &format!("load_{reg_id:?}"),
                )
                .unwrap(),
        ))
    }

    fn handle_struct_lit(
        &mut self,
        struct_lit: &register::Struct<Model>,
        reg_id: ID<Register<Model>>,
    ) -> Result<LlvmValue<'ctx>, Error> {
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
            return Ok(LlvmValue::Zst);
        };

        let tmp = self
            .inkwell_builder
            .build_alloca(struct_ty, &format!("tmp_struct_{reg_id:?}"))
            .unwrap();

        for (index, (field_id, val)) in values
            .into_iter()
            .filter_map(|x| x.1.into_basic().ok().map(|y| (x.0, y)))
            .enumerate()
        {
            let pointer_value = self
                .inkwell_builder
                .build_struct_gep(
                    struct_ty,
                    tmp,
                    index.try_into().unwrap(),
                    &format!("init_struct_{reg_id:?}_field_{field_id:?}_gep"),
                )
                .unwrap();

            self.inkwell_builder.build_store(pointer_value, val).unwrap();
        }

        Ok(LlvmValue::Basic(
            self.inkwell_builder
                .build_load(
                    struct_ty,
                    tmp,
                    &format!("load_tmp_struct_{reg_id:?}_lit"),
                )
                .unwrap(),
        ))
    }

    #[allow(clippy::too_many_lines)]
    fn handle_function_call(
        &mut self,
        function_call: &register::FunctionCall<Model>,
        reg_id: ID<Register<Model>>,
    ) -> Result<LlvmValue<'ctx>, Error> {
        let symbol_kind =
            *self.context.table().get::<SymbolKind>(function_call.callable_id);

        match symbol_kind {
            SymbolKind::ExternFunction => {
                let llvm_function =
                    self.context.get_extern_function(function_call.callable_id);

                let mut args = Vec::new();
                for arg in &function_call.arguments {
                    let LlvmValue::Basic(val) = self.get_value(arg)? else {
                        continue;
                    };

                    args.push(val.into());
                }

                let function_call = self
                    .inkwell_builder
                    .build_call(
                        llvm_function.llvm_function_value,
                        &args,
                        &format!("call_{reg_id:?}"),
                    )
                    .unwrap()
                    .try_as_basic_value()
                    .left()
                    .map_or(Ok(LlvmValue::Zst), |value| {
                        Ok(LlvmValue::Basic(value))
                    });

                function_call
            }
            SymbolKind::Function
            | SymbolKind::AdtImplementationFunction
            | SymbolKind::TraitImplementationFunction => {
                let mut args = Vec::new();
                for arg in &function_call.arguments {
                    let LlvmValue::Basic(val) = self.get_value(arg)? else {
                        continue;
                    };

                    args.push(val.into());
                }

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

                self.inkwell_builder
                    .build_call(
                        llvm_function.llvm_function_value,
                        &args,
                        &format!("call_{reg_id:?}"),
                    )
                    .unwrap()
                    .try_as_basic_value()
                    .left()
                    .map_or(Ok(LlvmValue::Zst), |value| {
                        Ok(LlvmValue::Basic(value))
                    })
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

                let mut args = Vec::new();
                for arg in &function_call.arguments {
                    let LlvmValue::Basic(val) = self.get_value(arg)? else {
                        continue;
                    };

                    args.push(val.into());
                }

                let llvm_function = self.context.get_function(&Call {
                    callable_id: trait_impl_fun_id,
                    instantiation: new_inst,
                });

                self.inkwell_builder
                    .build_call(
                        llvm_function.llvm_function_value,
                        &args,
                        &format!("call_{reg_id:?}"),
                    )
                    .unwrap()
                    .try_as_basic_value()
                    .left()
                    .map_or(Ok(LlvmValue::Zst), |value| {
                        Ok(LlvmValue::Basic(value))
                    })
            }

            kind => panic!("unexpected symbol kind: {kind:?}"),
        }
    }

    fn handle_array(
        &mut self,
        array: &register::Array<Model>,
        reg_id: ID<Register<Model>>,
    ) -> Result<LlvmValue<'ctx>, Error> {
        let mut values = Vec::new();
        for element in &array.elements {
            let LlvmValue::Basic(value) = self.get_value(element)? else {
                continue;
            };

            values.push(value);
        }

        let Ok(array_ty) = self.type_of_register(reg_id) else {
            return Ok(LlvmValue::Zst);
        };

        let tmp = self
            .inkwell_builder
            .build_alloca(array_ty, &format!("tmp_array_{reg_id:?}"))
            .unwrap();

        for (index, element) in values.into_iter().enumerate() {
            let pointer_value = unsafe {
                self.inkwell_builder
                    .build_gep(
                        array_ty,
                        tmp,
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

            self.inkwell_builder.build_store(pointer_value, element).unwrap();
        }

        Ok(LlvmValue::Basic(
            self.inkwell_builder
                .build_load(
                    array_ty,
                    tmp,
                    &format!("load_tmp_array_{reg_id:?}_lit",),
                )
                .unwrap(),
        ))
    }

    #[allow(clippy::too_many_lines)]
    fn handle_binary(
        &mut self,
        binary: &register::Binary<Model>,
        reg_id: ID<Register<Model>>,
    ) -> Result<LlvmValue<'ctx>, Error> {
        let lhs = self.get_value(&binary.lhs)?;
        let rhs = self.get_value(&binary.rhs)?;

        let lhs = into_basic!(lhs);
        let mut rhs = into_basic!(rhs);

        match binary.operator {
            BinaryOperator::Arithmetic(arithmetic_operator) => {
                enum Kind {
                    Signed,
                    Unsigned,
                    Float,
                }

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

                let kind = match pernix_lhs_ty {
                    Primitive::Int8
                    | Primitive::Int16
                    | Primitive::Int32
                    | Primitive::Int64
                    | Primitive::Isize => Kind::Signed,

                    Primitive::Uint8
                    | Primitive::Uint16
                    | Primitive::Uint32
                    | Primitive::Uint64
                    | Primitive::Usize => Kind::Unsigned,

                    Primitive::Float32 | Primitive::Float64 => Kind::Float,

                    Primitive::Bool => unreachable!("bool is not arithmetic"),
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
                };

                Ok(LlvmValue::Basic(value))
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
                    .unwrap()
                    .result
                    .into_primitive()
                    .unwrap();

                let kind = match pernix_lhs_ty {
                    Primitive::Int8
                    | Primitive::Int16
                    | Primitive::Int32
                    | Primitive::Int64
                    | Primitive::Isize => Kind::Signed,

                    Primitive::Uint8
                    | Primitive::Uint16
                    | Primitive::Uint32
                    | Primitive::Uint64
                    | Primitive::Usize => Kind::Unsigned,

                    Primitive::Float32 | Primitive::Float64 => Kind::Float,

                    Primitive::Bool => Kind::Bool,
                };

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

                        Ok(LlvmValue::Basic(
                            self.inkwell_builder
                                .build_int_compare(
                                    pred,
                                    lhs.into_int_value(),
                                    rhs.into_int_value(),
                                    &format!("cmp_{reg_id:?}"),
                                )
                                .unwrap()
                                .into(),
                        ))
                    }
                    Kind::Float => todo!(),
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

                Ok(LlvmValue::Basic(value.into()))
            }
        }
    }

    fn handle_phi(
        &mut self,
        phi: &register::Phi<Model>,
        reg_id: ID<Register<Model>>,
    ) -> Result<LlvmValue<'ctx>, Error> {
        let mut incoming_values = Vec::new();
        for (block_id, value) in &phi.incoming_values {
            let block = self.basic_block_map[block_id];
            let value = self.get_value(value)?;

            let value = into_basic!(value);

            incoming_values.push((value, block));
        }

        let Ok(ty) = self.type_of_register(reg_id) else {
            return Ok(LlvmValue::Zst);
        };

        let phi = self
            .inkwell_builder
            .build_phi(ty, &format!("phi_{reg_id:?}"))
            .unwrap();

        for (value, block) in incoming_values {
            phi.add_incoming(&[(&value, block)]);
        }

        Ok(LlvmValue::Basic(phi.as_basic_value()))
    }

    fn handle_tuple(
        &mut self,
        tuple: &register::Tuple<Model>,
        reg_id: ID<Register<Model>>,
    ) -> Result<LlvmValue<'ctx>, Error> {
        let mut values = Vec::new();
        for element in &tuple.elements {
            let LlvmValue::Basic(value) = self.get_value(&element.value)?
            else {
                continue;
            };

            if element.is_unpacked {
                // tuple is struct avlue
                let struct_value = value.into_struct_value();
                let count = struct_value.get_type().count_fields();

                for i in 0..count {
                    values.push(
                        self.inkwell_builder
                            .build_extract_value(
                                struct_value,
                                i,
                                &format!(
                                    "extract_tuple_{reg_id:?}_index_{i:?}"
                                ),
                            )
                            .unwrap(),
                    );
                }
            } else {
                values.push(value);
            }
        }

        let Ok(ty) = self.type_of_register(reg_id) else {
            return Ok(LlvmValue::Zst);
        };

        let tmp = self
            .inkwell_builder
            .build_alloca(ty, &format!("tmp_tuple_{reg_id:?}"))
            .unwrap();

        for (index, element) in values.into_iter().enumerate() {
            let pointer_value = self
                .inkwell_builder
                .build_struct_gep(
                    ty,
                    tmp,
                    index.try_into().unwrap(),
                    &format!("init_tuple_{reg_id:?}_index_{index:?}_gep"),
                )
                .unwrap();
            self.inkwell_builder.build_store(pointer_value, element).unwrap();
        }

        Ok(LlvmValue::Basic(
            self.inkwell_builder
                .build_load(ty, tmp, &format!("load_tmp_tuple_{reg_id:?}_lit"))
                .unwrap(),
        ))
    }

    #[allow(clippy::too_many_lines)]
    fn handle_variant(
        &mut self,
        variant: &register::Variant<Model>,
        reg_id: ID<Register<Model>>,
    ) -> Result<LlvmValue<'ctx>, Error> {
        let value = variant
            .associated_value
            .as_ref()
            .map(|variant| self.get_value(variant))
            .transpose()?;

        let ty = self.type_of_register_pnx(reg_id).into_symbol().unwrap();
        let llvm_enum_sig = self.context.get_enum_type(ty);

        match &*llvm_enum_sig {
            LlvmEnumSignature::Zst => Ok(LlvmValue::Zst),
            LlvmEnumSignature::NullablePointer(nullable_pointer) => {
                if nullable_pointer.null_variant_index as usize
                    == self
                        .context
                        .table()
                        .get::<VariantDeclarationOrder>(variant.variant_id)
                        .order
                {
                    Ok(LlvmValue::Basic(
                        self.context
                            .context()
                            .ptr_type(AddressSpace::default())
                            .const_null()
                            .into(),
                    ))
                } else {
                    Ok(LlvmValue::Basic(
                        value
                            .expect("should have associated value")
                            .into_basic()
                            .expect("pointer is not zst"),
                    ))
                }
            }
            LlvmEnumSignature::Transparent(_) => Ok(value.unwrap()),
            LlvmEnumSignature::Numeric(int_type) => Ok(LlvmValue::Basic(
                int_type
                    .const_int(
                        self.context
                            .table()
                            .get::<VariantDeclarationOrder>(variant.variant_id)
                            .order
                            .try_into()
                            .unwrap(),
                        false,
                    )
                    .into(),
            )),
            LlvmEnumSignature::TaggedUnion(tagged_union) => {
                // create the alloca as the given enum repr
                let alloca = self
                    .inkwell_builder
                    .build_alloca(
                        tagged_union.llvm_struct_type,
                        &format!("tmp_variant_{reg_id:?}"),
                    )
                    .unwrap();

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
                        alloca,
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

                if let Some(LlvmValue::Basic(value)) = value {
                    let payload_pointer = self
                        .inkwell_builder
                        .build_struct_gep(
                            variant_repr,
                            alloca,
                            1,
                            &format!("payload_{reg_id:?}_gep"),
                        )
                        .unwrap();

                    self.inkwell_builder
                        .build_store(payload_pointer, value)
                        .unwrap();
                }

                Ok(LlvmValue::Basic(
                    self.inkwell_builder
                        .build_load(
                            tagged_union.llvm_struct_type,
                            alloca,
                            &format!("load_tmp_variant_{reg_id:?}_lit"),
                        )
                        .unwrap(),
                ))
            }
        }
    }

    fn get_register_assignment_value(
        &mut self,
        register_assignment: &Assignment<Model>,
        reg_id: ID<Register<Model>>,
    ) -> Result<LlvmValue<'ctx>, Error> {
        match register_assignment {
            Assignment::Tuple(tuple) => self.handle_tuple(tuple, reg_id),
            Assignment::Load(load) => self.handle_load(load, reg_id),
            Assignment::Borrow(borrow) => {
                match self.get_address_value(&borrow.address)? {
                    LlvmValue::Basic(basic_value_enum) => {
                        Ok(LlvmValue::Basic(basic_value_enum))
                    }
                    LlvmValue::Zst => todo!(),
                }
            }
            Assignment::Prefix(prefix) => todo!(),
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
            Assignment::Cast(cast) => todo!(),
            Assignment::VariantNumber(variant_number) => todo!(),
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

        let (LlvmValue::Basic(source_address), LlvmValue::Basic(store_address)) = (
            self.get_address_value(&tuple_pack.tuple_address)?,
            self.get_address_value(&tuple_pack.store_address)?,
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

            let source_pointer = self
                .inkwell_builder
                .build_struct_gep(
                    source_tuple_sig.llvm_tuple_type,
                    source_address.into_pointer_value(),
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
                    store_address.into_pointer_value(),
                    store_index.try_into().unwrap(),
                    &format!("tuple_pack_store_{i:?}_gep"),
                )
                .unwrap();

            self.inkwell_builder.build_store(store_pointer, load).unwrap();
        }

        Ok(())
    }

    /// Translates the Pernix's basic block to LLVM's basic block if haven't
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
                Instruction::DropUnpackTuple(drop_unpack_tuple) => todo!(),
                Instruction::Drop(drop) => Ok(()),

                Instruction::RegisterDiscard(_)
                | Instruction::ScopePush(_)
                | Instruction::ScopePop(_) => Ok(()),
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

                match val {
                    LlvmValue::Basic(basic_value_enum) => {
                        self.inkwell_builder
                            .build_return(Some(&basic_value_enum))
                            .unwrap();
                    }
                    LlvmValue::Zst => {
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
                    Ok(val) => val.into_basic().expect("boolean is not zst"),
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

            Some(Terminator::Jump(Jump::Select(select))) => {}

            None => {
                self.inkwell_builder.build_return(None).unwrap();
            }
        }
    }
}
