use inkwell::values::BasicValueEnum;
use pernixc_arena::ID;
use pernixc_component::fields::Fields;
use pernixc_ir::{
    control_flow_graph::Block,
    instruction::{Instruction, Jump, RegisterAssignment, Terminator},
    value::register::{self, Assignment, BinaryOperator, Register},
};
use pernixc_table::component::SymbolKind;
use pernixc_term::{instantiation, r#type::Primitive};

use super::{Builder, Call, Error, LlvmValue};
use crate::{into_basic, r#type::LlvmType, Model};

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
        let LlvmType::Basic(pointee_ty) = self.type_of_address(&load.address)
        else {
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

        let LlvmType::Basic(struct_ty) = self.type_of_register(reg_id) else {
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

                self.inkwell_builder
                    .build_call(
                        llvm_function,
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
                todo!()
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

        let LlvmType::Basic(array_ty) = self.type_of_register(reg_id) else {
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
        let rhs = into_basic!(rhs);

        let lhs_ty = self.type_of_value(&binary.lhs).into_basic().unwrap();
        let rhs_ty = self.type_of_value(&binary.rhs).into_basic().unwrap();

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
                todo!()
            }
            BinaryOperator::Bitwise(bitwise_operator) => todo!(),
        }
    }

    fn get_register_assignment_value(
        &mut self,
        register_assignment: &Assignment<Model>,
        reg_id: ID<Register<Model>>,
    ) -> Result<LlvmValue<'ctx>, Error> {
        match register_assignment {
            Assignment::Tuple(tuple) => {
                todo!()
            }
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
                todo!()
            }
            Assignment::FunctionCall(function_call) => {
                self.handle_function_call(function_call, reg_id)
            }
            Assignment::Binary(binary) => self.handle_binary(binary, reg_id),
            Assignment::Array(array) => self.handle_array(array, reg_id),
            Assignment::Phi(phi) => todo!(),
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

    /// Translates the Pernix's basic block to LLVM's basic block if haven't
    pub fn build_basic_block(&mut self, block_id: ID<Block<Model>>) {
        if !self.built.insert(block_id) {
            return;
        }

        let current_block = self.basic_block_map[&block_id];
        self.inkwell_builder.position_at_end(current_block);

        let block = &self.function_ir.control_flow_graph.blocks()[block_id];

        for instruction in block.instructions() {
            let result = match instruction {
                Instruction::Store(store) => self.build_store(store),
                Instruction::RegisterAssignment(register_assignment) => {
                    self.build_register_assignment(*register_assignment)
                }
                Instruction::RegisterDiscard(register_discard) => Ok(()),
                Instruction::TuplePack(tuple_pack) => {
                    todo!()
                }
                Instruction::DropUnpackTuple(drop_unpack_tuple) => todo!(),
                Instruction::Drop(drop) => Ok(()),

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

                self.build_basic_block(jump.target);
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

                self.build_basic_block(jump.true_target);
                self.build_basic_block(jump.false_target);
            }

            Some(Terminator::Jump(Jump::Select(select))) => {}

            None => {
                self.inkwell_builder.build_return(None).unwrap();
            }
        }
    }
}
