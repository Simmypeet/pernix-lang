//! Contains the logic related to translate the Pernix's functions to LLVM's
//! functions.

use std::{
    borrow::Cow,
    collections::{BTreeSet, HashMap, HashSet},
    convert::Into,
};

use inkwell::{
    module::Linkage,
    types::BasicType,
    values::{AsValueRef, PointerValue},
};
use pernixc_arena::ID;
use pernixc_component::{
    fields::Fields, function_signature::FunctionSignature,
    implementation::Implementation,
};
use pernixc_ir::{
    address::{Address, Memory},
    control_flow_graph::Block,
    instruction::{Instruction, Jump, Terminator},
    value::{
        literal::{self, Literal, Numeric},
        register::{Assignment, Register},
        Value,
    },
    IR,
};
use pernixc_table::{
    component::{Implements, Name, Parent, SymbolKind},
    DisplayObject, GlobalID,
};
use pernixc_term::{
    generic_arguments::GenericArguments, generic_parameter::GenericParameters,
    instantiation::Instantiation, r#type::Primitive, Model as _,
};
use pernixc_type_system::{
    environment::{Environment, Premise},
    normalizer,
};

use crate::{context::Context, Model};

/// Represents an instantiation of a function.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Call {
    /// Available kinds are:
    ///
    /// - [`SymbolKind::Function`]
    /// - [`SymbolKind::AdtImplementationFunction`]
    /// - [`SymbolKind::TraitImplementationFunction`]
    pub callable_id: GlobalID,

    /// The instantiation of the function.
    pub instantiation: Instantiation<Model>,
}

/// Mpas between the function from Pernix to the LLVM function.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Map<'ctx> {
    llvm_functions_by_key: HashMap<Call, inkwell::values::FunctionValue<'ctx>>,
    extern_functions_by_global_id:
        HashMap<GlobalID, inkwell::values::FunctionValue<'ctx>>,
}

impl<'ctx> Context<'_, 'ctx> {
    fn get_qualified_name(
        &self,
        symbol_kind: SymbolKind,
        callable_id: GlobalID,
        instantiation: &Instantiation<Model>,
    ) -> String {
        match symbol_kind {
            SymbolKind::Function => {
                let qualified_name =
                    self.table().get_qualified_name(callable_id);

                let generic_params = self
                    .table()
                    .query::<GenericParameters>(callable_id)
                    .unwrap();

                let mut generic_arguments = instantiation
                    .create_generic_arguments(callable_id, &generic_params)
                    .unwrap();

                self.normalize_generic_arguments(&mut generic_arguments);

                format!("{}{}", qualified_name, DisplayObject {
                    table: self.table(),
                    display: &generic_arguments
                })
            }

            SymbolKind::TraitImplementationFunction
            | SymbolKind::AdtImplementationFunction => {
                let parent_implementation_id = GlobalID::new(
                    callable_id.target_id,
                    self.table().get::<Parent>(callable_id).parent.unwrap(),
                );
                let implemented_id =
                    self.table().get::<Implements>(parent_implementation_id).0;

                let mut implemented_generic_args =
                    GenericArguments::from_default_model(
                        self.table()
                            .query::<Implementation>(parent_implementation_id)
                            .unwrap()
                            .generic_arguments
                            .clone(),
                    );
                implemented_generic_args.instantiate(instantiation);
                self.normalize_generic_arguments(&mut implemented_generic_args);

                let function_generic_params = self
                    .table()
                    .query::<GenericParameters>(callable_id)
                    .unwrap();

                let mut function_generic_args = instantiation
                    .create_generic_arguments(
                        callable_id,
                        &function_generic_params,
                    )
                    .unwrap();
                self.normalize_generic_arguments(&mut function_generic_args);

                format!(
                    "{}{}{}{}",
                    self.table().get_qualified_name(implemented_id),
                    DisplayObject {
                        table: self.table(),
                        display: &implemented_generic_args
                    },
                    self.table().get::<Name>(callable_id).0,
                    DisplayObject {
                        table: self.table(),
                        display: &function_generic_args
                    }
                )
            }

            kind => panic!("unexpected symbol kind: {kind:?}"),
        }
    }

    /// Creates an extern function.
    pub fn get_extern_function(
        &mut self,
        callable_id: GlobalID,
    ) -> inkwell::values::FunctionValue<'ctx> {
        if let Some(llvm_function) = self
            .function_map()
            .extern_functions_by_global_id
            .get(&callable_id)
            .copied()
        {
            return llvm_function;
        }

        let function_signature =
            self.table().query::<FunctionSignature>(callable_id).unwrap();

        let return_ty = self.get_type(self.monomorphize_term(
            Model::from_default_type(function_signature.return_type.clone()),
            &Instantiation::default(),
        ));

        let param_tys = function_signature
            .parameter_order
            .iter()
            .copied()
            .map(|x| &function_signature.parameters[x])
            .map(|param| {
                self.get_type(self.monomorphize_term(
                    Model::from_default_type(param.r#type.clone()),
                    &Instantiation::default(),
                ))
            })
            .collect::<Vec<_>>();

        let llvm_function_type = return_ty.fn_type(
            &param_tys.iter().copied().map(Into::into).collect::<Vec<_>>(),
            false,
        );

        let name = self.table().get::<Name>(callable_id).0.clone();

        let llvm_function_value = self.module().add_function(
            &name,
            llvm_function_type,
            Some(Linkage::External),
        );

        self.function_map_mut()
            .extern_functions_by_global_id
            .insert(callable_id, llvm_function_value);

        llvm_function_value
    }

    /// Gets the function from the map or creates it.
    pub fn get_function(
        &mut self,
        key: &Call,
    ) -> inkwell::values::FunctionValue<'ctx> {
        if let Some(llvm_function) =
            self.function_map().llvm_functions_by_key.get(key).copied()
        {
            return llvm_function;
        }

        let symbol_kind = *self.table().get::<SymbolKind>(key.callable_id);
        let qualified_name = self.get_qualified_name(
            symbol_kind,
            key.callable_id,
            &key.instantiation,
        );

        let function_signature =
            self.table().query::<FunctionSignature>(key.callable_id).unwrap();

        let return_ty = self.get_type(self.monomorphize_term(
            Model::from_default_type(function_signature.return_type.clone()),
            &key.instantiation,
        ));
        let param_tys = function_signature
            .parameter_order
            .iter()
            .copied()
            .map(|x| &function_signature.parameters[x])
            .map(|param| {
                self.get_type(self.monomorphize_term(
                    Model::from_default_type(param.r#type.clone()),
                    &key.instantiation,
                ))
            })
            .collect::<Vec<_>>();

        let llvm_function_type = return_ty.fn_type(
            &param_tys.iter().copied().map(Into::into).collect::<Vec<_>>(),
            false,
        );
        let llvm_function_value = self.module().add_function(
            &qualified_name,
            llvm_function_type,
            None,
        );

        self.function_map_mut()
            .llvm_functions_by_key
            .insert(key.clone(), llvm_function_value);

        let pernix_ir = self.table().query::<IR>(key.callable_id).unwrap();
        let entry = pernix_ir.control_flow_graph.entry_block_id();

        let mut builder = Builder::new(
            self,
            key.callable_id,
            &key.instantiation,
            &function_signature,
            &pernix_ir,
            llvm_function_value,
            param_tys,
            return_ty,
        );

        builder.build_basic_block(entry);

        llvm_function_value
    }
}

struct Builder<'rctx, 'ctx, 'i, 'k> {
    context: &'rctx mut Context<'i, 'ctx>,
    callable_id: GlobalID,
    instantiation: &'k Instantiation<Model>,
    function_signature: &'k FunctionSignature,
    function_ir: &'k IR,

    function_value: inkwell::values::FunctionValue<'ctx>,

    return_type: inkwell::types::BasicTypeEnum<'ctx>,
    parameter_types: Vec<inkwell::types::BasicTypeEnum<'ctx>>,

    #[allow(clippy::struct_field_names)]
    inkwell_builder: inkwell::builder::Builder<'ctx>,

    basic_block_map:
        HashMap<ID<Block<Model>>, inkwell::basic_block::BasicBlock<'ctx>>,

    address_map: HashMap<Memory<Model>, inkwell::values::BasicValueEnum<'ctx>>,

    environment: Environment<'i, Model, normalizer::NoOp>,
    built: HashSet<ID<Block<Model>>>,

    register_map:
        HashMap<ID<Register<Model>>, inkwell::values::BasicValueEnum<'ctx>>,
}

impl<'rctx, 'ctx, 'i, 'k> Builder<'rctx, 'ctx, 'i, 'k> {
    #[allow(clippy::too_many_arguments)]
    fn new(
        context: &'rctx mut Context<'i, 'ctx>,
        callable_id: GlobalID,
        instantiation: &'k Instantiation<Model>,
        function_signature: &'k FunctionSignature,
        function_ir: &'k IR,
        function_value: inkwell::values::FunctionValue<'ctx>,
        parameter_types: Vec<inkwell::types::BasicTypeEnum<'ctx>>,
        return_type: inkwell::types::BasicTypeEnum<'ctx>,
    ) -> Self {
        let mut basic_block_map = HashMap::default();
        let mut address_map = HashMap::default();

        let builder = context.context().create_builder();

        for id in function_ir.control_flow_graph.blocks().ids() {
            let bb = context
                .context()
                .append_basic_block(function_value, &format!("block_{id:?}"));

            basic_block_map.insert(id, bb);
        }

        builder.position_at_end(
            basic_block_map[&function_ir.control_flow_graph.entry_block_id()],
        );

        // move the parameters to the alloca
        for (index, (param, id)) in parameter_types
            .iter()
            .zip(function_signature.parameter_order.iter().copied())
            .enumerate()
        {
            let alloca = builder
                .build_alloca(*param, &format!("param_{index}"))
                .unwrap();

            builder
                .build_store(
                    alloca,
                    function_value
                        .get_nth_param(index.try_into().unwrap())
                        .unwrap(),
                )
                .unwrap();

            address_map.insert(Memory::Parameter(id), alloca.into());
        }

        // create allocas for all the local variables
        for (pernixc_alloca_id, pernix_alloca) in
            function_ir.values.allocas.iter()
        {
            let alloca = builder
                .build_alloca(
                    context.get_type(context.monomorphize_term(
                        pernix_alloca.r#type.clone(),
                        instantiation,
                    )),
                    &format!("alloca_{pernixc_alloca_id:?}"),
                )
                .unwrap();

            address_map
                .insert(Memory::Alloca(pernixc_alloca_id), alloca.into());
        }

        let table = context.table();

        Self {
            context,
            callable_id,
            instantiation,
            function_signature,
            function_ir,
            function_value,
            return_type,
            parameter_types,
            inkwell_builder: builder,
            basic_block_map,
            address_map,
            environment: Environment::new(
                Cow::Owned(Premise::<Model> {
                    predicates: BTreeSet::default(),
                    query_site: Some(callable_id),
                }),
                table,
                normalizer::NO_OP,
            ),
            built: HashSet::default(),
            register_map: HashMap::default(),
        }
    }
}

impl<'ctx> Builder<'_, 'ctx, '_, '_> {
    /// Gets the LLVM integer value of the given type.
    fn get_integer_value(
        &self,
        value: u64,
        ty: Primitive,
    ) -> inkwell::values::BasicValueEnum<'ctx> {
        let llvm_int_type = match ty {
            Primitive::Int8 | Primitive::Uint8 => {
                self.context.context().i8_type()
            }
            Primitive::Int16 | Primitive::Uint16 => {
                self.context.context().i16_type()
            }
            Primitive::Int32 | Primitive::Uint32 => {
                self.context.context().i32_type()
            }
            Primitive::Int64 | Primitive::Uint64 => {
                self.context.context().i64_type()
            }
            Primitive::Usize | Primitive::Isize => self
                .context
                .context()
                .ptr_sized_int_type(self.context.target_data(), None),

            _ => unreachable!(),
        };

        llvm_int_type.const_int(value, false).into()
    }

    /// Translates numeric literal to LLVM value.
    fn get_numeric_value(
        &self,
        numeric_value: &Numeric<Model>,
    ) -> inkwell::values::BasicValueEnum<'ctx> {
        let primitive_type = numeric_value.r#type.as_primitive().unwrap();

        match primitive_type {
            Primitive::Int8
            | Primitive::Int16
            | Primitive::Int32
            | Primitive::Int64
            | Primitive::Uint8
            | Primitive::Uint16
            | Primitive::Uint32
            | Primitive::Uint64
            | Primitive::Isize
            | Primitive::Usize => self.get_integer_value(
                numeric_value.integer_string.parse::<u64>().unwrap(),
                *primitive_type,
            ),

            Primitive::Float32 | Primitive::Float64 => {
                let value = numeric_value.decimal_stirng.as_ref().map_or_else(
                    || numeric_value.integer_string.parse::<f64>().unwrap(),
                    |decimal_string| {
                        format!(
                            "{}.{}",
                            numeric_value.integer_string, decimal_string
                        )
                        .parse::<f64>()
                        .unwrap()
                    },
                );

                let llvm_float_type = match primitive_type {
                    Primitive::Float32 => self.context.context().f32_type(),
                    Primitive::Float64 => self.context.context().f64_type(),
                    _ => unreachable!(),
                };

                llvm_float_type.const_float(value).into()
            }

            Primitive::Bool => unreachable!(),
        }
    }

    fn get_string(
        &self,
        string: &literal::String,
    ) -> inkwell::values::BasicValueEnum<'ctx> {
        todo!()
    }

    /// Translates the value to LLVM value.
    fn get_value(
        &self,
        value: &Value<Model>,
    ) -> inkwell::values::BasicValueEnum<'ctx> {
        match value {
            Value::Register(id) => self.register_map[id],
            Value::Literal(literal) => match literal {
                Literal::Unit(unit) => todo!(),
                Literal::String(string) => self.get_string(string),
                Literal::Character(character) => self.get_integer_value(
                    character.character as u64,
                    *character.r#type.as_primitive().unwrap(),
                ),
                Literal::Unreachable(unreachable) => todo!(),
                Literal::Phantom(phantom) => {
                    todo!()
                }
                Literal::Numeric(numeric) => self.get_numeric_value(numeric),
                Literal::Boolean(boolean) => todo!(),
                Literal::Error(error) => todo!(),
            },
        }
    }

    /// Gets the LLVM address value of the given address.
    fn get_address_value(
        &mut self,
        address: &Address<Model>,
    ) -> inkwell::values::BasicValueEnum<'ctx> {
        match address {
            Address::Memory(memory) => self.address_map[memory],

            Address::Field(field) => {
                let struct_ty = self
                    .function_ir
                    .values
                    .type_of_address(
                        &field.struct_address,
                        self.callable_id,
                        &self.environment,
                    )
                    .unwrap()
                    .result;

                let struct_id = struct_ty.as_symbol().unwrap().id;
                let fields = self
                    .context
                    .table()
                    .query::<Fields>(struct_ty.as_symbol().unwrap().id)
                    .unwrap();

                let llvm_ty = self.context.get_type(struct_ty);
                let base_address =
                    self.get_address_value(&field.struct_address);

                let index = fields
                    .field_declaration_order
                    .iter()
                    .position(|&f| f == field.id)
                    .unwrap();

                self.inkwell_builder
                    .build_struct_gep(
                        llvm_ty,
                        base_address.into_pointer_value(),
                        index.try_into().unwrap(),
                        &format!(
                            "struct_{:?}_field_{:?}_gep",
                            struct_id, field.id
                        ),
                    )
                    .unwrap()
                    .into()
            }

            Address::Tuple(tuple) => todo!(),

            Address::Index(index) => {
                let base_address = self.get_address_value(&index.array_address);

                let pointee_type = self.context.get_type(
                    self.context.monomorphize_term(
                        self.function_ir
                            .values
                            .type_of_address(
                                &index.array_address,
                                self.callable_id,
                                &self.environment,
                            )
                            .unwrap()
                            .result,
                        self.instantiation,
                    ),
                );

                let index_value = self.get_value(&index.indexing_value);

                unsafe {
                    self.inkwell_builder
                        .build_gep(
                            pointee_type,
                            PointerValue::new(base_address.as_value_ref()),
                            &[
                                self.context.context().i64_type().const_zero(),
                                index_value.into_int_value(),
                            ],
                            "indexing",
                        )
                        .unwrap()
                        .into()
                }
            }
            Address::Variant(variant) => todo!(),
            Address::Reference(reference) => {
                let pointee_address =
                    self.get_address_value(&reference.reference_address);

                let pointee_type = self.type_of_address(address);

                self.inkwell_builder
                    .build_load(
                        pointee_type,
                        pointee_address.into_pointer_value(),
                        "dereference",
                    )
                    .unwrap()
            }
        }
    }

    fn build_store(&mut self, store: &pernixc_ir::instruction::Store<Model>) {
        let address = self.get_address_value(&store.address);
        let value = self.get_value(&store.value);

        self.inkwell_builder
            .build_store(address.into_pointer_value(), value)
            .unwrap();
    }

    fn type_of_address(
        &mut self,
        address: &Address<Model>,
    ) -> inkwell::types::BasicTypeEnum<'ctx> {
        let ty = self.function_ir.values.type_of_address(
            address,
            self.callable_id,
            &self.environment,
        );

        self.context.get_type(
            self.context
                .monomorphize_term(ty.unwrap().result, self.instantiation),
        )
    }

    fn type_of_register(
        &mut self,
        register_id: ID<Register<Model>>,
    ) -> inkwell::types::BasicTypeEnum<'ctx> {
        let ty = self.function_ir.values.type_of_register(
            register_id,
            self.callable_id,
            &self.environment,
        );

        self.context.get_type(
            self.context
                .monomorphize_term(ty.unwrap().result, self.instantiation),
        )
    }

    #[allow(clippy::too_many_lines)]
    fn build_register_assignment(
        &mut self,
        register_assignment: pernixc_ir::instruction::RegisterAssignment<Model>,
    ) {
        let reg = &self.function_ir.values.registers[register_assignment.id];

        let value = match &reg.assignment {
            Assignment::Tuple(tuple) => {
                todo!()
            }
            Assignment::Load(load) => {
                let ptr = self.get_address_value(&load.address);
                let pointee_ty = self.type_of_address(&load.address);

                self.inkwell_builder
                    .build_load(
                        pointee_ty,
                        ptr.into_pointer_value(),
                        &format!("load_{:?}", register_assignment.id),
                    )
                    .unwrap()
            }
            Assignment::Borrow(borrow) => {
                self.get_address_value(&borrow.address)
            }
            Assignment::Prefix(prefix) => todo!(),
            Assignment::Struct(struct_lit) => {
                let struct_ty = self.type_of_register(register_assignment.id);
                let tmp = self
                    .inkwell_builder
                    .build_alloca(
                        struct_ty,
                        &format!("tmp_struct_{:?}", register_assignment.id),
                    )
                    .unwrap();

                let fields = self
                    .context
                    .table()
                    .query::<Fields>(struct_lit.struct_id)
                    .unwrap();

                for (index, field_id) in
                    fields.field_declaration_order.iter().copied().enumerate()
                {
                    let field = &struct_lit.initializers_by_field_id[&field_id];
                    let pointer_value = self
                        .inkwell_builder
                        .build_struct_gep(
                            struct_ty,
                            tmp,
                            index.try_into().unwrap(),
                            &format!(
                                "init_struct_{:?}_field_{:?}_gep",
                                register_assignment.id, field_id
                            ),
                        )
                        .unwrap();

                    let value = self.get_value(field);
                    self.inkwell_builder
                        .build_store(pointer_value, value)
                        .unwrap();
                }

                self.inkwell_builder
                    .build_load(
                        struct_ty,
                        tmp,
                        &format!(
                            "load_tmp_struct_{:?}_lit",
                            register_assignment.id
                        ),
                    )
                    .unwrap()
            }
            Assignment::Variant(variant) => {
                todo!()
            }
            Assignment::FunctionCall(function_call) => {
                let symbol_kind = *self
                    .context
                    .table()
                    .get::<SymbolKind>(function_call.callable_id);

                match symbol_kind {
                    SymbolKind::ExternFunction => {
                        let llvm_function = self
                            .context
                            .get_extern_function(function_call.callable_id);

                        let args = function_call
                            .arguments
                            .iter()
                            .map(|arg| self.get_value(arg).into())
                            .collect::<Vec<_>>();

                        self.inkwell_builder
                            .build_call(llvm_function, &args, "call")
                            .unwrap()
                            .try_as_basic_value()
                            .left()
                            .unwrap()
                    }
                    _ => todo!(),
                }
            }
            Assignment::Binary(binary) => todo!(),
            Assignment::Array(array) => {
                let array_ty = self.type_of_register(register_assignment.id);
                let tmp = self
                    .inkwell_builder
                    .build_alloca(
                        array_ty,
                        &format!("tmp_array_{:?}", register_assignment.id),
                    )
                    .unwrap();

                for (index, element) in array.elements.iter().enumerate() {
                    let pointer_value = unsafe {
                        self.inkwell_builder
                            .build_gep(
                                array_ty,
                                tmp,
                                &[
                                    self.context
                                        .context()
                                        .i64_type()
                                        .const_zero(),
                                    self.context
                                        .context()
                                        .i64_type()
                                        .const_int(
                                            index.try_into().unwrap(),
                                            false,
                                        ),
                                ],
                                &format!(
                                    "init_array_{:?}_index_{:?}_gep",
                                    register_assignment.id, index
                                ),
                            )
                            .unwrap()
                    };

                    let value = self.get_value(element);
                    self.inkwell_builder
                        .build_store(pointer_value, value)
                        .unwrap();
                }

                self.inkwell_builder
                    .build_load(
                        array_ty,
                        tmp,
                        &format!(
                            "load_tmp_array_{:?}_lit",
                            register_assignment.id
                        ),
                    )
                    .unwrap()
            }
            Assignment::Phi(phi) => todo!(),
            Assignment::Cast(cast) => todo!(),
            Assignment::VariantNumber(variant_number) => todo!(),
        };

        self.register_map.insert(register_assignment.id, value);
    }

    fn build_basic_block(&mut self, block_id: ID<Block<Model>>) {
        if !self.built.insert(block_id) {
            return;
        }

        let current_block = self.basic_block_map[&block_id];
        self.inkwell_builder.position_at_end(current_block);

        let block = &self.function_ir.control_flow_graph.blocks()[block_id];

        for instruction in block.instructions() {
            match instruction {
                Instruction::Store(store) => self.build_store(store),
                Instruction::RegisterAssignment(register_assignment) => {
                    self.build_register_assignment(*register_assignment);
                }
                Instruction::RegisterDiscard(register_discard) => {}
                Instruction::TuplePack(tuple_pack) => {
                    todo!()
                }
                Instruction::DropUnpackTuple(drop_unpack_tuple) => todo!(),
                Instruction::Drop(drop) => {}

                Instruction::ScopePush(_) | Instruction::ScopePop(_) => {}
            }
        }

        // build the terminator
        match block.terminator() {
            Some(Terminator::Panic) => {
                self.inkwell_builder.build_unreachable().unwrap();
            }

            Some(Terminator::Return(ret)) => {
                let val = self.get_value(&ret.value);
                self.inkwell_builder.build_return(Some(&val)).unwrap();
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
                let condition = self.get_value(&jump.condition);

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

            None => todo!(),
        }
    }
}
