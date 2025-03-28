//! Contains the logic related to translate the Pernix's functions to LLVM's
//! functions.

use std::{
    borrow::Cow,
    collections::{BTreeSet, HashMap, HashSet},
    convert::Into,
    rc::Rc,
};

use derive_new::new;
use enum_as_inner::EnumAsInner;
use inkwell::{
    attributes::{Attribute, AttributeLoc},
    basic_block::BasicBlock,
    module::Linkage,
    types::{AnyType, BasicMetadataTypeEnum, BasicType, BasicTypeEnum},
    values::FunctionValue,
    AddressSpace,
};
use pernixc_arena::ID;
use pernixc_component::{
    function_signature::{FunctionSignature, Parameter},
    implementation::Implementation,
};
use pernixc_ir::{
    address::{Address, Memory},
    control_flow_graph::Block,
    value::{register::Register, Value},
    IR,
};
use pernixc_table::{
    component::{Extern, Implements, Name, Parent, SymbolKind},
    DisplayObject, GlobalID, TargetID,
};
use pernixc_term::{
    generic_arguments::GenericArguments, generic_parameter::GenericParameters,
    instantiation::Instantiation, r#type::Type, Model as _, Symbol, Tuple,
};
use pernixc_type_system::{
    environment::{Environment, Premise},
    normalizer,
};

use crate::{context::Context, r#type::IsAggregateTypeExt, zst::Zst, Model};

mod address;
mod drop;
mod instruction;
mod literal;

/// Represents the return type of the function.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReturnType<'ctx> {
    /// The function returns nothing, i.e., `void`.
    Void,

    /// The function returns scalar value (i.e., not a struct).
    Scalar(BasicTypeEnum<'ctx>),

    /// The function return type is an aggregate type. The return value must
    /// be provided as a pointer to the first parameter of the function.
    Sret(BasicTypeEnum<'ctx>),
}

/// Represents the parameter type of the function.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParamterType<'ctx> {
    /// The parameter is the pointer to the aggregate type and is attributed
    /// with `byval` attribute. The type stored in this variant here is not
    /// the pointer type but the actual type of the aggregate.
    AggregateByVal(BasicTypeEnum<'ctx>),

    /// A scalar type.
    Scalar(BasicTypeEnum<'ctx>),
}

/// Represents the translation from Pernix's function signature to LLVM's.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LlvmFunctionSignature<'ctx> {
    /// The return type of the function.
    pub return_type: ReturnType<'ctx>,

    /// The parameter types of the function. The `Zero-Sized type` will be
    /// filtered out. Note that the actual parameter counts created by the LLVM
    /// might not match the original Pernix's function signature but the
    /// ordering will be preserved.
    ///
    /// This parameter type list does not include the `sret` parameter in case
    /// of [`Self::return_type`] is [`ReturnType::Sret`].
    pub parameter_types: Vec<ParamterType<'ctx>>,

    /// The llvm function value.
    ///
    /// As stated, the actual parameter counts created by the LLVM might not
    /// match the original Pernix's function signature.
    pub llvm_function_value: FunctionValue<'ctx>,

    /// Maps the parameter ID in the Pernix to the parameter index in the LLVM
    /// function value. In case of the `sret` parameter, the index is already
    /// offset by 1.
    pub llvm_parameter_indices_by_index: HashMap<ID<Parameter>, usize>,
}

impl<'ctx> LlvmFunctionSignature<'ctx> {
    /// Gets the parameter index in the LLVM function value.
    #[must_use]
    pub fn get_llvm_parameter_type(
        &self,
        parameter_id: ID<Parameter>,
    ) -> Option<(ParamterType<'ctx>, usize)> {
        let index =
            self.llvm_parameter_indices_by_index.get(&parameter_id).copied()?;
        let is_sret = matches!(self.return_type, ReturnType::Sret(_));

        Some((self.parameter_types[index - usize::from(is_sret)], index))
    }
}

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
    llvm_functions_by_key: HashMap<Call, Rc<LlvmFunctionSignature<'ctx>>>,
    extern_functions_by_global_id:
        HashMap<GlobalID, Rc<LlvmFunctionSignature<'ctx>>>,

    tuple_drop: HashMap<Tuple<Type<Model>>, Option<FunctionValue<'ctx>>>,
    adt_drop: HashMap<Symbol<Model>, Option<FunctionValue<'ctx>>>,
    array_drop: HashMap<(Type<Model>, u64), Option<FunctionValue<'ctx>>>,
}

impl<'ctx> Context<'_, 'ctx> {
    fn get_function_qualified_name(
        &self,
        symbol_kind: SymbolKind,
        callable_id: GlobalID,
        instantiation: &Instantiation<Model>,
    ) -> String {
        match symbol_kind {
            SymbolKind::Function => {
                if callable_id == self.main_function_id() {
                    return "core::main".to_string();
                }

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
                    "{}{}::{}{}",
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

    fn create_intrinsic_function(
        &mut self,
        llvm_function_signature: &Rc<LlvmFunctionSignature<'ctx>>,
        key: &Call,
    ) {
        let entry_block = self.context().append_basic_block(
            llvm_function_signature.llvm_function_value,
            "entry",
        );
        let builder = self.context().create_builder();
        builder.position_at_end(entry_block);

        match self.table().get::<Name>(key.callable_id).0.as_str() {
            "sizeof" => {
                let ty = key.instantiation.types.values().next().unwrap();
                let llvm_ty = self.get_type(ty.clone());

                let size = llvm_ty.map_or(0, |llvm_ty| {
                    self.target_data().get_abi_size(&llvm_ty)
                });

                let size = self
                    .context()
                    .ptr_sized_int_type(self.target_data(), None)
                    .const_int(size, false);

                builder.build_return(Some(&size)).unwrap();
            }

            "alignof" => {
                let ty = key.instantiation.types.values().next().unwrap();
                let llvm_ty = self.get_type(ty.clone());

                let align = llvm_ty.map_or(0, |llvm_ty| {
                    u64::from(self.target_data().get_abi_alignment(&llvm_ty))
                });

                let align = self
                    .context()
                    .ptr_sized_int_type(self.target_data(), None)
                    .const_int(align, false);

                builder.build_return(Some(&align)).unwrap();
            }

            "dropAt" => {
                let ty = key.instantiation.types.values().next().unwrap();

                let drop = self.get_drop(ty.clone());

                if let Some(drop) = drop {
                    let param_value = llvm_function_signature
                        .llvm_function_value
                        .get_nth_param(0)
                        .unwrap();

                    builder
                        .build_call(drop, &[param_value.into()], "call_drop")
                        .unwrap();
                }

                builder.build_return(None).unwrap();
            }

            _ => unreachable!(),
        }
    }

    #[allow(clippy::too_many_lines)]
    fn create_function_value(
        &mut self,
        function_signature: &FunctionSignature,
        instantiation: &Instantiation<Model>,
        var_args: bool,
        name: &str,
        linkage: Option<Linkage>,
    ) -> LlvmFunctionSignature<'ctx> {
        // NOTE: because of filtering out the zst types, the parameter types
        // might not have the same number of parameters as the original pernix's
        // function  signature
        let mut llvm_parameter_indices_by_index = HashMap::default();
        let mut llvm_parameter_types =
            Vec::with_capacity(function_signature.parameter_order.len());

        let return_ty = {
            let ty = self.get_type(self.monomorphize_term(
                Model::from_default_type(
                    function_signature.return_type.clone(),
                ),
                instantiation,
            ));

            match ty {
                Ok(ty) => {
                    if ty.is_aggregate() {
                        ReturnType::Sret(ty)
                    } else {
                        ReturnType::Scalar(ty)
                    }
                }
                Err(Zst) => ReturnType::Void,
            }
        };

        for param_id in function_signature.parameter_order.iter().copied() {
            let param = &function_signature.parameters[param_id];
            let ty = self.get_type(self.monomorphize_term(
                Model::from_default_type(param.r#type.clone()),
                instantiation,
            ));

            if let Ok(basic_type_enum) = ty {
                llvm_parameter_indices_by_index.insert(
                    param_id,
                    llvm_parameter_types.len()
                        + usize::from(matches!(return_ty, ReturnType::Sret(_))),
                );

                llvm_parameter_types.push(if basic_type_enum.is_aggregate() {
                    ParamterType::AggregateByVal(basic_type_enum)
                } else {
                    ParamterType::Scalar(basic_type_enum)
                });
            }
        }

        let mut actual_llvm_parameters: Vec<BasicMetadataTypeEnum> =
            Vec::with_capacity(
                llvm_parameter_types.len()
                    + usize::from(matches!(return_ty, ReturnType::Sret(_))),
            );

        // add the sret parameter if the return type is sret
        if let ReturnType::Sret(_) = return_ty {
            actual_llvm_parameters
                .push(self.context().ptr_type(AddressSpace::default()).into());
        }

        // add the rest of the parameters
        for param_ty in llvm_parameter_types.iter().copied() {
            match param_ty {
                ParamterType::AggregateByVal(_) => {
                    actual_llvm_parameters.push(
                        self.context().ptr_type(AddressSpace::default()).into(),
                    );
                }
                ParamterType::Scalar(ty) => {
                    actual_llvm_parameters.push(ty.into());
                }
            }
        }

        let function_ty = match return_ty {
            ReturnType::Sret(_) | ReturnType::Void => self
                .context()
                .void_type()
                .fn_type(&actual_llvm_parameters, var_args),
            ReturnType::Scalar(basic_type_enum) => {
                basic_type_enum.fn_type(&actual_llvm_parameters, var_args)
            }
        };

        let function_value =
            self.module().add_function(name, function_ty, linkage);

        // add the attributes for the sret parameter
        if let ReturnType::Sret(return_ty) = return_ty {
            function_value.add_attribute(
                AttributeLoc::Param(0),
                self.create_type_attribute("sret", return_ty),
            );
        }

        // add the attributes for the byval parameters
        for (mut index, param_ty) in llvm_parameter_types.iter().enumerate() {
            // offset by 1 for the sret parameter
            index += usize::from(matches!(return_ty, ReturnType::Sret(_)));

            if let ParamterType::AggregateByVal(ty) = param_ty {
                function_value.add_attribute(
                    AttributeLoc::Param(index.try_into().unwrap()),
                    self.create_type_attribute("byval", *ty),
                );
            }
        }

        LlvmFunctionSignature {
            return_type: return_ty,
            parameter_types: llvm_parameter_types,
            llvm_function_value: function_value,
            llvm_parameter_indices_by_index,
        }
    }

    /// Creates an extern function.
    pub fn get_extern_function(
        &mut self,
        callable_id: GlobalID,
    ) -> Rc<LlvmFunctionSignature<'ctx>> {
        if let Some(llvm_function) = self
            .function_map()
            .extern_functions_by_global_id
            .get(&callable_id)
            .cloned()
        {
            return llvm_function;
        }

        let function_signature =
            self.table().query::<FunctionSignature>(callable_id).unwrap();

        let ext = *self.table().get::<Extern>(callable_id);
        let name = self.table().get::<Name>(callable_id).0.clone();

        let llvm_function_sign = Rc::new(self.create_function_value(
            &function_signature,
            &Instantiation::default(),
            match ext {
                Extern::C(extern_c) => extern_c.var_args,

                Extern::Unknown => unreachable!("unknown extern function"),
            },
            &name,
            Some(Linkage::External),
        ));

        self.function_map_mut()
            .extern_functions_by_global_id
            .insert(callable_id, llvm_function_sign.clone());

        llvm_function_sign
    }

    /// Gets the function from the map or creates it.
    pub fn get_function(
        &mut self,
        key: &Call,
    ) -> Rc<LlvmFunctionSignature<'ctx>> {
        if let Some(llvm_function) =
            self.function_map().llvm_functions_by_key.get(key).cloned()
        {
            return llvm_function;
        }

        let symbol_kind = *self.table().get::<SymbolKind>(key.callable_id);
        let qualified_name = self.get_function_qualified_name(
            symbol_kind,
            key.callable_id,
            &key.instantiation,
        );

        let function_signature =
            self.table().query::<FunctionSignature>(key.callable_id).unwrap();

        let llvm_function_signatue = Rc::new(self.create_function_value(
            &function_signature,
            &key.instantiation,
            false,
            &qualified_name,
            Some(Linkage::Private),
        ));

        self.function_map_mut()
            .llvm_functions_by_key
            .insert(key.clone(), llvm_function_signatue.clone());

        // build intrinsic function
        if key.callable_id.target_id == TargetID::CORE {
            self.create_intrinsic_function(&llvm_function_signatue, key);
        } else {
            let pernix_ir = self.table().query::<IR>(key.callable_id).unwrap();

            let mut builder = Builder::new(
                self,
                key.callable_id,
                &key.instantiation,
                &function_signature,
                &pernix_ir,
                llvm_function_signatue.clone(),
            );

            for block_id in pernix_ir.control_flow_graph.blocks().ids() {
                builder.build_basic_block(block_id);
            }

            // connect alloca entry block to the first block
            builder.inkwell_builder.position_at_end(builder.llvm_entry_block);
            builder
                .inkwell_builder
                .build_unconditional_branch(
                    builder.basic_block_map
                        [&pernix_ir.control_flow_graph.entry_block_id()],
                )
                .unwrap();
        }

        llvm_function_signatue
    }
}

struct Builder<'rctx, 'ctx, 'i, 'k> {
    context: &'rctx mut Context<'i, 'ctx>,
    callable_id: GlobalID,
    instantiation: &'k Instantiation<Model>,

    /// The entry block of the function.
    ///
    /// This block does not coorespond to any block in the Pernix's IR. This is
    /// purely used for allocating all the `alloca` instructions required
    /// for the function. Moreover, the reason why this block is created is
    /// created is that the in Pernix's IR, the entry block might be looped
    /// back to itself, therefore, it's not ideal to allocate the `alloca`
    /// instructions in the Pernix's entry block.
    llvm_entry_block: BasicBlock<'ctx>,

    #[allow(unused)]
    function_signature: &'k FunctionSignature,
    function_ir: &'k IR,

    #[allow(unused)]
    llvm_function_signature: Rc<LlvmFunctionSignature<'ctx>>,

    #[allow(clippy::struct_field_names)]
    inkwell_builder: inkwell::builder::Builder<'ctx>,

    basic_block_map:
        HashMap<ID<Block<Model>>, inkwell::basic_block::BasicBlock<'ctx>>,

    address_map: HashMap<Memory<Model>, LlvmAddress<'ctx>>,

    environment: Environment<'i, Model, normalizer::NoOp>,
    built: HashSet<ID<Block<Model>>>,

    register_map: HashMap<ID<Register<Model>>, Option<LlvmValue<'ctx>>>,
}

/// Represents the LLVM's memory address with the underlying type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, new)]
pub struct LlvmAddress<'ctx> {
    /// The memory address.
    pub address: inkwell::values::PointerValue<'ctx>,

    /// The underlying type of the memory address.
    pub r#type: BasicTypeEnum<'ctx>,
}

/// Represents the translateion from Pernix's value to LLVM's value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, EnumAsInner, derive_more::From)]
pub enum LlvmValue<'ctx> {
    /// The value is converted successfully, default case
    Scalar(inkwell::values::BasicValueEnum<'ctx>),

    /// The value is an aggregate type and its content is stored in the given
    /// memory.
    TmpAggegate(LlvmAddress<'ctx>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Error {
    /// Reaches to an unreachable (somehowe), therefore, must stop building the
    /// current block immediately.
    Unreachable,
}

impl<'ctx> Context<'_, 'ctx> {
    fn create_type_attribute(
        &self,
        name: &str,
        ty: BasicTypeEnum<'ctx>,
    ) -> Attribute {
        let sret_attribute_id = Attribute::get_named_enum_kind_id(name);

        self.context()
            .create_type_attribute(sret_attribute_id, ty.as_any_type_enum())
    }
}

impl<'rctx, 'ctx, 'i, 'k> Builder<'rctx, 'ctx, 'i, 'k> {
    #[allow(clippy::too_many_arguments)]
    fn new(
        context: &'rctx mut Context<'i, 'ctx>,
        callable_id: GlobalID,
        instantiation: &'k Instantiation<Model>,
        function_signature: &'k FunctionSignature,
        function_ir: &'k IR,
        llvm_function_signature: Rc<LlvmFunctionSignature<'ctx>>,
    ) -> Self {
        let mut basic_block_map = HashMap::default();
        let mut address_map = HashMap::default();

        let entry_block = context.context().append_basic_block(
            llvm_function_signature.llvm_function_value,
            "entry",
        );
        let builder = context.context().create_builder();

        // llvm decides that the first block created is the entry block
        for id in function_ir.control_flow_graph.blocks().ids() {
            let bb = context.context().append_basic_block(
                llvm_function_signature.llvm_function_value,
                &format!("block_{id:?}"),
            );

            basic_block_map.insert(id, bb);
        }

        builder.position_at_end(entry_block);

        // move the parameters to the alloca
        for param_id in function_signature.parameter_order.iter().copied() {
            let Some((parameter, index)) =
                llvm_function_signature.get_llvm_parameter_type(param_id)
            else {
                // ZST, reduced to no-op
                continue;
            };

            let address = match parameter {
                ParamterType::AggregateByVal(aggregate_ty) => LlvmAddress::new(
                    llvm_function_signature
                        .llvm_function_value
                        .get_nth_param(index.try_into().unwrap())
                        .unwrap()
                        .into_pointer_value(),
                    aggregate_ty,
                ),

                ParamterType::Scalar(basic_type_enum) => {
                    // create an memory address for the parameter
                    let alloca = builder
                        .build_alloca(
                            basic_type_enum,
                            &format!("param_{param_id:?}"),
                        )
                        .unwrap();

                    builder
                        .build_store(
                            alloca,
                            llvm_function_signature
                                .llvm_function_value
                                .get_nth_param(index.try_into().unwrap())
                                .unwrap(),
                        )
                        .unwrap();

                    LlvmAddress::new(alloca, basic_type_enum)
                }
            };

            address_map.insert(Memory::Parameter(param_id), address);
        }

        // create allocas for all the local variables
        for (pernixc_alloca_id, pernix_alloca) in
            function_ir.values.allocas.iter()
        {
            let Ok(ty) = context.get_type(context.monomorphize_term(
                pernix_alloca.r#type.clone(),
                instantiation,
            )) else {
                continue;
            };

            let alloca = builder
                .build_alloca(ty, &format!("alloca_{pernixc_alloca_id:?}"))
                .unwrap();

            address_map.insert(
                Memory::Alloca(pernixc_alloca_id),
                LlvmAddress::new(alloca, ty),
            );
        }

        let table = context.table();

        Self {
            llvm_entry_block: entry_block,
            context,
            callable_id,
            instantiation,
            function_signature,
            function_ir,
            llvm_function_signature,
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
    /// Translates the value to LLVM value.
    fn get_value(
        &mut self,
        value: &Value<Model>,
    ) -> Result<Option<LlvmValue<'ctx>>, Error> {
        match value {
            Value::Register(id) => Ok(self.register_map[id]),
            Value::Literal(literal) => self.get_literal_value(literal),
        }
    }

    #[allow(unused)]
    fn type_of_address_pnx(&mut self, address: &Address<Model>) -> Type<Model> {
        let ty = self.function_ir.values.type_of_address(
            address,
            self.callable_id,
            &self.environment,
        );

        self.context.monomorphize_term(ty.unwrap().result, self.instantiation)
    }

    fn type_of_address(
        &mut self,
        address: &Address<Model>,
    ) -> Result<BasicTypeEnum<'ctx>, Zst> {
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
    ) -> Result<BasicTypeEnum<'ctx>, Zst> {
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

    fn type_of_register_pnx(
        &mut self,
        register_id: ID<Register<Model>>,
    ) -> Type<Model> {
        let ty = self.function_ir.values.type_of_register(
            register_id,
            self.callable_id,
            &self.environment,
        );

        self.context.monomorphize_term(ty.unwrap().result, self.instantiation)
    }

    fn type_of_value_pnx(&mut self, value: &Value<Model>) -> Type<Model> {
        let ty = self.function_ir.values.type_of_value(
            value,
            self.callable_id,
            &self.environment,
        );

        self.context.monomorphize_term(ty.unwrap().result, self.instantiation)
    }

    fn create_aggregate_temporary(
        &mut self,
        ty: BasicTypeEnum<'ctx>,
        register_id: ID<Register<Model>>,
    ) -> LlvmAddress<'ctx> {
        let current_block = self.inkwell_builder.get_insert_block().unwrap();
        self.inkwell_builder.position_at_end(self.llvm_entry_block);

        let alloca = self
            .inkwell_builder
            .build_alloca(ty, &format!("tmp_aggregate_{register_id:?}"))
            .unwrap();

        self.inkwell_builder.position_at_end(current_block);

        LlvmAddress::new(alloca, ty)
    }
}
