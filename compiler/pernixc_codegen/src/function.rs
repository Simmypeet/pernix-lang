//! Contains the logic related to translate the Pernix's functions to LLVM's
//! functions.

use std::{
    borrow::Cow,
    collections::{BTreeSet, HashMap, HashSet},
    convert::Into,
    rc::Rc,
};

use enum_as_inner::EnumAsInner;
use inkwell::{
    module::Linkage,
    types::{BasicType, BasicTypeEnum, FunctionType},
    values::FunctionValue,
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
    DisplayObject, GlobalID,
};
use pernixc_term::{
    generic_arguments::GenericArguments, generic_parameter::GenericParameters,
    instantiation::Instantiation, r#type::Type, Model as _,
};
use pernixc_type_system::{
    environment::{Environment, Premise},
    normalizer,
};

use crate::{context::Context, zst::Zst, Model};

mod address;
mod instruction;
mod literal;

/// Represents the translation from Pernix's function signature to LLVM's.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LlvmFunctionSignature<'ctx> {
    /// The return type of the function. If `None`, then the function returns
    /// `void`. None will be returned if the return type is a `Zero-Sized
    /// type`.
    pub return_type: Option<BasicTypeEnum<'ctx>>,

    /// The parameter types of the function. The `Zero-Sized type` will be
    /// filtered out. Note that the actual parameter counts created by the LLVM
    /// might not match the original Pernix's function signature but the
    /// ordering will be preserved.
    pub parameter_types: Vec<BasicTypeEnum<'ctx>>,

    /// The llvm function value.
    ///
    /// As stated, the actual parameter counts created by the LLVM might not
    /// match the original Pernix's function signature.
    pub llvm_function_value: FunctionValue<'ctx>,

    /// Maps the parameter indices to the Pernix's parameter indices.
    pub llvm_parameter_indices_by_index: HashMap<ID<Parameter>, usize>,
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
                    return "main".to_string();
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

    fn create_function_type(
        &mut self,
        function_signature: &FunctionSignature,
        instantiation: &Instantiation<Model>,
        var_args: bool,
    ) -> (
        FunctionType<'ctx>,
        Vec<BasicTypeEnum<'ctx>>,
        Option<BasicTypeEnum<'ctx>>,
        HashMap<ID<Parameter>, usize>,
    ) {
        // NOTE: because of filtering out the zst types, the parameter types
        // might not have the same number of parameters as the original pernix's
        // function  signature
        let mut llvm_parameter_indices_by_index = HashMap::default();
        let mut llvm_parameter_types =
            Vec::with_capacity(function_signature.parameter_order.len());

        for param_id in function_signature.parameter_order.iter().copied() {
            let param = &function_signature.parameters[param_id];
            let ty = self.get_type(self.monomorphize_term(
                Model::from_default_type(param.r#type.clone()),
                instantiation,
            ));

            if let Ok(basic_type_enum) = ty {
                llvm_parameter_indices_by_index
                    .insert(param_id, llvm_parameter_types.len());
                llvm_parameter_types.push(basic_type_enum);
            }
        }

        let return_ty = self.get_type(self.monomorphize_term(
            Model::from_default_type(function_signature.return_type.clone()),
            instantiation,
        ));

        let function_ty = match &return_ty {
            Ok(ty) => ty.fn_type(
                &llvm_parameter_types
                    .iter()
                    .copied()
                    .map(std::convert::Into::into)
                    .collect::<Vec<_>>(),
                var_args,
            ),
            Err(Zst(_)) => self.context().void_type().fn_type(
                &llvm_parameter_types
                    .iter()
                    .copied()
                    .map(std::convert::Into::into)
                    .collect::<Vec<_>>(),
                var_args,
            ),
        };

        (
            function_ty,
            llvm_parameter_types,
            return_ty.ok(),
            llvm_parameter_indices_by_index,
        )
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

        let (llvm_function_type, parameter_types, return_ty, map) = self
            .create_function_type(
                &function_signature,
                &Instantiation::default(),
                match ext {
                    Extern::C(extern_c) => extern_c.var_args,

                    Extern::Unknown => unreachable!("unknown extern function"),
                },
            );

        let llvm_function_value = self.module().add_function(
            &name,
            llvm_function_type,
            Some(Linkage::External),
        );

        let sig = Rc::new(LlvmFunctionSignature {
            return_type: return_ty,
            parameter_types,
            llvm_function_value,
            llvm_parameter_indices_by_index: map,
        });

        self.function_map_mut()
            .extern_functions_by_global_id
            .insert(callable_id, sig.clone());

        sig
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

        let (llvm_function_type, parameter_tys, return_ty, map) = self
            .create_function_type(
                &function_signature,
                &key.instantiation,
                false,
            );

        let llvm_function_value = self.module().add_function(
            &qualified_name,
            llvm_function_type,
            None,
        );

        let llvm_function_signatue = Rc::new(LlvmFunctionSignature {
            return_type: return_ty,
            parameter_types: parameter_tys,
            llvm_function_value,
            llvm_parameter_indices_by_index: map,
        });
        self.function_map_mut()
            .llvm_functions_by_key
            .insert(key.clone(), llvm_function_signatue.clone());

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

        llvm_function_signatue
    }
}

struct Builder<'rctx, 'ctx, 'i, 'k> {
    context: &'rctx mut Context<'i, 'ctx>,
    callable_id: GlobalID,
    instantiation: &'k Instantiation<Model>,
    function_signature: &'k FunctionSignature,
    function_ir: &'k IR,

    llvm_function_signature: Rc<LlvmFunctionSignature<'ctx>>,

    #[allow(clippy::struct_field_names)]
    inkwell_builder: inkwell::builder::Builder<'ctx>,

    basic_block_map:
        HashMap<ID<Block<Model>>, inkwell::basic_block::BasicBlock<'ctx>>,

    address_map: HashMap<Memory<Model>, inkwell::values::BasicValueEnum<'ctx>>,

    environment: Environment<'i, Model, normalizer::NoOp>,
    built: HashSet<ID<Block<Model>>>,

    register_map: HashMap<ID<Register<Model>>, LlvmValue<'ctx>>,
}

/// Represents the translateion from Pernix's value to LLVM's value.
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, derive_more::From)]
pub enum LlvmValue<'ctx> {
    /// The value is converted successfully, default case
    Basic(inkwell::values::BasicValueEnum<'ctx>),

    /// The type of the value itself is `Zero-Sized type`, therefore, it's has
    /// no meaningful value.
    Zst,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Error {
    /// Reaches to an unreachable (somehowe), therefore, must stop building the
    /// current block immediately.
    Unreachable,
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

        let entry_block_id = function_ir.control_flow_graph.entry_block_id();
        let builder = context.context().create_builder();

        // llvm decides that the first block created is the entry block
        for id in std::iter::once(entry_block_id).chain(
            function_ir
                .control_flow_graph
                .blocks()
                .ids()
                .filter(|x| *x != entry_block_id),
        ) {
            let bb = context.context().append_basic_block(
                llvm_function_signature.llvm_function_value,
                &format!("block_{id:?}"),
            );

            basic_block_map.insert(id, bb);
        }

        builder.position_at_end(basic_block_map[&entry_block_id]);

        // move the parameters to the alloca
        for param_id in function_signature.parameter_order.iter().copied() {
            let Some(index) = llvm_function_signature
                .llvm_parameter_indices_by_index
                .get(&param_id)
                .copied()
            else {
                continue;
            };

            let alloca = builder
                .build_alloca(
                    llvm_function_signature.parameter_types[index],
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

            address_map.insert(Memory::Parameter(param_id), alloca.into());
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

/// Shortcircuits the function if the value is a `Zero-Sized type`.
#[macro_export]
macro_rules! into_basic {
    ($e:expr) => {
        match $e {
            $crate::function::LlvmValue::Basic(x) => x,
            x @ $crate::function::LlvmValue::Zst => return Ok(x),
        }
    };
}

impl<'ctx> Builder<'_, 'ctx, '_, '_> {
    /// Translates the value to LLVM value.
    fn get_value(
        &mut self,
        value: &Value<Model>,
    ) -> Result<LlvmValue<'ctx>, Error> {
        match value {
            Value::Register(id) => Ok(self.register_map[id].clone()),
            Value::Literal(literal) => self.get_literal_value(literal),
        }
    }

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
}
