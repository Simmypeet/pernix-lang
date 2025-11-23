//! Contains the LLVM code generation logic of the pernix compiler.

use diagnostic::{
    GenericParametersAreNotAllowedInMainFunction, InvalidMainFunctionSignature,
    MainIsNotAFunction,
};
use function::Call;
use inkwell::targets::TargetData;
use pernixc_handler::Handler;
use pernixc_query::TrackedEngine;
use pernixc_semantic_element::{
    parameter::get_parameters, return_type::get_return_type,
    where_clause::get_where_clause,
};
use pernixc_symbol::{
    get_target_root_module_id,
    kind::{Kind, get_kind},
    name::get_name,
};
use pernixc_target::Global;
use pernixc_term::{
    generic_parameters::get_generic_parameters,
    instantiation::Instantiation,
    r#type::{Tuple, Type},
};

use crate::diagnostic::{
    Diagnostic, WhereClausePredicatesAreNotAllowedInMainFunction,
};

pub mod constant;
pub mod context;
pub mod diagnostic;
pub mod function;
pub mod r#type;
pub mod zst;

/// The input of the code generation process.
pub struct Input<'i, 'ctx> {
    /// The llvm context used for the code generation.
    pub inkwell_context: &'ctx inkwell::context::Context,

    /// The target data used for the code generation.
    pub target_data: TargetData,

    /// The table that contains all the information about the program.
    pub engine: &'i TrackedEngine,

    /// The target ID that will be compiled as a binary.
    pub main_function_id: Global<pernixc_symbol::ID>,

    /// The handler used to generate the code.
    pub handler: &'i dyn Handler<Diagnostic>,
}

impl std::fmt::Debug for Input<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Input")
            .field("inkwell_context", self.inkwell_context)
            .field("table", &self.engine)
            .field("main_function_id", &self.main_function_id)
            .finish_non_exhaustive()
    }
}

async fn check_function_main(input: &Input<'_, '_>) -> bool {
    let main_function_id = input.main_function_id;
    let symbol_kind = input.engine.get_kind(main_function_id).await;

    // must be function type
    let mut has_error = if symbol_kind == Kind::Function {
        false
    } else {
        input.handler.receive(Diagnostic::MainIsNotAFunction(
            MainIsNotAFunction { main_function_id },
        ));
        true
    };

    let func_parameters =
        input.engine.get_parameters(main_function_id).await.unwrap();

    let return_type =
        input.engine.get_return_type(main_function_id).await.unwrap();

    if !func_parameters.parameters.is_empty()
        || *return_type != Type::Tuple(Tuple { elements: Vec::new() })
    {
        input.handler.receive(Diagnostic::InvalidMainFunctionSignature(
            InvalidMainFunctionSignature { main_function_id },
        ));
        has_error = true;
    }

    let generic_params =
        input.engine.get_generic_parameters(main_function_id).await.unwrap();

    // must not have any generic parameters
    if !generic_params.lifetimes().is_empty()
        || !generic_params.types().is_empty()
        || !generic_params.constants().is_empty()
    {
        input.handler.receive(
            Diagnostic::GenericParametersAreNotAllowedInMainFunction(
                GenericParametersAreNotAllowedInMainFunction {
                    main_function_id,
                },
            ),
        );
        has_error = true;
    }

    let where_clause =
        input.engine.get_where_clause(main_function_id).await.unwrap();

    // must not have any where clause predicates
    if !where_clause.is_empty() {
        input.handler.receive(
            Diagnostic::WhereClausePredicatesAreNotAllowedInMainFunction(
                WhereClausePredicatesAreNotAllowedInMainFunction {
                    main_function_id,
                },
            ),
        );
        has_error = true;
    }

    !has_error
}

/// The main driver of the code generation process.
///
/// This assumes that the table is validated and has no errors.
#[allow(clippy::missing_errors_doc)]
pub async fn codegen<'ctx>(
    input: Input<'_, 'ctx>,
) -> Option<inkwell::module::Module<'ctx>> {
    if !check_function_main(&input).await {
        return None;
    }

    let module = input.inkwell_context.create_module(
        input
            .engine
            .get_name(Global::new(
                input.main_function_id.target_id,
                input
                    .engine
                    .get_target_root_module_id(input.main_function_id.target_id)
                    .await,
            ))
            .await
            .as_str(),
    );

    let mut context = context::Context::new(
        input.inkwell_context,
        input.target_data,
        input.engine,
        input.handler,
        module,
        input.main_function_id,
    );

    let main = context
        .get_function(&Call {
            callable_id: input.main_function_id,
            instantiation: Instantiation::default(),
        })
        .await;

    // create a real function main that call `core::main`
    let c_int_type = context.context().i32_type();
    let main_type = c_int_type.fn_type(&[], false);

    let main_function = context.module().add_function(
        "main",
        main_type,
        Some(inkwell::module::Linkage::External),
    );

    let builder = context.context().create_builder();
    let entry_block =
        context.context().append_basic_block(main_function, "entry");

    builder.position_at_end(entry_block);

    builder.build_call(main.llvm_function_value, &[], "call_main").unwrap();
    builder.build_return(Some(&c_int_type.const_zero())).unwrap();

    Some(context.into_module())
}

#[cfg(test)]
mod test;
