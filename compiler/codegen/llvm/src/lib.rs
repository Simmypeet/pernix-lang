//! Contains the LLVM code generation logic of the pernix compiler.

use diagnostic::{
    GenericParametersAreNotAllowedInMainFunction, InvalidMainFunctionSignature,
    MainIsNotAFunction,
};
use function::Call;
use inkwell::targets::TargetData;
use pernixc_handler::Handler;
use pernixc_query::TrackedEngine;
use pernixc_target::Global;

use crate::diagnostic::Diagnostic;

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

fn check_function_main(input: &Input<'_, '_>) -> bool {
    let main_function_id = input.main_function_id;
    let symbol_kind = *input.engine.get::<SymbolKind>(main_function_id);

    // must be function type
    let mut has_error = if symbol_kind == SymbolKind::Function {
        false
    } else {
        input
            .handler
            .receive(Box::new(MainIsNotAFunction { main_function_id }));
        true
    };

    let func_sig =
        input.engine.query::<FunctionSignature>(main_function_id).unwrap();

    if !func_sig.parameters.is_empty()
        || func_sig.return_type != Type::Tuple(Tuple { elements: Vec::new() })
    {
        input.handler.receive(Box::new(InvalidMainFunctionSignature {
            main_function_id,
        }));
        has_error = true;
    }

    let generic_params =
        input.engine.query::<GenericParameters>(main_function_id).unwrap();

    // must not have any generic parameters
    if !generic_params.lifetimes().is_empty()
        || !generic_params.types().is_empty()
        || !generic_params.constants().is_empty()
    {
        input.handler.receive(Box::new(
            GenericParametersAreNotAllowedInMainFunction { main_function_id },
        ));
        has_error = true;
    }

    let where_clause =
        input.engine.query::<WhereClause>(main_function_id).unwrap();

    // must not have any where clause predicates
    if !where_clause.predicates.is_empty() {
        input.handler.receive(Box::new(
            diagnostic::WhereClausePredicatesAreNotAllowedInMainFunction {
                main_function_id,
            },
        ));
        has_error = true;
    }

    !has_error
}

/// The main driver of the code generation process.
///
/// This assumes that the table is validated and has no errors.
#[allow(clippy::missing_errors_doc)]
pub fn codegen<'ctx>(
    input: Input<'_, 'ctx>,
) -> Result<inkwell::module::Module<'ctx>, Abort> {
    if !check_function_main(&input) {
        return Err(Abort);
    }

    let module = input.inkwell_context.create_module(
        input
            .engine
            .get::<Name>(GlobalID::new(
                input.main_function_id.target_id,
                table::ID::ROOT_MODULE,
            ))
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

    let main = context.get_function(&Call {
        callable_id: input.main_function_id,
        instantiation: Instantiation::default(),
    });

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

    Ok(context.into_module())
}

#[cfg(test)]
mod test;
