//! Contains the LLVM code generation logic of the pernix compiler.

use function::Call;
use inkwell::targets::TargetData;
use pernixc_abort::Abort;
use pernixc_handler::Handler;
use pernixc_table::{
    component::{Name, SymbolKind},
    diagnostic::Diagnostic,
    GlobalID, Table,
};
use pernixc_term::instantiation::Instantiation;

pub mod context;
pub mod diagnostic;
pub mod function;
pub mod r#type;

/// The model used for the code generation process.
pub type Model = pernixc_ir::model::Model;

/// The input of the code generation process.
pub struct Input<'i, 'ctx> {
    /// The llvm context used for the code generation.
    pub inkwell_context: &'ctx inkwell::context::Context,

    /// The target data used for the code generation.
    pub target_data: TargetData,

    /// The table that contains all the information about the program.
    pub table: &'i Table,

    /// The target ID that will be compiled as a binary.
    pub main_function_id: GlobalID,

    /// The handler used to generate the code.
    pub handler: &'i dyn Handler<Box<dyn Diagnostic>>,
}

impl std::fmt::Debug for Input<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Input")
            .field("inkwell_context", self.inkwell_context)
            .field("table", &self.table)
            .field("main_function_id", &self.main_function_id)
            .finish_non_exhaustive()
    }
}

/// The main driver of the code generation process.
///
/// This assumes that the table is validated and has no errors.
#[allow(clippy::missing_errors_doc)]
pub fn codegen<'ctx>(
    input: Input<'_, 'ctx>,
) -> Result<inkwell::module::Module<'ctx>, Abort> {
    // Target::initialize_native(&InitializationConfig::default()).unwrap();

    let module = input.inkwell_context.create_module(
        input
            .table
            .get::<Name>(GlobalID::new(
                input.main_function_id.target_id,
                pernixc_table::ID::ROOT_MODULE,
            ))
            .as_str(),
    );

    let symbol_kind = *input.table.get::<SymbolKind>(input.main_function_id);
    let mut context = context::Context::new(
        input.inkwell_context,
        input.target_data,
        input.table,
        input.handler,
        module,
    );

    let user_main_function = context.get_function(&Call {
        callable_id: input.main_function_id,
        instantiation: Instantiation::default(),
    });

    // create the main function that calls the user's main function
    let main_fn_type = context.context().i32_type().fn_type(&[], false);
    let linkage = inkwell::module::Linkage::External;
    let main_fn =
        context.module().add_function("main", main_fn_type, Some(linkage));

    let entry = context.context().append_basic_block(main_fn, "entry");
    let builder = context.context().create_builder();

    builder.position_at_end(entry);

    let result = builder
        .build_call(user_main_function, &[], "result")
        .unwrap()
        .try_as_basic_value()
        .left()
        .unwrap();
    builder.build_return(Some(&result)).unwrap();

    Ok(context.into_module())
}

#[cfg(test)]
mod test;
