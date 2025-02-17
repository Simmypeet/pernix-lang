//! Contains the LLVM code generation logic of the pernix compiler.

use diagnostic::{
    GenericParametersAreNotAllowedInMainFunction, InvalidMainFunctionSignature,
    MainIsNotAFunction,
};
use function::Call;
use inkwell::targets::TargetData;
use pernixc_abort::Abort;
use pernixc_component::function_signature::FunctionSignature;
use pernixc_handler::Handler;
use pernixc_table::{
    component::{Name, SymbolKind},
    diagnostic::Diagnostic,
    GlobalID, Table,
};
use pernixc_term::{
    generic_parameter::GenericParameters,
    instantiation::Instantiation,
    r#type::{Primitive, Type},
    where_clause::WhereClause,
};

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

fn check_function_main(input: &Input<'_, '_>) -> bool {
    let main_function_id = input.main_function_id;
    let symbol_kind = *input.table.get::<SymbolKind>(main_function_id);

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
        input.table.query::<FunctionSignature>(main_function_id).unwrap();

    if !func_sig.parameters.is_empty()
        || func_sig.return_type != Type::Primitive(Primitive::Int32)
    {
        input.handler.receive(Box::new(InvalidMainFunctionSignature {
            main_function_id,
        }));
        has_error = true;
    }

    let generic_params =
        input.table.query::<GenericParameters>(main_function_id).unwrap();

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
        input.table.query::<WhereClause>(main_function_id).unwrap();

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
            .table
            .get::<Name>(GlobalID::new(
                input.main_function_id.target_id,
                pernixc_table::ID::ROOT_MODULE,
            ))
            .as_str(),
    );

    let mut context = context::Context::new(
        input.inkwell_context,
        input.target_data,
        input.table,
        input.handler,
        module,
        input.main_function_id,
    );

    let _ = context.get_function(&Call {
        callable_id: input.main_function_id,
        instantiation: Instantiation::default(),
    });

    Ok(context.into_module())
}

#[cfg(test)]
mod test;
