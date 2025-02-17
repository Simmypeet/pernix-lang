//! Contains the definition of [`Context`], the main state used for the code
//! generation process.

use getset::{CopyGetters, Getters, MutGetters};
use inkwell::targets::TargetData;
use pernixc_handler::Handler;
use pernixc_table::{diagnostic::Diagnostic, GlobalID, Table};

use crate::{constant, function, r#type};

/// The main state used for the code generation process.
#[derive(MutGetters, Getters, CopyGetters)]
pub struct Context<'i, 'ctx> {
    /// The LLVM context.
    #[get_copy = "pub"]
    context: &'ctx inkwell::context::Context,

    /// The target data used for the code generation.
    #[get = "pub"]
    target_data: TargetData,

    /// The table from the Pernix frontend.
    #[get_copy = "pub"]
    table: &'i Table,

    /// The handler used to report diagnostics during the code generation
    /// process.
    #[get_copy = "pub"]
    handler: &'i dyn Handler<Box<dyn Diagnostic>>,

    /// The main module that contains all the code.
    #[get = "pub"]
    module: inkwell::module::Module<'ctx>,

    /// The mapper object between the Pernix function and the LLVM function.
    #[get = "pub"]
    #[get_mut = "pub"]
    function_map: function::Map<'ctx>,

    /// The mapper object between the Pernix constant and the LLVM constant.
    #[get = "pub"]
    #[get_mut = "pub"]
    constant_map: constant::Map<'ctx>,

    /// The mapper object between the Pernix type and the LLVM type.
    #[get = "pub"]
    #[get_mut = "pub"]
    type_map: r#type::Map<'ctx>,

    /// The ID of the main function.
    #[get_copy = "pub"]
    main_function_id: GlobalID,
}

impl std::fmt::Debug for Context<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Context")
            .field("context", self.context)
            .field("target_data", &self.target_data)
            .field("table", &self.table)
            .field("module", &self.module)
            .field("function_map", &self.function_map)
            .field("type_map", &self.type_map)
            .field("main_function_id", &self.main_function_id)
            .finish_non_exhaustive()
    }
}

impl<'i, 'ctx> Context<'i, 'ctx> {
    /// Creates a new context.
    pub fn new(
        context: &'ctx inkwell::context::Context,
        target_data: TargetData,
        table: &'i Table,
        handler: &'i dyn Handler<Box<dyn Diagnostic>>,
        module: inkwell::module::Module<'ctx>,
        main_function_id: GlobalID,
    ) -> Self {
        let function_map = function::Map::default();
        let type_map = r#type::Map::default();
        let constant_map = constant::Map::default();

        Self {
            context,
            target_data,
            table,
            handler,
            module,
            function_map,
            constant_map,
            type_map,
            main_function_id,
        }
    }

    /// Consumes the context and returns the module.
    pub fn into_module(self) -> inkwell::module::Module<'ctx> { self.module }
}
