//! Contains the definition of [`Context`], the main state used for the code
//! generation process.

use getset::{CopyGetters, Getters};
use inkwell::targets::TargetData;
use pernixc_table::Table;

use crate::{function, r#type};

/// The main state used for the code generation process.
#[derive(Debug, Getters, CopyGetters)]
pub struct Context<'ctx> {
    /// The LLVM context.
    #[get_copy = "pub"]
    context: &'ctx inkwell::context::Context,

    /// The target data used for the code generation.
    #[get = "pub"]
    target_data: TargetData,

    /// The table from the Pernix frontend.
    #[get_copy = "pub"]
    table: &'ctx Table,

    /// The main module that contains all the code.
    #[get = "pub"]
    module: inkwell::module::Module<'ctx>,

    /// The mapper object between the Pernix function and the LLVM function.
    #[get = "pub"]
    function_map: function::Map<'ctx>,

    /// The mapper object between the Pernix type and the LLVM type.
    #[get = "pub"]
    type_map: r#type::Map<'ctx>,
}
