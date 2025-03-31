//! Contains the code related to checking the extern functions.

use diagnostic::GenericParametersAreNotAllowedInExternFunction;
use pernixc_abort::Abort;
use pernixc_handler::Handler;
use pernixc_semantic::{
    component::Extern, diagnostic::Diagnostic, GlobalID, Table,
};
use pernixc_semantic::term::generic_parameter::GenericParameters;

pub mod diagnostic;

/// Checks wel-fromedness of the extern function.
///
/// For example, extern "C" functions are not allowed to have generic
/// parameters.
#[allow(clippy::missing_errors_doc)]
pub fn extern_function_check(
    table: &Table,
    function_id: GlobalID,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) -> Result<(), Abort> {
    let calling_conventation = *table.get::<Extern>(function_id);

    match calling_conventation {
        Extern::C(_) => {
            let generic_parameters =
                table.query::<GenericParameters>(function_id)?;

            let has_generic_parameters = !generic_parameters.types().is_empty()
                || !generic_parameters.constants().is_empty();

            if has_generic_parameters {
                handler.receive(Box::new(
                    GenericParametersAreNotAllowedInExternFunction {
                        extern_function_id: function_id,
                    },
                ));
            }
        }

        Extern::Unknown => todo!(),
    }

    Ok(())
}

#[cfg(test)]
mod test;
