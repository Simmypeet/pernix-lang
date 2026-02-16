//! Formats generic parameters for hover display.

use pernixc_extend::extend;
use pernixc_qbice::TrackedEngine;
use pernixc_term::{
    display::{self, Display},
    generic_parameters::GenericParameters,
};

/// Formats the given generic parameters into the provided string buffer.
#[extend]
pub(super) async fn format_generic_parameters(
    self: &TrackedEngine,
    buffer: &mut (dyn std::fmt::Write + Send),
    generic_parameters: &GenericParameters,
) {
    // don't format if there are no generic parameters
    if generic_parameters.is_empty() {
        return;
    }

    write!(buffer, "[").unwrap();

    let mut first = true;
    for (_, lifetime) in generic_parameters.lifetime_parameters_as_order() {
        if !first {
            write!(buffer, ", ").unwrap();
        }
        first = false;

        write!(buffer, "'{}", lifetime.name.as_ref()).unwrap();
    }

    for (_, type_parameter) in generic_parameters.type_parameters_as_order() {
        if !first {
            write!(buffer, ", ").unwrap();
        }
        first = false;

        write!(buffer, "{}", type_parameter.name.as_ref()).unwrap();
    }

    let configuration = display::Configuration::builder()
        .short_qualified_identifiers(true)
        .build();

    for (_, constant_parameter) in
        generic_parameters.constant_parameters_as_order()
    {
        if !first {
            write!(buffer, ", ").unwrap();
        }
        first = false;

        write!(buffer, "{}", constant_parameter.name.as_ref()).unwrap();

        constant_parameter
            .r#type
            .write_async_with_configuration(self, buffer, &configuration)
            .await
            .unwrap();
    }

    write!(buffer, "]").unwrap();
}
