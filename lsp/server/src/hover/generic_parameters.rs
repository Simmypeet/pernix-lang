//! Formats generic parameters for hover display.

use std::fmt::Write;

use pernixc_extend::extend;
use pernixc_query::TrackedEngine;
use pernixc_term::{
    display::{self, Display},
    generic_parameters::GenericParameters,
};

/// Formats the given generic parameters into the provided string buffer.
#[extend]
pub async fn format_generic_parameters(
    self: &TrackedEngine,
    string_buffer: &mut String,
    generic_parameters: &GenericParameters,
) {
    // don't format if there are no generic parameters
    if generic_parameters.is_empty() {
        return;
    }

    string_buffer.push('[');

    let mut first = true;
    for (_, lifetime) in generic_parameters.lifetime_parameters_as_order() {
        if !first {
            string_buffer.push_str(", ");
        }
        first = false;

        write!(string_buffer, "'{}", lifetime.name).unwrap();
    }

    for (_, type_parameter) in generic_parameters.type_parameters_as_order() {
        if !first {
            string_buffer.push_str(", ");
        }
        first = false;

        write!(string_buffer, "{}", type_parameter.name).unwrap();
    }

    let configuration = display::Configuration::builder()
        .short_qualified_identifiers(true)
        .build();

    for (_, constant_parameter) in
        generic_parameters.constant_parameters_as_order()
    {
        if !first {
            string_buffer.push_str(", ");
        }
        first = false;

        write!(string_buffer, "{}", constant_parameter.name).unwrap();

        constant_parameter
            .r#type
            .write_async_with_configuration(self, string_buffer, &configuration)
            .await
            .unwrap();
    }

    string_buffer.push(']');
}
