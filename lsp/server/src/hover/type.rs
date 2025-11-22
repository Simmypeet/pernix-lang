//! Formats the signature of a struct for hover information.

use std::fmt::Write;

use pernixc_extend::extend;
use pernixc_query::{runtime::executor::CyclicError, TrackedEngine};
use pernixc_semantic_element::{
    type_alias::get_type_alias, where_clause::get_where_clause,
};
use pernixc_symbol::{accessibility::get_accessibility, name::get_name};
use pernixc_target::Global;
use pernixc_term::{
    display::Display, generic_parameters::get_generic_parameters,
};

use crate::hover::{
    accessibility::get_accessiblity_str,
    generic_parameters::format_generic_parameters, markdown::PERNIX_FENCE,
};

/// Formats the signature of the given enum into a string.
#[extend]
pub async fn format_type_signature(
    self: &TrackedEngine,
    type_id: Global<pernixc_symbol::ID>,
    has_definition: bool,
) -> Result<String, CyclicError> {
    let accessibility = self.get_accessibility(type_id).await;
    let generic_parameters = self.get_generic_parameters(type_id).await?;
    let name = self.get_name(type_id).await;
    let where_clause = self.get_where_clause(type_id).await;

    let mut string_buffer = format!("```{PERNIX_FENCE}\n");
    let accessibility_str = self.get_accessiblity_str(type_id).await?;

    write!(string_buffer, "{accessibility_str} type {name}").unwrap();
    self.format_generic_parameters(&mut string_buffer, &generic_parameters)
        .await;
    if has_definition {
        write!(string_buffer, " = ").unwrap();

        let config = pernixc_term::display::Configuration::builder()
            .short_qualified_identifiers(true)
            .build();
        let r#type = self.get_type_alias(type_id).await?;

        r#type
            .write_async_with_configuration(self, &mut string_buffer, &config)
            .await
            .unwrap();
    }

    writeln!(string_buffer).unwrap();

    string_buffer.push_str("```");

    Ok(string_buffer)
}
