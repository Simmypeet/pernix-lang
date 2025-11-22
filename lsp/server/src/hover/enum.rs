//! Format enum signature for hover display

use std::fmt::Write;

use pernixc_extend::extend;
use pernixc_query::{runtime::executor::CyclicError, TrackedEngine};
use pernixc_semantic_element::{
    variant::get_variant_associated_type, where_clause::get_where_clause,
};
use pernixc_symbol::{
    accessibility::get_accessibility, member::get_members, name::get_name,
    variant_declaration_order::get_variant_declaration_order,
};
use pernixc_target::Global;
use pernixc_term::{
    display::{self, Display},
    generic_parameters::get_generic_parameters,
};

use crate::hover::{
    accessibility::get_accessiblity_str,
    generic_parameters::format_generic_parameters, markdown::PERNIX_FENCE,
};

/// Formats the signature of the given enum into a string.
#[extend]
pub async fn format_enum_signature(
    self: &TrackedEngine,
    enum_id: Global<pernixc_symbol::ID>,
) -> Result<String, CyclicError> {
    let accessibility = self.get_accessibility(enum_id).await;
    let generic_parameters = self.get_generic_parameters(enum_id).await?;
    let name = self.get_name(enum_id).await;
    let where_clause = self.get_where_clause(enum_id).await;

    let mut string_buffer = format!("```{PERNIX_FENCE}\n");
    let accessibility_str = self.get_accessiblity_str(enum_id).await?;

    write!(string_buffer, "{accessibility_str} enum {name}").unwrap();
    self.format_generic_parameters(&mut string_buffer, &generic_parameters)
        .await;
    writeln!(string_buffer, ":").unwrap();

    let variant_member = self.get_members(enum_id).await;
    let mut variant_id_with_order =
        Vec::with_capacity(variant_member.member_ids_by_name.len());

    for (variant_name, variant_id) in &variant_member.member_ids_by_name {
        let variant_id = Global::new(enum_id.target_id, *variant_id);
        let order = self.get_variant_declaration_order(variant_id).await;

        variant_id_with_order.push((variant_id, order));
    }

    // sort by declaration order
    variant_id_with_order.sort_by_key(|(_, order)| *order);

    let configuration = display::Configuration::builder()
        .short_qualified_identifiers(true)
        .build();

    for (variant_id, _) in variant_id_with_order {
        let variant_name = self.get_name(variant_id).await;

        write!(string_buffer, "\t{variant_name}").unwrap();

        // if has associated data, format it
        let associated_ty =
            self.get_variant_associated_type(variant_id).await?;

        if let Some(associated_ty) = associated_ty {
            write!(string_buffer, "(").unwrap();
            associated_ty
                .write_async_with_configuration(
                    self,
                    &mut string_buffer,
                    &configuration,
                )
                .await
                .unwrap();
            write!(string_buffer, ")").unwrap();
        }

        // finish the line
        writeln!(string_buffer).unwrap();
    }

    string_buffer.push_str("```");

    Ok(string_buffer)
}
