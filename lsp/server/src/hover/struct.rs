//! Formats the signature of a struct for hover information.

use std::fmt::Write;

use pernixc_extend::extend;
use pernixc_query::{TrackedEngine, runtime::executor::CyclicError};
use pernixc_semantic_element::fields::get_fields;
use pernixc_target::Global;

use crate::{
    formatter::{Formatter, WriteSignatureOptions, assert_no_fmt_error},
    hover::markdown::PERNIX_FENCE,
};

/// Formats the signature of the given enum into a string.
#[extend]
pub async fn format_struct_signature(
    self: &TrackedEngine,
    struct_id: Global<pernixc_symbol::ID>,
) -> Result<String, CyclicError> {
    let mut string = format!("```{PERNIX_FENCE}\n");
    let mut formatter = Formatter::new(&mut string, self);

    formatter
        .new_line(async |mut x| {
            x.write_signature(
                struct_id,
                &WriteSignatureOptions::builder()
                    .signature_string("struct")
                    .build(),
            )
            .await?;

            x.indent(async |x| {
                let fields = self.get_fields(struct_id).await?;

                let configuration =
                    pernixc_term::display::Configuration::builder()
                        .short_qualified_identifiers(true)
                        .build();

                for (_, field) in fields.fields_as_order() {
                    x.new_line(async |mut x| {
                        x.format_accessibility(field.accessibility, struct_id)
                            .await?;

                        write!(x, " {}: ", field.name).unwrap();

                        x.write_type(&field.r#type).await?;

                        Ok(())
                    })
                    .await?;
                }

                Ok(())
            })
            .await?;

            Ok(())
        })
        .await
        .assert_no_fmt_error()?;

    string.push_str("\n```");

    Ok(string)
}
