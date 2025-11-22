//! Formats the signature of a struct for hover information.

use std::fmt::Write;

use pernixc_extend::extend;
use pernixc_query::{runtime::executor::CyclicError, TrackedEngine};
use pernixc_semantic_element::type_alias::get_type_alias;
use pernixc_target::Global;

use crate::{
    formatter::{assert_no_fmt_error, Formatter, WriteSignatureOptions},
    hover::markdown::PERNIX_FENCE,
};

/// Formats the signature of the given enum into a string.
#[extend]
pub async fn format_type_signature(
    self: &TrackedEngine,
    type_id: Global<pernixc_symbol::ID>,
    has_definition: bool,
) -> Result<String, CyclicError> {
    let mut string = format!("```{PERNIX_FENCE}\n");
    let mut formatter = Formatter::new(&mut string, self);

    formatter
        .new_line(async |mut x| {
            x.write_signature(
                type_id,
                &WriteSignatureOptions::builder()
                    .signature_string("type")
                    .build(),
            )
            .await?;

            if has_definition {
                write!(x, " = ").unwrap();

                let config = pernixc_term::display::Configuration::builder()
                    .short_qualified_identifiers(true)
                    .build();
                let r#type = self.get_type_alias(type_id).await?;
                x.write_type(&r#type).await?;
            }

            Ok(())
        })
        .await
        .assert_no_fmt_error()?;

    string.push_str("\n```");

    Ok(string)
}
