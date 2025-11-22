use std::fmt::Write;

use pernixc_extend::extend;
use pernixc_query::{TrackedEngine, runtime::executor::CyclicError};
use pernixc_semantic_element::variant::get_variant_associated_type;
use pernixc_symbol::{name::get_name, parent::get_parent_global};
use pernixc_target::Global;

use crate::{
    formatter::{Formatter, WriteSignatureOptions, assert_no_fmt_error},
    hover::markdown::PERNIX_FENCE,
};

#[extend]
pub async fn format_variant_signature(
    self: &TrackedEngine,
    variant_id: Global<pernixc_symbol::ID>,
) -> Result<String, CyclicError> {
    let parent_enum_id = self.get_parent_global(variant_id).await.unwrap();
    let enum_name = self.get_name(variant_id).await;
    let associated_type = self.get_variant_associated_type(variant_id).await?;

    let mut string = format!("```{PERNIX_FENCE}\n");
    let mut formatter = Formatter::new(&mut string, self);

    formatter
        .new_line(async |mut x| {
            x.write_signature(
                parent_enum_id,
                &WriteSignatureOptions::builder()
                    .signature_string("enum")
                    .build(),
            )
            .await?;

            x.indent(async |x| {
                x.new_line(async |mut x| {
                    write!(x, "{enum_name}").unwrap();

                    if let Some(associated_type) = associated_type {
                        write!(x, "(").unwrap();
                        x.write_type(&associated_type).await?;
                        write!(x, ")").unwrap();
                    }

                    Ok(())
                })
                .await
            })
            .await?;

            Ok(())
        })
        .await
        .assert_no_fmt_error()?;

    string.push_str("\n```");
    Ok(string)
}
