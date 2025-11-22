//! Formats the signature of a struct for hover information.

use std::fmt::Write;

use pernixc_extend::extend;
use pernixc_query::{TrackedEngine, runtime::executor::CyclicError};
use pernixc_semantic_element::type_alias::get_type_alias;
use pernixc_symbol::{
    kind::{Kind, get_kind},
    parent::get_parent_global,
};
use pernixc_target::Global;

use crate::{
    formatter::{
        self, Formatter, LinedFormatter, WriteSignatureOptions,
        assert_no_fmt_error,
    },
    hover::markdown::PERNIX_FENCE,
};

/// Formats the signature of the given enum into a string.
#[extend]
pub async fn format_type_signature(
    self: &TrackedEngine,
    type_id: Global<pernixc_symbol::ID>,
) -> Result<String, CyclicError> {
    let mut string = format!("```{PERNIX_FENCE}\n");
    let mut formatter = Formatter::new(&mut string, self);

    let parent_id = self.get_parent_global(type_id).await.unwrap();
    let parent_kind = self.get_kind(parent_id).await;

    match parent_kind {
        Kind::Module => {
            formatter
                .new_line(async |mut x| {
                    write_type_signature(self, &mut x, type_id, true).await
                })
                .await
                .assert_no_fmt_error()?;
        }

        // include the trait signature for associated types
        Kind::Trait => formatter
            .new_line(async |mut x| {
                // include the trait signature
                x.write_signature(
                    parent_id,
                    &WriteSignatureOptions::builder()
                        .signature_string("trait")
                        .build(),
                )
                .await?;

                x.indent(async |x| {
                    // trait type doesn't have a definition
                    x.new_line(async |mut x| {
                        write_type_signature(self, &mut x, type_id, false).await
                    })
                    .await
                })
                .await?;

                Ok(())
            })
            .await
            .assert_no_fmt_error()?,

        Kind::PositiveImplementation => formatter
            .new_line(async |mut x| {
                if x.write_implements_signature(parent_id).await? {
                    x.indent(async |x| {
                        x.new_line(async |mut x| {
                            write_type_signature(self, &mut x, type_id, true)
                                .await
                        })
                        .await
                    })
                    .await
                } else {
                    x.new_line(async |mut x| {
                        write_type_signature(self, &mut x, type_id, true).await
                    })
                    .await
                }
            })
            .await
            .assert_no_fmt_error()?,

        kind => unreachable!("unexpected parent {kind:?} kind for type alias"),
    }

    string.push_str("\n```");

    Ok(string)
}

async fn write_type_signature(
    engine: &TrackedEngine,
    formatter: &mut LinedFormatter<'_, '_, '_>,
    type_id: Global<pernixc_symbol::ID>,
    has_definition: bool,
) -> Result<(), formatter::Error> {
    formatter
        .write_signature(
            type_id,
            &WriteSignatureOptions::builder().signature_string("type").build(),
        )
        .await?;

    if has_definition {
        write!(formatter, " = ").unwrap();
        let r#type = engine.get_type_alias(type_id).await?;
        formatter.write_type(&r#type).await?;
    }

    Ok(())
}
