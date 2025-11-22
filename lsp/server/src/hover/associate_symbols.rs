use pernixc_extend::extend;
use pernixc_query::{TrackedEngine, runtime::executor::CyclicError};
use pernixc_symbol::{
    kind::{Kind, get_kind},
    parent::get_parent_global,
};
use pernixc_target::Global;

use crate::{
    formatter::{Formatter, WriteSignatureOptions, assert_no_fmt_error},
    hover::markdown::PERNIX_FENCE,
};

#[extend]
pub async fn format_associate_symbol(
    self: &TrackedEngine,
    type_id: Global<pernixc_symbol::ID>,
    fmt: impl AsyncFnOnce(
        &TrackedEngine,
        &mut crate::formatter::LinedFormatter<'_, '_, '_>,
        Global<pernixc_symbol::ID>,
    ) -> Result<(), crate::formatter::Error>,
) -> Result<String, CyclicError> {
    let mut string = format!("```{PERNIX_FENCE}\n");
    let mut formatter = Formatter::new(&mut string, self);

    let parent_id = self.get_parent_global(type_id).await.unwrap();
    let parent_kind = self.get_kind(parent_id).await;

    match parent_kind {
        Kind::Module => {
            formatter
                .new_line(async |mut x| fmt(self, &mut x, type_id).await)
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
                    x.new_line(async |mut x| fmt(self, &mut x, type_id).await)
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
                            fmt(self, &mut x, type_id).await
                        })
                        .await
                    })
                    .await
                } else {
                    x.new_line(async |mut x| fmt(self, &mut x, type_id).await)
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
