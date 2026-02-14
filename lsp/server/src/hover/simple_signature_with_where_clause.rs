use std::ops::Not;

use pernixc_extend::extend;
use pernixc_qbice::TrackedEngine;
use pernixc_semantic_element::where_clause::get_where_clause;
use pernixc_target::Global;

use crate::{
    formatter::{Formatter, WriteSignatureOptions},
    hover::markdown::PERNIX_FENCE,
};

#[extend]
pub async fn format_simple_signature_with_where_clause(
    self: &TrackedEngine,
    symbol: Global<pernixc_symbol::ID>,
    signature_string: &str,
) -> String {
    let mut string = format!("```{PERNIX_FENCE}\n");
    let mut formatter = Formatter::new(&mut string, self);

    formatter
        .new_line(async |mut x| {
            x.write_signature(
                symbol,
                &WriteSignatureOptions::builder()
                    .signature_string(signature_string)
                    .build(),
            )
            .await;

            let where_clause = self.get_where_clause(symbol).await;

            if where_clause.is_empty().not() {
                x.indent(async |x| {
                    x.format_where_clause(&where_clause).await;
                })
                .await;
            }
        })
        .await;

    string.push_str("\n```");

    string
}
