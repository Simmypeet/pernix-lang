use std::{fmt::Write, ops::Not};

use pernixc_extend::extend;
use pernixc_qbice::TrackedEngine;
use pernixc_semantic_element::{
    fields::get_fields, where_clause::get_where_clause,
};
use pernixc_target::Global;

use crate::{
    formatter::{Formatter, WriteSignatureOptions},
    hover::markdown::PERNIX_FENCE,
};

#[extend]
pub async fn format_struct_signature(
    self: &TrackedEngine,
    struct_id: Global<pernixc_symbol::ID>,
) -> String {
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
            .await;

            x.indent(async |x| {
                let predicates = self.get_where_clause(struct_id).await;

                let wher_clause_formatted =
                    x.format_where_clause(&predicates).await;

                let fields = self.get_fields(struct_id).await;

                if wher_clause_formatted
                    && fields.field_ids_by_name.is_empty().not()
                {
                    // add space for fields after where clause
                    x.immediate_line();
                }

                for (_, field) in fields.fields_as_order() {
                    x.new_line(async |mut x| {
                        x.format_accessibility(field.accessibility, struct_id)
                            .await;

                        write!(x, " {}: ", field.name.as_ref()).unwrap();

                        x.write_type(&field.r#type).await;
                    })
                    .await;
                }
            })
            .await;
        })
        .await;

    string.push_str("\n```");

    string
}
