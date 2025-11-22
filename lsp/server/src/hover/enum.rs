use std::fmt::Write;

use pernixc_extend::extend;
use pernixc_query::{TrackedEngine, runtime::executor::CyclicError};
use pernixc_semantic_element::variant::get_variant_associated_type;
use pernixc_symbol::{
    member::get_members, name::get_name,
    variant_declaration_order::get_variant_declaration_order,
};
use pernixc_target::Global;

use crate::{
    formatter::{Formatter, WriteSignatureOptions, assert_no_fmt_error},
    hover::markdown::PERNIX_FENCE,
};

#[extend]
pub async fn format_enum_signature(
    self: &TrackedEngine,
    enum_id: Global<pernixc_symbol::ID>,
) -> Result<String, CyclicError> {
    let mut string = format!("```{PERNIX_FENCE}\n");
    let mut formatter = Formatter::new(&mut string, self);

    formatter
        .new_line(async |mut x| {
            x.write_signature(
                enum_id,
                &WriteSignatureOptions::builder()
                    .signature_string("enum")
                    .build(),
            )
            .await?;

            x.indent(async |x| {
                let variant_member = self.get_members(enum_id).await;
                let mut variant_id_with_order =
                    Vec::with_capacity(variant_member.member_ids_by_name.len());

                for (variant_name, variant_id) in
                    &variant_member.member_ids_by_name
                {
                    let variant_id =
                        Global::new(enum_id.target_id, *variant_id);
                    let order =
                        self.get_variant_declaration_order(variant_id).await;

                    variant_id_with_order.push((variant_id, order));
                }

                // sort by declaration order
                variant_id_with_order.sort_by_key(|(_, order)| *order);

                for (variant_id, _) in variant_id_with_order {
                    let variant_name = self.get_name(variant_id).await;

                    x.new_line(async |mut x| {
                        write!(x, "{variant_name}").unwrap();

                        // if has associated data, format it
                        let associated_ty = self
                            .get_variant_associated_type(variant_id)
                            .await?;

                        if let Some(associated_ty) = associated_ty {
                            write!(x, "(").unwrap();
                            x.write_type(&associated_ty).await?;
                            write!(x, ")").unwrap();
                        }

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
