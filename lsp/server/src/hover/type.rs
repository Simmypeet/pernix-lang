use std::{fmt::Write, ops::Not};

use pernixc_extend::extend;
use pernixc_query::{TrackedEngine, runtime::executor::CyclicError};
use pernixc_semantic_element::{
    type_alias::get_type_alias, where_clause::get_where_clause,
};
use pernixc_symbol::kind::{Kind, get_kind};
use pernixc_target::Global;

use crate::{
    formatter::{self, LinedFormatter, WriteSignatureOptions},
    hover::associate_symbols::format_associate_symbol,
};

#[extend]
pub async fn format_type_signature(
    self: &TrackedEngine,
    type_id: Global<pernixc_symbol::ID>,
) -> Result<String, CyclicError> {
    self.format_associate_symbol(type_id, write_type_signature).await
}

async fn write_type_signature(
    engine: &TrackedEngine,
    formatter: &mut LinedFormatter<'_, '_, '_>,
    type_id: Global<pernixc_symbol::ID>,
) -> Result<(), formatter::Error> {
    let has_definition = match engine.get_kind(type_id).await {
        Kind::Type | Kind::ImplementationType => true,
        Kind::TraitType => false,
        _ => unreachable!(),
    };

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

    let where_clauses = engine.get_where_clause(type_id).await?;

    if where_clauses.is_empty().not() {
        formatter
            .indent(async |x| {
                x.format_where_clause(&where_clauses).await?;

                Ok(())
            })
            .await?;
    }

    Ok(())
}
