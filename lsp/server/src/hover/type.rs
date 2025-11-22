//! Formats the signature of a struct for hover information.

use std::fmt::Write;

use pernixc_extend::extend;
use pernixc_query::{TrackedEngine, runtime::executor::CyclicError};
use pernixc_semantic_element::type_alias::get_type_alias;
use pernixc_symbol::kind::{Kind, get_kind};
use pernixc_target::Global;

use crate::{
    formatter::{self, LinedFormatter, WriteSignatureOptions},
    hover::associate_symbols::format_associate_symbol,
};

/// Formats the signature of the given enum into a string.
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

    Ok(())
}
