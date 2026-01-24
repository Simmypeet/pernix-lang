//! Defines the query to get the declaration order of a variant in an enum.

use linkme::distributed_slice;
use pernixc_qbice::{Config, PERNIX_PROGRAM, TrackedEngine};
use pernixc_symbol::variant_declaration_order::Key;
use qbice::{executor, program::Registration};

use crate::table::get_table_of_symbol;

#[executor(config = Config)]
async fn variant_declaration_order_executor(
    key: &Key,
    engine: &TrackedEngine,
) -> usize {
    let id = key.variant_id;
    let table = engine.get_table_of_symbol(id).await;

    table
        .variant_declaration_orders
        .get(&id.id)
        .copied()
        .unwrap_or_else(|| panic!("invalid symbol ID: {:?}", id.id))
}

#[distributed_slice(PERNIX_PROGRAM)]
static VARIANT_DECLARATION_ORDER_EXECUTOR: Registration<Config> =
    Registration::new::<Key, VariantDeclarationOrderExecutor>();
