use linkme::distributed_slice;
use pernixc_qbice::{Config, PERNIX_PROGRAM, TrackedEngine};
use pernixc_symbol::member::{Key, Member};
use qbice::{executor, program::Registration, storage::intern::Interned};

use crate::table::get_table_of_symbol;

#[executor(config = Config)]
async fn member_executor(
    key: &Key,
    engine: &TrackedEngine,
) -> Interned<Member> {
    let id = key.symbol_id;
    let table = engine.get_table_of_symbol(id).await;

    table
        .members
        .get(&id.id)
        .cloned()
        .unwrap_or_else(|| panic!("invalid symbol ID: {:?}", id.id))
}

#[distributed_slice(PERNIX_PROGRAM)]
static MEMBER_EXECUTOR: Registration<Config> =
    Registration::new::<Key, MemberExecutor>();
