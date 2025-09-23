#[pernixc_query::query(
    key(Key),
    id(Global<ID>),
    value(SharedStr),
    executor(Executor),
    extend(method(get_name), no_cyclic)
)]
#[allow(clippy::unnecessary_wraps)]
pub async fn executor(
    id: Global<ID>,
    engine: &TrackedEngine,
) -> Result<SharedStr, CyclicError> {
    let table = engine.get_table_of_symbol(id).await;

    Ok(table
        .names
        .get(&id.id)
        .cloned()
        .unwrap_or_else(|| panic!("invalid symbol ID: {:?}", id.id)))
}

pernixc_register::register!(Key, Executor);
