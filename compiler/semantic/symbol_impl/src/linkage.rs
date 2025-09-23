#[pernixc_query::query(
    key(Key),
    id(Global<ID>),
    value(Linkage),
    executor(Executor),
    extend(method(get_linkage), no_cyclic)
)]
pub async fn executor(
    id: Global<ID>,
    engine: &TrackedEngine,
) -> Result<Linkage, CyclicError> {
    let table = engine.get_table_of_symbol(id).await;

    Ok(table.function_linkages.get(&id.id).copied().unwrap_or_else(|| {
        panic!("invalid symbol ID or symbol is not a function: {:?}", id.id)
    }))
}

pernixc_register::register!(Key, Executor);
