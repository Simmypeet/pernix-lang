/// The executor for the [`Parent`] component.

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Executor;

impl pernixc_query::runtime::executor::Executor<Key> for Executor {
    async fn execute(
        &self,
        engine: &TrackedEngine,
        key: &Key,
    ) -> Result<Option<ID>, pernixc_query::runtime::executor::CyclicError> {
        if key.0.id == engine.get_target_root_module_id(key.0.target_id).await {
            return Ok(None);
        }

        let intermediate = engine
            .query(&IntermediateKey(key.0.target_id))
            .await
            .expect("should have no cyclic dependencies");

        let parent_id = intermediate.get(&key.0.id).copied().unwrap();

        Ok(Some(parent_id))
    }
}

#[pernixc_query::query(
    key(IntermediateKey),
    id(TargetID),
    value(Arc<HashMap<ID, ID>>),
    executor(IntermediateExecutor),
)]
pub async fn intermediate_executor(
    target_id: TargetID,
    engine: &TrackedEngine,
) -> Result<Arc<HashMap<ID, ID>>, pernixc_query::runtime::executor::CyclicError>
{
    let map = engine.query(&crate::MapKey(target_id)).await?;

    let mut key_and_member_tasks = Vec::new();

    for (symbol, _) in map.keys_by_symbol_id.iter() {
        let engine = engine.clone();
        let symbol = *symbol;

        key_and_member_tasks.push(tokio::spawn(async move {
            let table: Arc<crate::Table> =
                engine.get_table_of_symbol(target_id.make_global(symbol)).await;

            table.members.get(&symbol).map(|members| {
                (
                    symbol,
                    members
                        .member_ids_by_name
                        .values()
                        .copied()
                        .chain(members.unnameds.iter().copied())
                        .collect::<Vec<_>>(),
                )
            })
        }));
    }

    let mut key_and_members = Vec::new();
    for task in key_and_member_tasks {
        let Some((symbol, members)) = task.await.unwrap() else {
            continue;
        };

        key_and_members.push((symbol, members));
    }

    let mut parent_map = HashMap::default();

    for (symbol, members) in key_and_members {
        for member in members {
            assert!(parent_map.insert(member, symbol).is_none());
        }
    }

    Ok(Arc::new(parent_map))
}

pernixc_register::register!(IntermediateKey, IntermediateExecutor);

pernixc_register::register!(Key, Executor);
