use pernixc_handler::Storage;
use pernixc_resolution::{
    Resolver, generic_parameter_namespace::get_generic_parameter_namespace,
};
use pernixc_semantic_element::instance_associated_value;
use pernixc_symbol::syntax::get_instance_associated_value_syntax;
use pernixc_term::instance::Instance;

use crate::{build, occurrences::Occurrences};

mod diagnostic;

impl build::Build for instance_associated_value::Key {
    type Diagnostic = diagnostic::Diagnostic;

    async fn execute(
        engine: &pernixc_qbice::TrackedEngine,
        key: &Self,
    ) -> build::Output<Self> {
        let Some(syn) =
            engine.get_instance_associated_value_syntax(key.symbol_id).await
        else {
            return build::Output::new(
                engine.intern(Instance::Error(pernixc_term::error::Error)),
                engine,
            );
        };

        let storage = Storage::<diagnostic::Diagnostic>::default();
        let mut observer = Occurrences::default();
        let extra_namespace =
            engine.get_generic_parameter_namespace(key.symbol_id).await;

        let mut resolver = Resolver::builder()
            .tracked_engine(engine)
            .handler(&storage)
            .observer(&mut observer)
            .referring_site(key.symbol_id)
            .extra_namespace(&extra_namespace)
            .build();

        let inst = resolver.resolve_instance_value(&syn).await;

        build::Output::new_with(
            engine.intern(inst),
            storage.into_vec(),
            observer,
            engine,
        )
    }
}
