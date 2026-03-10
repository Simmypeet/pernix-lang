use std::borrow::Cow;

use pernixc_handler::Storage;
use pernixc_resolution::{
    Resolver, generic_parameter_namespace::get_generic_parameter_namespace,
};
use pernixc_semantic_element::{
    instance_associated_value, trait_ref::get_trait_ref,
};
use pernixc_source_file::SourceElement;
use pernixc_symbol::syntax::get_instance_associated_value_syntax;
use pernixc_term::instance::Instance;
use pernixc_type_system::{
    environment::{Environment, get_active_premise},
    normalizer,
};

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

        // checks the the instance's trait_ref matches the expected
        let Some(expected_trait_ref) =
            engine.get_trait_ref(key.symbol_id).await
        else {
            // the trait_ref of this symbol is malformed, so we can't do
            // any more checks, but we can still return the instance value
            return build::Output::new_with(
                engine.intern(inst),
                storage.into_vec(),
                observer,
                engine,
            );
        };

        let active_preemise = engine.get_active_premise(key.symbol_id).await;
        let environment = Environment::new_do_outlives_check(
            Cow::Borrowed(&active_preemise),
            Cow::Borrowed(engine),
            normalizer::NO_OP,
        );

        // perform the check
        let _ = environment
            .check_instance_trait_ref(
                &inst,
                &expected_trait_ref,
                &syn.span(),
                &storage,
            )
            .await;

        build::Output::new_with(
            engine.intern(inst),
            storage.into_vec(),
            observer,
            engine,
        )
    }
}

crate::build::register_build! {instance_associated_value::Key}
