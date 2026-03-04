use std::ops::Deref;

use pernixc_handler::{Handler, Storage};
use pernixc_resolution::{
    Config, ExtraNamespaceWithForallLifetimes,
    generic_parameter_namespace::get_generic_parameter_namespace,
    term::resolve_qualified_identifier_trait_ref,
};
use pernixc_source_file::SourceElement;
use pernixc_symbol::{
    kind::{Kind, get_kind},
    syntax::get_instance_trait_ref_syntax,
};

use crate::{
    build, occurrences::Occurrences,
    trait_ref::diagnostic::HigherRankedLifetimesInModuleLevelInstance,
};

mod diagnostic;

impl build::Build for pernixc_semantic_element::trait_ref::Key {
    type Diagnostic = diagnostic::Diagnostic;

    async fn execute(
        engine: &pernixc_qbice::TrackedEngine,
        key: &Self,
    ) -> build::Output<Self> {
        let mut occurrences = Occurrences::default();
        let storage = Storage::<diagnostic::Diagnostic>::default();
        let mut extra_namespace = engine
            .get_generic_parameter_namespace(key.symbol_id)
            .await
            .deref()
            .clone();

        let Some(trait_ref) =
            engine.get_instance_trait_ref_syntax(key.symbol_id).await
        else {
            return build::Output::new(None, engine);
        };

        let Some(qualified_identifier) = trait_ref.qualified_identifier()
        else {
            return build::Output::new(None, engine);
        };

        let kind = engine.get_kind(key.symbol_id).await;
        let forall_lifetimes = trait_ref.higher_ranked_lifetimes();

        // the module-level instance symbol can't contain forall lifetimes,
        // report an error if it does
        if kind == Kind::Instance
            && let Some(syn) = forall_lifetimes.as_ref()
        {
            storage.receive(
                HigherRankedLifetimesInModuleLevelInstance::builder()
                    .span(syn.span())
                    .build(),
            );
        }

        let extra_namespace = ExtraNamespaceWithForallLifetimes::new(
            &mut extra_namespace,
            (kind != Kind::Instance)
                .then_some(forall_lifetimes.as_ref())
                .flatten(),
            &storage,
        );

        let Some(result) = engine
            .resolve_qualified_identifier_trait_ref(
                &qualified_identifier,
                Config::builder()
                    .observer(&mut occurrences)
                    .extra_namespace(extra_namespace.extra_namespace())
                    .referring_site(key.symbol_id)
                    .build(),
                &storage,
            )
            .await
        else {
            return build::Output::new_with(
                None,
                storage.into_vec(),
                occurrences,
                engine,
            );
        };

        build::Output::new_with(
            Some(engine.intern(result)),
            storage.into_vec(),
            occurrences,
            engine,
        )
    }
}

build::register_build!(pernixc_semantic_element::trait_ref::Key);
