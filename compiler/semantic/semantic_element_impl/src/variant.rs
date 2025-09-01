use std::{borrow::Cow, sync::Arc};

use pernixc_handler::Storage;
use pernixc_query::runtime::executor;
use pernixc_resolution::{
    generic_parameter_namespace::get_generic_parameter_namespace,
    term::resolve_type, Config,
};
use pernixc_source_file::SourceElement;
use pernixc_symbol::syntax::get_variant_associated_type_syntax;
use pernixc_type_system::{
    environment::{get_active_premise, Environment},
    normalizer,
};

use crate::{
    build::{self, Output},
    occurrences::Occurrences,
    variant::diagnostic::Diagnostic,
};

pub mod diagnostic;

impl build::Build for pernixc_semantic_element::variant::Key {
    type Diagnostic = diagnostic::Diagnostic;

    async fn execute(
        engine: &pernixc_query::TrackedEngine,
        key: &Self,
    ) -> Result<build::Output<Self>, executor::CyclicError> {
        let syntax = engine.get_variant_associated_type_syntax(key.0).await;

        let Some(syntax_tree) = syntax else {
            return Ok(Output {
                item: None,
                diagnostics: Arc::default(),
                occurrences: Arc::default(),
            });
        };

        let storage = Storage::<Diagnostic>::default();
        let mut occurrences = Occurrences::default();
        let extra_namespace =
            engine.get_generic_parameter_namespace(key.0).await?;

        let mut ty = engine
            .resolve_type(
                &syntax_tree,
                Config::builder()
                    .observer(&mut occurrences)
                    .extra_namespace(&extra_namespace)
                    .referring_site(key.0)
                    .build(),
                &storage,
            )
            .await?;

        let premise = engine.get_active_premise(key.0).await?;
        let env = Environment::new(
            Cow::Borrowed(&premise),
            Cow::Borrowed(engine),
            normalizer::NO_OP,
        );

        ty = env
            .simplify_and_check_lifetime_constraints(
                &ty,
                &syntax_tree.span(),
                &storage,
            )
            .await?;

        Ok(Output {
            item: Some(Arc::new(ty)),
            diagnostics: storage.into_vec().into(),
            occurrences: Arc::new(occurrences),
        })
    }
}

build::register_build!(pernixc_semantic_element::variant::Key);
