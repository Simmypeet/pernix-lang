use std::borrow::Cow;

use pernixc_handler::Storage;
use pernixc_qbice::TrackedEngine;
use pernixc_resolution::{
    Config, generic_parameter_namespace::get_generic_parameter_namespace,
    term::resolve_type,
};
use pernixc_source_file::SourceElement;
use pernixc_symbol::syntax::get_type_alias_syntax;
use pernixc_term::r#type::Type;
use pernixc_type_system::{
    environment::{Environment, get_active_premise},
    normalizer,
};

use crate::{
    build::{self, Output},
    occurrences::Occurrences,
    type_alias::diagnostic::Diagnostic,
};

pub mod diagnostic;

impl build::Build for pernixc_semantic_element::type_alias::Key {
    type Diagnostic = diagnostic::Diagnostic;

    async fn execute(
        engine: &TrackedEngine,
        key: &Self,
    ) -> build::Output<Self> {
        let Some(syntax_tree) =
            engine.get_type_alias_syntax(key.symbol_id).await
        else {
            return Output {
                item: engine.intern(Type::Error(pernixc_term::error::Error)),
                diagnostics: engine.intern_unsized([]),
                occurrences: engine.intern(Occurrences::default()),
            };
        };

        let storage = Storage::<Diagnostic>::default();
        let mut occurrences = Occurrences::default();
        let extra_namespace =
            engine.get_generic_parameter_namespace(key.symbol_id).await;

        let mut ty = engine
            .resolve_type(
                &syntax_tree,
                Config::builder()
                    .observer(&mut occurrences)
                    .extra_namespace(&extra_namespace)
                    .referring_site(key.symbol_id)
                    .build(),
                &storage,
            )
            .await;

        let premise = engine.get_active_premise(key.symbol_id).await;
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
            .await;

        Output {
            item: engine.intern(ty),
            diagnostics: engine.intern_unsized(storage.into_vec()),
            occurrences: engine.intern(occurrences),
        }
    }
}

build::register_build!(pernixc_semantic_element::type_alias::Key);
