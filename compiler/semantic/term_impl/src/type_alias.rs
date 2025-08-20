use std::{borrow::Cow, sync::Arc};

use pernixc_handler::Storage;
use pernixc_query::runtime::executor;
use pernixc_resolution::{
    generic_parameter_namespace::get_generic_parameter_namespace,
    term::resolve_type, Config,
};
use pernixc_symbol::syntax::get_type_alias_syntax;
use pernixc_term::r#type::Type;
use pernixc_type_system::{
    environment::{get_active_premise, Environment},
    normalizer,
};

use crate::{
    build::{self, Build},
    occurrences::Occurrences,
    type_alias::diagnostic::Diagnostic,
};

pub mod diagnostic;

pub type BuildKey = build::Key<Arc<Type>, Diagnostic>;

#[derive(Debug)]
pub struct BuildExecutor;

impl executor::Executor<BuildKey> for BuildExecutor {
    async fn execute(
        &self,
        engine: &pernixc_query::TrackedEngine,
        key: &BuildKey,
    ) -> Result<build::Build<Arc<Type>, Diagnostic>, executor::CyclicError>
    {
        let Some(syntax_tree) = engine.get_type_alias_syntax(key.id).await
        else {
            return Ok(Build {
                item: Arc::new(Type::Error(pernixc_term::error::Error)),
                diagnostics: Arc::default(),
                occurrences: Arc::default(),
            });
        };

        let storage = Storage::<Diagnostic>::default();
        let mut occurrences = Occurrences::default();
        let extra_namespace =
            engine.get_generic_parameter_namespace(key.id).await?;

        let mut ty = engine.resolve_type(
            &syntax_tree,
            Config::builder()
                .observer(&mut occurrences)
                .extra_namespace(&extra_namespace)
                .referring_site(key.id)
                .build(),
            &storage,
        );

        let premise = engine.get_active_premise(key.id).await?;
        let env = Environment::new(
            Cow::Borrowed(&premise),
            Cow::Borrowed(engine),
            normalizer::NO_OP,
        );

        /*
        ty = env.simplify_and_check_lifetime_constraints(
            &ty,
            &syntax_tree.span(),
            handler,
        );
        */

        todo!()
    }
}
