use std::borrow::Cow;

use pernixc_handler::Handler;
use pernixc_ir::transform::{Element, ResolutionMut, Transformer};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_qbice::TrackedEngine;
use pernixc_term::{
    constant::Constant,
    instance::Instance,
    lifetime::Lifetime,
    sub_term::TermLocation,
    r#type::Type,
    visitor::{self, MutableRecursive},
};
use pernixc_type_system::{
    UnrecoverableError,
    environment::{Environment, Premise},
    term::Term,
};

use crate::{
    binder::{
        Binder,
        inference_context::{InferenceContext, RenderingMap},
    },
    diagnostic::{Diagnostic, TypeAnnotationRequired},
};

struct SearchInference {
    found_inference: bool,
}

impl MutableRecursive<Lifetime> for SearchInference {
    fn visit(
        &mut self,
        _: &mut Lifetime,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        true
    }
}

impl MutableRecursive<Type> for SearchInference {
    fn visit(
        &mut self,
        term: &mut Type,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        if term.is_inference() {
            self.found_inference = true;
        }

        true
    }
}

impl MutableRecursive<Constant> for SearchInference {
    fn visit(
        &mut self,
        term: &mut Constant,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        if term.is_inference() {
            self.found_inference = true;
        }

        true
    }
}

impl MutableRecursive<Instance> for SearchInference {
    fn visit(
        &mut self,
        _: &mut Instance,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        true
    }
}

struct EraseInference;

impl MutableRecursive<Lifetime> for EraseInference {
    fn visit(
        &mut self,
        _: &mut Lifetime,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        true
    }
}

impl MutableRecursive<Type> for EraseInference {
    fn visit(
        &mut self,
        term: &mut Type,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        if term.is_inference() {
            *term = Type::Error(pernixc_term::error::Error);
        }

        true
    }
}

impl MutableRecursive<Constant> for EraseInference {
    fn visit(
        &mut self,
        term: &mut Constant,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        if term.is_inference() {
            *term = Constant::Error(pernixc_term::error::Error);
        }

        true
    }
}

impl MutableRecursive<Instance> for EraseInference {
    fn visit(
        &mut self,
        _: &mut Instance,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        true
    }
}

struct ReplaceInference<'a, 'x> {
    inference_context: &'x mut InferenceContext,
    premise: &'a Premise,
    engine: &'a TrackedEngine,
    rendering_map: RenderingMap,
    handler: &'a dyn Handler<Diagnostic>,

    unrecoverable_error: Option<UnrecoverableError>,
}

impl ReplaceInference<'_, '_> {
    const fn bail(&mut self) -> Result<(), UnrecoverableError> {
        if let Some(e) = self.unrecoverable_error.take() {
            Err(e)
        } else {
            Ok(())
        }
    }
}

async fn simplify_inference<T: Term + Default>(
    env: &Environment<'_, InferenceContext>,
    term: &mut T,
    span: RelativeSpan,
    handler: &dyn Handler<Diagnostic>,
) -> Result<(), UnrecoverableError> {
    match env.simplify(std::mem::take(term)).await {
        Ok(simplified) => {
            *term = simplified.result.clone();

            Ok(())
        }

        Err(overflow) => {
            Err(overflow.report_as_type_calculating_overflow(span, &handler))
        }
    }
}

fn erase_inference(resolution: &mut ResolutionMut<'_>) {
    for mut term in resolution.iter_all_term_mut() {
        let mut erase_inference = EraseInference;

        match &mut term {
            pernixc_term::TermMut::Constant(constant) => {
                visitor::accept_recursive_mut(*constant, &mut erase_inference);
            }
            pernixc_term::TermMut::Lifetime(lifetime) => {
                visitor::accept_recursive_mut(*lifetime, &mut erase_inference);
            }
            pernixc_term::TermMut::Type(ty) => {
                visitor::accept_recursive_mut(*ty, &mut erase_inference);
            }
            pernixc_term::TermMut::Instance(instance) => {
                visitor::accept_recursive_mut(*instance, &mut erase_inference);
            }
        }
    }
}

impl ReplaceInference<'_, '_> {
    async fn finalize_type_inference(
        &mut self,
        span: RelativeSpan,
        resolution: &mut ResolutionMut<'_>,
    ) -> Result<bool, ()> {
        let env = Environment::new(
            Cow::Borrowed(self.premise),
            Cow::Borrowed(self.engine),
            self.inference_context,
        );

        let mut found_inference = false;
        for mut term in resolution.iter_all_term_mut() {
            let result = match &mut term {
                pernixc_term::TermMut::Constant(constant) => {
                    simplify_inference(&env, *constant, span, self.handler)
                        .await
                }

                pernixc_term::TermMut::Lifetime(_) => continue,

                pernixc_term::TermMut::Type(ty) => {
                    simplify_inference(&env, *ty, span, self.handler).await
                }
                pernixc_term::TermMut::Instance(instance) => {
                    simplify_inference(&env, *instance, span, self.handler)
                        .await
                }
            };

            if let Err(e) = result {
                self.unrecoverable_error = Some(e);
                return Err(());
            }

            let mut search_inference =
                SearchInference { found_inference: false };

            match &mut term {
                pernixc_term::TermMut::Constant(constant) => {
                    visitor::accept_recursive_mut(
                        *constant,
                        &mut search_inference,
                    );
                }
                pernixc_term::TermMut::Lifetime(lifetime) => {
                    visitor::accept_recursive_mut(
                        *lifetime,
                        &mut search_inference,
                    );
                }
                pernixc_term::TermMut::Type(ty) => {
                    visitor::accept_recursive_mut(*ty, &mut search_inference);
                }
                pernixc_term::TermMut::Instance(instance) => {
                    visitor::accept_recursive_mut(
                        *instance,
                        &mut search_inference,
                    );
                }
            }

            found_inference |= search_inference.found_inference;
        }

        Ok(found_inference)
    }
}

impl Transformer for ReplaceInference<'_, '_> {
    #[allow(clippy::too_many_lines)]
    async fn transform(
        &mut self,
        mut resolution: ResolutionMut<'_>,
        span: RelativeSpan,
    ) {
        if self.unrecoverable_error.is_some() {
            return;
        }

        let Ok(found_inference) =
            self.finalize_type_inference(span, &mut resolution).await
        else {
            return;
        };

        if found_inference {
            self.handler.receive(
                TypeAnnotationRequired {
                    span,
                    term: resolution.to_owned(),
                    rendering_map: self.rendering_map.clone(),
                }
                .into(),
            );
        }

        erase_inference(&mut resolution);

        match resolution {
            ResolutionMut::Symbol(symbol) => {
                let inst = symbol.create_instantiation(self.engine).await;

                if let Err(e) = self
                    .inference_context
                    .resolve_inferring_instance_variable(
                        symbol.id(),
                        &inst,
                        &span,
                        self.engine,
                        self.premise,
                        self.handler,
                    )
                    .await
                {
                    self.unrecoverable_error = Some(e);
                }
            }

            ResolutionMut::Variant(_)
            | ResolutionMut::AssociatedSymbol(_)
            | ResolutionMut::InstanceAssociated(_)
            | ResolutionMut::Type(_)
            | ResolutionMut::Lifetime(_) => {}
        }
    }
}

impl Binder<'_> {
    pub(super) async fn transform_inference(
        &mut self,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<(), UnrecoverableError> {
        self.inference_context.fill_default_inferences();
        let rendering_map = self.get_rendering_map();

        let mut transformer = ReplaceInference {
            inference_context: &mut self.inference_context,
            premise: &self.environment.premise,
            engine: self.engine,
            rendering_map,
            handler,

            unrecoverable_error: None,
        };

        self.ir.transform(&mut transformer, self.engine).await;
        transformer.bail()?;

        self.ir_map.transform(&mut transformer, self.engine).await;
        transformer.bail()?;

        self.effect_handler_context
            .transform(&mut transformer, self.engine)
            .await;
        transformer.bail()?;

        self.closure_parameters_map
            .transform(&mut transformer, self.engine)
            .await;
        transformer.bail()?;

        self.captures_map.transform(&mut transformer, self.engine).await;
        transformer.bail()?;

        Ok(())
    }
}
