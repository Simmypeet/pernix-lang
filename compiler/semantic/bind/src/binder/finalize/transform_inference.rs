use std::borrow::Cow;

use pernixc_handler::Handler;
use pernixc_ir::transform::{Element, ResolutionMut, Transformer};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_term::{
    constant::Constant,
    instance::Instance,
    lifetime::Lifetime,
    sub_term::TermLocation,
    r#type::Type,
    visitor::{self, MutableRecursive},
};
use pernixc_type_system::{environment::Environment, term::Term};

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
        instance: &mut Instance,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        if instance.is_inference() {
            self.found_inference = true;
        }

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
        instance: &mut Instance,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        if instance.is_inference() {
            *instance = Instance::Error(pernixc_term::error::Error);
        }

        true
    }
}

#[derive(Clone)]
struct ReplaceInference<'a> {
    environment: &'a Environment<'a, InferenceContext>,
    rendering_map: RenderingMap,
    handler: &'a dyn Handler<Diagnostic>,
}

impl ReplaceInference<'_> {
    async fn simplify_inference<T: Term + Default>(
        &self,
        term: &mut T,
        span: RelativeSpan,
    ) {
        match self.environment.simplify(std::mem::take(term)).await {
            Ok(simplified) => {
                *term = simplified.result.clone();
            }

            Err(overflow) => {
                overflow
                    .report_as_type_calculating_overflow(span, &self.handler);
            }
        }
    }
}

impl Transformer for ReplaceInference<'_> {
    async fn transform(
        &mut self,
        mut resolution: ResolutionMut<'_>,
        span: RelativeSpan,
    ) {
        for mut term in resolution.iter_all_term_mut() {
            match &mut term {
                pernixc_term::TermMut::Constant(constant) => {
                    self.simplify_inference(*constant, span).await;
                }

                pernixc_term::TermMut::Lifetime(_) => continue,

                pernixc_term::TermMut::Type(ty) => {
                    self.simplify_inference(*ty, span).await;
                }
                pernixc_term::TermMut::Instance(instance) => {
                    self.simplify_inference(*instance, span).await;
                }
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

            if search_inference.found_inference {
                self.handler.receive(
                    TypeAnnotationRequired {
                        span,
                        term: term.to_owned_term(),
                        rendering_map: self.rendering_map.clone(),
                    }
                    .into(),
                );
            }

            let mut erase_inference = EraseInference;

            match &mut term {
                pernixc_term::TermMut::Constant(constant) => {
                    visitor::accept_recursive_mut(
                        *constant,
                        &mut erase_inference,
                    );
                }
                pernixc_term::TermMut::Lifetime(lifetime) => {
                    visitor::accept_recursive_mut(
                        *lifetime,
                        &mut erase_inference,
                    );
                }
                pernixc_term::TermMut::Type(ty) => {
                    visitor::accept_recursive_mut(*ty, &mut erase_inference);
                }
                pernixc_term::TermMut::Instance(instance) => {
                    visitor::accept_recursive_mut(
                        *instance,
                        &mut erase_inference,
                    );
                }
            }
        }
    }
}

impl Binder<'_> {
    pub(super) async fn transform_inference(
        &mut self,
        handler: &dyn Handler<Diagnostic>,
    ) {
        self.inference_context.fill_default_inferences();
        let rendering_map = self.get_rendering_map();

        let mut transformer = ReplaceInference {
            environment: &Environment::new(
                Cow::Borrowed(self.premise()),
                Cow::Borrowed(self.engine),
                &self.inference_context,
            ),
            rendering_map,
            handler,
        };

        self.ir.transform(&mut transformer, self.engine).await;
        self.ir_map.transform(&mut transformer, self.engine).await;
        self.effect_handler_context
            .transform(&mut transformer, self.engine)
            .await;
        self.closure_parameters_map
            .transform(&mut transformer, self.engine)
            .await;
        self.captures_map.transform(&mut transformer, self.engine).await;
    }
}
