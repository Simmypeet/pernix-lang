use std::borrow::Cow;

use pernixc_handler::Handler;
use pernixc_ir::resolution_visitor::{
    MutableResolutionVisitor, ResolutionMut, ResolutionVisitable,
};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_qbice::TrackedEngine;
use pernixc_resolution::qualified_identifier::Variant;
use pernixc_semantic_element::trait_ref::create_instantiation;
use pernixc_symbol::parent::get_parent_global;
use pernixc_term::{
    TermMut,
    constant::Constant,
    generic_arguments::{AssociatedSymbol, Symbol},
    instance::{Instance, InstanceAssociated},
    lifetime::Lifetime,
    sub_term::TermLocation,
    r#type::Type,
    visitor::{self, AsyncRecursive, MutableRecursive},
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

struct InstanceInferenceResolver<'x, 'a> {
    inference_context: &'x mut InferenceContext,
    premise: &'a Premise,
    engine: &'a TrackedEngine,
    handler: &'a dyn Handler<Diagnostic>,

    span: RelativeSpan,
    unrecoverable_error: Option<UnrecoverableError>,
}

impl InstanceInferenceResolver<'_, '_> {
    async fn resolve_inferring_instance_variable_on_symbol(
        &mut self,
        symbol: &Symbol,
    ) -> bool {
        let inst = symbol.create_instantiation(self.engine).await;

        if let Err(er) = self
            .inference_context
            .resolve_inferring_instance_variable(
                symbol.id(),
                &inst,
                &self.span,
                self.engine,
                self.premise,
                self.handler,
            )
            .await
        {
            self.unrecoverable_error = Some(er);
            false
        } else {
            true
        }
    }

    async fn resolve_inferring_instance_variable_on_variant(
        &mut self,
        symbol: &Variant,
    ) -> bool {
        let enum_inst = symbol.create_instantiation(self.engine).await;
        let enum_id = symbol.parent_enum_id(self.engine).await;

        if let Err(err) = self
            .inference_context
            .resolve_inferring_instance_variable(
                enum_id,
                &enum_inst,
                &self.span,
                self.engine,
                self.premise,
                self.handler,
            )
            .await
        {
            self.unrecoverable_error = Some(err);
            false
        } else {
            true
        }
    }

    async fn resolve_inferring_instance_variable_on_associated_symbol(
        &mut self,
        associated_symbol: &AssociatedSymbol,
    ) -> bool {
        let inst = associated_symbol.create_instantiation(self.engine).await;

        if let Err(err) = self
            .inference_context
            .resolve_inferring_instance_variable(
                associated_symbol.id(),
                &inst,
                &self.span,
                self.engine,
                self.premise,
                self.handler,
            )
            .await
        {
            self.unrecoverable_error = Some(err);
            return false;
        }

        let parent_id = self
            .engine
            .get_parent_global(associated_symbol.id())
            .await
            .unwrap();

        if let Err(err) = self
            .inference_context
            .resolve_inferring_instance_variable(
                parent_id,
                &inst,
                &self.span,
                self.engine,
                self.premise,
                self.handler,
            )
            .await
        {
            self.unrecoverable_error = Some(err);
            return false;
        }

        true
    }

    async fn resolve_inferring_instance_variable_on_instance_associated(
        &mut self,
        instance_associated: &InstanceAssociated,
    ) -> bool {
        let Some(inst) =
            instance_associated.create_instantiation(self.engine).await
        else {
            return true;
        };

        if let Err(err) = self
            .inference_context
            .resolve_inferring_instance_variable(
                instance_associated.trait_associated_symbol_id(),
                &inst,
                &self.span,
                self.engine,
                self.premise,
                self.handler,
            )
            .await
        {
            self.unrecoverable_error = Some(err);
            return false;
        }

        true
    }

    async fn reucrsive_resolve_inference_instance(
        &mut self,
        term_muts: impl Iterator<Item = TermMut<'_>>,
    ) -> bool {
        for term in term_muts {
            if self.unrecoverable_error.is_some() {
                return false;
            }

            match term {
                TermMut::Constant(constant) => {
                    visitor::accept_recursive_async(constant, self).await;
                }
                TermMut::Lifetime(_) => {}

                TermMut::Type(ty) => {
                    visitor::accept_recursive_async(ty, self).await;
                }
                TermMut::Instance(instance) => {
                    visitor::accept_recursive_async(instance, self).await;
                }
            }
        }

        if self.unrecoverable_error.is_some() {
            return false;
        }

        true
    }

    async fn resolve_inference_instance_on_resolution(
        &mut self,
        resolution: &mut ResolutionMut<'_>,
    ) -> bool {
        match resolution {
            ResolutionMut::Symbol(symbol) => {
                if !self
                    .resolve_inferring_instance_variable_on_symbol(symbol)
                    .await
                {
                    return false;
                }

                self.reucrsive_resolve_inference_instance(
                    symbol.iter_all_term_mut(),
                )
                .await
            }

            ResolutionMut::Variant(symbol) => {
                if !self
                    .resolve_inferring_instance_variable_on_variant(symbol)
                    .await
                {
                    return false;
                }

                self.reucrsive_resolve_inference_instance(
                    symbol.iter_all_term_mut(),
                )
                .await
            }

            ResolutionMut::AssociatedSymbol(associated_symbol) => {
                if !self
                    .resolve_inferring_instance_variable_on_associated_symbol(
                        associated_symbol,
                    )
                    .await
                {
                    return false;
                }

                self.reucrsive_resolve_inference_instance(
                    associated_symbol.iter_all_term_mut(),
                )
                .await
            }

            ResolutionMut::InstanceAssociated(instance_associated) => {
                if !self
                    .resolve_inferring_instance_variable_on_instance_associated(
                        instance_associated,
                    )
                    .await
                {
                    return false;
                }

                self.reucrsive_resolve_inference_instance(
                    instance_associated.iter_all_term_mut(),
                )
                .await
            }

            ResolutionMut::Type(ty) => {
                self.reucrsive_resolve_inference_instance(std::iter::once(
                    TermMut::Type(ty),
                ))
                .await
            }

            ResolutionMut::Lifetime(_) => self.unrecoverable_error.is_none(),
        }
    }
}

impl AsyncRecursive<Lifetime> for InstanceInferenceResolver<'_, '_> {
    async fn visit(
        &mut self,
        _: &Lifetime,
        _: impl Iterator<Item = TermLocation> + Send,
    ) -> bool {
        if self.unrecoverable_error.is_some() {
            return false;
        }

        true
    }
}

impl AsyncRecursive<Type> for InstanceInferenceResolver<'_, '_> {
    async fn visit(
        &mut self,
        ty: &Type,
        _: impl Iterator<Item = TermLocation> + Send,
    ) -> bool {
        if self.unrecoverable_error.is_some() {
            return false;
        }

        match ty {
            Type::Pointer(_)
            | Type::Reference(_)
            | Type::Array(_)
            | Type::Tuple(_)
            | Type::Phantom(_)
            | Type::Inference(_)
            | Type::Primitive(_)
            | Type::FunctionSignature(_)
            | Type::Error(_)
            | Type::Parameter(_) => true,

            Type::Symbol(symbol) => {
                self.resolve_inferring_instance_variable_on_symbol(symbol).await
            }

            Type::InstanceAssociated(instance_associated) => {
                self.resolve_inferring_instance_variable_on_instance_associated(
                    instance_associated,
                )
                .await
            }

            Type::AssociatedSymbol(associated_symbol) => {
                self.resolve_inferring_instance_variable_on_associated_symbol(
                    associated_symbol,
                )
                .await
            }
        }
    }
}

impl AsyncRecursive<Constant> for InstanceInferenceResolver<'_, '_> {
    async fn visit(
        &mut self,
        _: &Constant,
        _: impl Iterator<Item = TermLocation> + Send,
    ) -> bool {
        if self.unrecoverable_error.is_some() {
            return false;
        }

        true
    }
}

impl AsyncRecursive<Instance> for InstanceInferenceResolver<'_, '_> {
    async fn visit(
        &mut self,
        instance: &Instance,
        _: impl Iterator<Item = TermLocation> + Send,
    ) -> bool {
        if self.unrecoverable_error.is_some() {
            return false;
        }

        match instance {
            Instance::AnonymousTrait(_)
            | Instance::Inference(_)
            | Instance::Error(_)
            | Instance::Parameter(_) => true,

            Instance::Symbol(symbol) => {
                self.resolve_inferring_instance_variable_on_symbol(symbol).await
            }

            Instance::InstanceAssociated(instance_associated) => {
                self.resolve_inferring_instance_variable_on_instance_associated(
                    instance_associated,
                )
                .await
            }
        }
    }
}

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

impl MutableResolutionVisitor for ReplaceInference<'_, '_> {
    #[allow(clippy::too_many_lines)]
    async fn visit_mut(
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

        let mut resolver = InstanceInferenceResolver {
            inference_context: self.inference_context,
            premise: self.premise,
            engine: self.engine,
            handler: self.handler,
            span,
            unrecoverable_error: None,
        };

        resolver
            .resolve_inference_instance_on_resolution(&mut resolution)
            .await;

        self.unrecoverable_error = resolver.unrecoverable_error;

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
    }
}

impl Binder<'_> {
    pub(super) async fn transform_inference(
        &mut self,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<(), UnrecoverableError> {
        self.inference_context.fill_default_inferences();
        let rendering_map = self.get_rendering_map();

        let mut visitor = ReplaceInference {
            inference_context: &mut self.inference_context,
            premise: &self.environment.premise,
            engine: self.engine,
            rendering_map,
            handler,

            unrecoverable_error: None,
        };

        self.ir.accept_mut(&mut visitor).await;
        visitor.bail()?;

        self.ir_map.accept_mut(&mut visitor).await;
        visitor.bail()?;

        self.effect_handler_context.accept_mut(&mut visitor).await;
        visitor.bail()?;

        self.closure_parameters_map.accept_mut(&mut visitor).await;
        visitor.bail()?;

        self.captures_map.accept_mut(&mut visitor).await;
        visitor.bail()?;

        Ok(())
    }
}
