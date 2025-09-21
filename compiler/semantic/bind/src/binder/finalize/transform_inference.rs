use std::borrow::Cow;

use pernixc_handler::Handler;
use pernixc_ir::transform::{ConstantTermSource, Transformer, TypeTermSource};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::runtime::executor::CyclicError;
use pernixc_term::{
    lifetime::Lifetime,
    r#type::Type,
    sub_term::TermLocation,
    visitor::{self, MutableRecursive},
};
use pernixc_type_system::environment::Environment;

use crate::{
    binder::Binder,
    diagnostic::{
        ConstantAnnotationRequired, Diagnostic, TypeAnnotationRequired,
    },
    inference_context::InferenceContext,
};

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

impl MutableRecursive<pernixc_term::constant::Constant> for EraseInference {
    fn visit(
        &mut self,
        term: &mut pernixc_term::constant::Constant,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        if term.is_inference() {
            *term = pernixc_term::constant::Constant::Error(
                pernixc_term::error::Error,
            );
        }

        true
    }
}

#[derive(Clone, Copy)]
struct ReplaceInference<'a> {
    environment: &'a Environment<'a, InferenceContext>,
    handler: &'a dyn Handler<Diagnostic>,
}

impl Transformer<Lifetime> for ReplaceInference<'_> {
    async fn transform(
        &mut self,
        _term: &mut Lifetime,
        _source: <Lifetime as pernixc_ir::transform::Transformable>::Source,
        _span: Option<RelativeSpan>,
    ) -> Result<(), CyclicError> {
        Ok(())
    }
}

impl Transformer<Type> for ReplaceInference<'_> {
    async fn transform(
        &mut self,
        term: &mut Type,
        source: TypeTermSource,
        span: Option<RelativeSpan>,
    ) -> Result<(), CyclicError> {
        match self.environment.simplify(std::mem::take(term)).await {
            Ok(simplified) => {
                *term = simplified.result.clone();
            }
            Err(pernixc_type_system::Error::Overflow(overflow)) => {
                overflow.report_as_type_calculating_overflow(
                    span.unwrap(),
                    &self.handler,
                );
            }
            Err(pernixc_type_system::Error::CyclicDependency(err)) => {
                return Err(err)
            }
        }

        if let (true, TypeTermSource::GenericParameter(generic_parameter)) =
            (term.is_inference(), source)
        {
            self.handler.receive(Diagnostic::TypeAnnotationRequired(
                TypeAnnotationRequired {
                    span: span.unwrap(),
                    generic_parameter,
                },
            ));
        }

        // Erase any remaining inference variables.
        visitor::accept_recursive_mut(term, &mut EraseInference);

        Ok(())
    }
}

impl Transformer<pernixc_term::constant::Constant> for ReplaceInference<'_> {
    async fn transform(
        &mut self,
        term: &mut pernixc_term::constant::Constant,
        source: <pernixc_term::constant::Constant as pernixc_ir::transform::Transformable>::Source,
        span: Option<RelativeSpan>,
    ) -> Result<(), CyclicError> {
        match self.environment.simplify(std::mem::take(term)).await {
            Ok(simplified) => {
                *term = simplified.result.clone();
            }
            Err(pernixc_type_system::Error::Overflow(overflow)) => {
                overflow.report_as_type_calculating_overflow(
                    span.unwrap(),
                    &self.handler,
                );
            }
            Err(pernixc_type_system::Error::CyclicDependency(err)) => {
                return Err(err)
            }
        }

        if let (true, ConstantTermSource::GenericParameter(generic_parameter)) =
            (term.is_inference(), source)
        {
            self.handler.receive(Diagnostic::ConstantAnnotationRequired(
                ConstantAnnotationRequired {
                    span: span.unwrap(),
                    generic_parameter,
                },
            ));
        }

        // Erase any remaining inference variables.
        visitor::accept_recursive_mut(term, &mut EraseInference);

        Ok(())
    }
}

impl Binder<'_> {
    pub(super) async fn transform_inference(
        &mut self,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<(), CyclicError> {
        self.inference_context.fill_default_inferences();

        let mut transformer = ReplaceInference {
            environment: &Environment::new(
                Cow::Borrowed(&self.premise),
                Cow::Borrowed(self.engine),
                &self.inference_context,
            ),
            handler,
        };

        self.ir.transform(self.engine, &mut transformer).await?;

        Ok(())
    }
}
