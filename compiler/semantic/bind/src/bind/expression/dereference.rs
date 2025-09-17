use pernixc_handler::Handler;
use pernixc_ir::address::{Address, Reference};
use pernixc_lexical::tree::{RelativeLocation, RelativeSpan};
use pernixc_source_file::SourceElement;
use pernixc_term::r#type::{Qualifier, Type};

use crate::{
    bind::{
        expression::dereference::diagnostic::{CannotDereference, Diagnostic},
        Bind, Expression, LValue,
    },
    binder::{Binder, BindingError, Error},
};

pub mod diagnostic;

impl Binder<'_> {
    pub(super) async fn bind_dereference<'a, T>(
        &mut self,
        dereference: &'a T,
        final_span: RelativeSpan,
        handler: &dyn Handler<crate::diagnostic::Diagnostic>,
    ) -> Result<Expression, Error>
    where
        Self: Bind<&'a T>,
        T: SourceElement<Location = RelativeLocation>,
    {
        let operand =
            self.bind_as_lvalue(dereference, true, None, handler).await?;

        // expected a reference type
        let operand_type =
            self.type_of_address(&operand.address, handler).await?;

        let reference_type = match operand_type {
            Type::Reference(reference) => reference,
            found_type => {
                handler.receive(
                    Diagnostic::CannotDereference(CannotDereference {
                        found_type,
                        span: final_span,
                        type_inference_map: self.type_inference_rendering_map(),
                        constant_inference_map: self
                            .constant_inference_rendering_map(),
                    })
                    .into(),
                );
                return Err(Error::Binding(BindingError(dereference.span())));
            }
        };

        let new_qualifier = reference_type.qualifier.min(
            if operand.address.is_behind_reference() {
                operand.qualifier
            } else {
                Qualifier::Mutable
            },
        );

        Ok(Expression::LValue(LValue {
            address: Address::Reference(Reference {
                qualifier: reference_type.qualifier,
                reference_address: Box::new(operand.address),
            }),
            span: final_span,
            qualifier: new_qualifier,
        }))
    }
}
