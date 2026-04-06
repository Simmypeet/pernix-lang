//! Shared helpers for binding refutable pattern matches.

use pernixc_arena::ID;
use pernixc_handler::Handler;
use pernixc_ir::{
    address::{Address, Memory},
    pattern::{NameBindingPoint, Refutable, Wildcard},
    scope,
    value::{
        Value,
        literal::{self, Literal},
    },
};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_source_file::SourceElement;
use pernixc_term::r#type::{Qualifier, Type};

use crate::{
    bind::{Bind, Expression, Guidance},
    binder::{Binder, Error, UnrecoverableError},
    diagnostic::{Diagnostic, FoundPackTuplePatternInMatchArmPattern},
    infer::constraint,
    pattern::insert_name_binding,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MatchScrutineeBindingResult {
    address: Address,
    qualifier: Qualifier,
    from_lvalue: bool,
    address_type: Type,
    span: RelativeSpan,
}

impl MatchScrutineeBindingResult {
    #[must_use]
    pub const fn address(&self) -> &Address { &self.address }

    #[must_use]
    pub const fn qualifier(&self) -> Qualifier { self.qualifier }

    #[must_use]
    pub const fn from_lvalue(&self) -> bool { self.from_lvalue }

    #[must_use]
    pub const fn address_type(&self) -> &Type { &self.address_type }

    #[must_use]
    pub const fn span(&self) -> &RelativeSpan { &self.span }
}

impl Binder<'_> {
    pub async fn bind_match_scrutinee_expression(
        &mut self,
        binary: &pernixc_syntax::expression::binary::Binary,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<MatchScrutineeBindingResult, Error> {
        let scope_id = self.stack().current_scope().scope_id();
        let span = binary.span();

        let (address, qualifier, from_lvalue) = match self
            .bind(binary, &Guidance::Expression(None), handler)
            .await
        {
            Ok(Expression::LValue(lvalue)) => {
                (lvalue.address, lvalue.qualifier, true)
            }

            Ok(Expression::RValue(value)) => (
                Address::Memory(Memory::Alloca(
                    self.create_alloca_with_value(
                        value, scope_id, None, span, handler,
                    )
                    .await?,
                )),
                Qualifier::Mutable,
                false,
            ),

            Err(Error::Binding(semantic_error)) => {
                let ty_inference =
                    self.create_type_inference(constraint::Type::All(true));

                (
                    Address::Memory(Memory::Alloca(
                        self.create_alloca_with_value(
                            Value::Literal(Literal::Error(literal::Error {
                                r#type: Type::Inference(ty_inference),
                                span: semantic_error.0,
                            })),
                            scope_id,
                            None,
                            span,
                            handler,
                        )
                        .await?,
                    )),
                    Qualifier::Mutable,
                    false,
                )
            }

            Err(Error::Unrecoverable(abrupt_error)) => {
                return Err(Error::Unrecoverable(abrupt_error));
            }
        };

        let address_type = self
            .type_of_address(&address, handler)
            .await
            .map_err(Error::Unrecoverable)?;

        Ok(MatchScrutineeBindingResult {
            address,
            qualifier,
            from_lvalue,
            address_type,
            span,
        })
    }

    pub async fn create_name_binding_point_from_match_scrutinee<
        T: insert_name_binding::InsertNameBinding,
    >(
        &mut self,
        pattern: &T,
        scrutinee: &MatchScrutineeBindingResult,
        scope_id: ID<scope::Scope>,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<NameBindingPoint, UnrecoverableError> {
        let mut name_binding_point = NameBindingPoint::default();

        self.insert_name_binding_point(
            &mut name_binding_point,
            pattern,
            scrutinee.address_type(),
            scrutinee.address().clone(),
            scrutinee.qualifier(),
            &insert_name_binding::Config {
                must_copy: scrutinee.from_lvalue(),
                scope_id,
                address_span: Some(*scrutinee.span()),
            },
            handler,
        )
        .await?;

        Ok(name_binding_point)
    }
}

fn replace_refutable_in_tuple_pack_internal(
    reftuable: &mut Refutable,
    in_tuple_pack: bool,
    handler: &dyn Handler<Diagnostic>,
) {
    match reftuable {
        pattern @ (Refutable::Boolean(_)
        | Refutable::Integer(_)
        | Refutable::Enum(_)) => {
            if in_tuple_pack {
                let span = pattern.span();
                *pattern = Refutable::Wildcard(Wildcard { span });

                handler.receive(
                    Diagnostic::FoundPackTuplePatternInMatchArmPattern(
                        FoundPackTuplePatternInMatchArmPattern {
                            pattern_span: span,
                        },
                    ),
                );
            }
        }

        Refutable::Wildcard(_) | Refutable::Named(_) => {}

        Refutable::Tuple(tuple) => {
            for pat in &mut tuple.elements {
                replace_refutable_in_tuple_pack_internal(
                    &mut pat.pattern,
                    pat.is_packed || in_tuple_pack,
                    handler,
                );
            }
        }
        Refutable::Structural(structural) => {
            for field in structural.patterns_by_field_id.values_mut() {
                replace_refutable_in_tuple_pack_internal(
                    field,
                    in_tuple_pack,
                    handler,
                );
            }
        }
    }
}

pub fn replace_refutable_in_tuple_pack(
    reftuable: &mut Refutable,
    handler: &dyn Handler<Diagnostic>,
) {
    replace_refutable_in_tuple_pack_internal(reftuable, false, handler);
}
