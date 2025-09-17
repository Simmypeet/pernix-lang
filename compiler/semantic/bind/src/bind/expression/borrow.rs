use pernixc_ir::value::register::{Assignment, Borrow, Register};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_term::{lifetime::Lifetime, r#type::Qualifier};

use crate::{
    bind::{
        expression::borrow::diagnostic::{
            Diagnostic, MismatchedQualifierForReferenceOf,
        },
        LValue,
    },
    binder::Binder,
};

pub mod diagnostic;

impl Binder<'_> {
    pub(super) fn borrow_lvalue(
        &mut self,
        lvalue: LValue,
        borrow_qualifier: Qualifier,
        borrow_span: RelativeSpan,
        handler: &dyn pernixc_handler::Handler<crate::diagnostic::Diagnostic>,
    ) -> pernixc_arena::ID<Register> {
        if lvalue.qualifier < borrow_qualifier {
            handler.receive(
                Diagnostic::MismatchedQualifierForReferenceOf(
                    MismatchedQualifierForReferenceOf {
                        reference_of_span: borrow_span,
                        found_qualifier: lvalue.qualifier,
                        expected_qualifier: borrow_qualifier,
                        is_behind_reference: lvalue
                            .address
                            .is_behind_reference(),
                    },
                )
                .into(),
            );
        }

        self.create_register_assignment(
            Assignment::Borrow(Borrow {
                address: lvalue.address,
                qualifier: borrow_qualifier,
                lifetime: Lifetime::Erased,
            }),
            borrow_span,
        )
    }
}
