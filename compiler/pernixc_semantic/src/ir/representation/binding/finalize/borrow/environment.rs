use std::collections::HashSet;

use pernixc_base::{handler::Handler, source_file::Span};

use crate::{
    arena::Arena,
    error::{
        self, OverflowOperation, TypeSystemOverflow, UnsatisifedPredicate,
        VariableDoesNotLiveLongEnough,
    },
    ir::{
        self,
        address::Memory,
        representation::{
            borrow::{Loan, Model as BorrowModel, Origin},
            Values,
        },
    },
    symbol::{table, GlobalID},
    type_system::{
        environment::Environment as TyEnvironment,
        normalizer::Normalizer,
        observer::Observer,
        predicate::{Outlives, Predicate},
        sub_term::TermLocation,
        term::{constant::Constant, lifetime::Lifetime, r#type::Type},
        visitor::{self, Recursive},
        Compute, Satisfied,
    },
};

/// Contains all the information related to borrows and accesses used for
/// borrow checking.
///
/// This is used for processing whether the borrows are valid or not.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Environment {
    /// Contains the borrow origins.
    pub origins: Arena<Origin>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct LoanVisitor<'a> {
    origins: &'a Arena<Origin>,
    loans: HashSet<Loan>,
}

impl Recursive<'_, Lifetime<BorrowModel>> for LoanVisitor<'_> {
    fn visit(
        &mut self,
        term: &'_ Lifetime<BorrowModel>,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        match term {
            Lifetime::Static => {
                self.loans.insert(Loan::Static);
            }
            Lifetime::Parameter(member_id) => {
                self.loans.insert(Loan::LifetimeParameter(*member_id));
            }
            Lifetime::Inference(origin) => {
                self.loans.extend(
                    self.origins.get(*origin).unwrap().loans.iter().copied(),
                );
            }
            Lifetime::Forall(_) => unreachable!(),
            Lifetime::Error(_) => {}
        }

        true
    }
}

impl Recursive<'_, Type<BorrowModel>> for LoanVisitor<'_> {
    fn visit(
        &mut self,
        _: &'_ Type<BorrowModel>,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        true
    }
}

impl Recursive<'_, Constant<BorrowModel>> for LoanVisitor<'_> {
    fn visit(
        &mut self,
        _: &'_ Constant<BorrowModel>,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        true
    }
}

impl Environment {
    pub fn merge(&mut self, other: &Self) {
        assert_eq!(self.origins.len(), other.origins.len());

        for id in other.origins.ids() {
            self.origins
                .get_mut(id)
                .unwrap()
                .loans
                .extend(other.origins.get(id).unwrap().loans.iter().cloned());
        }
    }

    /// Gets the loans existing in the given type.
    pub fn get_loans(&self, ty: &Type<BorrowModel>) -> HashSet<Loan> {
        let mut visitor =
            LoanVisitor { origins: &self.origins, loans: HashSet::new() };

        visitor::accept_recursive(ty, &mut visitor);

        visitor.loans
    }

    pub fn handle_outlives<S: table::State>(
        &mut self,
        outlives: &Outlives<Lifetime<BorrowModel>>,
        checking_span: Span,
        current_site: GlobalID,
        values: &Values<BorrowModel>,
        ty_environment: &TyEnvironment<
            BorrowModel,
            S,
            impl Normalizer<BorrowModel, S>,
            impl Observer<BorrowModel, S>,
        >,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<(), TypeSystemOverflow<ir::Model>> {
        match (&outlives.operand, &outlives.bound) {
            (operand, Lifetime::Inference(bound_origin)) => match operand {
                Lifetime::Inference(operand_inference) => {
                    let operand_loans = self
                        .origins
                        .get(*operand_inference)
                        .unwrap()
                        .loans
                        .iter()
                        .copied()
                        .collect::<Vec<_>>();

                    let bound_origin =
                        self.origins.get_mut(*bound_origin).unwrap();

                    bound_origin.loans.extend(operand_loans);
                }

                Lifetime::Static => {
                    self.origins
                        .get_mut(*bound_origin)
                        .unwrap()
                        .loans
                        .insert(Loan::Static);
                }
                Lifetime::Parameter(member_id) => {
                    self.origins
                        .get_mut(*bound_origin)
                        .unwrap()
                        .loans
                        .insert(Loan::LifetimeParameter(*member_id));
                }
                Lifetime::Forall(_) => unreachable!(),
                Lifetime::Error(_) => {}
            },

            (Lifetime::Inference(origin_id), bound) => {
                let origin = self.origins.get(*origin_id).unwrap();

                for loan in &origin.loans {
                    if let Loan::Borrow(borrow) = loan {
                        let root_memory = values
                            .registers
                            .get(*borrow)
                            .unwrap()
                            .assignment
                            .as_reference_of()
                            .unwrap()
                            .address
                            .get_root_memory();

                        let variable_span = match root_memory {
                            Memory::Parameter(id) => ty_environment
                                .table()
                                .get_callable(current_site.try_into().unwrap())
                                .unwrap()
                                .parameters()
                                .get(*id)
                                .unwrap()
                                .span
                                .clone()
                                .unwrap(),
                            Memory::Alloca(id) => {
                                values.allocas.get(*id).unwrap().span.clone()
                            }
                        };

                        handler.receive(Box::new(
                            VariableDoesNotLiveLongEnough {
                                variable_span,
                                for_lifetime: Some(bound.clone()),
                                instantiation_span: checking_span.clone(),
                            },
                        ));
                    } else {
                        // create a regular outlives predicate
                        let outlives = Outlives::new(
                            match loan {
                                Loan::Borrow(_) => unreachable!(),
                                Loan::LifetimeParameter(member_id) => {
                                    Lifetime::Parameter(*member_id)
                                }
                                Loan::Static => Lifetime::Static,
                            },
                            bound.clone(),
                        );

                        if outlives
                            .query(ty_environment)
                            .map_err(|overflow_error| TypeSystemOverflow::<
                                ir::Model,
                            > {
                                operation: OverflowOperation::Predicate(
                                    Predicate::from_other_model(
                                        Predicate::LifetimeOutlives(
                                            outlives.clone(),
                                        ),
                                    ),
                                ),
                                overflow_span: checking_span.clone(),
                                overflow_error,
                            })?
                            .is_none()
                        {
                            handler.receive(Box::new(UnsatisifedPredicate {
                                predicate: Predicate::LifetimeOutlives(
                                    outlives,
                                ),
                                instantiation_span: checking_span.clone(),
                                predicate_declaration_span: None,
                            }));
                        }
                    }
                }
            }

            _ => {
                match outlives.query(ty_environment).map_err(
                    |overflow_error| TypeSystemOverflow::<ir::Model> {
                        operation: OverflowOperation::Predicate(
                            Predicate::from_other_model(
                                Predicate::LifetimeOutlives(outlives.clone()),
                            ),
                        ),
                        overflow_span: checking_span.clone(),
                        overflow_error,
                    },
                )? {
                    Some(Satisfied) => {}
                    None => {
                        handler.receive(Box::new(UnsatisifedPredicate {
                            predicate: Predicate::LifetimeOutlives(
                                outlives.clone(),
                            ),
                            instantiation_span: checking_span,
                            predicate_declaration_span: None,
                        }));
                    }
                }
            }
        }

        Ok(())
    }
}
