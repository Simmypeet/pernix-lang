use std::collections::{HashMap, HashSet};

use pernixc_base::{handler::Handler, source_file::Span};

use super::context::Access;
use crate::{
    arena::{Arena, ID},
    error::{
        self, OverflowOperation, TypeSystemOverflow, UnsatisifedPredicate,
        VariableDoesNotLiveLongEnough,
    },
    ir::{
        self,
        address::{Address, Memory},
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

    pub invalidated_loans: HashMap<Loan, HashSet<ID<Access>>>,
    pub occurred_accesses: HashSet<ID<Access>>,
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
    /// Invalidates the other loans because of the mutable access to the given
    /// address.
    pub fn invalidate(
        &mut self,
        address: &Address<BorrowModel>,
        values: &Values<BorrowModel>,
        by_access_id: ID<Access>,
    ) {
        for (_, origin) in &self.origins {
            for loan in &origin.loans {
                let Loan::Borrow(borrow) = loan else {
                    continue;
                };

                let borrowed_address = &values
                    .registers
                    .get(*borrow)
                    .unwrap()
                    .assignment
                    .as_reference_of()
                    .unwrap()
                    .address;

                // put the loan in the invalidated loans if the address is a
                // child
                if borrowed_address.is_child_of(&address) {
                    self.invalidated_loans.insert(
                        loan.clone(),
                        std::iter::once(by_access_id).collect(),
                    );
                }
            }
        }
    }

    pub fn merge(&mut self, other: &Self) {
        assert_eq!(self.origins.len(), other.origins.len());

        for id in other.origins.ids() {
            self.origins
                .get_mut(id)
                .unwrap()
                .loans
                .extend(other.origins.get(id).unwrap().loans.iter().cloned());
        }

        for (loan, accesses) in &other.invalidated_loans {
            self.invalidated_loans
                .entry(*loan)
                .or_insert_with(HashSet::new)
                .extend(accesses.iter().cloned());
        }

        self.occurred_accesses.extend(other.occurred_accesses.iter().cloned());
    }

    /// Gets the loans existing in the given lifetime.
    pub fn get_loans_of_lifetime(
        &self,
        lifetime: &Lifetime<BorrowModel>,
    ) -> HashSet<Loan> {
        match lifetime {
            Lifetime::Static => std::iter::once(Loan::Static).collect(),
            Lifetime::Parameter(member_id) => {
                std::iter::once(Loan::LifetimeParameter(*member_id)).collect()
            }
            Lifetime::Inference(origin) => {
                self.origins.get(*origin).unwrap().loans.clone()
            }
            Lifetime::Forall(_) => unreachable!(),
            Lifetime::Error(_) => HashSet::new(),
        }
    }

    /// Gets the loans existing in the given type.
    pub fn get_loans(&self, ty: &Type<BorrowModel>) -> HashSet<Loan> {
        let mut visitor =
            LoanVisitor { origins: &self.origins, loans: HashSet::new() };

        visitor::accept_recursive(ty, &mut visitor);

        visitor.loans
    }

    fn simplify_loan<S: table::State>(
        &self,
        loan: Loan,
        checking_span: Span,
        values: &Values<BorrowModel>,
        current_site: GlobalID,
        ty_environment: &TyEnvironment<
            BorrowModel,
            S,
            impl Normalizer<BorrowModel, S>,
            impl Observer<BorrowModel, S>,
        >,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<HashSet<Loan>, TypeSystemOverflow<ir::Model>> {
        match loan {
            Loan::Borrow(id) => {
                let mut borrowed_address = &values
                    .registers
                    .get(id)
                    .unwrap()
                    .assignment
                    .as_reference_of()
                    .unwrap()
                    .address;

                loop {
                    match borrowed_address {
                        ir::address::Address::Memory(_) => {
                            return Ok(
                                std::iter::once(Loan::Borrow(id)).collect()
                            );
                        }
                        ir::address::Address::Field(field) => {
                            borrowed_address = &field.struct_address;
                        }
                        ir::address::Address::Tuple(tuple) => {
                            borrowed_address = &tuple.tuple_address;
                        }
                        ir::address::Address::Index(index) => {
                            borrowed_address = &index.array_address;
                        }
                        ir::address::Address::Variant(variant) => {
                            borrowed_address = &variant.enum_address;
                        }
                        ir::address::Address::Reference(reference) => {
                            let reference_ty = values
                                .type_of_address(
                                    &reference.reference_address,
                                    current_site,
                                    ty_environment,
                                )
                                .map_err(|x| TypeSystemOverflow::<ir::Model> {
                                    operation: OverflowOperation::TypeOf,
                                    overflow_span: checking_span.clone(),
                                    overflow_error: x.into_overflow().unwrap(),
                                })?
                                .result
                                .into_reference()
                                .unwrap();

                            match reference_ty.lifetime {
                                Lifetime::Static => {
                                    return Ok(
                                        std::iter::once(Loan::Static).collect()
                                    )
                                }
                                Lifetime::Parameter(member_id) => {
                                    return Ok(std::iter::once(
                                        Loan::LifetimeParameter(member_id),
                                    )
                                    .collect());
                                }
                                Lifetime::Inference(origin) => {
                                    let mut final_loans = HashSet::new();
                                    for loan in
                                        &self.origins.get(origin).unwrap().loans
                                    {
                                        final_loans.extend(
                                            self.simplify_loan(
                                                loan.clone(),
                                                checking_span.clone(),
                                                values,
                                                current_site,
                                                ty_environment,
                                                handler,
                                            )?,
                                        );
                                    }

                                    return Ok(final_loans);
                                }
                                Lifetime::Forall(_) => unreachable!(),
                                Lifetime::Error(_) => return Ok(HashSet::new()),
                            }
                        }
                    }
                }
            }
            Loan::LifetimeParameter(member_id) => {
                Ok(std::iter::once(Loan::LifetimeParameter(member_id))
                    .collect())
            }
            Loan::Static => Ok(std::iter::once(Loan::Static).collect()),
        }
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
                let mut final_origins = HashSet::new();
                let origin = self.origins.get(*origin_id).unwrap();

                for loan in &origin.loans {
                    final_origins.extend(self.simplify_loan(
                        loan.clone(),
                        checking_span.clone(),
                        values,
                        current_site,
                        ty_environment,
                        handler,
                    )?);
                }

                for loan in final_origins {
                    if let Loan::Borrow(borrow) = loan {
                        let borrowed_address = &values
                            .registers
                            .get(borrow)
                            .unwrap()
                            .assignment
                            .as_reference_of()
                            .unwrap()
                            .address;

                        let root_memory = borrowed_address.get_root_memory();

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
                                    Lifetime::Parameter(member_id)
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
