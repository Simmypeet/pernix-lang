use std::sync::Arc;

use pernixc_arena::Arena;
use pernixc_component::{
    function_signature::FunctionSignature,
    implied_predicates::ImpliedPredicates,
};
use pernixc_handler::{Panic, Storage};
use pernixc_table::{
    component::{Member, Name, Parent, SymbolKind},
    diagnostic::Diagnostic,
    GlobalID, Table, TargetID,
};
use pernixc_term::{
    elided_lifetimes::ElidedLifetimes, generic_parameter::GenericParameters,
    r#type::Type, where_clause::WhereClause, Default,
};

use super::{
    expression::{Bind, Config, Target},
    infer, Binder,
};
use crate::Value;

#[derive(Debug)]
pub struct Template {
    pub table: Table,
    pub function_id: GlobalID,
    pub test_module_id: GlobalID,
}

impl Template {
    pub fn new() -> Self {
        Self::new_with_return_type(Type::Tuple(pernixc_term::Tuple {
            elements: Vec::new(),
        }))
    }

    pub fn new_with_return_type(ty: Type<Default>) -> Self {
        let table = Table::new(Arc::new(Panic));

        let test_root_module_id =
            GlobalID::new(TargetID(1), pernixc_table::ID::ROOT_MODULE);

        let test_function_id = GlobalID::new(
            TargetID(1),
            pernixc_table::ID(pernixc_table::ID::ROOT_MODULE.0 + 1),
        );

        assert!(table.add_component(test_root_module_id, SymbolKind::Module));
        assert!(
            table.add_component(test_root_module_id, Name("test".to_string()))
        );
        assert!(
            table.add_component(test_root_module_id, Parent { parent: None })
        );
        assert!(table.add_component(
            test_root_module_id,
            Member(
                std::iter::once(("Test".to_string(), test_function_id.id))
                    .collect(),
            ),
        ));

        assert!(table.add_component(test_function_id, SymbolKind::Function));
        assert!(
            table.add_component(test_function_id, GenericParameters::default())
        );
        assert!(
            table.add_component(test_function_id, ImpliedPredicates::default())
        );
        assert!(
            table.add_component(test_function_id, ElidedLifetimes::default())
        );
        assert!(table.add_component(test_function_id, WhereClause::default()));
        assert!(table.add_component(test_function_id, FunctionSignature {
            parameters: Arena::default(),
            parameter_order: Vec::new(),
            return_type: ty,
        }));
        assert!(table.add_component(test_function_id, Parent {
            parent: Some(test_root_module_id.id),
        }));

        Self {
            table,
            function_id: test_function_id,
            test_module_id: test_root_module_id,
        }
    }

    pub fn create_binder(&self) -> Binder<'_> {
        let storage: Storage<Box<dyn Diagnostic>> = Storage::new();

        let binder = Binder::new_function(
            &self.table,
            self.function_id,
            std::iter::empty(),
            &storage,
        )
        .unwrap();

        binder
    }
}

pub trait BindExt<'a> {
    fn bind_as_rvalue_success<T>(&mut self, syntax: T) -> Value<infer::Model>
    where
        Binder<'a>: Bind<T>;

    fn bind_as_rvalue_error_fatal<T>(
        &mut self,
        syntax: T,
    ) -> Vec<Box<dyn Diagnostic>>
    where
        Binder<'a>: Bind<T>;
}

impl<'a> BindExt<'a> for Binder<'a> {
    fn bind_as_rvalue_success<T>(&mut self, syntax: T) -> Value<infer::Model>
    where
        Self: Bind<T>,
    {
        let storage = Storage::<Box<dyn Diagnostic>>::new();
        let expression = self
            .bind(syntax, Config { target: Target::RValue }, &storage)
            .unwrap()
            .into_r_value()
            .unwrap();

        let storage = std::mem::take(&mut *storage.as_vec_mut());

        assert!(storage.is_empty(), "{storage:?}");

        expression
    }

    fn bind_as_rvalue_error_fatal<T>(
        &mut self,
        syntax: T,
    ) -> Vec<Box<dyn Diagnostic>>
    where
        Self: Bind<T>,
    {
        let storage = Storage::<Box<dyn Diagnostic>>::new();
        assert!(self
            .bind(syntax, Config { target: Target::RValue }, &storage)
            .is_err());

        let storage = std::mem::take(&mut *storage.as_vec_mut());

        storage
    }
}
