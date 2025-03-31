use std::sync::Arc;

use pernixc_arena::Arena;
use pernixc_component::{
    function_signature::FunctionSignature,
    implied_predicates::ImpliedPredicates,
};
use pernixc_handler::{Panic, Storage};
use pernixc_semantic::{
    component::{Member, Name, Parent, SymbolKind},
    diagnostic::Diagnostic,
    GlobalID, Table, TargetID,
};
use pernixc_syntax::syntax_tree::{
    item::function::ParameterKind, ConnectedList,
};
use pernixc_term::{
    elided_lifetimes::ElidedLifetimes, generic_parameter::GenericParameters,
    r#type::Type, where_clause::WhereClause, Default,
};

use super::{
    expression::{Bind, Config, LValue, Target},
    infer, Binder,
};
use crate::value::Value;

#[derive(Debug)]
pub struct Template {
    pub table: Table,
    pub function_id: GlobalID,

    #[allow(unused)]
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
            GlobalID::new(TargetID(1), pernixc_semantic::ID::ROOT_MODULE);

        let test_function_id = GlobalID::new(
            TargetID(1),
            pernixc_semantic::ID(pernixc_semantic::ID::ROOT_MODULE.0 + 1),
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
                std::iter::once(("test".to_string(), test_function_id.id))
                    .collect(),
            ),
        ));

        assert!(table.add_component(test_function_id, SymbolKind::Function));
        assert!(table.add_component(test_function_id, Name("test".to_string())));
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
            parent: Some(test_root_module_id.id)
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

pub fn build_table(source: impl std::fmt::Display) -> Table {
    let (table, errors): (Table, _) =
        pernixc_builder::utility::build_table(source);

    assert!(errors.is_empty());

    table
}

pub trait CreateBinderAtExt {
    fn create_binder_at<'a>(
        &self,
        qualified_function_name: impl IntoIterator<Item = &'a str>,
    ) -> Binder<'_>;
}

impl CreateBinderAtExt for Table {
    fn create_binder_at<'a>(
        &self,
        qualified_function_name: impl IntoIterator<Item = &'a str>,
    ) -> Binder<'_> {
        let function_id =
            self.get_by_qualified_name(qualified_function_name).unwrap();
        assert_eq!(*self.get::<SymbolKind>(function_id), SymbolKind::Function);

        let function_signature =
        self.get::<pernixc_semantic::component::syntax_tree::FunctionSignature>(
            function_id,
        );

        Binder::new_function(
            self,
            function_id,
            function_signature
                .parameters
                .connected_list
                .as_ref()
                .into_iter()
                .flat_map(ConnectedList::elements)
                .filter_map(ParameterKind::as_regular)
                .map(|x| &x.irrefutable_pattern),
            &Panic,
        )
        .unwrap()
    }
}

pub trait BindExt<'a> {
    fn bind_as_rvalue_success<T>(&mut self, syntax: T) -> Value<infer::Model>
    where
        Self: Bind<T>;

    fn bind_as_rvalue_error<T>(
        &mut self,
        syntax: T,
    ) -> (Value<infer::Model>, Vec<Box<dyn Diagnostic>>)
    where
        Self: Bind<T>;

    fn bind_as_rvalue_error_fatal<T>(
        &mut self,
        syntax: T,
    ) -> Vec<Box<dyn Diagnostic>>
    where
        Self: Bind<T>;

    fn bind_as_lvalue_success<T>(&mut self, syntax: T) -> LValue
    where
        Self: Bind<T>;
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

    fn bind_as_lvalue_success<T>(&mut self, syntax: T) -> LValue
    where
        Self: Bind<T>,
    {
        let storage = Storage::<Box<dyn Diagnostic>>::new();
        let lvalue = self
            .bind(syntax, Config { target: Target::LValue }, &storage)
            .unwrap()
            .into_l_value()
            .unwrap();

        let storage = std::mem::take(&mut *storage.as_vec_mut());

        assert!(storage.is_empty(), "{storage:?}");

        lvalue
    }

    fn bind_as_rvalue_error<T>(
        &mut self,
        syntax: T,
    ) -> (Value<infer::Model>, Vec<Box<dyn Diagnostic>>)
    where
        Self: Bind<T>,
    {
        let storage = Storage::<Box<dyn Diagnostic>>::new();
        let value = self
            .bind(syntax, Config { target: Target::RValue }, &storage)
            .unwrap_or_else(|err| panic!("{err:?} {storage:?}"))
            .into_r_value()
            .unwrap();

        let storage = std::mem::take(&mut *storage.as_vec_mut());

        (value, storage)
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
