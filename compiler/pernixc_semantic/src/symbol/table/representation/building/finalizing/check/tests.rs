use std::{fmt::Display, sync::Arc};

use pernixc_base::{diagnostic::Storage, source_file::SourceFile};
use pernixc_syntax::syntax_tree::target::{self, Target};

use crate::{
    error::{Error, MismatchedImplementationArguments, UnsatisifedPredicate},
    symbol::{
        table::{self, representation::Index, Suboptimal, Success, Table},
        LifetimeParameterID, TypeParameterID,
    },
    type_system::{
        model,
        predicate::{self, Predicate},
        term::{lifetime::Lifetime, r#type::Type, GenericArguments},
    },
};

#[derive(Debug, derive_more::From)]
pub enum ParseTargetError {
    #[allow(dead_code)]
    Target(target::Error),
    #[allow(dead_code)]
    Syntax(pernixc_syntax::error::Error),
    #[allow(dead_code)]
    Lexical(pernixc_lexical::error::Error),
}

struct BuildTableError {
    table: Table<Suboptimal>,
    errors: Vec<Box<dyn Error>>,
}

fn build_table_from_source(
    source: impl Display,
) -> Result<Table<Success>, BuildTableError> {
    let source_file = Arc::new(SourceFile::temp(source).unwrap());

    let storage = Storage::<ParseTargetError>::default();
    let target = Target::parse(&source_file, "test".to_string(), &storage);

    assert!(
        storage.as_vec().is_empty(),
        "Failed to parse target: {:?}",
        storage.as_vec()
    );

    let storage = Storage::<Box<dyn Error>>::default();
    let table = table::build(std::iter::once(target), &storage);

    table.map_err(|error| match error {
        table::BuildTableError::DuplicateTargetName(_) => unreachable!(),
        table::BuildTableError::Suboptimal(table) => {
            BuildTableError { table, errors: storage.into_vec() }
        }
    })
}

fn check_lifetime_matching_error(
    errors: &[Box<dyn Error>],
    first_lifetime: Lifetime<model::Default>,
    second_lifetime: Lifetime<model::Default>,
) {
    assert!(errors.iter().any(|error| {
        let Some(error) = error
            .as_any()
            .downcast_ref::<UnsatisifedPredicate<model::Default>>()
        else {
            return false;
        };

        let Predicate::LifetimeOutlives(outlives) = &error.predicate else {
            return false;
        };

        outlives.operand == first_lifetime && outlives.bound == second_lifetime
    }));

    assert!(errors.iter().any(|error| {
        let Some(error) = error
            .as_any()
            .downcast_ref::<UnsatisifedPredicate<model::Default>>()
        else {
            return false;
        };

        let Predicate::LifetimeOutlives(outlives) = &error.predicate else {
            return false;
        };

        outlives.operand == second_lifetime && outlives.bound == first_lifetime
    }));
}

#[test]
fn simplify_and_check_lifetime_constraints() {
    const SOURCE_CODE: &str = r"
    public trait Foo['a, T] {
        public type Bar;
    }

    public struct Baz['a, 'b, T] 
    where
        trait Foo['a, T],
        Foo['a, T]::Bar = int32,
    {
        public first: Foo['b, T]::Bar, // this is an error
        public second: &'a int32,
        public third: &'b int32,
    }
    ";

    let BuildTableError { table, errors } =
        build_table_from_source(SOURCE_CODE).unwrap_err();

    let baz_sym_id = table
        .get_by_qualified_name(["test", "Baz"].into_iter())
        .unwrap()
        .into_struct()
        .unwrap();
    let baz_sym = table.get(baz_sym_id).unwrap();

    let a_lifetime_id = baz_sym
        .generic_declaration
        .parameters
        .lifetime_parameter_ids_by_name()
        .get("a")
        .copied()
        .unwrap();
    let b_lifetime_id = baz_sym
        .generic_declaration
        .parameters
        .lifetime_parameter_ids_by_name()
        .get("b")
        .copied()
        .unwrap();

    check_lifetime_matching_error(
        &errors,
        Lifetime::Parameter(LifetimeParameterID {
            parent: baz_sym_id.into(),
            id: a_lifetime_id,
        }),
        Lifetime::Parameter(LifetimeParameterID {
            parent: baz_sym_id.into(),
            id: b_lifetime_id,
        }),
    )
}

#[test]
#[allow(clippy::similar_names)]
fn trait_occurrence() {
    const SOURCE_CODE: &str = r"
    public trait Foo['a, T] {
        public type Output;
    }

    public struct Bar['a, T] {
        public first: Foo['a, T]::Output
    }

    public struct Qux['a, 'b, T] 
    where
        trait Foo['a, T]
    {
        public first: Foo['b, T]::Output
    }
    ";

    let BuildTableError { table, errors } =
        build_table_from_source(SOURCE_CODE).unwrap_err();

    let foo_id = table
        .get_by_qualified_name(["test", "Foo"].into_iter())
        .unwrap()
        .into_trait()
        .unwrap();

    let bar_id = table
        .get_by_qualified_name(["test", "Bar"].into_iter())
        .unwrap()
        .into_struct()
        .unwrap();
    let bar_sym = table.get(bar_id).unwrap();

    let bar_a_param = bar_sym
        .generic_declaration
        .parameters
        .lifetime_parameter_ids_by_name()
        .get("a")
        .copied()
        .unwrap();
    let bar_t_param = bar_sym
        .generic_declaration
        .parameters
        .type_parameter_ids_by_name()
        .get("T")
        .copied()
        .unwrap();

    let expected_predicate = Predicate::Trait(predicate::Trait {
        id: foo_id,
        is_const: false,
        generic_arguments: GenericArguments {
            lifetimes: vec![Lifetime::Parameter(LifetimeParameterID {
                parent: bar_id.into(),
                id: bar_a_param,
            })],
            types: vec![Type::Parameter(TypeParameterID {
                parent: bar_id.into(),
                id: bar_t_param,
            })],
            constants: Vec::new(),
        },
    });

    assert!(errors.iter().any(|error| {
        let Some(error) = error
            .as_any()
            .downcast_ref::<UnsatisifedPredicate<model::Default>>()
        else {
            return false;
        };

        error.predicate == expected_predicate
    }));

    let qux_id = table
        .get_by_qualified_name(["test", "Qux"].into_iter())
        .unwrap()
        .into_struct()
        .unwrap();

    let qux_sym = table.get(qux_id).unwrap();

    let qux_a_param = qux_sym
        .generic_declaration
        .parameters
        .lifetime_parameter_ids_by_name()
        .get("a")
        .copied()
        .unwrap();
    let qux_b_param = qux_sym
        .generic_declaration
        .parameters
        .lifetime_parameter_ids_by_name()
        .get("b")
        .copied()
        .unwrap();

    check_lifetime_matching_error(
        &errors,
        Lifetime::Parameter(LifetimeParameterID {
            parent: qux_id.into(),
            id: qux_a_param,
        }),
        Lifetime::Parameter(LifetimeParameterID {
            parent: qux_id.into(),
            id: qux_b_param,
        }),
    )
}

#[test]
fn mismatched_implementation_argument() {
    const SOURCE_CODE: &str = r"
    public struct Tuple[T, U] {
        public first:  T,
        public second: T,
    }

    implements[T] Tuple[T, T] {
        public type Output = T;
    }

    public type Instantiation = Tuple[int32, int64]::Output;
    ";

    let BuildTableError { table: _, errors } =
        build_table_from_source(SOURCE_CODE).unwrap_err();

    errors.iter().any(|error| {
        error
            .as_any()
            .downcast_ref::<MismatchedImplementationArguments<model::Default>>()
            .is_some()
    });
}
