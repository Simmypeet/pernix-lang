use std::{fmt::Display, sync::Arc};

use pernixc_base::{handler::Storage, source_file::SourceFile};
use pernixc_syntax::syntax_tree::target::{self, Target};

use crate::{
    error::{
        AdtImplementationIsNotGeneralEnough, Error,
        ImplementationIsNotGeneralEnough, MismatchedImplementationArguments,
        UnsatisifedPredicate,
    },
    symbol::{
        table::{self, representation::Index, Suboptimal, Success, Table},
        GenericID, LifetimeParameterID, TypeParameterID,
    },
    type_system::{
        equality, model,
        predicate::{self, ConstantType, Outlives, Predicate},
        term::{
            lifetime::Lifetime,
            r#type::{Primitive, Qualifier, Reference, Type},
            GenericArguments, MemberSymbol,
        },
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
    let source_file =
        Arc::new(SourceFile::new(source.to_string(), "test".into()));

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
fn predicate_requirements() {
    const SOURCE_CODE: &str = r"
    public trait Fizz['a, T] {}

    public trait Identity[T] {
        public type Output;
    }

    implements[T] Identity[T] {
        public type Output = int32;
    }

    public type Qux['a, T] = &'a T
    where
        trait Fizz['a, T] + Identity[T],
        Identity[T]::Output = T,
        const T,
        tuple T,
        T: 'a,
        'a: 'static;

    public type Instantiate['a, 'b] = Qux['a, &'b float32];
    ";

    let BuildTableError { table, errors } =
        build_table_from_source(SOURCE_CODE).unwrap_err();

    let fizz_id = table
        .get_by_qualified_name(["test", "Fizz"].into_iter())
        .unwrap()
        .into_trait()
        .unwrap();

    let qux_id = table
        .get_by_qualified_name(["test", "Qux"].into_iter())
        .unwrap()
        .into_type()
        .unwrap();

    let identity_output_id = table
        .get_by_qualified_name(["test", "Identity", "Output"].into_iter())
        .unwrap()
        .into_trait_type()
        .unwrap();

    let inst_id = table
        .get_by_qualified_name(["test", "Instantiate"].into_iter())
        .unwrap()
        .into_type()
        .unwrap();

    let _qux_sym = table.get(qux_id).unwrap();
    let inst_sym = table.get(inst_id).unwrap();

    let inst_a_lt = Lifetime::Parameter(LifetimeParameterID {
        parent: GenericID::Type(inst_id),
        id: inst_sym
            .generic_declaration
            .parameters
            .lifetime_parameter_ids_by_name()
            .get("a")
            .copied()
            .unwrap(),
    });
    let inst_b_lt = Lifetime::Parameter(LifetimeParameterID {
        parent: GenericID::Type(inst_id),
        id: inst_sym
            .generic_declaration
            .parameters
            .lifetime_parameter_ids_by_name()
            .get("b")
            .copied()
            .unwrap(),
    });

    let ref_b_float32 = Type::Reference(Reference {
        qualifier: Qualifier::Immutable,
        lifetime: inst_b_lt.clone(),
        pointee: Box::new(Type::Primitive(Primitive::Float32)),
    });

    let expect_predicate = |predicate| {
        errors.iter().any(|x| {
            let Some(error) = x
                .as_any()
                .downcast_ref::<UnsatisifedPredicate<model::Default>>()
            else {
                return false;
            };

            error.predicate == predicate
        })
    };

    // Fizz['a, &'b float32]
    let expected_fizz = Predicate::PositiveTrait(predicate::PositiveTrait {
        id: fizz_id,
        is_const: false,
        generic_arguments: GenericArguments {
            lifetimes: vec![inst_a_lt.clone()],
            types: vec![ref_b_float32.clone()],
            constants: Vec::new(),
        },
    });
    assert!(expect_predicate(expected_fizz));

    // Identity[&'b float32]::Output = &'b float32
    let expected_identity = Predicate::TraitTypeEquality(equality::Equality {
        lhs: MemberSymbol {
            id: identity_output_id,
            member_generic_arguments: GenericArguments::default(),
            parent_generic_arguments: GenericArguments {
                lifetimes: Vec::new(),
                types: vec![ref_b_float32.clone()],
                constants: Vec::new(),
            },
        },
        rhs: ref_b_float32.clone(),
    });
    assert!(expect_predicate(expected_identity));

    // const &'b float32
    let expected_const =
        Predicate::ConstantType(ConstantType(ref_b_float32.clone()));
    assert!(expect_predicate(expected_const));

    // tuple &'b float32
    let expected_tuple =
        Predicate::TupleType(predicate::Tuple(ref_b_float32.clone()));
    assert!(expect_predicate(expected_tuple));

    // &'b float32: 'a
    let expected_type_outlives = Predicate::TypeOutlives(Outlives {
        operand: ref_b_float32.clone(),
        bound: inst_a_lt.clone(),
    });
    assert!(expect_predicate(expected_type_outlives));

    // 'a: 'static
    let expected_lifetime_outlives = Predicate::LifetimeOutlives(Outlives {
        operand: inst_a_lt.clone(),
        bound: Lifetime::Static,
    });
    assert!(expect_predicate(expected_lifetime_outlives));
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

    let expected_predicate =
        Predicate::PositiveTrait(predicate::PositiveTrait {
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

#[test]
fn implementation_is_not_general_enough() {
    const SOURCE_CODE: &str = r"
    public struct TwoReferences['a, 'b, T] 
    where
        T: 'a + 'b
    {
        public first: &'a T,
        public second: &'a T,
    }

    implements['a, T] TwoReferences['a, 'a, T] 
    where
        T: 'a
    {
        public type MyAliass = T;
    }

    public type Constrainted[T] = T
    where
        T: 'static,
        tuple for['x, 'y] TwoReferences['x, 'y, T]::MyAliass;
    ";

    let BuildTableError { table: _, errors } =
        build_table_from_source(SOURCE_CODE).unwrap_err();

    errors.iter().any(|error| {
        error
            .as_any()
            .downcast_ref::<AdtImplementationIsNotGeneralEnough<model::Default>>()
            .is_some()
    });
}

#[test]
fn trait_implementation_is_not_general_enough() {
    const SOURCE_CODE: &str = r"
    public trait Fizz['a, 'b, T] {
        public type Buzz;
    }

    implements['a, T] Fizz['a, 'a, T] {
        public type Buzz = T;
    }

    public type Qux[T] = T
    where
        trait for['x, 'y] Fizz['x, 'y, T];

    public type Instantiate = Qux[int32];
    ";

    let BuildTableError { table: _, errors } =
        build_table_from_source(SOURCE_CODE).unwrap_err();

    errors.iter().any(|error| {
        error
            .as_any()
            .downcast_ref::<ImplementationIsNotGeneralEnough<model::Default>>()
            .is_some()
    });
}

#[test]
fn reference_type_occurrence() {
    const SOURCE_CODE: &str = r"
    public struct ReferenceWrapper['a, T] {
        private inner: &'a T,
    }
    ";

    let BuildTableError { table, errors } =
        build_table_from_source(SOURCE_CODE).unwrap_err();

    let reference_wrapper_id = table
        .get_by_qualified_name(["test", "ReferenceWrapper"].into_iter())
        .unwrap()
        .into_struct()
        .unwrap();

    let reference_wrapper_sym = table.get(reference_wrapper_id).unwrap();

    let a_lifetime_id = reference_wrapper_sym
        .generic_declaration
        .parameters
        .lifetime_parameter_ids_by_name()
        .get("a")
        .copied()
        .unwrap();

    let t_type_id = reference_wrapper_sym
        .generic_declaration
        .parameters
        .type_parameter_ids_by_name()
        .get("T")
        .copied()
        .unwrap();

    let expected_predicate = Predicate::TypeOutlives(Outlives {
        operand: Type::Parameter(TypeParameterID {
            parent: GenericID::Struct(reference_wrapper_id),
            id: t_type_id,
        }),
        bound: Lifetime::Parameter(LifetimeParameterID {
            parent: GenericID::Struct(reference_wrapper_id),
            id: a_lifetime_id,
        }),
    });
    assert!(errors.iter().any(|error| {
        let Some(error) = error
            .as_any()
            .downcast_ref::<UnsatisifedPredicate<model::Default>>()
        else {
            return false;
        };

        error.predicate == expected_predicate
    }),);
}

#[test]
fn check_unpacked_ocurrence() {
    const SOURCE_CODE: &str = r"
    public struct SurroundedWithBools[T] {
        public surrounded: (bool, ...T, bool),
    }
    ";

    let BuildTableError { table, errors } =
        build_table_from_source(SOURCE_CODE).unwrap_err();

    let surrounded_with_bools_id = table
        .get_by_qualified_name(["test", "SurroundedWithBools"].into_iter())
        .unwrap()
        .into_struct()
        .unwrap();

    let surrounded_with_bools_sym =
        table.get(surrounded_with_bools_id).unwrap();

    let t_type_id = surrounded_with_bools_sym
        .generic_declaration
        .parameters
        .type_parameter_ids_by_name()
        .get("T")
        .copied()
        .unwrap();

    let expected_predicate = Predicate::TupleType(predicate::Tuple(
        Type::Parameter(TypeParameterID {
            parent: GenericID::Struct(surrounded_with_bools_id),
            id: t_type_id,
        }),
    ));

    assert!(errors.iter().any(|error| {
        let Some(error) = error
            .as_any()
            .downcast_ref::<UnsatisifedPredicate<model::Default>>()
        else {
            return false;
        };

        error.predicate == expected_predicate
    }));
}
