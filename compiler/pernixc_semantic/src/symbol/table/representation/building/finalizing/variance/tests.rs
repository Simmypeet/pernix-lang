use std::{fmt::Display, sync::Arc};

use pernixc_base::{diagnostic::Storage, source_file::SourceFile};
use pernixc_syntax::syntax_tree::target::{self, Target};

use crate::{
    error::Error,
    symbol::{
        table::{
            representation::{build, Index},
            Success, Table,
        },
        Variance,
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

fn build_table_from_source(source: impl Display) -> Table<Success> {
    let source_file = Arc::new(SourceFile::temp(source).unwrap());

    let storage = Storage::<ParseTargetError>::default();
    let target = Target::parse(&source_file, "test".to_string(), &storage);

    assert!(
        storage.as_vec().is_empty(),
        "Failed to parse target: {:?}",
        storage.as_vec()
    );

    let storage = Storage::<Box<dyn Error>>::default();
    let table = build(std::iter::once(target), &storage);

    assert!(
        storage.as_vec().is_empty(),
        "Failed to build table: {:?}",
        storage.as_vec()
    );

    table.unwrap()
}

const BASIC_VARIANCES: &str = r"
public struct A[B, C, I]
where
    I: 'static
{
    public covariant: local C,
    public invariant: &'static mutable I
}
";

#[allow(clippy::similar_names)]
#[test]
fn basic_variances() {
    for _ in 0..1000 {
        let table = build_table_from_source(BASIC_VARIANCES);

        let struct_a_sym = table
            .get_by_qualified_name(["test", "A"].into_iter())
            .ok()
            .and_then(|x| table.get(x.into_struct().unwrap()))
            .unwrap();

        let struct_a_b_param = struct_a_sym
            .generic_declaration
            .parameters
            .type_parameter_ids_by_name()
            .get("B")
            .copied()
            .unwrap();
        let struct_a_c_param = struct_a_sym
            .generic_declaration
            .parameters
            .type_parameter_ids_by_name()
            .get("C")
            .copied()
            .unwrap();
        let struct_a_i_param = struct_a_sym
            .generic_declaration
            .parameters
            .type_parameter_ids_by_name()
            .get("I")
            .copied()
            .unwrap();

        assert_eq!(
            struct_a_sym
                .generic_parameter_variances
                .variances_by_type_ids
                .get(&struct_a_b_param)
                .copied()
                .unwrap(),
            Variance::Covariant
        );
        assert_eq!(
            struct_a_sym
                .generic_parameter_variances
                .variances_by_type_ids
                .get(&struct_a_c_param)
                .copied()
                .unwrap(),
            Variance::Covariant
        );
        assert_eq!(
            struct_a_sym
                .generic_parameter_variances
                .variances_by_type_ids
                .get(&struct_a_i_param)
                .copied()
                .unwrap(),
            Variance::Invariant
        );
    }
}

const RECURSIVE_INVARIANT: &str = r"
public enum A['a, T] 
where
    T: 'a
{
    Invariant(&'a mutable T),
    Covariant(B['a, T]),
}

public struct B['a, T]
where
    T: 'a
{
    public a: A['a, T],
}
";

#[test]
#[allow(clippy::similar_names)]
fn recurisve_invariant() {
    // 1000 cases
    for _ in 0..1000 {
        let table = build_table_from_source(RECURSIVE_INVARIANT);

        let enum_a_sym = table
            .get_by_qualified_name(["test", "A"].into_iter())
            .ok()
            .and_then(|x| table.get(x.into_enum().unwrap()))
            .unwrap();

        let struct_b_sym = table
            .get_by_qualified_name(["test", "B"].into_iter())
            .ok()
            .and_then(|x| table.get(x.into_struct().unwrap()))
            .unwrap();

        let enum_a_lt_param = enum_a_sym
            .generic_declaration
            .parameters
            .lifetime_parameter_ids_by_name()
            .get("a")
            .copied()
            .unwrap();

        let struct_b_lt_param = struct_b_sym
            .generic_declaration
            .parameters
            .lifetime_parameter_ids_by_name()
            .get("a")
            .copied()
            .unwrap();

        let enum_a_t_param = enum_a_sym
            .generic_declaration
            .parameters
            .type_parameter_ids_by_name()
            .get("T")
            .copied()
            .unwrap();
        let struct_b_t_param = struct_b_sym
            .generic_declaration
            .parameters
            .type_parameter_ids_by_name()
            .get("T")
            .copied()
            .unwrap();

        assert_eq!(
            enum_a_sym
                .generic_parameter_variances
                .variances_by_type_ids
                .get(&enum_a_t_param)
                .copied()
                .unwrap(),
            Variance::Invariant
        );
        assert_eq!(
            struct_b_sym
                .generic_parameter_variances
                .variances_by_type_ids
                .get(&struct_b_t_param)
                .copied()
                .unwrap(),
            Variance::Invariant
        );

        assert_eq!(
            enum_a_sym
                .generic_parameter_variances
                .variances_by_lifetime_ids
                .get(&enum_a_lt_param)
                .copied()
                .unwrap(),
            Variance::Covariant
        );

        assert_eq!(
            struct_b_sym
                .generic_parameter_variances
                .variances_by_lifetime_ids
                .get(&struct_b_lt_param)
                .copied()
                .unwrap(),
            Variance::Covariant
        );
    }
}

const SINGLE_STRUCT_RECURSIVE_BIVARIANT: &str = r"
public struct A[T] {
    public a: A[T]
}
";

#[test]
#[allow(clippy::similar_names)]
fn single_struct_recursive_covariant() {
    for _ in 0..1000 {
        let table = build_table_from_source(SINGLE_STRUCT_RECURSIVE_BIVARIANT);

        let struct_a_sym = table
            .get_by_qualified_name(["test", "A"].into_iter())
            .ok()
            .and_then(|x| table.get(x.into_struct().unwrap()))
            .unwrap();

        let struct_a_t_param = struct_a_sym
            .generic_declaration
            .parameters
            .type_parameter_ids_by_name()
            .get("T")
            .copied()
            .unwrap();

        assert_eq!(
            struct_a_sym
                .generic_parameter_variances
                .variances_by_type_ids
                .get(&struct_a_t_param)
                .copied()
                .unwrap(),
            Variance::Covariant
        );
    }
}

const MULTIPLE_STRUCT_RECURSIVE_BIVARIANT: &str = r"
public struct A[T] {
    public b: B[T],
}

public struct B[T] {
    public a: A[T],
}
";

#[test]
#[allow(clippy::similar_names)]
fn multiple_struct_recursive_bivariant() {
    for _ in 0..1000 {
        let table =
            build_table_from_source(MULTIPLE_STRUCT_RECURSIVE_BIVARIANT);

        let struct_a_sym = table
            .get_by_qualified_name(["test", "A"].into_iter())
            .ok()
            .and_then(|x| table.get(x.into_struct().unwrap()))
            .unwrap();

        let struct_b_sym = table
            .get_by_qualified_name(["test", "B"].into_iter())
            .ok()
            .and_then(|x| table.get(x.into_struct().unwrap()))
            .unwrap();

        let struct_a_t_param = struct_a_sym
            .generic_declaration
            .parameters
            .type_parameter_ids_by_name()
            .get("T")
            .copied()
            .unwrap();
        let struct_b_t_param = struct_b_sym
            .generic_declaration
            .parameters
            .type_parameter_ids_by_name()
            .get("T")
            .copied()
            .unwrap();

        assert_eq!(
            struct_a_sym
                .generic_parameter_variances
                .variances_by_type_ids
                .get(&struct_a_t_param)
                .copied()
                .unwrap(),
            Variance::Covariant
        );
        assert_eq!(
            struct_b_sym
                .generic_parameter_variances
                .variances_by_type_ids
                .get(&struct_b_t_param)
                .copied()
                .unwrap(),
            Variance::Covariant
        );
    }
}
