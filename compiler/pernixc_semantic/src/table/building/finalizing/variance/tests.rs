use std::{fmt::Display, sync::Arc};

use pernixc_base::{diagnostic::Storage, source_file::SourceFile};
use pernixc_syntax::syntax_tree::target::{self, Target};

use crate::{
    error::Error,
    symbol::Variance,
    table::{build, Index, Success, Table},
};

#[derive(Debug, derive_more::From)]
pub enum ParseTargetError {
    Target(target::Error),
    Syntax(pernixc_syntax::error::Error),
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
    let table = build(rayon::iter::once(target), &storage);

    assert!(
        storage.as_vec().is_empty(),
        "Failed to build table: {:?}",
        storage.as_vec()
    );

    table.unwrap()
}

const RECURSIVE_INVARIANT: &str = r"
public struct A[T]
where
    T: 'static
{
    public b: B[T],
    public invariant: &'static mutable T,
}

public struct B[T]
where
    T: 'static
{
    public a: A[T],
}

";

#[test]
#[allow(clippy::similar_names)]
fn recurisve_invariant() {
    // 1000 cases
    for i in 0..1000 {
        println!("attempt: {i}");
        let table = build_table_from_source(RECURSIVE_INVARIANT);

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

        /*
        assert_eq!(
            struct_a_sym
                .generic_parameter_variances
                .variances_by_type_ids
                .get(&struct_a_t_param)
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
        */
    }
}

const RECURSIVE_BIVARIANT: &str = r"
public struct A[T] {
    public b: B[T],
}

public struct B[T] {
    public a: A[T],
}
";

#[test]
#[allow(clippy::similar_names)]
fn recursive_bivariant() {
    // 1000 cases
    for i in 0..1000 {
        println!("attempt: {i}");
        let table = build_table_from_source(RECURSIVE_BIVARIANT);

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
            Variance::Bivariant
        );
        assert_eq!(
            struct_b_sym
                .generic_parameter_variances
                .variances_by_type_ids
                .get(&struct_b_t_param)
                .copied()
                .unwrap(),
            Variance::Bivariant
        );
    }
}
