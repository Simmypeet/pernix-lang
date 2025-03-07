use crate::{
    extern_function_check::diagnostic::GenericParametersAreNotAllowedInExternFunction,
    utility::build_table,
};

const LIFETIME_PARAMETERS_ARE_ALLOWED: &str = r#"
extern "C":
    public function write['a](
        fildes: int32,
        buf: *uint8,
        bytes: usize,
    ) -> isize

"#;

#[test]
fn lifetime_parameters_are_allowed() {
    assert!(build_table(LIFETIME_PARAMETERS_ARE_ALLOWED).1.is_empty());
}

const GENERIC_PARAMETERS_ARE_NOT_ALLOWED: &str = r#"
extern "C":
    public function ident[T](x: T) -> T

"#;

#[test]
fn generic_parameters_are_not_allowed() {
    let (table, diagnostics) = build_table(GENERIC_PARAMETERS_ARE_NOT_ALLOWED);

    let ident_id = table.get_by_qualified_name(["test", "ident"]).unwrap();

    assert_eq!(diagnostics.len(), 1);

    let err = diagnostics[0]
        .as_any()
        .downcast_ref::<GenericParametersAreNotAllowedInExternFunction>()
        .unwrap();

    assert_eq!(err.extern_function_id, ident_id);
}
