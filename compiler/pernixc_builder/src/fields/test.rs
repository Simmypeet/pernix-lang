use pernixc_component::fields::Fields;
use pernixc_table::component::Accessibility;
use pernixc_term::{
    generic_parameter::{GenericParameters, TypeParameterID},
    r#type::Type,
};

use crate::{fields::diagnostic::FieldDuplication, test::build_table};

const FIELDS: &str = r"
public struct Pair[T, U] {
    public first: T,
    private second: U,
}
";

#[test]
fn fields() {
    let (table, errors) = build_table(FIELDS);

    assert_eq!(errors.len(), 0);

    let pair_struct_id = table.get_by_qualified_name(["test", "Pair"]).unwrap();

    let generic_parameters =
        table.query::<GenericParameters>(pair_struct_id).unwrap();
    let fields = table.query::<Fields>(pair_struct_id).unwrap();

    let test_module = table.get_by_qualified_name(["test"]).unwrap();

    let t_ty = Type::Parameter(TypeParameterID {
        parent: pair_struct_id,
        id: generic_parameters.type_parameter_ids_by_name()["T"],
    });
    let u_ty = Type::Parameter(TypeParameterID {
        parent: pair_struct_id,
        id: generic_parameters.type_parameter_ids_by_name()["U"],
    });

    assert_eq!(fields.fields.len(), 2);

    let first_id = fields.field_ids_by_name["first"];
    let second_id = fields.field_ids_by_name["second"];

    assert_eq!(fields.field_declaration_order, [first_id, second_id]);

    let first = &fields.fields[first_id];
    let second = &fields.fields[second_id];

    assert_eq!(first.name, "first");
    assert_eq!(first.r#type, t_ty);
    assert_eq!(first.accessibility, Accessibility::Public);

    assert_eq!(second.name, "second");
    assert_eq!(second.r#type, u_ty);
    assert_eq!(second.accessibility, Accessibility::Scoped(test_module.id));
}

const FIELD_DUPLICATION: &str = r"
public struct Pair[T, U] {
    public first: T,
    public first: U,
}
";

#[test]
fn field_duplication() {
    let (table, errors) = build_table(FIELD_DUPLICATION);

    assert_eq!(errors.len(), 1);

    let pair_struct_id = table.get_by_qualified_name(["test", "Pair"]).unwrap();

    let fields = table.query::<Fields>(pair_struct_id).unwrap();

    let error = errors
        .first()
        .unwrap()
        .as_any()
        .downcast_ref::<FieldDuplication>()
        .unwrap();

    assert_eq!(error.struct_id, pair_struct_id);
    assert_eq!(error.field_id, fields.field_ids_by_name["first"]);
    assert_eq!(error.redeclaration_span.str(), "first");
}
