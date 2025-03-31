use pernixc_component::fields::Fields;
use pernixc_semantic::diagnostic::Diagnostic;
use pernixc_semantic::term::{
    generic_arguments::GenericArguments,
    generic_parameter::{
        GenericParameters, LifetimeParameterID, TypeParameterID,
    },
    lifetime::Lifetime,
    predicate::{
        Compatible, ConstantType, NegativeMarker, Outlives, PositiveMarker,
        PositiveTrait, Predicate, Tuple,
    },
    r#type::{Primitive, Qualifier, Reference, TraitMember, Type},
    Default, MemberSymbol, Symbol,
};
use pernixc_type_system::diagnostic::{
    ImplementationIsNotGeneralEnough, UnsatisfiedPredicate,
};

use crate::utility::build_table;

const PREDICATE_REQUIREMENTS: &str = r"
public trait Fizz['a, T]:
    pass


public trait Identity[T]:
    public type Output


implements[T] Identity[T]:
    type Output = int32


public type Qux['a, T] = &'a T:
    where:
        trait Fizz['a, T] + Identity[T]
        Identity[T]::Output = T
        T: 'a + const + tuple
        'a: 'static

public type Instantiate['a, 'b] = Qux['a, &'b float32]
";

#[test]
#[allow(clippy::similar_names)]
fn predicate_requirements() {
    let (table, errors) = build_table(PREDICATE_REQUIREMENTS);

    let fizz_id = table.get_by_qualified_name(["test", "Fizz"]).unwrap();

    let identity_output_id =
        table.get_by_qualified_name(["test", "Identity", "Output"]).unwrap();

    let inst_id = table.get_by_qualified_name(["test", "Instantiate"]).unwrap();

    let inst_generic_parameters =
        table.query::<GenericParameters>(inst_id).unwrap();

    let inst_a_lt = Lifetime::Parameter(LifetimeParameterID {
        parent: inst_id,
        id: inst_generic_parameters.lifetime_parameter_ids_by_name()["a"],
    });

    let inst_b_lt = Lifetime::Parameter(LifetimeParameterID {
        parent: inst_id,
        id: inst_generic_parameters.lifetime_parameter_ids_by_name()["b"],
    });

    let ref_b_float32 = Type::Reference(Reference {
        qualifier: Qualifier::Immutable,
        lifetime: inst_b_lt,
        pointee: Box::new(Type::Primitive(Primitive::Float32)),
    });

    let expect_predicate = |predicate| {
        errors.iter().any(|x| {
            let Some(error) =
                x.as_any().downcast_ref::<UnsatisfiedPredicate<Default>>()
            else {
                return false;
            };

            error.predicate == predicate
        })
    };

    // Fizz['a, &'b float32]
    let expected_fizz = Predicate::PositiveTrait(PositiveTrait {
        trait_id: fizz_id,
        is_const: false,
        generic_arguments: GenericArguments {
            lifetimes: vec![inst_a_lt],
            types: vec![ref_b_float32.clone()],
            constants: Vec::new(),
        },
    });
    assert!(expect_predicate(expected_fizz));

    // Identity[&'b float32]::Output = &'b float32
    let expected_identity = Predicate::TraitTypeCompatible(Compatible {
        lhs: TraitMember(MemberSymbol {
            id: identity_output_id,
            member_generic_arguments: GenericArguments::default(),
            parent_generic_arguments: GenericArguments {
                lifetimes: Vec::new(),
                types: vec![ref_b_float32.clone()],
                constants: Vec::new(),
            },
        }),
        rhs: ref_b_float32.clone(),
    });
    assert!(expect_predicate(expected_identity));

    // const &'b float32
    let expected_const =
        Predicate::ConstantType(ConstantType(ref_b_float32.clone()));
    assert!(expect_predicate(expected_const));

    // tuple &'b float32
    let expected_tuple = Predicate::TupleType(Tuple(ref_b_float32.clone()));
    assert!(expect_predicate(expected_tuple));

    // &'b float32: 'a
    let expected_type_outlives = Predicate::TypeOutlives(Outlives {
        operand: ref_b_float32,
        bound: inst_a_lt,
    });
    assert!(expect_predicate(expected_type_outlives));

    // 'a: 'static
    let expected_lifetime_outlives = Predicate::LifetimeOutlives(Outlives {
        operand: inst_a_lt,
        bound: Lifetime::Static,
    });
    assert!(expect_predicate(expected_lifetime_outlives));
}

fn check_lifetime_matching_error(
    errors: &[Box<dyn Diagnostic>],
    first_lifetime: Lifetime<Default>,
    second_lifetime: Lifetime<Default>,
) {
    assert!(errors.iter().any(|error| {
        let Some(error) =
            error.as_any().downcast_ref::<UnsatisfiedPredicate<Default>>()
        else {
            return false;
        };

        let Predicate::LifetimeOutlives(outlives) = &error.predicate else {
            return false;
        };

        outlives.operand == first_lifetime && outlives.bound == second_lifetime
    }));

    assert!(errors.iter().any(|error| {
        let Some(error) =
            error.as_any().downcast_ref::<UnsatisfiedPredicate<Default>>()
        else {
            return false;
        };

        let Predicate::LifetimeOutlives(outlives) = &error.predicate else {
            return false;
        };

        outlives.operand == second_lifetime && outlives.bound == first_lifetime
    }));
}

const SIMPLIFY_AND_CHECK_LIFETIME_CONSTRAINTS: &str = r"
public trait Foo['a, T]:
    public type Bar

public struct Baz['a, 'b, T] :
    where:
        trait for['x] Foo['x, T]
        Foo['a, T]::Bar = int32

    public first: Foo['b, T]::Bar // this is an error
    public second: &'a int32
    public third: &'b int32
";

#[test]
#[allow(clippy::similar_names)]
fn simplify_and_check_lifetime_constraints() {
    let (table, errors) = build_table(SIMPLIFY_AND_CHECK_LIFETIME_CONSTRAINTS);

    let baz_sym_id = table.get_by_qualified_name(["test", "Baz"]).unwrap();

    let baz_generic_parameters =
        table.query::<GenericParameters>(baz_sym_id).unwrap();

    let baz_a_lt = Lifetime::Parameter(LifetimeParameterID {
        parent: baz_sym_id,
        id: baz_generic_parameters.lifetime_parameter_ids_by_name()["a"],
    });

    let baz_b_lt = Lifetime::Parameter(LifetimeParameterID {
        parent: baz_sym_id,
        id: baz_generic_parameters.lifetime_parameter_ids_by_name()["b"],
    });

    assert_eq!(errors.len(), 2);

    check_lifetime_matching_error(&errors, baz_a_lt, baz_b_lt);

    let baz_fields = table.query::<Fields>(baz_sym_id).unwrap();

    let first_field = &baz_fields.fields[baz_fields.field_ids_by_name["first"]];

    assert_eq!(first_field.r#type, Type::Primitive(Primitive::Int32));
}

const TRAIT_OCCURRENCES: &str = r"
public trait Foo['a, T]:
    public type Output


public struct Bar['a, T]:
    public first: Foo['a, T]::Output


public struct Qux['a, 'b, T]:
    where:
        trait Foo['a, T]

    public first: Foo['b, T]::Output

";

#[test]
#[allow(clippy::similar_names)]
fn trait_occurrences() {
    let (table, errors) = build_table(TRAIT_OCCURRENCES);

    assert_eq!(errors.len(), 3, "{errors:?}");

    let foo_id = table.get_by_qualified_name(["test", "Foo"]).unwrap();
    let bar_id = table.get_by_qualified_name(["test", "Bar"]).unwrap();

    let bar_generic_parameters =
        table.query::<GenericParameters>(bar_id).unwrap();

    let bar_a_lt = Lifetime::Parameter(LifetimeParameterID {
        parent: bar_id,
        id: bar_generic_parameters.lifetime_parameter_ids_by_name()["a"],
    });
    let bar_t_ty = Type::Parameter(TypeParameterID {
        parent: bar_id,
        id: bar_generic_parameters.type_parameter_ids_by_name()["T"],
    });

    let expected_predicate = Predicate::PositiveTrait(PositiveTrait {
        trait_id: foo_id,
        is_const: false,
        generic_arguments: GenericArguments {
            lifetimes: vec![bar_a_lt],
            types: vec![bar_t_ty],
            constants: Vec::new(),
        },
    });

    assert!(errors.iter().any(|error| {
        let Some(error) =
            error.as_any().downcast_ref::<UnsatisfiedPredicate<Default>>()
        else {
            return false;
        };

        error.predicate == expected_predicate
    }));

    let qux_id = table.get_by_qualified_name(["test", "Qux"]).unwrap();

    let qux_generic_parameters =
        table.query::<GenericParameters>(qux_id).unwrap();

    let qux_a_lt = Lifetime::Parameter(LifetimeParameterID::new(
        qux_id,
        qux_generic_parameters.lifetime_parameter_ids_by_name()["a"],
    ));
    let qux_b_lt = Lifetime::Parameter(LifetimeParameterID::new(
        qux_id,
        qux_generic_parameters.lifetime_parameter_ids_by_name()["b"],
    ));

    check_lifetime_matching_error(&errors, qux_a_lt, qux_b_lt);
}

const TRAIT_IMPLEMENTATION_IS_NOT_GENERAL_ENOUGH: &str = r"
public trait Fizz['a, 'b, T]:
    public type Buzz


implements['a, T] Fizz['a, 'a, T]:
    type Buzz = T


public type Qux[T] = T:
    where:
        trait for['x, 'y] Fizz['x, 'y, T]

public type Instantiate = Qux[int32]
";

#[test]
fn trait_implementation_is_not_general_enough() {
    let (_, errors) = build_table(TRAIT_IMPLEMENTATION_IS_NOT_GENERAL_ENOUGH);

    assert_eq!(errors.len(), 1);

    let error = errors[0]
        .as_any()
        .downcast_ref::<ImplementationIsNotGeneralEnough<Default>>()
        .unwrap();

    assert_eq!(error.generic_arguments.lifetimes.len(), 2);
    assert!(
        error.generic_arguments.lifetimes[0]
            != error.generic_arguments.lifetimes[1]
    );
    assert!(error.generic_arguments.lifetimes.iter().all(Lifetime::is_forall));

    assert_eq!(error.generic_arguments.types.len(), 1);
    assert_eq!(
        error.generic_arguments.types[0],
        Type::Primitive(Primitive::Int32)
    );
}

const REFERENCE_TYPE_OCCURRENCE: &str = r"
public struct ReferenceWrapper['a, T]:
    private inner: &'a T

";

#[test]
fn reference_type_occurrence() {
    let (table, errors) = build_table(REFERENCE_TYPE_OCCURRENCE);

    let reference_wrapper_id =
        table.get_by_qualified_name(["test", "ReferenceWrapper"]).unwrap();

    let reference_wrapper_generic_parameters =
        table.query::<GenericParameters>(reference_wrapper_id).unwrap();

    let a_lt = Lifetime::Parameter(LifetimeParameterID {
        parent: reference_wrapper_id,
        id: reference_wrapper_generic_parameters
            .lifetime_parameter_ids_by_name()["a"],
    });

    let t_ty = Type::Parameter(TypeParameterID {
        parent: reference_wrapper_id,
        id: reference_wrapper_generic_parameters.type_parameter_ids_by_name()
            ["T"],
    });

    let expected_predicate = Predicate::TypeOutlives(Outlives::new(t_ty, a_lt));

    assert_eq!(errors.len(), 1);

    assert!(errors.iter().any(|error| {
        let Some(error) =
            error.as_any().downcast_ref::<UnsatisfiedPredicate<Default>>()
        else {
            return false;
        };

        error.predicate == expected_predicate
    }),);
}

const CHECK_UNPACKED_OCCURRENCE: &str = r"
public struct SurroundedWithBools[T]:
    public surrounded: (bool, ...T, bool)

";

#[test]
fn check_unpacked_occurrence() {
    let (table, errors) = build_table(CHECK_UNPACKED_OCCURRENCE);

    let surrounded_with_bools_id =
        table.get_by_qualified_name(["test", "SurroundedWithBools"]).unwrap();

    let generic_parameters =
        table.query::<GenericParameters>(surrounded_with_bools_id).unwrap();

    let t_type_id = Type::Parameter(TypeParameterID::new(
        surrounded_with_bools_id,
        generic_parameters.type_parameter_ids_by_name()["T"],
    ));

    let expected_predicate = Predicate::TupleType(Tuple(t_type_id));

    assert_eq!(errors.len(), 1);

    assert!(errors.iter().any(|error| {
        let Some(error) =
            error.as_any().downcast_ref::<UnsatisfiedPredicate<Default>>()
        else {
            return false;
        };

        error.predicate == expected_predicate
    }));
}

const TRAIT_IMPLEMENTATION_IS_NOT_GENERAL_ENOUGH_BY_CONSTRAINTS: &str = r"
public trait Fizz['a, T]:
    pass


final implements['a, 'b, T: 'b] Fizz['a, &'b T]:
    pass


final implements['a, 'b, T: 'b] Fizz['a, &'b mut T]:
    where:
        'a: 'b // this constraint makes lifetime 'a be specific to 'b


public type WithRequirement[T] = T:
    where:
        trait for['x] Fizz['x, T]


// this instantiation is valid
public type First['a, T] = WithRequirement[&'a T]:
    where:
        T: 'a


// this instantiation is invalid (not general enough)
public type Second['a, T] = WithRequirement[&'a mut T]:
    where:
        T: 'a
";

#[test]
fn trait_implementation_is_not_general_enough_by_constraints() {
    let (table, errors) =
        build_table(TRAIT_IMPLEMENTATION_IS_NOT_GENERAL_ENOUGH_BY_CONSTRAINTS);

    assert_eq!(errors.len(), 1);

    let error = errors[0]
        .as_any()
        .downcast_ref::<ImplementationIsNotGeneralEnough<Default>>()
        .unwrap();

    let second_id = table.get_by_qualified_name(["test", "Second"]).unwrap();
    let second_generic_params =
        table.query::<GenericParameters>(second_id).unwrap();

    let second_a_lt = Lifetime::Parameter(LifetimeParameterID {
        parent: second_id,
        id: second_generic_params.lifetime_parameter_ids_by_name()["a"],
    });
    let second_t_ty = Type::Parameter(TypeParameterID {
        parent: second_id,
        id: second_generic_params.type_parameter_ids_by_name()["T"],
    });

    assert_eq!(error.generic_arguments.lifetimes.len(), 1);
    assert!(error.generic_arguments.lifetimes[0].is_forall());

    assert_eq!(error.generic_arguments.types.len(), 1);
    assert_eq!(
        error.generic_arguments.types[0],
        Type::Reference(Reference {
            qualifier: Qualifier::Mutable,
            lifetime: second_a_lt,
            pointee: Box::new(second_t_ty),
        })
    );
}

const FOR_ALL_LIFETIME_AS_AN_OPERAND_IN_OUTLIVES: &str = r"
public trait Fizz['a, T]:
    pass


final implements['a, 'b, T] Fizz['a, &'b T]:
    where:
        T: 'b


final implements['a, 'b, T: 'b] Fizz['a, &'b mut T]:
    where:
        'b: 'a


public type WithRequirement[T] = T:
    where:
        trait for['x] Fizz['x, T]


// this instantiation is valid
public type First['a, T] = WithRequirement[&'a T]:
    where:
        T: 'a


// this instantiation is invalid ('a: 'static is not satisfied) 
public type Second['a, T] = WithRequirement[&'a mut T]:
    where:
        T: 'a
";

#[test]
fn for_all_lifetime_as_an_operand_in_outlives() {
    let (table, errors) =
        build_table(FOR_ALL_LIFETIME_AS_AN_OPERAND_IN_OUTLIVES);

    assert_eq!(errors.len(), 1);

    let error = errors[0]
        .as_any()
        .downcast_ref::<UnsatisfiedPredicate<Default>>()
        .unwrap();

    let found = error.predicate.as_lifetime_outlives().unwrap();

    let second_id = table.get_by_qualified_name(["test", "Second"]).unwrap();

    let second_generic_params =
        table.query::<GenericParameters>(second_id).unwrap();

    let second_a_lt = Lifetime::Parameter(LifetimeParameterID {
        parent: second_id,
        id: second_generic_params.lifetime_parameter_ids_by_name()["a"],
    });

    assert_eq!(found.bound, Lifetime::Static);
    assert_eq!(found.operand, second_a_lt);
}

const MARKER_SATISFIED: &str = r"
public marker Marker[T]


final implements Marker[int32]:
    pass


final implements Marker[bool]:
    pass


public struct MyStruct:
    public a: int32
    public b: bool


public struct EmptyStruct:
    pass


public enum MyEnum:
    A(int32)
    B(bool)


public enum EmptyEnum:
    pass


public type WithRequirement[T] = T:
    where:
        marker Marker[T]


public type InstMyStruct    = WithRequirement[MyStruct]
public type InstEmptyStruct = WithRequirement[EmptyStruct]
public type InstMyEnum      = WithRequirement[MyEnum]
public type InstEmptyEnum   = WithRequirement[EmptyEnum]
";

#[test]
pub fn marker_satisfied() {
    let (_, errors) = build_table(MARKER_SATISFIED);

    assert!(errors.is_empty());
}

const MARKER_NOT_SATISFIED: &str = r"
public marker Marker[T]


final implements Marker[int32]:
    pass


public struct MyStruct:
    public a: int32
    public b: bool


public type WithRequirement[T] = T:
    where:
        marker Marker[T]


public type InstMyStruct = WithRequirement[MyStruct]
";

#[test]
pub fn marker_not_satisfied() {
    let (table, errors) = build_table(MARKER_NOT_SATISFIED);

    assert_eq!(errors.len(), 1);

    let error = errors[0]
        .as_any()
        .downcast_ref::<UnsatisfiedPredicate<Default>>()
        .unwrap();

    let marker_id = table.get_by_qualified_name(["test", "Marker"]).unwrap();
    let my_struct_id =
        table.get_by_qualified_name(["test", "MyStruct"]).unwrap();

    let expected_predicate =
        Predicate::PositiveMarker::<Default>(PositiveMarker {
            marker_id,
            generic_arguments: GenericArguments {
                lifetimes: Vec::new(),
                types: vec![Type::Symbol(Symbol {
                    id: my_struct_id,
                    generic_arguments: GenericArguments::default(),
                })],
                constants: Vec::new(),
            },
        });

    assert_eq!(error.predicate, expected_predicate);
}

const MARKER_WITH_CONSTRAINTS: &str = r"
public marker Fizz[T]
public marker Buzz[T]


final implements Fizz[int32]:
    pass


final implements[T] Buzz[(T,)]:
    where:
        marker Fizz[T]


public struct MyStruct:
    public a: (int32,)
    public b: (bool,)


public type WithRequirement[T] = T:
    where:
        marker Buzz[T]


public type InstMyStruct = WithRequirement[MyStruct]
";

#[test]
fn marker_with_constraints() {
    let (table, errors) = build_table(MARKER_WITH_CONSTRAINTS);

    assert_eq!(errors.len(), 1);

    let error = errors[0]
        .as_any()
        .downcast_ref::<UnsatisfiedPredicate<Default>>()
        .unwrap();

    let marker_id = table.get_by_qualified_name(["test", "Fizz"]).unwrap();

    let expected_predicate =
        Predicate::PositiveMarker::<Default>(PositiveMarker {
            marker_id,
            generic_arguments: GenericArguments {
                lifetimes: Vec::new(),
                types: vec![Type::Primitive(Primitive::Bool)],
                constants: Vec::new(),
            },
        });

    assert_eq!(error.predicate, expected_predicate);
}

const NEGATIVE_MARKER_SATISFIED: &str = r"
public marker Marker[T]


final implements Marker[int32]:
    pass


public struct MyStruct:
    public a: int32
    public b: bool


public type WithRequirement[T] = T:
    where:
        marker not Marker[T]

public type InstMyStruct = WithRequirement[MyStruct]
";

#[test]
fn negative_marker_satisfied() {
    let (_, errors) = build_table(NEGATIVE_MARKER_SATISFIED);

    assert!(errors.is_empty());
}

const NEGATIVE_MARKER_NOT_SATISFIED: &str = r"
public marker Marker[T]


final implements Marker[int32]:
    pass


final implements Marker[bool]:
    pass


public struct MyStruct:
    public a: int32
    public b: bool


public type WithRequirement[T] = T:
    where:
        marker not Marker[T]


public type InstMyStruct = WithRequirement[MyStruct]
";

#[test]
fn negative_marker_not_satisfied() {
    let (table, errors) = build_table(NEGATIVE_MARKER_NOT_SATISFIED);

    assert_eq!(errors.len(), 1);

    let error = errors[0]
        .as_any()
        .downcast_ref::<UnsatisfiedPredicate<Default>>()
        .unwrap();

    let marker_id = table.get_by_qualified_name(["test", "Marker"]).unwrap();
    let my_struct_id =
        table.get_by_qualified_name(["test", "MyStruct"]).unwrap();

    let expected_predicate =
        Predicate::NegativeMarker::<Default>(NegativeMarker {
            marker_id,
            generic_arguments: GenericArguments {
                lifetimes: Vec::new(),
                types: vec![Type::Symbol(Symbol {
                    id: my_struct_id,
                    generic_arguments: GenericArguments::default(),
                })],
                constants: Vec::new(),
            },
        });

    assert_eq!(error.predicate, expected_predicate);
}

const EMPTY_STRUCT_NEGATIVE_MARKER_NOT_SATISFIED: &str = r"
public marker Marker[T]


final implements Marker[int32]:
    pass


public struct EmptyStruct:
    pass
    

public enum EmptyEnum:
    pass


public type WithRequirement[T] = T:
    where:
        marker not Marker[T]


public type InstEmptyStruct = WithRequirement[EmptyStruct]
public type InstEmptyEnum   = WithRequirement[EmptyEnum]
";

#[test]
fn empty_struct_negative_marker_not_satisfied() {
    let (table, errors) =
        build_table(EMPTY_STRUCT_NEGATIVE_MARKER_NOT_SATISFIED);

    assert_eq!(errors.len(), 2);

    let marker_id = table.get_by_qualified_name(["test", "Marker"]).unwrap();
    let struct_id =
        table.get_by_qualified_name(["test", "EmptyStruct"]).unwrap();
    let enum_id = table.get_by_qualified_name(["test", "EmptyEnum"]).unwrap();

    assert!(errors.iter().any(|x| x
        .as_any()
        .downcast_ref::<UnsatisfiedPredicate<Default>>()
        .is_some_and(|x| {
            x.predicate
                == Predicate::NegativeMarker(NegativeMarker {
                    marker_id,
                    generic_arguments: GenericArguments {
                        lifetimes: Vec::new(),
                        types: vec![Type::Symbol(Symbol {
                            id: struct_id,
                            generic_arguments: GenericArguments::default(),
                        })],
                        constants: Vec::new(),
                    },
                })
        })));

    assert!(errors.iter().any(|x| x
        .as_any()
        .downcast_ref::<UnsatisfiedPredicate<Default>>()
        .is_some_and(|x| {
            x.predicate
                == Predicate::NegativeMarker(NegativeMarker {
                    marker_id,
                    generic_arguments: GenericArguments {
                        lifetimes: Vec::new(),
                        types: vec![Type::Symbol(Symbol {
                            id: enum_id,
                            generic_arguments: GenericArguments::default(),
                        })],
                        constants: Vec::new(),
                    },
                })
        })));
}
