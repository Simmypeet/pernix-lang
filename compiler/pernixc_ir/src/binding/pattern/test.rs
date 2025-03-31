use pernixc_semantic::component::derived::fields::Fields;
use pernixc_handler::Panic;
use pernixc_source_file::SourceElement;
use pernixc_syntax::{syntax_tree, utility::parse};
use pernixc_semantic::term::{
    generic_arguments::GenericArguments,
    generic_parameter::{GenericParameters, TypeParameterID},
    lifetime::Lifetime,
    r#type::{Primitive, Qualifier, Reference, Tuple, Type},
    Symbol, TupleElement,
};

use crate::{
    address::{self, Address, Field, Memory},
    binding::{
        infer,
        test::{build_table, CreateBinderAtExt, Template},
        Binder,
    },
    instruction::Instruction,
    model::Erased,
    pattern::{Irrefutable, NameBindingPoint, Named},
    value::{register::Assignment, Value},
    Representation,
};

impl Representation<infer::Model> {
    /// Check for the named pattern that bound as `ref QUALIFIER IDENTIFIER`
    fn check_reference_bound_named_pattern(
        &self,
        named_pattern: &Named,
        binding_point: &NameBindingPoint<infer::Model>,
        expected_name: &str,
        expected_stored_adress: &Address<infer::Model>,
        qualifier: Qualifier,
        ty: &Type<infer::Model>,
    ) {
        assert_eq!(named_pattern.name, expected_name);
        let binding =
            binding_point.named_patterns_by_name.get(expected_name).unwrap();

        // should store the address at some alloca
        let Address::Memory(Memory::Alloca(stored_address_alloca_id)) =
            binding.load_address
        else {
            panic!("Expected an alloca address")
        };

        // always entry block
        let block = self
            .control_flow_graph
            .get_block(self.control_flow_graph.entry_block_id())
            .unwrap();

        let reference_of_register_id = block
            .instructions()
            .iter()
            .find_map(|instruction| {
                let Instruction::RegisterAssignment(register_assignment) =
                    instruction
                else {
                    return None;
                };

                let Assignment::Borrow(reference_of) = &self
                    .values
                    .registers
                    .get(register_assignment.id)
                    .unwrap()
                    .assignment
                else {
                    return None;
                };

                if &reference_of.address == expected_stored_adress {
                    Some(register_assignment.id)
                } else {
                    None
                }
            })
            .unwrap();

        assert!(block.instructions().iter().any(|instruction| {
            let Instruction::Store(store) = instruction else {
                return false;
            };

            store.value == Value::Register(reference_of_register_id)
                && store.address
                    == Address::Memory(Memory::Alloca(stored_address_alloca_id))
        }));

        let alloca = self.values.allocas.get(stored_address_alloca_id).unwrap();

        assert_eq!(
            alloca.r#type,
            Type::Reference(Reference {
                qualifier,
                lifetime: Lifetime::Inference(Erased),
                pointee: Box::new(ty.clone())
            })
        );
    }
}

impl Binder<'_> {
    fn bind_irrefutable(
        &mut self,
        pattern_syn: &syntax_tree::pattern::Irrefutable,
        ty: &Type<infer::Model>,
        address: Address<infer::Model>,
    ) -> (Irrefutable, NameBindingPoint<infer::Model>) {
        let irrefutable =
            self.bind_pattern(ty, pattern_syn, &Panic).unwrap().unwrap();

        let mut binding_point = NameBindingPoint::default();

        self.insert_irrefutable_named_binding_point(
            &mut binding_point,
            &irrefutable,
            ty,
            address,
            None,
            Qualifier::Mutable,
            false,
            self.stack.current_scope().scope_id(),
            &Panic,
        )
        .unwrap();

        (irrefutable, binding_point)
    }
}

#[test]
fn value_bound_named() {
    const VALUE_BOUND_NAMED: &str = "mut helloWorld";

    let template = Template::new();
    let mut binder = template.create_binder();

    let syn = parse::<syntax_tree::pattern::Irrefutable>(VALUE_BOUND_NAMED);

    let alloca_id = binder.create_alloca(
        Type::Tuple(term::Tuple { elements: Vec::new() }),
        syn.span(),
    );

    let (pattern, binding_point) = binder.bind_irrefutable(
        &syn,
        &Type::Tuple(term::Tuple { elements: Vec::new() }),
        Address::Memory(Memory::Alloca(alloca_id)),
    );

    let Irrefutable::Named(pattern) = pattern else {
        panic!("Expected a named pattern")
    };

    assert_eq!(pattern.name, "helloWorld");
    assert!(pattern.is_mutable);
    assert_eq!(pattern.reference_binding, None);

    let name = binding_point.named_patterns_by_name.get("helloWorld").unwrap();

    assert!(name.mutable);
    assert_eq!(name.load_address, Address::Memory(Memory::Alloca(alloca_id)));
}

#[test]
fn reference_bound_named() {
    const SIMPLE_NAMED_REFERENCE_BOUND: &str = "&mut helloWorld";

    let template = Template::new();
    let mut binder = template.create_binder();

    let syn = parse::<syntax_tree::pattern::Irrefutable>(
        SIMPLE_NAMED_REFERENCE_BOUND,
    );
    let alloca_id = binder.create_alloca(
        Type::Tuple(term::Tuple { elements: Vec::new() }),
        syn.span(),
    );
    let (pattern, binding_point) = binder.bind_irrefutable(
        &syn,
        &Type::Tuple(term::Tuple { elements: Vec::new() }),
        Address::Memory(Memory::Alloca(alloca_id)),
    );

    let Irrefutable::Named(pattern) = pattern else {
        panic!("Expected a named pattern")
    };

    binder.intermediate_representation.check_reference_bound_named_pattern(
        &pattern,
        &binding_point,
        "helloWorld",
        &Address::Memory(Memory::Alloca(alloca_id)),
        Qualifier::Mutable,
        &Type::Tuple(term::Tuple { elements: Vec::new() }),
    );
}

const STRUCT_DECLARATION: &str = r"
public struct Test:
    public a: int32
    public b: float32


public function test():
    pass
";

#[test]
#[allow(clippy::too_many_lines)]
fn value_bound_struct() {
    const VALUE_BOUND_STRUCT: &str = "{ &mut a, mut b }";

    let table = build_table(STRUCT_DECLARATION);

    let struct_id = table.get_by_qualified_name(["test", "Test"]).unwrap();

    let struct_ty = Type::Symbol(Symbol {
        id: struct_id,
        generic_arguments: GenericArguments::default(),
    });

    let fields = table.query::<Fields>(struct_id).unwrap();
    let a_field_id = fields.field_ids_by_name["a"];
    let b_field_id = fields.field_ids_by_name["b"];

    let mut binder = table.create_binder_at(["test", "test"]);

    let syn = parse::<syntax_tree::pattern::Irrefutable>(VALUE_BOUND_STRUCT);
    let struct_alloca_id = binder.create_alloca(struct_ty.clone(), syn.span());

    let (pattern, binding_point) = binder.bind_irrefutable(
        &syn,
        &struct_ty,
        Address::Memory(Memory::Alloca(struct_alloca_id)),
    );

    let Irrefutable::Structural(pattern) = pattern else {
        panic!("Expected a named pattern")
    };

    // ref a field check
    {
        let Some(Irrefutable::Named(pattern)) =
            pattern.patterns_by_field_id.get(&a_field_id)
        else {
            panic!("Expected a named pattern")
        };

        binder.intermediate_representation.check_reference_bound_named_pattern(
            pattern,
            &binding_point,
            "a",
            &Address::Field(Field {
                struct_address: Box::new(Address::Memory(Memory::Alloca(
                    struct_alloca_id,
                ))),
                id: a_field_id,
            }),
            Qualifier::Mutable,
            &Type::Primitive(Primitive::Int32),
        );
    }

    // mutable b check field
    {
        let Some(Irrefutable::Named(pattern)) =
            pattern.patterns_by_field_id.get(&b_field_id)
        else {
            panic!("Expected a named pattern")
        };

        assert_eq!(pattern.name, "b");
        assert!(pattern.is_mutable);
        assert_eq!(pattern.reference_binding, None);

        assert_eq!(
            binding_point.named_patterns_by_name.get("b").unwrap().load_address,
            Address::Field(Field {
                struct_address: Box::new(Address::Memory(Memory::Alloca(
                    struct_alloca_id,
                ))),
                id: b_field_id,
            })
        );
    }
}

#[test]
#[allow(clippy::too_many_lines)]
fn reference_bound_struct() {
    // both a and b are bound by reference
    const REFERENCE_BOUND_STRUCT: &str = "{ &a, mut b }";

    let table = build_table(STRUCT_DECLARATION);

    let struct_id = table.get_by_qualified_name(["test", "Test"]).unwrap();
    let mut binder = table.create_binder_at(["test", "test"]);

    // no error

    let struct_ty = Type::Symbol(Symbol {
        id: struct_id,
        generic_arguments: GenericArguments::default(),
    });
    let reference_struct_ty = Type::Reference(Reference {
        qualifier: Qualifier::Immutable,
        lifetime: Lifetime::Static,
        pointee: Box::new(struct_ty),
    });

    let fields = table.query::<Fields>(struct_id).unwrap();
    let a_field_id = fields.field_ids_by_name["a"];
    let b_field_id = fields.field_ids_by_name["b"];

    let syn =
        parse::<syntax_tree::pattern::Irrefutable>(REFERENCE_BOUND_STRUCT);
    let struct_alloca_id =
        binder.create_alloca(reference_struct_ty.clone(), syn.span());

    let (pattern, binding_point) = binder.bind_irrefutable(
        &syn,
        &reference_struct_ty,
        Address::Memory(Memory::Alloca(struct_alloca_id)),
    );

    let Irrefutable::Structural(pattern) = pattern else {
        panic!("Expected a named pattern")
    };

    let base_struct_address = Address::Reference(address::Reference {
        qualifier: Qualifier::Immutable,
        reference_address: Box::new(Address::Memory(Memory::Alloca(
            struct_alloca_id,
        ))),
    });

    // ref a field check
    {
        let Some(Irrefutable::Named(pattern)) =
            pattern.patterns_by_field_id.get(&a_field_id)
        else {
            panic!("Expected a named pattern")
        };

        binder.intermediate_representation.check_reference_bound_named_pattern(
            pattern,
            &binding_point,
            "a",
            &Address::Field(Field {
                struct_address: Box::new(base_struct_address.clone()),
                id: a_field_id,
            }),
            Qualifier::Immutable,
            &Type::Primitive(Primitive::Int32),
        );
    }

    // mutable b check field
    {
        let Some(Irrefutable::Named(pattern)) =
            pattern.patterns_by_field_id.get(&b_field_id)
        else {
            panic!("Expected a named pattern")
        };

        binder.intermediate_representation.check_reference_bound_named_pattern(
            pattern,
            &binding_point,
            "b",
            &Address::Field(Field {
                struct_address: Box::new(base_struct_address),
                id: b_field_id,
            }),
            Qualifier::Immutable,
            &Type::Primitive(Primitive::Float32),
        );
    }
}

#[test]
#[allow(missing_docs)]
fn value_bound_tuple() {
    const VALUE_BOUND_TUPLE: &str = "(&a, mut b)";

    let template = Template::new();
    let mut binder = template.create_binder();

    let tuple_ty = Type::Tuple(Tuple {
        elements: vec![
            TupleElement {
                term: Type::Primitive(Primitive::Bool),
                is_unpacked: false,
            },
            TupleElement {
                term: Type::Primitive(Primitive::Int32),
                is_unpacked: false,
            },
        ],
    });

    let syn = parse::<syntax_tree::pattern::Irrefutable>(VALUE_BOUND_TUPLE);
    let tuple_alloca_id = binder.create_alloca(tuple_ty.clone(), syn.span());

    let (pattern, binding_point) = binder.bind_irrefutable(
        &syn,
        &tuple_ty,
        Address::Memory(Memory::Alloca(tuple_alloca_id)),
    );

    let Irrefutable::Tuple(pattern) = pattern else {
        panic!("Expected a named pattern")
    };

    // ref a tuple check
    {
        let Some(pattern) = pattern.elements.first() else {
            panic!("Expected a named pattern")
        };

        assert!(!pattern.is_packed);

        let Irrefutable::Named(pattern) = &pattern.pattern else {
            panic!("Expected a named pattern")
        };

        binder.intermediate_representation.check_reference_bound_named_pattern(
            pattern,
            &binding_point,
            "a",
            &Address::Tuple(address::Tuple {
                tuple_address: Box::new(Address::Memory(Memory::Alloca(
                    tuple_alloca_id,
                ))),
                offset: address::Offset::FromStart(0),
            }),
            Qualifier::Immutable,
            &Type::Primitive(Primitive::Bool),
        );
    }

    // mutable b check field
    {
        let Some(pattern) = pattern.elements.last() else {
            panic!("Expected a named pattern")
        };

        assert!(!pattern.is_packed);

        let Irrefutable::Named(pattern) = &pattern.pattern else {
            panic!("Expected a named pattern")
        };

        assert_eq!(pattern.name, "b");
        assert_eq!(pattern.reference_binding, None);
        assert!(pattern.is_mutable);

        assert_eq!(
            binding_point.named_patterns_by_name.get("b").unwrap().load_address,
            Address::Tuple(address::Tuple {
                tuple_address: Box::new(Address::Memory(Memory::Alloca(
                    tuple_alloca_id
                ))),
                offset: address::Offset::FromStart(1),
            })
        );
    }
}

#[test]
fn reference_bound_tuple() {
    const REFERENCE_BOUND_TUPLE: &str = "(&a, mut b)";

    let template = Template::new();
    let mut binder = template.create_binder();

    let tuple_ty = Type::Tuple(Tuple {
        elements: vec![
            TupleElement {
                term: Type::Primitive(Primitive::Bool),
                is_unpacked: false,
            },
            TupleElement {
                term: Type::Primitive(Primitive::Int32),
                is_unpacked: false,
            },
        ],
    });
    let reference_tuple_ty = Type::Reference(Reference {
        qualifier: Qualifier::Mutable,
        lifetime: Lifetime::Static,
        pointee: Box::new(tuple_ty),
    });

    let syn = parse::<syntax_tree::pattern::Irrefutable>(REFERENCE_BOUND_TUPLE);
    let tuple_alloca_id =
        binder.create_alloca(reference_tuple_ty.clone(), syn.span());

    let (pattern, binding_point) = binder.bind_irrefutable(
        &syn,
        &reference_tuple_ty,
        Address::Memory(Memory::Alloca(tuple_alloca_id)),
    );

    let Irrefutable::Tuple(pattern) = pattern else {
        panic!("Expected a named pattern")
    };

    let base_tuple_address = Address::Reference(address::Reference {
        qualifier: Qualifier::Mutable,
        reference_address: Box::new(Address::Memory(Memory::Alloca(
            tuple_alloca_id,
        ))),
    });

    // ref a tuple check
    {
        let pattern = pattern.elements.first().unwrap();

        assert!(!pattern.is_packed);

        let Irrefutable::Named(pattern) = &pattern.pattern else {
            panic!("Expected a named pattern")
        };

        binder.intermediate_representation.check_reference_bound_named_pattern(
            pattern,
            &binding_point,
            "a",
            &Address::Tuple(address::Tuple {
                tuple_address: Box::new(base_tuple_address.clone()),
                offset: address::Offset::FromStart(0),
            }),
            Qualifier::Immutable,
            &Type::Primitive(Primitive::Bool),
        );
    }

    // mutable b check field
    {
        let pattern = pattern.elements.last().unwrap();

        assert!(!pattern.is_packed);

        let Irrefutable::Named(pattern) = &pattern.pattern else {
            panic!("Expected a named pattern")
        };

        binder.intermediate_representation.check_reference_bound_named_pattern(
            pattern,
            &binding_point,
            "b",
            &Address::Tuple(address::Tuple {
                tuple_address: Box::new(base_tuple_address),
                offset: address::Offset::FromStart(1),
            }),
            Qualifier::Mutable,
            &Type::Primitive(Primitive::Int32),
        );
    }
}

const WITH_UNPACKED_TYPE_PARAMETER: &str = r"
public function test[T: tuple]():
    pass
";

#[test]
#[allow(clippy::too_many_lines)]
fn more_packed_tuple() {
    const PACKED_TUPLE: &str = "(a, ...b,  c)";
    let table = build_table(WITH_UNPACKED_TYPE_PARAMETER);
    let mut binder = table.create_binder_at(["test", "test"]);

    let test_function_id =
        table.get_by_qualified_name(["test", "test"]).unwrap();
    let test_generic_params =
        table.query::<GenericParameters>(test_function_id).unwrap();

    let tuple_ty = Type::Tuple(Tuple {
        elements: vec![
            TupleElement {
                term: Type::Primitive(Primitive::Int8),
                is_unpacked: false,
            },
            TupleElement {
                term: Type::Primitive(Primitive::Int16),
                is_unpacked: false,
            },
            TupleElement {
                term: Type::Parameter(TypeParameterID::new(
                    test_function_id,
                    test_generic_params.type_parameter_ids_by_name()["T"],
                )),
                is_unpacked: true,
            },
            TupleElement {
                term: Type::Primitive(Primitive::Int32),
                is_unpacked: false,
            },
            TupleElement {
                term: Type::Primitive(Primitive::Int64),
                is_unpacked: false,
            },
            TupleElement {
                term: Type::Primitive(Primitive::Int32),
                is_unpacked: false,
            },
        ],
    });

    let syn = parse::<syntax_tree::pattern::Irrefutable>(PACKED_TUPLE);
    let tuple_alloca_id = binder.create_alloca(tuple_ty.clone(), syn.span());

    let (pattern, binding_point) = binder.bind_irrefutable(
        &syn,
        &tuple_ty,
        Address::Memory(Memory::Alloca(tuple_alloca_id)),
    );

    let Irrefutable::Tuple(pattern) = pattern else {
        panic!("Expected a tuple pattern")
    };

    // a tuple field
    {
        let pattern = pattern.elements.first().unwrap();

        assert!(!pattern.is_packed);

        let Irrefutable::Named(pattern) = &pattern.pattern else {
            panic!("Expected a named pattern")
        };

        assert_eq!(pattern.name, "a");

        assert_eq!(
            binding_point.named_patterns_by_name.get("a").unwrap().load_address,
            Address::Tuple(address::Tuple {
                tuple_address: Box::new(Address::Memory(Memory::Alloca(
                    tuple_alloca_id
                ))),
                offset: address::Offset::FromStart(0),
            })
        );
    }

    // b tuple check
    {
        assert!(pattern.elements[1].is_packed);
        let Irrefutable::Named(_) = &pattern.elements[1].pattern else {
            panic!("Expected a named pattern")
        };

        let load_address = binding_point
            .named_patterns_by_name
            .get("b")
            .unwrap()
            .load_address
            .clone();

        let block = binder.current_block();

        // instruction for packing the unpacked
        assert!(block.instructions().iter().any(|inst| {
            let Instruction::TuplePack(pack) = inst else {
                return false;
            };

            pack.before_packed_element_count == 2
                && pack.after_packed_element_count == 3
                && pack.tuple_address
                    == Address::Memory(Memory::Alloca(tuple_alloca_id))
                && pack.store_address == load_address
        }));
        // store int16
        assert!(block.instructions().iter().any(|inst| {
            let Instruction::Store(store) = inst else {
                return false;
            };

            store.address
                == Address::Tuple(address::Tuple {
                    tuple_address: Box::new(load_address.clone()),
                    offset: address::Offset::FromStart(0),
                })
                && store.value.as_register().copied().is_some_and(|reg| {
                    binder
                        .intermediate_representation
                        .values
                        .registers
                        .get(reg)
                        .and_then(|reg| reg.assignment.as_load())
                        .is_some_and(|x| {
                            x.address
                                == Address::Tuple(address::Tuple {
                                    tuple_address: Box::new(Address::Memory(
                                        Memory::Alloca(tuple_alloca_id),
                                    )),
                                    offset: address::Offset::FromStart(1),
                                })
                        })
                })
        }));
        // store int32
        assert!(block.instructions().iter().any(|inst| {
            let Instruction::Store(store) = inst else {
                return false;
            };

            store.address
                == Address::Tuple(address::Tuple {
                    tuple_address: Box::new(load_address.clone()),
                    offset: address::Offset::FromEnd(1),
                })
                && store.value.as_register().copied().is_some_and(|reg| {
                    binder
                        .intermediate_representation
                        .values
                        .registers
                        .get(reg)
                        .and_then(|reg| reg.assignment.as_load())
                        .is_some_and(|x| {
                            x.address
                                == Address::Tuple(address::Tuple {
                                    tuple_address: Box::new(Address::Memory(
                                        Memory::Alloca(tuple_alloca_id),
                                    )),
                                    offset: address::Offset::FromEnd(2),
                                })
                        })
                })
        }));
        // store int64
        assert!(block.instructions().iter().any(|inst| {
            let Instruction::Store(store) = inst else {
                return false;
            };

            store.address
                == Address::Tuple(address::Tuple {
                    tuple_address: Box::new(load_address.clone()),
                    offset: address::Offset::FromEnd(0),
                })
                && store.value.as_register().copied().is_some_and(|reg| {
                    binder
                        .intermediate_representation
                        .values
                        .registers
                        .get(reg)
                        .and_then(|reg| reg.assignment.as_load())
                        .is_some_and(|x| {
                            x.address
                                == Address::Tuple(address::Tuple {
                                    tuple_address: Box::new(Address::Memory(
                                        Memory::Alloca(tuple_alloca_id),
                                    )),
                                    offset: address::Offset::FromEnd(1),
                                })
                        })
                })
        }));

        // should be stored at some alloca
        assert_eq!(
            binder
                .intermediate_representation
                .values
                .allocas
                .get(*load_address.as_memory().unwrap().as_alloca().unwrap())
                .unwrap()
                .r#type,
            Type::Tuple(Tuple {
                elements: vec![
                    TupleElement {
                        term: Type::Primitive(Primitive::Int16),
                        is_unpacked: false
                    },
                    TupleElement {
                        term: Type::Parameter(TypeParameterID::new(
                            test_function_id,
                            test_generic_params.type_parameter_ids_by_name()
                                ["T"]
                        )),
                        is_unpacked: true
                    },
                    TupleElement {
                        term: Type::Primitive(Primitive::Int32),
                        is_unpacked: false
                    },
                    TupleElement {
                        term: Type::Primitive(Primitive::Int64),
                        is_unpacked: false
                    },
                ]
            }),
        );
    }

    // c check field
    {
        assert!(!pattern.elements.last().unwrap().is_packed);

        let Irrefutable::Named(pattern) =
            &pattern.elements.last().unwrap().pattern
        else {
            panic!("Expected a named pattern")
        };

        assert_eq!(pattern.name, "c");

        assert_eq!(
            binding_point.named_patterns_by_name.get("c").unwrap().load_address,
            Address::Tuple(address::Tuple {
                tuple_address: Box::new(Address::Memory(Memory::Alloca(
                    tuple_alloca_id
                ))),
                offset: address::Offset::FromEnd(0),
            })
        );
    }
}
