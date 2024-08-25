use std::{fmt::Display, sync::Arc};

use pernixc_base::{
    diagnostic::{Counter, Storage},
    source_file::SourceFile,
};
use pernixc_lexical::token_stream::TokenStream;
use pernixc_syntax::{parser::Parser, syntax_tree};

use crate::{
    arena::ID,
    error::Error,
    ir::{
        address::{self, Address, Field, Memory, Stack},
        instruction::Instruction,
        pattern::{self, Irrefutable, Named},
        representation::{
            binding::{
                infer::{self, Erased},
                Binder,
            },
            Representation,
        },
        value::{
            register::{Assignment, LoadKind},
            Value,
        },
    },
    symbol::{
        self,
        table::{
            representation::{Index, IndexMut, Insertion},
            resolution, Building, Table,
        },
        Accessibility, AdtTemplate, Function, FunctionDefinition,
        FunctionTemplate, GenericDeclaration, GenericID, MemberID,
        StructDefinition,
    },
    type_system::{
        self,
        term::{
            lifetime::Lifetime,
            r#type::{Primitive, Qualifier, Reference, SymbolID, Type},
            GenericArguments, Symbol, Tuple, TupleElement,
        },
    },
};

fn create_pattern(source: impl Display) -> syntax_tree::pattern::Irrefutable {
    let counter = Counter::default();

    let source_file =
        Arc::new(SourceFile::new(source.to_string(), "test".into()));
    let token_stream = TokenStream::tokenize(&source_file, &counter);

    // no error
    assert_eq!(counter.count(), 0);

    let mut parser = Parser::new(&token_stream, source_file);
    let pattern = parser.parse_irrefutable_pattern(&counter).unwrap();

    // no error
    assert_eq!(counter.count(), 0);

    pattern
}

impl Representation<infer::Model> {
    /// Check for the named pattern that bound as `ref QUALIFIER IDENTIFIER`
    fn check_reference_bound_named_pattern(
        &self,
        named_pattern: &Named<infer::Model>,
        expected_name: &str,
        expected_stored_adress: &Address<infer::Model>,
        qualifier: Qualifier,
        ty: &Type<infer::Model>,
    ) {
        assert_eq!(named_pattern.name, expected_name);

        // should store the address at some alloca
        let Stack::Alloca(stored_address_alloca_id) =
            named_pattern.load_address
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

                let Assignment::ReferenceOf(reference_of) = &self
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

        let alloca = self.allocas.get(stored_address_alloca_id).unwrap();

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

fn create_dummy_function() -> (Table<Building>, ID<Function>) {
    let mut table = Table::default();

    let Insertion { id: test_module_id, duplication } =
        table.create_root_module("test".to_string());

    assert!(duplication.is_none());

    let Insertion { id: function_id, duplication } = table
        .insert_member(
            "test".to_string(),
            Accessibility::Public,
            test_module_id,
            None,
            GenericDeclaration::default(),
            FunctionTemplate::<FunctionDefinition>::default(),
        )
        .unwrap();

    assert!(duplication.is_none());

    (table, function_id)
}

#[test]
fn value_bound_named() {
    const VALUE_BOUND_NAMED: &str = "mutable helloWorld";

    let (table, function_id) = create_dummy_function();
    let pattern = create_pattern(VALUE_BOUND_NAMED);

    let storage: Storage<Box<dyn Error>> = Storage::default();
    let mut binder = Binder::new_function(
        &table,
        resolution::NoOp,
        type_system::observer::NoOp,
        function_id,
        std::iter::empty(),
        &storage,
    )
    .unwrap();

    let alloca_id = binder.create_alloca(Type::default(), None);

    let pattern = binder
        .create_irrefutable(
            &pattern,
            &Type::default(),
            &Address::Memory(Memory::Alloca(alloca_id)),
            &storage,
        )
        .unwrap();

    // no error
    assert!(storage.as_vec().is_empty());

    let Irrefutable::Named(pattern) = pattern else {
        panic!("Expected a named pattern")
    };

    assert_eq!(pattern.name, "helloWorld");
    assert_eq!(pattern.load_address, Stack::Alloca(alloca_id));
    assert!(pattern.mutable);
}

#[test]
fn reference_bound_named() {
    const SIMPLE_NAMED_VALUE_BOUND: &str = "ref unique helloWorld";

    let (table, function_id) = create_dummy_function();
    let pattern = create_pattern(SIMPLE_NAMED_VALUE_BOUND);

    let storage: Storage<Box<dyn Error>> = Storage::default();
    let mut binder = Binder::new_function(
        &table,
        resolution::NoOp,
        type_system::observer::NoOp,
        function_id,
        std::iter::empty(),
        &storage,
    )
    .unwrap();

    let alloca_id = binder.create_alloca(Type::default(), None);

    let pattern = binder
        .create_irrefutable(
            &pattern,
            &Type::default(),
            &Address::Memory(Memory::Alloca(alloca_id)),
            &storage,
        )
        .unwrap();

    // no error
    assert!(storage.as_vec().is_empty());

    let Irrefutable::Named(pattern) = pattern else {
        panic!("Expected a named pattern")
    };

    binder.intermediate_representation.check_reference_bound_named_pattern(
        &pattern,
        "helloWorld",
        &Address::Memory(Memory::Alloca(alloca_id)),
        Qualifier::Unique,
        &Type::default(),
    );
}

#[test]
#[allow(clippy::too_many_lines)]
fn value_bound_struct() {
    const VALUE_BOUND_STRUCT: &str = "{ ref unique a, mutable b }";

    let (mut table, function_id) = create_dummy_function();
    let struct_id = {
        let core_module_id = table
            .get_by_qualified_name(std::iter::once("core"))
            .unwrap()
            .into_module()
            .unwrap();

        let Insertion { id: struct_id, duplication } = table
            .insert_member(
                "Test".to_string(),
                Accessibility::Public,
                core_module_id,
                None,
                GenericDeclaration::default(),
                AdtTemplate::<StructDefinition>::default(),
            )
            .unwrap();

        assert!(duplication.is_none());

        let struct_sym = table.get_mut(struct_id).unwrap();

        struct_sym
            .insert_field(symbol::Field {
                accessibility: Accessibility::Public,
                r#type: Type::Primitive(Primitive::Int32),
                name: "a".to_string(),
                span: None,
            })
            .unwrap();

        struct_sym
            .insert_field(symbol::Field {
                accessibility: Accessibility::Public,
                r#type: Type::Primitive(Primitive::Float32),
                name: "b".to_string(),
                span: None,
            })
            .unwrap();

        struct_id
    };

    let pattern = create_pattern(VALUE_BOUND_STRUCT);
    let struct_ty = Type::Symbol(Symbol {
        id: SymbolID::Struct(struct_id),
        generic_arguments: GenericArguments::default(),
    });

    let a_field_id =
        table.get(struct_id).unwrap().fields().get_id("a").unwrap();
    let b_field_id =
        table.get(struct_id).unwrap().fields().get_id("b").unwrap();

    let storage: Storage<Box<dyn Error>> = Storage::default();
    let mut binder = Binder::new_function(
        &table,
        resolution::NoOp,
        type_system::observer::NoOp,
        function_id,
        std::iter::empty(),
        &storage,
    )
    .unwrap();

    let struct_alloca_id = binder.create_alloca(struct_ty.clone(), None);

    let pattern = binder
        .create_irrefutable(
            &pattern,
            &struct_ty,
            &Address::Memory(Memory::Alloca(struct_alloca_id)),
            &storage,
        )
        .unwrap();

    // no error
    assert!(storage.as_vec().is_empty());

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
            "a",
            &Address::Field(Field {
                struct_address: Box::new(Address::Memory(Memory::Alloca(
                    struct_alloca_id,
                ))),
                id: a_field_id,
            }),
            Qualifier::Unique,
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
        assert!(pattern.mutable);

        let destructed_variable = pattern.load_address.into_alloca().unwrap();

        let register_id = binder
            .current_block()
            .instructions()
            .iter()
            .find_map(|inst| {
                let Instruction::Store(init) = inst else { return None };

                (init.address
                    == Address::Memory(Memory::Alloca(destructed_variable)))
                .then_some(*init.value.as_register().unwrap())
            })
            .unwrap();

        let load = binder
            .intermediate_representation
            .registers
            .get(register_id)
            .unwrap()
            .assignment
            .as_load()
            .unwrap();

        assert_eq!(
            load.address,
            Address::Field(Field {
                struct_address: Box::new(Address::Memory(Memory::Alloca(
                    struct_alloca_id,
                ))),
                id: b_field_id,
            })
        );

        assert_eq!(load.kind, LoadKind::Copy);
        assert_eq!(
            binder
                .intermediate_representation
                .allocas
                .get(destructed_variable)
                .unwrap()
                .r#type,
            Type::Primitive(Primitive::Float32)
        );
    }
}

#[test]
#[allow(clippy::too_many_lines)]
fn reference_bound_struct() {
    // both a and b are bound by reference
    const REFERENCE_BOUND_STRUCT: &str = "{ ref a, mutable b }";

    let (mut table, function_id) = create_dummy_function();
    let struct_id = {
        let core_module_id = table
            .get_by_qualified_name(std::iter::once("core"))
            .unwrap()
            .into_module()
            .unwrap();

        let Insertion { id: struct_id, duplication } = table
            .insert_member(
                "Test".to_string(),
                Accessibility::Public,
                core_module_id,
                None,
                GenericDeclaration::default(),
                AdtTemplate::<StructDefinition>::default(),
            )
            .unwrap();

        assert!(duplication.is_none());

        let struct_sym = table.get_mut(struct_id).unwrap();

        struct_sym
            .insert_field(symbol::Field {
                accessibility: Accessibility::Public,
                r#type: Type::Primitive(Primitive::Int32),
                name: "a".to_string(),
                span: None,
            })
            .unwrap();

        struct_sym
            .insert_field(symbol::Field {
                accessibility: Accessibility::Public,
                r#type: Type::Primitive(Primitive::Float32),
                name: "b".to_string(),
                span: None,
            })
            .unwrap();

        struct_id
    };

    let mut binder = Binder::new_function(
        &table,
        resolution::NoOp,
        type_system::observer::NoOp,
        function_id,
        std::iter::empty(),
        &Counter::default(),
    )
    .unwrap();
    let storage: Storage<Box<dyn Error>> = Storage::default();

    let pattern = create_pattern(REFERENCE_BOUND_STRUCT);
    let struct_ty = Type::Symbol(Symbol {
        id: SymbolID::Struct(struct_id),
        generic_arguments: GenericArguments::default(),
    });
    let reference_struct_ty = Type::Reference(Reference {
        qualifier: Qualifier::Immutable,
        lifetime: Lifetime::Static,
        pointee: Box::new(struct_ty.clone()),
    });

    let a_field_id =
        table.get(struct_id).unwrap().fields().get_id("a").unwrap();
    let b_field_id =
        table.get(struct_id).unwrap().fields().get_id("b").unwrap();

    let struct_alloca_id =
        binder.create_alloca(reference_struct_ty.clone(), None);

    let pattern = binder
        .create_irrefutable(
            &pattern,
            &reference_struct_ty,
            &Address::Memory(Memory::Alloca(struct_alloca_id)),
            &storage,
        )
        .unwrap();

    // no error
    assert!(storage.as_vec().is_empty());

    let Irrefutable::Structural(pattern) = pattern else {
        panic!("Expected a named pattern")
    };

    let load_address_register = binder
        .intermediate_representation
        .registers
        .iter()
        .find_map(|(idx, i)| {
            let Assignment::Load(load) = &i.assignment else {
                return None;
            };

            if load.address == Address::Memory(Memory::Alloca(struct_alloca_id))
                && binder.type_of_register(idx).unwrap()
                    == Type::Reference(Reference {
                        qualifier: Qualifier::Immutable,
                        lifetime: Lifetime::Static,
                        pointee: Box::new(struct_ty.clone()),
                    })
            {
                Some(idx)
            } else {
                None
            }
        })
        .unwrap();

    // ref a field check
    {
        let Some(Irrefutable::Named(pattern)) =
            pattern.patterns_by_field_id.get(&a_field_id)
        else {
            panic!("Expected a named pattern")
        };

        binder.intermediate_representation.check_reference_bound_named_pattern(
            pattern,
            "a",
            &Address::Field(Field {
                struct_address: Box::new(Address::Memory(
                    Memory::ReferenceValue(Value::Register(
                        load_address_register,
                    )),
                )),
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
            "b",
            &Address::Field(Field {
                struct_address: Box::new(Address::Memory(
                    Memory::ReferenceValue(Value::Register(
                        load_address_register,
                    )),
                )),
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
    const VALUE_BOUND_TUPLE: &str = "(ref a, mutable b)";

    let (table, function_id) = create_dummy_function();

    let pattern = create_pattern(VALUE_BOUND_TUPLE);
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

    let storage: Storage<Box<dyn Error>> = Storage::default();
    let mut binder = Binder::new_function(
        &table,
        resolution::NoOp,
        type_system::observer::NoOp,
        function_id,
        std::iter::empty(),
        &storage,
    )
    .unwrap();

    let tuple_alloca_id = binder.create_alloca(tuple_ty.clone(), None);

    let pattern = binder
        .create_irrefutable(
            &pattern,
            &tuple_ty,
            &Address::Memory(Memory::Alloca(tuple_alloca_id)),
            &storage,
        )
        .unwrap();

    // no error
    assert!(storage.as_vec().is_empty());

    let Irrefutable::Tuple(pattern::Tuple::Regular(pattern)) = pattern else {
        panic!("Expected a named pattern")
    };

    // ref a tuple check
    {
        let Some(Irrefutable::Named(pattern)) = pattern.elements.first() else {
            panic!("Expected a named pattern")
        };

        binder.intermediate_representation.check_reference_bound_named_pattern(
            pattern,
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
        let Some(Irrefutable::Named(pattern)) = pattern.elements.get(1) else {
            panic!("Expected a named pattern")
        };

        assert_eq!(pattern.name, "b");
        assert!(pattern.mutable);

        // should store the address at some alloca
        let destructed_variable = pattern.load_address.into_alloca().unwrap();

        let register_id = binder
            .current_block()
            .instructions()
            .iter()
            .find_map(|inst| {
                let Instruction::Store(init) = inst else { return None };

                (init.address
                    == Address::Memory(Memory::Alloca(destructed_variable)))
                .then_some(*init.value.as_register().unwrap())
            })
            .unwrap();

        let load = binder
            .intermediate_representation
            .registers
            .get(register_id)
            .unwrap()
            .assignment
            .as_load()
            .unwrap();

        assert_eq!(
            load.address,
            Address::Tuple(address::Tuple {
                tuple_address: Box::new(Address::Memory(Memory::Alloca(
                    tuple_alloca_id
                ))),
                offset: address::Offset::FromStart(1),
            })
        );

        assert_eq!(load.kind, LoadKind::Copy);
        assert_eq!(
            binder
                .intermediate_representation
                .allocas
                .get(destructed_variable)
                .unwrap()
                .r#type,
            Type::Primitive(Primitive::Int32)
        );
    }
}

#[test]
fn reference_bound_tuple() {
    const REFERENCE_BOUND_TUPLE: &str = "(ref a, mutable b)";

    let (table, function_id) = create_dummy_function();

    let pattern = create_pattern(REFERENCE_BOUND_TUPLE);
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
        pointee: Box::new(tuple_ty.clone()),
    });

    let storage: Storage<Box<dyn Error>> = Storage::default();
    let mut binder = Binder::new_function(
        &table,
        resolution::NoOp,
        type_system::observer::NoOp,
        function_id,
        std::iter::empty(),
        &storage,
    )
    .unwrap();

    let tuple_alloca_id =
        binder.create_alloca(reference_tuple_ty.clone(), None);

    let pattern = binder
        .create_irrefutable(
            &pattern,
            &reference_tuple_ty,
            &Address::Memory(Memory::Alloca(tuple_alloca_id)),
            &storage,
        )
        .unwrap();

    let Irrefutable::Tuple(pattern::Tuple::Regular(pattern)) = pattern else {
        panic!("Expected a named pattern")
    };

    let load_address_register = binder
        .intermediate_representation
        .registers
        .iter()
        .find_map(|(idx, i)| {
            let Assignment::Load(load) = &i.assignment else {
                return None;
            };

            if load.address == Address::Memory(Memory::Alloca(tuple_alloca_id))
                && binder.type_of_register(idx).unwrap()
                    == Type::Reference(Reference {
                        qualifier: Qualifier::Mutable,
                        lifetime: Lifetime::Static,
                        pointee: Box::new(tuple_ty.clone()),
                    })
            {
                Some(idx)
            } else {
                None
            }
        })
        .unwrap();

    // ref a tuple check
    {
        let Some(Irrefutable::Named(pattern)) = pattern.elements.first() else {
            panic!("Expected a named pattern")
        };

        binder.intermediate_representation.check_reference_bound_named_pattern(
            pattern,
            "a",
            &Address::Tuple(address::Tuple {
                tuple_address: Box::new(Address::Memory(
                    Memory::ReferenceValue(Value::Register(
                        load_address_register,
                    )),
                )),
                offset: address::Offset::FromStart(0),
            }),
            Qualifier::Mutable,
            &Type::Primitive(Primitive::Bool),
        );
    }

    // mutable b check field
    {
        let Some(Irrefutable::Named(pattern)) = pattern.elements.get(1) else {
            panic!("Expected a named pattern")
        };

        binder.intermediate_representation.check_reference_bound_named_pattern(
            pattern,
            "b",
            &Address::Tuple(address::Tuple {
                tuple_address: Box::new(Address::Memory(
                    Memory::ReferenceValue(Value::Register(
                        load_address_register,
                    )),
                )),
                offset: address::Offset::FromStart(1),
            }),
            Qualifier::Mutable,
            &Type::Primitive(Primitive::Int32),
        );
    }
}

#[test]
#[allow(clippy::too_many_lines)]
fn packed_tuple() {
    const PACKED_TUPLE: &str = "(ref a, b, mutable c)";

    let (table, function_id) = create_dummy_function();

    let pattern = create_pattern(PACKED_TUPLE);
    let tuple_ty = Type::Tuple(Tuple {
        elements: vec![
            TupleElement {
                term: Type::Primitive(Primitive::Bool),
                is_unpacked: false,
            },
            TupleElement {
                term: Type::Parameter(MemberID {
                    parent: GenericID::Struct(ID::new(0)),
                    id: ID::new(0),
                }),
                is_unpacked: true,
            },
            TupleElement {
                term: Type::Primitive(Primitive::Int32),
                is_unpacked: false,
            },
        ],
    });

    let storage: Storage<Box<dyn Error>> = Storage::default();
    let mut binder = Binder::new_function(
        &table,
        resolution::NoOp,
        type_system::observer::NoOp,
        function_id,
        std::iter::empty(),
        &storage,
    )
    .unwrap();

    let tuple_alloca_id = binder.create_alloca(tuple_ty.clone(), None);

    let pattern = binder
        .create_irrefutable(
            &pattern,
            &tuple_ty,
            &Address::Memory(Memory::Alloca(tuple_alloca_id)),
            &storage,
        )
        .unwrap();

    // no error
    assert!(storage.as_vec().is_empty());

    let Irrefutable::Tuple(pattern::Tuple::Packed(pattern)) = pattern else {
        panic!("Expected a named pattern")
    };

    // ref a tuple check
    {
        let Some(Irrefutable::Named(pattern)) =
            pattern.before_packed_elements.first()
        else {
            panic!("Expected a named pattern")
        };

        binder.intermediate_representation.check_reference_bound_named_pattern(
            pattern,
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

    // mutable c check field
    {
        let Some(Irrefutable::Named(pattern)) =
            pattern.after_packed_elements.first()
        else {
            panic!("Expected a named pattern")
        };

        assert_eq!(pattern.name, "c");
        assert!(pattern.mutable);

        let destructed_variable = pattern.load_address.into_alloca().unwrap();

        let register_id = binder
            .current_block()
            .instructions()
            .iter()
            .find_map(|inst| {
                let Instruction::Store(init) = inst else { return None };

                (init.address
                    == Address::Memory(Memory::Alloca(destructed_variable)))
                .then_some(*init.value.as_register().unwrap())
            })
            .unwrap();

        let load = binder
            .intermediate_representation
            .registers
            .get(register_id)
            .unwrap()
            .assignment
            .as_load()
            .unwrap();

        assert_eq!(
            load.address,
            Address::Tuple(address::Tuple {
                tuple_address: Box::new(Address::Memory(Memory::Alloca(
                    tuple_alloca_id
                ))),
                offset: address::Offset::FromEnd(0),
            })
        );
    }

    {
        let block = binder.current_block();

        let Irrefutable::Named(pat) = pattern.packed_element.as_ref() else {
            panic!("Expected a named pattern")
        };

        let store_address = block
            .instructions()
            .iter()
            .find_map(|inst| {
                let Instruction::TuplePack(pack) = inst else {
                    return None;
                };

                if pack.before_packed_element_count == 1
                    && pack.after_packed_element_count == 1
                    && pack.tuple_address
                        == Address::Memory(Memory::Alloca(tuple_alloca_id))
                {
                    Some(&pack.store_address)
                } else {
                    None
                }
            })
            .unwrap();

        assert_eq!(
            store_address.as_memory().unwrap().as_alloca().unwrap(),
            pat.load_address.as_alloca().unwrap()
        );

        assert_eq!(
            binder
                .intermediate_representation
                .allocas
                .get(*pat.load_address.as_alloca().unwrap())
                .unwrap()
                .r#type,
            Type::Parameter(MemberID {
                parent: GenericID::Struct(ID::new(0)),
                id: ID::new(0),
            })
        );
    }
}

/*
#[test]
#[allow(clippy::too_many_lines)]
fn optimize_unused_pack_tuple() {
    const PACKED_TUPLE: &str = "(ref a, ?, mutable c)";

    let mut representation = Representation::default();
    let (table, referring_site) = create_table();

    let pattern = create_pattern(PACKED_TUPLE);
    let tuple_ty = Type::Tuple(Tuple {
        elements: vec![
            TupleElement::Regular(Type::Primitive(Primitive::Bool)),
            TupleElement::Unpacked(Type::Parameter(MemberID {
                parent: GenericID::Struct(ID::new(0)),
                id: ID::new(0),
            })),
            TupleElement::Regular(Type::Primitive(Primitive::Int32)),
        ],
    });

    let tuple_alloca_id = representation
        .allocas
        .insert(Alloca { r#type: tuple_ty.clone(), span: None });

    let counter = Counter::default();

    let pattern = representation
        .create_irrefutable(
            &table,
            &pattern,
            &tuple_ty,
            &Address::Alloca(tuple_alloca_id),
            representation.control_flow_graph.entry_block_id(),
            referring_site.into(),
            &counter,
        )
        .unwrap();

    // no error
    assert_eq!(counter.count(), 0);

    let Irrefutable::Tuple(pattern::Tuple::Packed(pattern)) = pattern else {
        panic!("Expected a named pattern")
    };

    // ref a tuple check
    {
        let Some(Irrefutable::Named(pattern)) =
            pattern.before_packed_elements.first()
        else {
            panic!("Expected a named pattern")
        };

        representation.check_reference_bound_named_pattern(
            pattern,
            "a",
            &Address::Tuple(address::Tuple {
                tuple_address: Box::new(Address::Alloca(tuple_alloca_id)),
                offset: address::Offset::FromStart(0),
            }),
            Qualifier::Immutable,
            &Type::Primitive(Primitive::Bool),
        );
    }

    // mutable c check field
    {
        let Some(Irrefutable::Named(pattern)) =
            pattern.after_packed_elements.first()
        else {
            panic!("Expected a named pattern")
        };

        assert_eq!(pattern.name, "c");
        assert!(pattern.mutable);

        // should store the address at some alloca
        let Address::Tuple(tuple_address) = &pattern.load_address else {
            panic!("Expected a tuple address")
        };

        assert_eq!(tuple_address.offset, address::Offset::FromEnd(0));
        assert_eq!(
            &*tuple_address.tuple_address,
            &Address::Alloca(tuple_alloca_id)
        );
    }

    {
        let block = representation
            .control_flow_graph
            .get_block(representation.control_flow_graph.entry_block_id())
            .unwrap();

        // no need to pack the tuple since it's not used (wildcard)
        assert!(!block.instructions().iter().any(|inst| {
            let Instruction::Basic(Basic::TuplePack(pack)) = inst else {
                return false;
            };

            pack.before_packed_element_count == 1
                && pack.after_packed_element_count == 1
                && pack.tuple_address == Address::Alloca(tuple_alloca_id)
        }));
    }
}
*/
