use std::{fmt::Display, sync::Arc};

use pernixc_base::{
    handler::{Counter, Storage},
    source_file::SourceFile,
};
use pernixc_lexical::token_stream::TokenStream;
use pernixc_syntax::{parser::Parser, syntax_tree};

use crate::{
    arena::ID,
    error::{self, Error},
    ir::{
        address::{self, Address, Field, Memory},
        instruction::Instruction,
        pattern::{BindingKind, Irrefutable, NameBindingPoint, Named, Pattern},
        representation::{
            binding::{
                infer::{self, Erased},
                Binder,
            },
            Representation,
        },
        value::{register::Assignment, Value},
    },
    symbol::{
        self,
        table::{
            self,
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
            self,
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

impl<
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>,
    > Binder<'_, S, RO, TO>
{
    fn bind_irrefutable(
        &mut self,
        source: impl Display,
        ty: &Type<infer::Model>,
        address: Address<infer::Model>,
    ) -> (Irrefutable, NameBindingPoint<infer::Model>) {
        let pattern_syn = create_pattern(source);

        let storage = Storage::<Box<dyn error::Error>>::default();

        let irrefutable = Irrefutable::bind(
            &pattern_syn,
            ty,
            self.current_site,
            &self.create_environment(),
            &storage,
        )
        .unwrap();

        let storage = storage.into_vec();
        assert!(storage.is_empty(), "{storage:?}");

        let mut binding_point = NameBindingPoint::default();

        let storage = Storage::<Box<dyn error::Error>>::default();

        self.insert_named_binding_point(
            &mut binding_point,
            &irrefutable.result,
            ty,
            address,
            &storage,
        );

        let storage = storage.into_vec();
        assert!(storage.is_empty(), "{storage:?}");

        (irrefutable.result, binding_point)
    }
}

#[test]
fn value_bound_named() {
    const VALUE_BOUND_NAMED: &str = "mutable helloWorld";

    let (table, function_id) = create_dummy_function();

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

    assert!(storage.as_vec().is_empty());

    let alloca_id = binder.create_alloca(Type::default(), None);

    let (pattern, binding_point) = binder.bind_irrefutable(
        VALUE_BOUND_NAMED,
        &Type::default(),
        Address::Memory(Memory::Alloca(alloca_id)),
    );

    let Irrefutable::Named(pattern) = pattern else {
        panic!("Expected a named pattern")
    };

    assert_eq!(pattern.name, "helloWorld");
    assert_eq!(pattern.kind, BindingKind::Value(true));

    let name = binding_point.named_patterns_by_name.get("helloWorld").unwrap();

    assert_eq!(name.mutable, true);
    assert_eq!(name.load_address, Address::Memory(Memory::Alloca(alloca_id)));
}

#[test]
fn reference_bound_named() {
    const SIMPLE_NAMED_REFERENCE_BOUND: &str = "ref unique helloWorld";

    let (table, function_id) = create_dummy_function();

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
    let (pattern, binding_point) = binder.bind_irrefutable(
        SIMPLE_NAMED_REFERENCE_BOUND,
        &Type::default(),
        Address::Memory(Memory::Alloca(alloca_id)),
    );

    // no error
    assert!(storage.as_vec().is_empty());

    let Irrefutable::Named(pattern) = pattern else {
        panic!("Expected a named pattern")
    };

    binder.intermediate_representation.check_reference_bound_named_pattern(
        &pattern,
        &binding_point,
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

    let (pattern, binding_point) = binder.bind_irrefutable(
        VALUE_BOUND_STRUCT,
        &struct_ty,
        Address::Memory(Memory::Alloca(struct_alloca_id)),
    );

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
            &binding_point,
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
        assert_eq!(pattern.kind, BindingKind::Value(true));

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

    let storage: Storage<Box<dyn Error>> = Storage::default();

    let mut binder = Binder::new_function(
        &table,
        resolution::NoOp,
        type_system::observer::NoOp,
        function_id,
        std::iter::empty(),
        &Counter::default(),
    )
    .unwrap();

    // no error
    assert!(storage.as_vec().is_empty());

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

    let (pattern, binding_point) = binder.bind_irrefutable(
        REFERENCE_BOUND_STRUCT,
        &reference_struct_ty,
        Address::Memory(Memory::Alloca(struct_alloca_id)),
    );

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
            &binding_point,
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
            &binding_point,
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

    // no error
    assert!(storage.as_vec().is_empty());

    let tuple_alloca_id = binder.create_alloca(tuple_ty.clone(), None);

    let (pattern, binding_point) = binder.bind_irrefutable(
        VALUE_BOUND_TUPLE,
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
        assert_eq!(pattern.kind, BindingKind::Value(true));

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
    const REFERENCE_BOUND_TUPLE: &str = "(ref a, mutable b)";

    let (table, function_id) = create_dummy_function();

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

    let (pattern, binding_point) = binder.bind_irrefutable(
        REFERENCE_BOUND_TUPLE,
        &reference_tuple_ty,
        Address::Memory(Memory::Alloca(tuple_alloca_id)),
    );

    let Irrefutable::Tuple(pattern) = pattern else {
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
fn more_packed_tuple() {
    const PACKED_TUPLE: &str = "(a, ...b,  c)";

    let (table, function_id) = create_dummy_function();

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

    let (pattern, binding_point) = binder.bind_irrefutable(
        PACKED_TUPLE,
        &tuple_ty,
        Address::Memory(Memory::Alloca(tuple_alloca_id)),
    );

    // no error
    assert!(storage.as_vec().is_empty());

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
        )
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

            &store.address
                == &Address::Tuple(address::Tuple {
                    tuple_address: Box::new(load_address.clone()),
                    offset: address::Offset::FromStart(0),
                })
                && store.value.as_register().copied().map_or(false, |reg| {
                    binder
                        .intermediate_representation
                        .registers
                        .get(reg)
                        .and_then(|reg| reg.assignment.as_load())
                        .map_or(false, |x| {
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
                && store.value.as_register().copied().map_or(false, |reg| {
                    binder
                        .intermediate_representation
                        .registers
                        .get(reg)
                        .and_then(|reg| reg.assignment.as_load())
                        .map_or(false, |x| {
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
                && store.value.as_register().copied().map_or(false, |reg| {
                    binder
                        .intermediate_representation
                        .registers
                        .get(reg)
                        .and_then(|reg| reg.assignment.as_load())
                        .map_or(false, |x| {
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
                .allocas
                .get(*load_address.as_memory().unwrap().as_alloca().unwrap())
                .unwrap()
                .r#type,
            Type::Tuple(term::Tuple {
                elements: vec![
                    TupleElement {
                        term: Type::Primitive(Primitive::Int16),
                        is_unpacked: false
                    },
                    TupleElement {
                        term: Type::Parameter(MemberID {
                            parent: GenericID::Struct(ID::new(0)),
                            id: ID::new(0),
                        }),
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
        )
    }
}
