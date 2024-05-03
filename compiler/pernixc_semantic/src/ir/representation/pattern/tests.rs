use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    sync::Arc,
};

use pernixc_base::{
    diagnostic::{Counter, Handler},
    source_file::SourceFile,
};
use pernixc_lexical::token_stream::TokenStream;
use pernixc_syntax::{parser::Parser, syntax_tree};

use crate::{
    arena::{Map, ID},
    ir::{
        address::{self, Address, Field},
        alloca::Alloca,
        instruction::{Basic, Instruction},
        representation::Representation,
        value::{register::Register, Value},
    },
    pattern::{self, Irrefutable, Named},
    semantic::term::{
        lifetime::{Lifetime, Local},
        r#type::{Primitive, Qualifier, Reference, SymbolID, Type},
        GenericArguments, Symbol, Tuple, TupleElement,
    },
    symbol::{
        self, Accessibility, GenericDeclaration, GenericID,
        GenericParameterVariances, GlobalID, MemberID, Module, Struct,
    },
    table::{self, resolution::Resolution, NoContainer},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct State;

impl table::State for State {
    type Container = NoContainer;

    fn on_global_id_resolved(
        _: &table::Table<Self>,
        _: GlobalID,
        _: GlobalID,
        _: &dyn Handler<Box<dyn crate::error::Error>>,
    ) {
    }

    fn on_resolved(
        _: &table::Table<Self>,
        _: Resolution,
        _: GlobalID,
        _: &dyn Handler<Box<dyn crate::error::Error>>,
    ) {
    }
}

type Table = table::Table<State>;

fn create_pattern(source: impl Display) -> syntax_tree::pattern::Irrefutable {
    let counter = Counter::default();

    let source_file = Arc::new(SourceFile::temp(source).unwrap());
    let token_stream = TokenStream::tokenize(&source_file, &counter);

    // no error
    assert_eq!(counter.count(), 0);

    let mut parser = Parser::new(&token_stream);
    let pattern = parser.parse_irrefutable_pattern(&counter).unwrap();

    // no error
    assert_eq!(counter.count(), 0);

    pattern
}

impl<T> Representation<T> {
    /// Check for the named pattern that bound as `ref QUALIFIER IDENTIFIER`
    fn check_reference_bound_named_pattern(
        &self,
        named_pattern: &Named,
        expected_name: &str,
        expected_stored_adress: &Address,
        qualifier: Qualifier,
        lifetime: Lifetime,
        ty: &Type,
    ) {
        assert_eq!(named_pattern.name, expected_name);

        // should store the address at some alloca
        let Address::Alloca(stored_address_alloca_id) =
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
                let Instruction::Basic(Basic::RegisterAssignment(
                    register_assignment,
                )) = instruction
                else {
                    return None;
                };

                let Register::ReferenceOf(reference_of) =
                    self.registers.get(register_assignment.id).unwrap()
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
            let Instruction::Basic(Basic::Store(store)) = instruction else {
                return false;
            };

            store.value == Value::Register(reference_of_register_id)
                && store.address == Address::Alloca(stored_address_alloca_id)
        }));

        assert!(block.instructions().iter().any(|instruction| {
            let Instruction::Basic(Basic::AllocaAllocation(alloca_allocation)) =
                instruction
            else {
                return false;
            };

            alloca_allocation.id == stored_address_alloca_id
        }));

        let alloca = self.allocas.get(stored_address_alloca_id).unwrap();

        assert_eq!(
            alloca.r#type,
            Type::Reference(Reference {
                qualifier,
                lifetime,
                pointee: Box::new(ty.clone())
            })
        );
    }
}

fn create_table() -> (Table, ID<Module>) {
    let mut table = Table::default();

    let module_id = {
        let id = table.representation.modules.insert_with(|_| Module {
            name: "test".to_string(),
            accessibility: Accessibility::Public,
            parent_module_id: None,
            child_ids_by_name: HashMap::new(),
            span: None,
            usings: HashSet::new(),
        });

        assert!(table
            .representation
            .root_module_ids_by_name
            .insert("test".to_string(), id)
            .is_none());

        id
    };

    (table, module_id)
}

fn create_table_with_struct(
    f: impl FnOnce(ID<Struct>, ID<Module>) -> Struct,
) -> (Table, ID<Module>, ID<Struct>) {
    let (mut table, module_id) = create_table();

    let struct_id =
        table.representation.structs.insert_with(|idx| f(idx, module_id));

    let struct_name =
        table.representation.structs.get(struct_id).unwrap().name.clone();

    assert!(table
        .representation
        .modules
        .get_mut(module_id)
        .unwrap()
        .child_ids_by_name
        .insert(struct_name, struct_id.into())
        .is_none());

    (table, module_id, struct_id)
}

#[test]
fn value_bound_named() {
    const VALUE_BOUND_NAMED: &str = "mutable helloWorld";

    let mut representation = Representation::<()>::default();
    let (table, referring_site) = create_table();
    let pattern = create_pattern(VALUE_BOUND_NAMED);

    let alloca_id = representation
        .allocas
        .insert(Alloca { r#type: Type::default(), span: None });

    let counter = Counter::default();

    let pattern = representation
        .create_irrefutable(
            &table,
            &pattern,
            &Type::default(),
            &Address::Alloca(alloca_id),
            representation.control_flow_graph.entry_block_id(),
            referring_site.into(),
            &counter,
        )
        .unwrap();

    // no error
    assert_eq!(counter.count(), 0);

    let Irrefutable::Named(pattern) = pattern else {
        panic!("Expected a named pattern")
    };

    assert_eq!(pattern.name, "helloWorld");
    assert_eq!(pattern.load_address, Address::Alloca(alloca_id));
    assert!(pattern.mutable);
}

#[test]
fn reference_bound_named() {
    const SIMPLE_NAMED_VALUE_BOUND: &str = "ref unique helloWorld";

    let mut representation = Representation::<()>::default();
    let (table, referring_site) = create_table();
    let pattern = create_pattern(SIMPLE_NAMED_VALUE_BOUND);

    let alloca_id = representation
        .allocas
        .insert(Alloca { r#type: Type::default(), span: None });

    let counter = Counter::default();

    let pattern = representation
        .create_irrefutable(
            &table,
            &pattern,
            &Type::default(),
            &Address::Alloca(alloca_id),
            representation.control_flow_graph.entry_block_id(),
            referring_site.into(),
            &counter,
        )
        .unwrap();

    // no error
    assert_eq!(counter.count(), 0);

    let Irrefutable::Named(pattern) = pattern else {
        panic!("Expected a named pattern")
    };

    representation.check_reference_bound_named_pattern(
        &pattern,
        "helloWorld",
        &Address::Alloca(alloca_id),
        Qualifier::Unique,
        Lifetime::Local(Local(
            representation.control_flow_graph.starting_scope_id(),
        )),
        &Type::default(),
    );
}

#[test]
#[allow(clippy::too_many_lines)]
fn value_bound_struct() {
    const VALUE_BOUND_STRUCT: &str = "{ ref unique a, mutable b }";

    let mut representation = Representation::<()>::default();
    let (table, referring_site, struct_id) =
        create_table_with_struct(|_, parent_module_id| Struct {
            name: "Test".to_string(),
            accessibility: Accessibility::Public,
            parent_module_id,
            generic_declaration: GenericDeclaration::default(),
            fields: {
                let mut fields = Map::new();

                fields
                    .insert("a".to_string(), symbol::Field {
                        accessibility: Accessibility::Public,
                        name: "a".to_string(),
                        r#type: Type::Primitive(Primitive::Int32),
                        span: None,
                    })
                    .unwrap();
                fields
                    .insert("b".to_string(), symbol::Field {
                        accessibility: Accessibility::Public,
                        name: "b".to_string(),
                        r#type: Type::Primitive(Primitive::Float32),
                        span: None,
                    })
                    .unwrap();

                fields
            },
            implementations: HashSet::new(),
            generic_parameter_variances: GenericParameterVariances::default(),
            span: None,
        });
    let pattern = create_pattern(VALUE_BOUND_STRUCT);
    let struct_ty = Type::Symbol(Symbol {
        id: SymbolID::Struct(struct_id),
        generic_arguments: GenericArguments::default(),
    });

    let a_field_id = table
        .representation
        .structs
        .get(struct_id)
        .unwrap()
        .fields
        .get_id("a")
        .unwrap();
    let b_field_id = table
        .representation
        .structs
        .get(struct_id)
        .unwrap()
        .fields
        .get_id("b")
        .unwrap();

    let struct_alloca_id = representation
        .allocas
        .insert(Alloca { r#type: struct_ty.clone(), span: None });

    let counter = Counter::default();

    let pattern = representation
        .create_irrefutable(
            &table,
            &pattern,
            &struct_ty,
            &Address::Alloca(struct_alloca_id),
            representation.control_flow_graph.entry_block_id(),
            referring_site.into(),
            &counter,
        )
        .unwrap();

    // no error
    assert_eq!(counter.count(), 0);

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

        representation.check_reference_bound_named_pattern(
            pattern,
            "a",
            &Address::Field(Field {
                struct_address: Box::new(Address::Alloca(struct_alloca_id)),
                id: a_field_id,
            }),
            Qualifier::Unique,
            Lifetime::Local(Local(
                representation.control_flow_graph.starting_scope_id(),
            )),
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

        // should store the address at some alloca
        let Address::Field(field_address) = &pattern.load_address else {
            panic!("Expected a field address")
        };

        assert_eq!(field_address.id, b_field_id);
        assert_eq!(
            &*field_address.struct_address,
            &Address::Alloca(struct_alloca_id)
        );
    }
}

#[test]
#[allow(clippy::too_many_lines)]
fn reference_bound_struct() {
    // both a and b are bound by reference
    const REFERENCE_BOUND_STRUCT: &str = "{ ref a, mutable b }";

    let mut representation = Representation::<()>::default();
    let (table, referring_site, struct_id) =
        create_table_with_struct(|_, parent_module_id| Struct {
            name: "Test".to_string(),
            accessibility: Accessibility::Public,
            parent_module_id,
            generic_declaration: GenericDeclaration::default(),
            fields: {
                let mut fields = Map::new();

                fields
                    .insert("a".to_string(), symbol::Field {
                        accessibility: Accessibility::Public,
                        name: "a".to_string(),
                        r#type: Type::Primitive(Primitive::Int32),
                        span: None,
                    })
                    .unwrap();
                fields
                    .insert("b".to_string(), symbol::Field {
                        accessibility: Accessibility::Public,
                        name: "b".to_string(),
                        r#type: Type::Primitive(Primitive::Float32),
                        span: None,
                    })
                    .unwrap();

                fields
            },
            implementations: HashSet::new(),
            generic_parameter_variances: GenericParameterVariances::default(),
            span: None,
        });
    let pattern = create_pattern(REFERENCE_BOUND_STRUCT);
    let reference_struct_ty = Type::Reference(Reference {
        qualifier: Qualifier::Immutable,
        lifetime: Lifetime::Static,
        pointee: Box::new(Type::Symbol(Symbol {
            id: SymbolID::Struct(struct_id),
            generic_arguments: GenericArguments::default(),
        })),
    });

    let a_field_id = table
        .representation
        .structs
        .get(struct_id)
        .unwrap()
        .fields
        .get_id("a")
        .unwrap();
    let b_field_id = table
        .representation
        .structs
        .get(struct_id)
        .unwrap()
        .fields
        .get_id("b")
        .unwrap();

    let struct_alloca_id = representation
        .allocas
        .insert(Alloca { r#type: reference_struct_ty.clone(), span: None });

    let counter = Counter::default();

    let pattern = representation
        .create_irrefutable(
            &table,
            &pattern,
            &reference_struct_ty,
            &Address::Alloca(struct_alloca_id),
            representation.control_flow_graph.entry_block_id(),
            referring_site.into(),
            &counter,
        )
        .unwrap();

    // no error
    assert_eq!(counter.count(), 0);

    let Irrefutable::Structural(pattern) = pattern else {
        panic!("Expected a named pattern")
    };

    let load_address_register = representation
        .registers
        .iter()
        .find_map(|(idx, i)| {
            let Register::Load(load) = i else {
                return None;
            };

            if load.address == Address::Alloca(struct_alloca_id) {
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

        representation.check_reference_bound_named_pattern(
            pattern,
            "a",
            &Address::Field(Field {
                struct_address: Box::new(Address::Value(Value::Register(
                    load_address_register,
                ))),
                id: a_field_id,
            }),
            Qualifier::Immutable,
            Lifetime::Static,
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

        representation.check_reference_bound_named_pattern(
            pattern,
            "b",
            &Address::Field(Field {
                struct_address: Box::new(Address::Value(Value::Register(
                    load_address_register,
                ))),
                id: b_field_id,
            }),
            Qualifier::Immutable,
            Lifetime::Static,
            &Type::Primitive(Primitive::Float32),
        );
    }
}

#[test]
#[allow(missing_docs)]
fn value_bound_tuple() {
    const VALUE_BOUND_TUPLE: &str = "(ref a, mutable b)";

    let mut representation = Representation::<()>::default();
    let (table, referring_site) = create_table();

    let pattern = create_pattern(VALUE_BOUND_TUPLE);
    let tuple_ty = Type::Tuple(Tuple {
        elements: vec![
            TupleElement::Regular(Type::Primitive(Primitive::Bool)),
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

    let Irrefutable::Tuple(pattern::Tuple::Regular(pattern)) = pattern else {
        panic!("Expected a named pattern")
    };

    // ref a tuple check
    {
        let Some(Irrefutable::Named(pattern)) = pattern.elements.first() else {
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
            Lifetime::Local(Local(
                representation.control_flow_graph.starting_scope_id(),
            )),
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
        let Address::Tuple(tuple_address) = &pattern.load_address else {
            panic!("Expected a tuple address")
        };

        assert_eq!(tuple_address.offset, address::Offset::FromStart(1));
        assert_eq!(
            &*tuple_address.tuple_address,
            &Address::Alloca(tuple_alloca_id)
        );
    }
}

#[test]
fn reference_bound_tuple() {
    const REFERENCE_BOUND_TUPLE: &str = "(ref a, mutable b)";

    let mut representation = Representation::<()>::default();
    let (table, referring_site) = create_table();

    let pattern = create_pattern(REFERENCE_BOUND_TUPLE);
    let tuple_ty = Type::Reference(Reference {
        qualifier: Qualifier::Mutable,
        lifetime: Lifetime::Static,
        pointee: Box::new(Type::Tuple(Tuple {
            elements: vec![
                TupleElement::Regular(Type::Primitive(Primitive::Bool)),
                TupleElement::Regular(Type::Primitive(Primitive::Int32)),
            ],
        })),
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

    let Irrefutable::Tuple(pattern::Tuple::Regular(pattern)) = pattern else {
        panic!("Expected a named pattern")
    };

    let load_address_register = representation
        .registers
        .iter()
        .find_map(|(idx, i)| {
            let Register::Load(load) = i else {
                return None;
            };

            if load.address == Address::Alloca(tuple_alloca_id) {
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

        representation.check_reference_bound_named_pattern(
            pattern,
            "a",
            &Address::Tuple(address::Tuple {
                tuple_address: Box::new(Address::Value(Value::Register(
                    load_address_register,
                ))),
                offset: address::Offset::FromStart(0),
            }),
            Qualifier::Mutable,
            Lifetime::Static,
            &Type::Primitive(Primitive::Bool),
        );
    }

    // mutable b check field
    {
        let Some(Irrefutable::Named(pattern)) = pattern.elements.get(1) else {
            panic!("Expected a named pattern")
        };

        representation.check_reference_bound_named_pattern(
            pattern,
            "b",
            &Address::Tuple(address::Tuple {
                tuple_address: Box::new(Address::Value(Value::Register(
                    load_address_register,
                ))),
                offset: address::Offset::FromStart(1),
            }),
            Qualifier::Mutable,
            Lifetime::Static,
            &Type::Primitive(Primitive::Int32),
        );
    }
}

#[test]
#[allow(clippy::too_many_lines)]
fn packed_tuple() {
    const PACKED_TUPLE: &str = "(ref a, b, mutable c)";

    let mut representation = Representation::<()>::default();
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
            Lifetime::Local(Local(
                representation.control_flow_graph.starting_scope_id(),
            )),
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

        let Irrefutable::Named(pat) = pattern.packed_element.as_ref() else {
            panic!("Expected a named pattern")
        };

        let store_address = block
            .instructions()
            .iter()
            .find_map(|inst| {
                let Instruction::Basic(Basic::TuplePack(pack)) = inst else {
                    return None;
                };

                if pack.before_packed_element_count == 1
                    && pack.after_packed_element_count == 1
                    && pack.tuple_address == Address::Alloca(tuple_alloca_id)
                {
                    Some(&pack.store_address)
                } else {
                    None
                }
            })
            .unwrap();

        assert_eq!(store_address, &pat.load_address);

        assert!(block.instructions().iter().any(|inst| {
            let Instruction::Basic(Basic::AllocaAllocation(inst)) = inst else {
                return false;
            };

            inst.id == *pat.load_address.as_alloca().unwrap()
        }));

        assert_eq!(
            representation
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

#[test]
#[allow(clippy::too_many_lines)]
fn optimize_unused_pack_tuple() {
    const PACKED_TUPLE: &str = "(ref a, ?, mutable c)";

    let mut representation = Representation::<()>::default();
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
            Lifetime::Local(Local(
                representation.control_flow_graph.starting_scope_id(),
            )),
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
