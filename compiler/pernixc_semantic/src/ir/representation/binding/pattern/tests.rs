use std::{fmt::Display, sync::Arc};

use pernixc_base::{diagnostic::Counter, source_file::SourceFile};
use pernixc_lexical::token_stream::TokenStream;
use pernixc_syntax::{parser::Parser, syntax_tree};

use crate::{
    arena::ID,
    ir::{
        address::{self, Address, Field},
        alloca::Alloca,
        instruction::{Basic, Instruction},
        pattern::{self, Irrefutable, Named},
        representation::Representation,
        value::{register::Register, Value},
    },
    semantic::{
        fresh::Fresh,
        model::Model,
        term::{
            constant::Constant,
            lifetime::Lifetime,
            r#type::{Primitive, Qualifier, Reference, SymbolID, Type},
            GenericArguments, Symbol, Tuple, TupleElement,
        },
    },
    symbol::{
        self,
        table::{
            representation::{Index, IndexMut, Insertion},
            Building, Table,
        },
        Accessibility, AdtTemplate, GenericDeclaration, GenericID, MemberID,
        Module, StructDefinition,
    },
};

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct SimpleInference;

impl Fresh for SimpleInference {
    fn fresh() -> Self { Self }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct SimpleModel;

impl From<SimpleInference> for Lifetime<SimpleModel> {
    fn from(val: SimpleInference) -> Self { Self::Inference(val) }
}

impl From<SimpleInference> for Type<SimpleModel> {
    fn from(val: SimpleInference) -> Self { Self::Inference(val) }
}

impl From<SimpleInference> for Constant<SimpleModel> {
    fn from(val: SimpleInference) -> Self { Self::Inference(val) }
}

impl Model for SimpleModel {
    type LifetimeInference = SimpleInference;
    type TypeInference = SimpleInference;
    type ConstantInference = SimpleInference;
}

impl Representation<SimpleModel> {
    /// Check for the named pattern that bound as `ref QUALIFIER IDENTIFIER`
    fn check_reference_bound_named_pattern(
        &self,
        named_pattern: &Named<SimpleModel>,
        expected_name: &str,
        expected_stored_adress: &Address<SimpleModel>,
        qualifier: Qualifier,
        ty: &Type<SimpleModel>,
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
            let Instruction::Basic(Basic::Initialize(store)) = instruction
            else {
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
                lifetime: Lifetime::Inference(SimpleInference),
                pointee: Box::new(ty.clone())
            })
        );
    }
}

fn create_table() -> (Table<Building>, ID<Module>) {
    let mut table = Table::<Building>::default();

    let Insertion { id, duplication } =
        table.create_root_module("test".to_string());

    assert!(duplication.is_none());

    (table, id)
}

#[test]
fn value_bound_named() {
    const VALUE_BOUND_NAMED: &str = "mutable helloWorld";

    let mut representation = Representation::<SimpleModel>::default();
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

    let mut representation = Representation::default();
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
        &Type::default(),
    );
}

#[test]
#[allow(clippy::too_many_lines)]
fn value_bound_struct() {
    const VALUE_BOUND_STRUCT: &str = "{ ref unique a, mutable b }";

    let mut representation = Representation::default();
    let (table, referring_site, struct_id) = {
        let (mut table, root_module) = create_table();

        let Insertion { id: struct_id, duplication } = table
            .insert_member(
                "Test".to_string(),
                Accessibility::Public,
                root_module,
                None,
                GenericDeclaration::default(),
                AdtTemplate::<StructDefinition>::default(),
            )
            .unwrap();

        assert!(duplication.is_none());

        let struct_sym = table.get_mut(struct_id).unwrap();

        struct_sym
            .fields
            .insert("a".to_string(), symbol::Field {
                accessibility: Accessibility::Public,
                r#type: Type::Primitive(Primitive::Int32),
                name: "a".to_string(),
                span: None,
            })
            .unwrap();

        struct_sym
            .fields
            .insert("b".to_string(), symbol::Field {
                accessibility: Accessibility::Public,
                r#type: Type::Primitive(Primitive::Float32),
                name: "b".to_string(),
                span: None,
            })
            .unwrap();

        (table, root_module, struct_id)
    };

    let pattern = create_pattern(VALUE_BOUND_STRUCT);
    let struct_ty = Type::Symbol(Symbol {
        id: SymbolID::Struct(struct_id),
        generic_arguments: GenericArguments::default(),
    });

    let a_field_id = table.get(struct_id).unwrap().fields.get_id("a").unwrap();
    let b_field_id = table.get(struct_id).unwrap().fields.get_id("b").unwrap();

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

    let mut representation = Representation::default();
    let (table, referring_site, struct_id) = {
        let (mut table, root_module) = create_table();

        let Insertion { id: struct_id, duplication } = table
            .insert_member(
                "Test".to_string(),
                Accessibility::Public,
                root_module,
                None,
                GenericDeclaration::default(),
                AdtTemplate::<StructDefinition>::default(),
            )
            .unwrap();

        assert!(duplication.is_none());

        let struct_sym = table.get_mut(struct_id).unwrap();

        struct_sym
            .fields
            .insert("a".to_string(), symbol::Field {
                accessibility: Accessibility::Public,
                r#type: Type::Primitive(Primitive::Int32),
                name: "a".to_string(),
                span: None,
            })
            .unwrap();

        struct_sym
            .fields
            .insert("b".to_string(), symbol::Field {
                accessibility: Accessibility::Public,
                r#type: Type::Primitive(Primitive::Float32),
                name: "b".to_string(),
                span: None,
            })
            .unwrap();

        (table, root_module, struct_id)
    };

    let pattern = create_pattern(REFERENCE_BOUND_STRUCT);
    let reference_struct_ty = Type::Reference(Reference {
        qualifier: Qualifier::Immutable,
        lifetime: Lifetime::Static,
        pointee: Box::new(Type::Symbol(Symbol {
            id: SymbolID::Struct(struct_id),
            generic_arguments: GenericArguments::default(),
        })),
    });

    let a_field_id = table.get(struct_id).unwrap().fields.get_id("a").unwrap();
    let b_field_id = table.get(struct_id).unwrap().fields.get_id("b").unwrap();

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

            if load.address == Address::Alloca(struct_alloca_id)
                && load.address_type == reference_struct_ty
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
            &Type::Primitive(Primitive::Float32),
        );
    }
}

#[test]
#[allow(missing_docs)]
fn value_bound_tuple() {
    const VALUE_BOUND_TUPLE: &str = "(ref a, mutable b)";

    let mut representation = Representation::default();
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

    let mut representation = Representation::default();
    let (table, referring_site) = create_table();

    let pattern = create_pattern(REFERENCE_BOUND_TUPLE);
    let reference_tuple_ty = Type::Reference(Reference {
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
        .insert(Alloca { r#type: reference_tuple_ty.clone(), span: None });

    let counter = Counter::default();

    let pattern = representation
        .create_irrefutable(
            &table,
            &pattern,
            &reference_tuple_ty,
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

            if load.address == Address::Alloca(tuple_alloca_id)
                && load.address_type == reference_tuple_ty
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
            &Type::Primitive(Primitive::Int32),
        );
    }
}

#[test]
#[allow(clippy::too_many_lines)]
fn packed_tuple() {
    const PACKED_TUPLE: &str = "(ref a, b, mutable c)";

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
