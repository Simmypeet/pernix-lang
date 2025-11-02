//! Contains the logic for building the cpatures.

use flexstr::SharedStr;
use getset::Getters;
use pernixc_arena::Arena;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_semantic_element::{fields::get_fields, parameter::get_parameters};
use pernixc_term::r#type::Type;

use crate::{
    address::{self, Address, Memory},
    capture::{Capture, CaptureMode, Captures},
    pattern::{NameBinding, NameBindingPoint},
    typer::{self, Typer},
};

/// A builder for constructing a [`Captures`] structure.
#[derive(Debug, Clone, Default)]
pub struct Builder {
    captures: Arena<Capture>,
    name_binding_point: NameBindingPoint,
}

impl Builder {
    /// Creates a new capture builder.
    #[must_use]
    pub fn new() -> Self { Self::default() }

    /// Inserts a named binding into the capture builder. The name should not
    /// be already inserted.
    pub fn insert_named_binding(
        &mut self,
        new_name: SharedStr,
        name_binding: &NameBinding,
        address_ty: Type,
    ) {
        // check if this memory address dominates any existing captures, if
        // so, we re-adjust the existing captures to use this memory address
        let mut dominated_captures = Vec::new();

        for (existing_name, existing_binding) in
            &self.name_binding_point.named_patterns_by_name
        {
            let existing_binding_root = existing_binding
                .load_address
                .get_root_memory()
                .as_capture()
                .copied()
                .unwrap();

            let existing_captured_address =
                &self.captures[existing_binding_root].parent_captured_address;

            // if the existing captured address is a child of the new
            // captured address, we re-adjust the existing capture to use
            if existing_captured_address == &name_binding.load_address
                || existing_captured_address
                    .is_child_of(&name_binding.load_address)
            {
                dominated_captures.push(existing_name.clone());
            }
        }

        // add a new capture for the new name binding
        let new_capture = Capture {
            parent_captured_address: name_binding.load_address.clone(),
            address_type: address_ty,

            // NOTE: While binding, we always capture by value. the
            // memory-checker pass will later adjust this to be by reference if
            // needed.
            capture_mode: CaptureMode::ByValue,

            span: Some(name_binding.span),
            drop_order: 0, // will be set later
        };

        // insert a new capture and cooresponding name binding
        let new_capture_id = self.captures.insert(new_capture);

        assert!(
            self.name_binding_point
                .named_patterns_by_name
                .insert(new_name, NameBinding {
                    mutable: name_binding.mutable,
                    load_address: Address::Memory(Memory::Capture(
                        new_capture_id
                    )),
                    span: name_binding.span,
                },)
                .is_none(),
            "attempted to insert duplicate name binding"
        );

        // re-adjust all the dominated captures to use the new captured memory
        let mut removing_captures = Vec::new();

        for dominated_name in dominated_captures {
            let dominated_binding = self
                .name_binding_point
                .named_patterns_by_name
                .get_mut(&dominated_name)
                .unwrap();

            let mut dominated_address = dominated_binding.load_address.clone();
            let dominated_root = dominated_address
                .get_root_memory()
                .as_capture()
                .copied()
                .unwrap();

            let dominated_root_captured_address =
                &self.captures[dominated_root].parent_captured_address;

            // replace the captured root with the actual original captured
            // address
            assert!(dominated_address.replace_with(
                &Address::Memory(Memory::Capture(dominated_root)),
                dominated_root_captured_address.clone(),
            ));

            // replace the dominated binding to use the new captured memory
            assert!(dominated_address.replace_with(
                &name_binding.load_address,
                Address::Memory(Memory::Capture(new_capture_id)),
            ));

            // update the dominated binding
            dominated_binding.load_address = dominated_address;

            // mark the dominated capture for removal
            removing_captures.push(dominated_root);
        }

        // remove all the dominated captures
        for dominated_capture in removing_captures {
            // it is possible we remove the same capture multiple times if
            // multiple names capture the same memory
            let _ = self.captures.remove(dominated_capture);
        }
    }

    /// Checks if the name has already been inserted to the capture.
    #[must_use]
    pub fn contains_name(&self, name: &str) -> bool {
        self.name_binding_point.contains_name(name)
    }

    /// Builds the capture and returns the name binding point and captures.
    pub async fn build<T: Typer<Address>, E: typer::Environment>(
        mut self,
        env: &E,
        typer: &T,
    ) -> Result<CapturesWithNameBindingPoint, T::Error> {
        // determine the drop order of all captures
        let mut drop_orders = Vec::new();

        for capture_id in self.captures.ids() {
            let capture = &self.captures[capture_id];

            let (drop_order, _) = DropOrder::get_drop_order(
                &capture.parent_captured_address,
                capture.span.unwrap(),
                env,
                typer,
            )
            .await?;

            drop_orders.push((capture_id, drop_order));
        }

        // sort the drop orders
        drop_orders.sort_by(|a, b| a.1.cmp(&b.1));

        // assign the drop orders
        for (drop_index, (capture_id, _)) in drop_orders.iter().enumerate() {
            self.captures[*capture_id].drop_order = drop_index;
        }

        Ok(CapturesWithNameBindingPoint {
            name_binding_point: self.name_binding_point,
            captures: Captures { captures: self.captures },
        })
    }
}

/// Represents the [`Captures`] along with the name binding point used for
/// referring to captured variables.
#[derive(Debug, Clone, PartialEq, Eq, Getters)]
pub struct CapturesWithNameBindingPoint {
    /// The [`NameBindingPoint`] containing available name binding for
    /// referencing to the captured variables.
    #[get = "pub"]
    name_binding_point: NameBindingPoint,

    /// The [`Captures`]
    #[get = "pub"]
    captures: Captures,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct AllocaOrder {
    depth: usize,
    declaration_order: usize,
}

impl PartialOrd for AllocaOrder {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for AllocaOrder {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.depth.cmp(&other.depth).reverse().then_with(|| {
            self.declaration_order.cmp(&other.declaration_order).reverse()
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct ParameterOrder {
    declaration_order: usize,
}

impl PartialOrd for ParameterOrder {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ParameterOrder {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.declaration_order.cmp(&other.declaration_order).reverse()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct CaptureOrder {
    drop_order: usize,
}

impl PartialOrd for CaptureOrder {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for CaptureOrder {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.drop_order.cmp(&other.drop_order)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum RootOrder {
    Alloca(AllocaOrder),
    Parameter(ParameterOrder),
    Capture(CaptureOrder),
}

impl PartialOrd for RootOrder {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for RootOrder {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            (Self::Alloca(a), Self::Alloca(b)) => a.cmp(b),
            (Self::Parameter(a), Self::Parameter(b)) => a.cmp(b),
            (Self::Capture(a), Self::Capture(b)) => a.cmp(b),
            _ => {
                let drop_order_variant = |order: &Self| match order {
                    Self::Alloca(_) => 0,
                    Self::Parameter(_) => 1,
                    Self::Capture(_) => 2,
                };

                drop_order_variant(self).cmp(&drop_order_variant(other))
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct FieldProjection {
    field_index: usize,
}

impl PartialOrd for FieldProjection {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for FieldProjection {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.field_index.cmp(&other.field_index)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct TupleProjection {
    field_index: usize,
}

impl PartialOrd for TupleProjection {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for TupleProjection {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.field_index.cmp(&other.field_index)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
enum Projection {
    Field(FieldProjection),
    Tuple(TupleProjection),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct DropOrder {
    root: RootOrder,
    projections: Vec<Projection>,
}

impl PartialOrd for DropOrder {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for DropOrder {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.root.cmp(&other.root).then_with(|| {
            let min_len =
                std::cmp::min(self.projections.len(), other.projections.len());

            for i in 0..min_len {
                let ord = self.projections[i].cmp(&other.projections[i]);
                if ord != std::cmp::Ordering::Equal {
                    return ord;
                }
            }

            self.projections.len().cmp(&other.projections.len())
        })
    }
}

impl DropOrder {
    #[allow(clippy::too_many_lines)]
    async fn get_drop_order<T: Typer<Address>, E: typer::Environment>(
        address: &Address,
        span: RelativeSpan,
        env: &E,
        typer: &T,
    ) -> Result<(Self, bool), T::Error> {
        match address {
            Address::Memory(memory) => {
                let root_order = match memory {
                    Memory::Parameter(id) => {
                        let parameters = env
                            .tracked_engine()
                            .get_parameters(env.current_site())
                            .await?;

                        RootOrder::Parameter(ParameterOrder {
                            declaration_order: parameters
                                .parameter_order
                                .iter()
                                .position(|x| *x == *id)
                                .unwrap(),
                        })
                    }

                    Memory::Alloca(id) => {
                        let scope_id =
                            env.values().allocas[*id].declared_in_scope_id;
                        let scope_depth =
                            env.scope_tree().scopes()[scope_id].depth;
                        let declared_order =
                            env.values().allocas[*id].declaration_order;

                        RootOrder::Alloca(AllocaOrder {
                            depth: scope_depth,
                            declaration_order: declared_order,
                        })
                    }

                    Memory::Capture(id) => RootOrder::Capture(CaptureOrder {
                        drop_order: env.captures().unwrap()[*id].drop_order,
                    }),
                };

                Ok((Self { root: root_order, projections: Vec::new() }, true))
            }

            Address::Field(field) => {
                let (mut drop_order, should_continue) =
                    Box::pin(Self::get_drop_order(
                        &field.struct_address,
                        span,
                        env,
                        typer,
                    ))
                    .await?;

                if !should_continue {
                    return Ok((drop_order, false));
                }

                let ty = typer.type_of(&*field.struct_address, env).await?;

                // should've been a struct type
                let struct_ty = ty.as_symbol().unwrap();
                let struct_id = struct_ty.id;

                let fields = env.tracked_engine().get_fields(struct_id).await?;

                let field_index = fields
                    .field_declaration_order
                    .iter()
                    .position(|x| *x == field.id)
                    .unwrap();

                drop_order
                    .projections
                    .push(Projection::Field(FieldProjection { field_index }));

                Ok((drop_order, true))
            }

            Address::Tuple(tuple) => {
                let (mut drop_order, should_continue) =
                    Box::pin(Self::get_drop_order(
                        &tuple.tuple_address,
                        span,
                        env,
                        typer,
                    ))
                    .await?;

                if !should_continue {
                    return Ok((drop_order, false));
                }

                let ty = typer.type_of(&*tuple.tuple_address, env).await?;
                let tuple_ty = ty.as_tuple().unwrap();

                drop_order.projections.push(Projection::Tuple(
                    TupleProjection {
                        field_index: match tuple.offset {
                            address::Offset::FromStart(st) => st,
                            address::Offset::FromEnd(st) => {
                                tuple_ty.elements.len() - 1 - st
                            }
                            address::Offset::Unpacked => tuple_ty
                                .elements
                                .iter()
                                .position(|x| x.is_unpacked)
                                .unwrap(),
                        },
                    },
                ));

                Ok((drop_order, true))
            }

            Address::Index(index) => {
                let (drop_order, _) = Box::pin(Self::get_drop_order(
                    &index.array_address,
                    span,
                    env,
                    typer,
                ))
                .await?;

                Ok((drop_order, false))
            }

            Address::Variant(variant) => {
                Box::pin(Self::get_drop_order(
                    &variant.enum_address,
                    span,
                    env,
                    typer,
                ))
                .await
            }

            Address::Reference(reference) => {
                let (drop_order, _) = Box::pin(Self::get_drop_order(
                    &reference.reference_address,
                    span,
                    env,
                    typer,
                ))
                .await?;

                Ok((drop_order, false))
            }
        }
    }
}
