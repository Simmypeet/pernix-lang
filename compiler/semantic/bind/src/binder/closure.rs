//! Defines the logic for building nested binders, such as for closures,
//! effect handlers, do blocks, etc.

use flexstr::SharedStr;
use pernixc_arena::Arena;
use pernixc_ir::{
    closure::Capture,
    instruction::{self, ScopePush},
    pattern::{NameBinding, NameBindingPoint},
    IR,
};
use pernixc_type_system::UnrecoverableError;

use crate::binder::{block, r#loop, stack::Stack, Binder};

#[derive(Default)]
struct Captures {
    captures: Arena<Capture>,
    name_binding_point: NameBindingPoint,
}

impl Captures {
    pub fn new(parent_stack: &Stack) -> Self {
        let mut captures = Captures::default();

        for scope in parent_stack.scopes().iter().rev() {
            for named_binding in scope
                .named_binding_points()
                .iter()
                .rev()
                .flat_map(|x| x.named_patterns_by_name.iter())
            {
                captures
                    .try_insert_named_binding(named_binding.0, named_binding.1);
            }
        }

        captures
    }

    pub fn try_insert_named_binding(
        &mut self,
        name: &SharedStr,
        name_binding: &NameBinding,
    ) {
        // only insert if it doesn't already exist
        if self
            .name_binding_point
            .named_patterns_by_name
            .contains_key(name.as_str())
        {
            return;
        }

        // check if this memory address dominates any existing captures, if
        // so, we re-adjust the existing captures to use this memory address

        for (existing_name, existing_binding) in
            &self.name_binding_point.named_patterns_by_name
        {
            let existing_binding_root =
                existing_binding.load_address.get_root_memory();
        }
    }
}

impl Binder<'_> {
    /// Creates a nested binder that can be used to produce a nested IR.
    pub async fn nested_binder<T>(
        &mut self,
        f: impl AsyncFnOnce(&mut Self) -> Result<(), UnrecoverableError>,
    ) -> Result<IR, UnrecoverableError> {
        // temporary move out the inference context for the inner binder
        let inference_context = std::mem::take(&mut self.inference_context);

        let ir = IR::default();
        let current_block_id = ir.control_flow_graph.entry_block_id();
        let stack = Stack::new(ir.scope_tree.root_scope_id(), false);

        let mut binder = Self {
            engine: self.engine,
            environment: self.environment,
            ir,
            current_block_id,
            stack,
            inference_context,
            unreachable_register_ids: Vec::new(),
            block_context: block::Context::default(),
            loop_context: r#loop::Context::default(),
        };

        let root_scope_id = binder.ir.scope_tree.root_scope_id();
        binder.push_instruction(instruction::Instruction::ScopePush(
            ScopePush(root_scope_id),
        ));

        let result = f(&mut binder).await;

        // restore back the inference context
        self.inference_context = binder.inference_context;

        result.map(|()| binder.ir)
    }
}
