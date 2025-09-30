use std::sync::atomic::{AtomicU64, Ordering};

use pernixc_term::{
    inference,
    r#type::{Primitive, Type},
};

use super::super::{Inference, Table, View};
use crate::infer::constraint;

type TypeTable = Table<constraint::Type>;

fn create_int32_type() -> Type { Type::Primitive(Primitive::Int32) }

#[allow(dead_code)]
fn create_uint32_type() -> Type { Type::Primitive(Primitive::Uint32) }

#[allow(dead_code)]
fn create_float64_type() -> Type { Type::Primitive(Primitive::Float64) }

fn create_inference_variable() -> inference::Variable<Type> {
    static COUNTER: AtomicU64 = AtomicU64::new(1);
    inference::Variable::new(COUNTER.fetch_add(1, Ordering::SeqCst))
}

// Helper function to assert that a variable doesn't exist in the table
fn assert_variable_not_registered(
    table: &TypeTable,
    var: inference::Variable<Type>,
) {
    assert!(table.get_inference(var).is_none());
}

// Helper function to assert that a variable has a specific constraint
fn assert_variable_has_constraint(
    table: &TypeTable,
    var: inference::Variable<Type>,
    expected_constraint: constraint::Type,
) {
    match table.get_view(var).unwrap() {
        View::Constraint(constraint) => {
            assert_eq!(*constraint, expected_constraint);
        }
        View::Known(_) => panic!("Expected constraint view, got known type"),
    }
}

// Helper function to assert that a variable has a specific known type
fn assert_variable_has_known_type(
    table: &TypeTable,
    var: inference::Variable<Type>,
    expected_type: &Type,
) {
    match table.get_view(var).unwrap() {
        View::Known(type_val) => {
            assert_eq!(type_val, expected_type);
        }
        View::Constraint(_) => panic!("Expected known type, got constraint"),
    }
}

#[test]
fn register_log_rollback() {
    let mut table = TypeTable::default();

    // Start checkpoint
    let checkpoint = table.start_checkpoint();

    // Register some variables
    let var1 = create_inference_variable();
    let var2 = create_inference_variable();
    let var3 = create_inference_variable();

    table.register(var1, constraint::Type::All(true));
    table.register(var2, constraint::Type::Number);
    table.register(var3, constraint::Type::Integer);

    // Verify variables are registered
    assert_variable_has_constraint(&table, var1, constraint::Type::All(true));
    assert_variable_has_constraint(&table, var2, constraint::Type::Number);
    assert_variable_has_constraint(&table, var3, constraint::Type::Integer);

    // Rollback to checkpoint
    table.restore(checkpoint);

    // Verify all variables are unregistered
    assert_variable_not_registered(&table, var1);
    assert_variable_not_registered(&table, var2);
    assert_variable_not_registered(&table, var3);
}

#[test]
fn combine_log_rollback() {
    let mut table = TypeTable::default();

    // Register a variable
    let var = create_inference_variable();
    table.register(var, constraint::Type::All(true));

    // Start checkpoint after registration
    let checkpoint = table.start_checkpoint();

    // Apply progressively more restrictive constraints (this generates Combine
    // logs)
    table.assign_constraint(var, constraint::Type::Number).unwrap();
    assert_variable_has_constraint(&table, var, constraint::Type::Number);

    table.assign_constraint(var, constraint::Type::Integer).unwrap();
    assert_variable_has_constraint(&table, var, constraint::Type::Integer);

    table.assign_constraint(var, constraint::Type::SignedInteger).unwrap();
    assert_variable_has_constraint(
        &table,
        var,
        constraint::Type::SignedInteger,
    );

    // Rollback to checkpoint
    table.restore(checkpoint);

    // Should be back to original All(true) constraint
    assert_variable_has_constraint(&table, var, constraint::Type::All(true));
}

#[test]
fn assign_to_known_log_rollback() {
    let mut table = TypeTable::default();

    // Register and unify multiple variables
    let var1 = create_inference_variable();
    let var2 = create_inference_variable();
    let var3 = create_inference_variable();

    table.register(var1, constraint::Type::All(true));
    table.register(var2, constraint::Type::Number);
    table.register(var3, constraint::Type::Integer);

    // Get constraint IDs and unify them
    let var1_id = match table.get_inference(var1).unwrap() {
        Inference::Inferring(id) => *id,
        Inference::Known(_) => panic!("Expected inferring state"),
    };
    let var2_id = match table.get_inference(var2).unwrap() {
        Inference::Inferring(id) => *id,
        Inference::Known(_) => panic!("Expected inferring state"),
    };
    let var3_id = match table.get_inference(var3).unwrap() {
        Inference::Inferring(id) => *id,
        Inference::Known(_) => panic!("Expected inferring state"),
    };

    table.unify_infers(var1_id, var2_id).unwrap();
    table.unify_infers(var1_id, var3_id).unwrap();

    // All should now have Integer constraint
    assert_variable_has_constraint(&table, var1, constraint::Type::Integer);
    assert_variable_has_constraint(&table, var2, constraint::Type::Integer);
    assert_variable_has_constraint(&table, var3, constraint::Type::Integer);

    // Start checkpoint before assigning known type
    let checkpoint = table.start_checkpoint();

    // Assign known type (this generates AssignToKnown log)
    let int32_type = create_int32_type();
    table.assign_known(var1_id, int32_type.clone()).unwrap();

    // All variables should now be known
    assert_variable_has_known_type(&table, var1, &int32_type);
    assert_variable_has_known_type(&table, var2, &int32_type);
    assert_variable_has_known_type(&table, var3, &int32_type);

    // Rollback to checkpoint
    table.restore(checkpoint);

    // Should be back to inferring with Integer constraint
    assert_variable_has_constraint(&table, var1, constraint::Type::Integer);
    assert_variable_has_constraint(&table, var2, constraint::Type::Integer);
    assert_variable_has_constraint(&table, var3, constraint::Type::Integer);
}

#[test]
fn unify_constraint_log_rollback() {
    let mut table = TypeTable::default();

    // Register variables with different constraints
    let var1 = create_inference_variable();
    let var2 = create_inference_variable();

    table.register(var1, constraint::Type::Number);
    table.register(var2, constraint::Type::Integer);

    // Verify initial constraints
    assert_variable_has_constraint(&table, var1, constraint::Type::Number);
    assert_variable_has_constraint(&table, var2, constraint::Type::Integer);

    // Start checkpoint before unification
    let checkpoint = table.start_checkpoint();

    // Get constraint IDs and unify (this generates UnifyConstraint log)
    let var1_id = match table.get_inference(var1).unwrap() {
        Inference::Inferring(id) => *id,
        Inference::Known(_) => panic!("Expected inferring state"),
    };
    let var2_id = match table.get_inference(var2).unwrap() {
        Inference::Inferring(id) => *id,
        Inference::Known(_) => panic!("Expected inferring state"),
    };

    table.unify_infers(var1_id, var2_id).unwrap();

    // Both should now have Integer constraint (more restrictive)
    assert_variable_has_constraint(&table, var1, constraint::Type::Integer);
    assert_variable_has_constraint(&table, var2, constraint::Type::Integer);

    // Rollback to checkpoint
    table.restore(checkpoint);

    // Should be back to original constraints
    assert_variable_has_constraint(&table, var1, constraint::Type::Number);
    assert_variable_has_constraint(&table, var2, constraint::Type::Integer);
}

#[test]
fn mixed_operations_rollback() {
    let mut table = TypeTable::default();

    // Start checkpoint
    let checkpoint = table.start_checkpoint();

    // Mix of all operation types

    // 1. Register variables (Register logs)
    let var1 = create_inference_variable();
    let var2 = create_inference_variable();
    let var3 = create_inference_variable();
    let var4 = create_inference_variable();

    table.register(var1, constraint::Type::All(true));
    table.register(var2, constraint::Type::Number);
    table.register(var3, constraint::Type::Integer);
    table.register(var4, constraint::Type::UnsignedInteger);

    // 2. Apply constraint restrictions (Combine logs)
    table.assign_constraint(var1, constraint::Type::Number).unwrap();
    table.assign_constraint(var1, constraint::Type::Integer).unwrap();

    // 3. Unify constraints (UnifyConstraint logs)
    let var1_id = match table.get_inference(var1).unwrap() {
        Inference::Inferring(id) => *id,
        Inference::Known(_) => panic!("Expected inferring state"),
    };
    let var2_id = match table.get_inference(var2).unwrap() {
        Inference::Inferring(id) => *id,
        Inference::Known(_) => panic!("Expected inferring state"),
    };
    let var3_id = match table.get_inference(var3).unwrap() {
        Inference::Inferring(id) => *id,
        Inference::Known(_) => panic!("Expected inferring state"),
    };

    table.unify_infers(var1_id, var2_id).unwrap();
    table.unify_infers(var1_id, var3_id).unwrap();

    // 4. Assign known type (AssignToKnown logs)
    let int32_type = create_int32_type();
    table.assign_known(var1_id, int32_type.clone()).unwrap();

    // Verify final state
    assert_variable_has_known_type(&table, var1, &int32_type);
    assert_variable_has_known_type(&table, var2, &int32_type);
    assert_variable_has_known_type(&table, var3, &int32_type);
    assert_variable_has_constraint(
        &table,
        var4,
        constraint::Type::UnsignedInteger,
    );

    // Rollback everything
    table.restore(checkpoint);

    // All variables should be unregistered
    assert_variable_not_registered(&table, var1);
    assert_variable_not_registered(&table, var2);
    assert_variable_not_registered(&table, var3);
    assert_variable_not_registered(&table, var4);
}

#[test]
fn nested_checkpoints() {
    let mut table = TypeTable::default();

    // Outer checkpoint
    let outer_checkpoint = table.start_checkpoint();

    // Register first batch of variables
    let var1 = create_inference_variable();
    let var2 = create_inference_variable();

    table.register(var1, constraint::Type::All(true));
    table.register(var2, constraint::Type::Number);

    // Inner checkpoint
    let inner_checkpoint = table.start_checkpoint();

    // Register more variables and perform operations
    let var3 = create_inference_variable();
    let var4 = create_inference_variable();

    table.register(var3, constraint::Type::Integer);
    table.register(var4, constraint::Type::Floating);

    // Apply some constraints
    table.assign_constraint(var1, constraint::Type::Number).unwrap();
    table.assign_constraint(var2, constraint::Type::Integer).unwrap();

    // Verify all variables exist
    assert_variable_has_constraint(&table, var1, constraint::Type::Number);
    assert_variable_has_constraint(&table, var2, constraint::Type::Integer);
    assert_variable_has_constraint(&table, var3, constraint::Type::Integer);
    assert_variable_has_constraint(&table, var4, constraint::Type::Floating);

    // Rollback to inner checkpoint
    table.restore(inner_checkpoint);

    // var3 and var4 should be gone, var1 and var2 should be back to original
    // state
    assert_variable_has_constraint(&table, var1, constraint::Type::All(true));
    assert_variable_has_constraint(&table, var2, constraint::Type::Number);
    assert_variable_not_registered(&table, var3);
    assert_variable_not_registered(&table, var4);

    // Rollback to outer checkpoint
    table.restore(outer_checkpoint);

    // All variables should be gone
    assert_variable_not_registered(&table, var1);
    assert_variable_not_registered(&table, var2);
    assert_variable_not_registered(&table, var3);
    assert_variable_not_registered(&table, var4);
}

#[test]
fn multiple_nested_checkpoints_complex() {
    let mut table = TypeTable::default();

    // Level 0 checkpoint
    let checkpoint_0 = table.start_checkpoint();

    let var1 = create_inference_variable();
    table.register(var1, constraint::Type::All(true));

    // Level 1 checkpoint
    let checkpoint_1 = table.start_checkpoint();

    let var2 = create_inference_variable();
    table.register(var2, constraint::Type::Number);
    table.assign_constraint(var1, constraint::Type::Number).unwrap();

    // Level 2 checkpoint
    let checkpoint_2 = table.start_checkpoint();

    let var3 = create_inference_variable();
    table.register(var3, constraint::Type::Integer);

    // Unify var1 and var2
    let var1_id = match table.get_inference(var1).unwrap() {
        Inference::Inferring(id) => *id,
        Inference::Known(_) => panic!("Expected inferring state"),
    };
    let var2_id = match table.get_inference(var2).unwrap() {
        Inference::Inferring(id) => *id,
        Inference::Known(_) => panic!("Expected inferring state"),
    };

    table.unify_infers(var1_id, var2_id).unwrap();

    // Level 3 checkpoint
    let checkpoint_3 = table.start_checkpoint();

    // Assign known type
    let int32_type = create_int32_type();
    table.assign_known(var1_id, int32_type.clone()).unwrap();

    // Verify all are known
    assert_variable_has_known_type(&table, var1, &int32_type);
    assert_variable_has_known_type(&table, var2, &int32_type);
    assert_variable_has_constraint(&table, var3, constraint::Type::Integer);

    // Rollback to level 3 -> undo known type assignment
    table.restore(checkpoint_3);

    assert_variable_has_constraint(&table, var1, constraint::Type::Number);
    assert_variable_has_constraint(&table, var2, constraint::Type::Number);
    assert_variable_has_constraint(&table, var3, constraint::Type::Integer);

    // Rollback to level 2 -> undo unification
    table.restore(checkpoint_2);

    assert_variable_has_constraint(&table, var1, constraint::Type::Number);
    assert_variable_has_constraint(&table, var2, constraint::Type::Number);
    assert_variable_not_registered(&table, var3);

    // Rollback to level 1 -> undo var2 registration and var1 constraint
    table.restore(checkpoint_1);

    assert_variable_has_constraint(&table, var1, constraint::Type::All(true));
    assert_variable_not_registered(&table, var2);
    assert_variable_not_registered(&table, var3);

    // Rollback to level 0 -> undo var1 registration
    table.restore(checkpoint_0);

    assert_variable_not_registered(&table, var1);
    assert_variable_not_registered(&table, var2);
    assert_variable_not_registered(&table, var3);
}

#[test]
fn checkpoint_with_failed_operations() {
    let mut table = TypeTable::default();

    // Register variables with incompatible constraints
    let var1 = create_inference_variable();
    let var2 = create_inference_variable();

    table.register(var1, constraint::Type::UnsignedInteger);
    table.register(var2, constraint::Type::Floating);

    let checkpoint = table.start_checkpoint();

    // These operations should succeed
    let var3 = create_inference_variable();
    table.register(var3, constraint::Type::Integer);

    // Try to unify incompatible constraints (should fail)
    let var1_id = match table.get_inference(var1).unwrap() {
        Inference::Inferring(id) => *id,
        Inference::Known(_) => panic!("Expected inferring state"),
    };
    let var2_id = match table.get_inference(var2).unwrap() {
        Inference::Inferring(id) => *id,
        Inference::Known(_) => panic!("Expected inferring state"),
    };

    assert!(table.unify_infers(var1_id, var2_id).is_err());

    // Failed operation shouldn't affect state
    assert_variable_has_constraint(
        &table,
        var1,
        constraint::Type::UnsignedInteger,
    );
    assert_variable_has_constraint(&table, var2, constraint::Type::Floating);
    assert_variable_has_constraint(&table, var3, constraint::Type::Integer);

    // Rollback should still work correctly
    table.restore(checkpoint);

    assert_variable_has_constraint(
        &table,
        var1,
        constraint::Type::UnsignedInteger,
    );
    assert_variable_has_constraint(&table, var2, constraint::Type::Floating);
    assert_variable_not_registered(&table, var3);
}

#[test]
fn checkpoint_isolation_between_tables() {
    let mut table1 = TypeTable::default();
    let mut table2 = TypeTable::default();

    // Create checkpoints on both tables
    let checkpoint1 = table1.start_checkpoint();
    let checkpoint2 = table2.start_checkpoint();

    // Add variables to both tables
    let var1 = create_inference_variable();
    let var2 = create_inference_variable();

    table1.register(var1, constraint::Type::Number);
    table2.register(var2, constraint::Type::Integer);

    // Verify both tables have their variables
    assert_variable_has_constraint(&table1, var1, constraint::Type::Number);
    assert_variable_not_registered(&table1, var2);

    assert_variable_has_constraint(&table2, var2, constraint::Type::Integer);
    assert_variable_not_registered(&table2, var1);

    // Rollback table1
    table1.restore(checkpoint1);

    // Only table1 should be affected
    assert_variable_not_registered(&table1, var1);
    assert_variable_not_registered(&table1, var2);

    assert_variable_has_constraint(&table2, var2, constraint::Type::Integer);
    assert_variable_not_registered(&table2, var1);

    // Rollback table2
    table2.restore(checkpoint2);

    // Now table2 should also be empty
    assert_variable_not_registered(&table2, var1);
    assert_variable_not_registered(&table2, var2);
}

#[test]
fn checkpoint_with_complex_unification_chains() {
    let mut table = TypeTable::default();

    // Create a chain of variables to unify
    let vars: Vec<_> = (0..5).map(|_| create_inference_variable()).collect();

    // Register with progressively more restrictive constraints
    table.register(vars[0], constraint::Type::All(true));
    table.register(vars[1], constraint::Type::Number);
    table.register(vars[2], constraint::Type::Integer);
    table.register(vars[3], constraint::Type::SignedInteger);
    table.register(vars[4], constraint::Type::UnsignedInteger);

    let checkpoint = table.start_checkpoint();

    // Get constraint IDs
    let var_ids: Vec<_> = vars
        .iter()
        .map(|&var| match table.get_inference(var).unwrap() {
            Inference::Inferring(id) => *id,
            Inference::Known(_) => panic!("Expected inferring state"),
        })
        .collect();

    // Unify first four variables (avoiding the incompatible UnsignedInteger)
    for i in 1..4 {
        table.unify_infers(var_ids[0], var_ids[i]).unwrap();
    }

    // All unified variables should have SignedInteger constraint
    for &var in &vars[0..4] {
        assert_variable_has_constraint(
            &table,
            var,
            constraint::Type::SignedInteger,
        );
    }
    assert_variable_has_constraint(
        &table,
        vars[4],
        constraint::Type::UnsignedInteger,
    );

    // Assign known type to the unified group
    let int32_type = create_int32_type();
    table.assign_known(var_ids[0], int32_type.clone()).unwrap();

    // All unified variables should be known
    for &var in &vars[0..4] {
        assert_variable_has_known_type(&table, var, &int32_type);
    }
    assert_variable_has_constraint(
        &table,
        vars[4],
        constraint::Type::UnsignedInteger,
    );

    // Rollback to checkpoint
    table.restore(checkpoint);

    // Should be back to original constraints
    assert_variable_has_constraint(
        &table,
        vars[0],
        constraint::Type::All(true),
    );
    assert_variable_has_constraint(&table, vars[1], constraint::Type::Number);
    assert_variable_has_constraint(&table, vars[2], constraint::Type::Integer);
    assert_variable_has_constraint(
        &table,
        vars[3],
        constraint::Type::SignedInteger,
    );
    assert_variable_has_constraint(
        &table,
        vars[4],
        constraint::Type::UnsignedInteger,
    );
}
