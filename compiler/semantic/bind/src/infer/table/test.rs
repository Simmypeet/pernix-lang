use std::sync::atomic::{AtomicU64, Ordering};

use pernixc_term::{
    inference,
    r#type::{Primitive, Type},
};

use super::{Inference, Table, View};
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

#[test]
fn unified_variables_propagate_concrete_type() {
    let mut table = TypeTable::default();

    // Create three inference variables
    let var1 = create_inference_variable();
    let var2 = create_inference_variable();
    let var3 = create_inference_variable();

    // Register all with "All" constraint (can be any type)
    table.register(var1, constraint::Type::All(true));
    table.register(var2, constraint::Type::All(true));
    table.register(var3, constraint::Type::All(true));

    // Get their constraint IDs
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

    // Unify var1 with var2
    table.unify_infers(var1_id, var2_id).unwrap();

    // Unify var2 with var3 (transitively unifying all three)
    table.unify_infers(var1_id, var3_id).unwrap();

    // Assign concrete type to one of them
    let int32_type = create_int32_type();
    table.assign_known(var1_id, int32_type.clone()).unwrap();

    // All variables should now be inferred to the same concrete type
    assert_eq!(table.get_view(var1).unwrap(), View::Known(&int32_type));
    assert_eq!(table.get_view(var2).unwrap(), View::Known(&int32_type));
    assert_eq!(table.get_view(var3).unwrap(), View::Known(&int32_type));
}

#[test]
fn constraint_assignment_limits_domain() {
    let mut table = TypeTable::default();

    let var = create_inference_variable();

    // Start with "All" constraint (can be any type)
    table.register(var, constraint::Type::All(true));

    // Assign a more restrictive "Number" constraint
    table.assign_constraint(var, constraint::Type::Number).unwrap();

    // Should now have Number constraint
    match table.get_view(var).unwrap() {
        View::Constraint(constraint) => {
            assert_eq!(*constraint, constraint::Type::Number);
        }
        View::Known(_) => panic!("Expected constraint view"),
    }

    // Further restrict to "Integer"
    table.assign_constraint(var, constraint::Type::Integer).unwrap();

    // Should now have Integer constraint
    match table.get_view(var).unwrap() {
        View::Constraint(constraint) => {
            assert_eq!(*constraint, constraint::Type::Integer);
        }
        View::Known(_) => panic!("Expected constraint view"),
    }

    // Further restrict to "SignedInteger"
    table.assign_constraint(var, constraint::Type::SignedInteger).unwrap();

    // Should now have SignedInteger constraint
    match table.get_view(var).unwrap() {
        View::Constraint(constraint) => {
            assert_eq!(*constraint, constraint::Type::SignedInteger);
        }
        View::Known(_) => panic!("Expected constraint view"),
    }
}

#[test]
fn known_type_satisfies_constraint() {
    let mut table = TypeTable::default();

    let var = create_inference_variable();

    // Register with Integer constraint
    table.register(var, constraint::Type::Integer);

    let var_id = match table.get_inference(var).unwrap() {
        Inference::Inferring(id) => *id,
        Inference::Known(_) => panic!("Expected inferring state"),
    };

    // Assign int32 type - should succeed as int32 satisfies Integer constraint
    let int32_type = create_int32_type();
    table.assign_known(var_id, int32_type.clone()).unwrap();

    // Variable should now be known
    assert_eq!(table.get_view(var).unwrap(), View::Known(&int32_type));
}

#[test]
fn known_type_violates_constraint() {
    let mut table = TypeTable::default();

    let var = create_inference_variable();

    // Register with UnsignedInteger constraint
    table.register(var, constraint::Type::UnsignedInteger);

    let var_id = match table.get_inference(var).unwrap() {
        Inference::Inferring(id) => *id,
        Inference::Known(_) => panic!("Expected inferring state"),
    };

    // Try to assign int32 type - should fail as int32 doesn't satisfy
    // UnsignedInteger constraint
    let int32_type = create_int32_type();
    let result = table.assign_known(var_id, int32_type);

    assert!(result.is_err());
}

#[test]
fn assign_constraint_to_known_type_succeeds() {
    let mut table = TypeTable::default();

    let var = create_inference_variable();

    // Register with All constraint
    table.register(var, constraint::Type::All(true));

    let var_id = match table.get_inference(var).unwrap() {
        Inference::Inferring(id) => *id,
        Inference::Known(_) => panic!("Expected inferring state"),
    };

    // Assign concrete type
    let int32_type = create_int32_type();
    table.assign_known(var_id, int32_type.clone()).unwrap();

    // Try to assign constraint that the known type satisfies
    table.assign_constraint(var, constraint::Type::Integer).unwrap();
    table.assign_constraint(var, constraint::Type::Number).unwrap();
    table.assign_constraint(var, constraint::Type::SignedInteger).unwrap();

    // Should still be known with the same type
    assert_eq!(table.get_view(var).unwrap(), View::Known(&int32_type));
}

#[test]
fn assign_constraint_to_known_type_fails() {
    let mut table = TypeTable::default();

    let var = create_inference_variable();

    // Register with All constraint
    table.register(var, constraint::Type::All(true));

    let var_id = match table.get_inference(var).unwrap() {
        Inference::Inferring(id) => *id,
        Inference::Known(_) => panic!("Expected inferring state"),
    };

    // Assign concrete int32 type
    let int32_type = create_int32_type();
    table.assign_known(var_id, int32_type).unwrap();

    // Try to assign constraint that the known type doesn't satisfy
    let result =
        table.assign_constraint(var, constraint::Type::UnsignedInteger);

    assert!(result.is_err());
}

#[test]
fn unify_compatible_constraints() {
    let mut table = TypeTable::default();

    let var1 = create_inference_variable();
    let var2 = create_inference_variable();

    // Register with compatible constraints
    table.register(var1, constraint::Type::Number);
    table.register(var2, constraint::Type::Integer);

    let var1_id = match table.get_inference(var1).unwrap() {
        Inference::Inferring(id) => *id,
        Inference::Known(_) => panic!("Expected inferring state"),
    };
    let var2_id = match table.get_inference(var2).unwrap() {
        Inference::Inferring(id) => *id,
        Inference::Known(_) => panic!("Expected inferring state"),
    };

    // Unify them - should succeed and result in more restrictive constraint
    table.unify_infers(var1_id, var2_id).unwrap();

    // Both variables should now have the combined constraint (Integer)
    match table.get_view(var1).unwrap() {
        View::Constraint(constraint) => {
            assert_eq!(*constraint, constraint::Type::Integer);
        }
        View::Known(_) => panic!("Expected constraint view"),
    }

    match table.get_view(var2).unwrap() {
        View::Constraint(constraint) => {
            assert_eq!(*constraint, constraint::Type::Integer);
        }
        View::Known(_) => panic!("Expected constraint view"),
    }
}

#[test]
fn unify_incompatible_constraints() {
    let mut table = TypeTable::default();

    let var1 = create_inference_variable();
    let var2 = create_inference_variable();

    // Register with incompatible constraints
    table.register(var1, constraint::Type::UnsignedInteger);
    table.register(var2, constraint::Type::Floating);

    let var1_id = match table.get_inference(var1).unwrap() {
        Inference::Inferring(id) => *id,
        Inference::Known(_) => panic!("Expected inferring state"),
    };
    let var2_id = match table.get_inference(var2).unwrap() {
        Inference::Inferring(id) => *id,
        Inference::Known(_) => panic!("Expected inferring state"),
    };

    // Try to unify them - should fail as constraints cannot be combined
    let result = table.unify_infers(var1_id, var2_id);

    assert!(result.is_err());
}

#[test]
fn complex_constraint_combination_hierarchy() {
    let mut table = TypeTable::default();

    let var1 = create_inference_variable();
    let var2 = create_inference_variable();
    let var3 = create_inference_variable();

    // Set up a hierarchy: All -> Number -> Signed -> SignedInteger
    table.register(var1, constraint::Type::All(true));
    table.register(var2, constraint::Type::Number);
    table.register(var3, constraint::Type::Signed);

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

    // Unify var1 (All) with var2 (Number) -> should result in Number
    table.unify_infers(var1_id, var2_id).unwrap();

    // Unify result with var3 (Signed) -> should result in Signed
    table.unify_infers(var1_id, var3_id).unwrap();

    // Apply SignedInteger constraint to further restrict
    table.assign_constraint(var1, constraint::Type::SignedInteger).unwrap();

    // All variables should now have SignedInteger constraint
    match table.get_view(var1).unwrap() {
        View::Constraint(constraint) => {
            assert_eq!(*constraint, constraint::Type::SignedInteger);
        }
        View::Known(_) => panic!("Expected constraint view"),
    }

    match table.get_view(var2).unwrap() {
        View::Constraint(constraint) => {
            assert_eq!(*constraint, constraint::Type::SignedInteger);
        }
        View::Known(_) => panic!("Expected constraint view"),
    }

    match table.get_view(var3).unwrap() {
        View::Constraint(constraint) => {
            assert_eq!(*constraint, constraint::Type::SignedInteger);
        }
        View::Known(_) => panic!("Expected constraint view"),
    }
}

#[test]
fn assign_incompatible_constraint_to_inferring() {
    let mut table = TypeTable::default();

    let var = create_inference_variable();

    // Register with UnsignedInteger constraint
    table.register(var, constraint::Type::UnsignedInteger);

    // Try to assign incompatible Floating constraint
    let result = table.assign_constraint(var, constraint::Type::Floating);

    assert!(result.is_err());
}

#[test]
fn unify_self_constraint() {
    let mut table = TypeTable::default();

    let var = create_inference_variable();

    table.register(var, constraint::Type::Number);

    let var_id = match table.get_inference(var).unwrap() {
        Inference::Inferring(id) => *id,
        Inference::Known(_) => panic!("Expected inferring state"),
    };

    // Unifying a constraint with itself should be a no-op
    table.unify_infers(var_id, var_id).unwrap();

    // Should still have the same constraint
    match table.get_view(var).unwrap() {
        View::Constraint(constraint) => {
            assert_eq!(*constraint, constraint::Type::Number);
        }
        View::Known(_) => panic!("Expected constraint view"),
    }
}

#[test]
fn multiple_variables_unified_then_constrained() {
    let mut table = TypeTable::default();

    // Create a set of variables
    let vars: Vec<_> = (0..5).map(|_| create_inference_variable()).collect();

    // Register all with All constraint
    for &var in &vars {
        table.register(var, constraint::Type::All(true));
    }

    // Get all constraint IDs
    let var_ids: Vec<_> = vars
        .iter()
        .map(|&var| match table.get_inference(var).unwrap() {
            Inference::Inferring(id) => *id,
            Inference::Known(_) => panic!("Expected inferring state"),
        })
        .collect();

    // Unify all variables together
    for i in 1..var_ids.len() {
        table.unify_infers(var_ids[0], var_ids[i]).unwrap();
    }

    // Apply progressively more restrictive constraints
    table.assign_constraint(vars[0], constraint::Type::Number).unwrap();
    table.assign_constraint(vars[1], constraint::Type::Integer).unwrap();
    table.assign_constraint(vars[2], constraint::Type::SignedInteger).unwrap();

    // All variables should have the most restrictive constraint
    for &var in &vars {
        match table.get_view(var).unwrap() {
            View::Constraint(constraint) => {
                assert_eq!(*constraint, constraint::Type::SignedInteger);
            }
            View::Known(_) => panic!("Expected constraint view"),
        }
    }

    // Finally assign a concrete type
    let int32_type = create_int32_type();
    table.assign_known(var_ids[0], int32_type.clone()).unwrap();

    // All variables should now be known to the same type
    for &var in &vars {
        assert_eq!(table.get_view(var).unwrap(), View::Known(&int32_type));
    }
}

#[test]
#[allow(clippy::too_many_lines, clippy::cognitive_complexity)]
fn unrelated_variables_stay_unchanged_during_unification() {
    let mut table = TypeTable::default();

    // Create variables that will be unified
    let unified_var1 = create_inference_variable();
    let unified_var2 = create_inference_variable();
    let unified_var3 = create_inference_variable();

    // Create unrelated variables that should remain unchanged
    let unrelated_var1 = create_inference_variable();
    let unrelated_var2 = create_inference_variable();
    let unrelated_var3 = create_inference_variable();

    // Register all variables with different constraints
    table.register(unified_var1, constraint::Type::All(true));
    table.register(unified_var2, constraint::Type::Number);
    table.register(unified_var3, constraint::Type::Integer);

    table.register(unrelated_var1, constraint::Type::UnsignedInteger);
    table.register(unrelated_var2, constraint::Type::Floating);
    table.register(unrelated_var3, constraint::Type::Signed);

    // Get constraint IDs for unified variables
    let unified_var1_id = match table.get_inference(unified_var1).unwrap() {
        Inference::Inferring(id) => *id,
        Inference::Known(_) => panic!("Expected inferring state"),
    };
    let unified_var2_id = match table.get_inference(unified_var2).unwrap() {
        Inference::Inferring(id) => *id,
        Inference::Known(_) => panic!("Expected inferring state"),
    };
    let unified_var3_id = match table.get_inference(unified_var3).unwrap() {
        Inference::Inferring(id) => *id,
        Inference::Known(_) => panic!("Expected inferring state"),
    };

    // Verify initial states of unrelated variables before any unification
    match table.get_view(unrelated_var1).unwrap() {
        View::Constraint(constraint) => {
            assert_eq!(*constraint, constraint::Type::UnsignedInteger);
        }
        View::Known(_) => panic!("Expected constraint view"),
    }
    match table.get_view(unrelated_var2).unwrap() {
        View::Constraint(constraint) => {
            assert_eq!(*constraint, constraint::Type::Floating);
        }
        View::Known(_) => panic!("Expected constraint view"),
    }
    match table.get_view(unrelated_var3).unwrap() {
        View::Constraint(constraint) => {
            assert_eq!(*constraint, constraint::Type::Signed);
        }
        View::Known(_) => panic!("Expected constraint view"),
    }

    // Unify the specific variables
    table.unify_infers(unified_var1_id, unified_var2_id).unwrap();
    table.unify_infers(unified_var1_id, unified_var3_id).unwrap();

    // Verify unified variables now have the most restrictive constraint
    // (Integer)
    match table.get_view(unified_var1).unwrap() {
        View::Constraint(constraint) => {
            assert_eq!(*constraint, constraint::Type::Integer);
        }
        View::Known(_) => panic!("Expected constraint view"),
    }
    match table.get_view(unified_var2).unwrap() {
        View::Constraint(constraint) => {
            assert_eq!(*constraint, constraint::Type::Integer);
        }
        View::Known(_) => panic!("Expected constraint view"),
    }
    match table.get_view(unified_var3).unwrap() {
        View::Constraint(constraint) => {
            assert_eq!(*constraint, constraint::Type::Integer);
        }
        View::Known(_) => panic!("Expected constraint view"),
    }

    // Verify unrelated variables remain completely unchanged
    match table.get_view(unrelated_var1).unwrap() {
        View::Constraint(constraint) => {
            assert_eq!(*constraint, constraint::Type::UnsignedInteger);
        }
        View::Known(_) => panic!("Expected constraint view"),
    }
    match table.get_view(unrelated_var2).unwrap() {
        View::Constraint(constraint) => {
            assert_eq!(*constraint, constraint::Type::Floating);
        }
        View::Known(_) => panic!("Expected constraint view"),
    }
    match table.get_view(unrelated_var3).unwrap() {
        View::Constraint(constraint) => {
            assert_eq!(*constraint, constraint::Type::Signed);
        }
        View::Known(_) => panic!("Expected constraint view"),
    }

    // Further verify by assigning a concrete type to unified variables
    let int32_type = create_int32_type();
    table.assign_known(unified_var1_id, int32_type.clone()).unwrap();

    // All unified variables should be known
    assert_eq!(table.get_view(unified_var1).unwrap(), View::Known(&int32_type));
    assert_eq!(table.get_view(unified_var2).unwrap(), View::Known(&int32_type));
    assert_eq!(table.get_view(unified_var3).unwrap(), View::Known(&int32_type));

    // Unrelated variables should still be unchanged
    match table.get_view(unrelated_var1).unwrap() {
        View::Constraint(constraint) => {
            assert_eq!(*constraint, constraint::Type::UnsignedInteger);
        }
        View::Known(_) => panic!("Expected constraint view"),
    }
    match table.get_view(unrelated_var2).unwrap() {
        View::Constraint(constraint) => {
            assert_eq!(*constraint, constraint::Type::Floating);
        }
        View::Known(_) => panic!("Expected constraint view"),
    }
    match table.get_view(unrelated_var3).unwrap() {
        View::Constraint(constraint) => {
            assert_eq!(*constraint, constraint::Type::Signed);
        }
        View::Known(_) => panic!("Expected constraint view"),
    }
}

#[test]
#[allow(clippy::too_many_lines, clippy::cognitive_complexity)]
fn partial_unification_preserves_other_variables() {
    let mut table = TypeTable::default();

    // Create multiple groups of variables
    let group1_var1 = create_inference_variable();
    let group1_var2 = create_inference_variable();

    let group2_var1 = create_inference_variable();
    let group2_var2 = create_inference_variable();

    let independent_var = create_inference_variable();

    // Register with different constraints
    table.register(group1_var1, constraint::Type::Number);
    table.register(group1_var2, constraint::Type::Integer);

    table.register(group2_var1, constraint::Type::Floating);
    table.register(group2_var2, constraint::Type::Floating);

    table.register(independent_var, constraint::Type::UnsignedInteger);

    // Get constraint IDs
    let group1_var1_id = match table.get_inference(group1_var1).unwrap() {
        Inference::Inferring(id) => *id,
        Inference::Known(_) => panic!("Expected inferring state"),
    };
    let group1_var2_id = match table.get_inference(group1_var2).unwrap() {
        Inference::Inferring(id) => *id,
        Inference::Known(_) => panic!("Expected inferring state"),
    };
    let group2_var1_id = match table.get_inference(group2_var1).unwrap() {
        Inference::Inferring(id) => *id,
        Inference::Known(_) => panic!("Expected inferring state"),
    };
    let group2_var2_id = match table.get_inference(group2_var2).unwrap() {
        Inference::Inferring(id) => *id,
        Inference::Known(_) => panic!("Expected inferring state"),
    };

    // Verify initial states before any unification
    match table.get_view(group2_var1).unwrap() {
        View::Constraint(constraint) => {
            assert_eq!(*constraint, constraint::Type::Floating);
        }
        View::Known(_) => panic!("Expected constraint view"),
    }
    match table.get_view(group2_var2).unwrap() {
        View::Constraint(constraint) => {
            assert_eq!(*constraint, constraint::Type::Floating);
        }
        View::Known(_) => panic!("Expected constraint view"),
    }
    match table.get_view(independent_var).unwrap() {
        View::Constraint(constraint) => {
            assert_eq!(*constraint, constraint::Type::UnsignedInteger);
        }
        View::Known(_) => panic!("Expected constraint view"),
    }

    // Unify only group1 variables
    table.unify_infers(group1_var1_id, group1_var2_id).unwrap();

    // Group1 variables should be unified with Integer constraint
    match table.get_view(group1_var1).unwrap() {
        View::Constraint(constraint) => {
            assert_eq!(*constraint, constraint::Type::Integer);
        }
        View::Known(_) => panic!("Expected constraint view"),
    }
    match table.get_view(group1_var2).unwrap() {
        View::Constraint(constraint) => {
            assert_eq!(*constraint, constraint::Type::Integer);
        }
        View::Known(_) => panic!("Expected constraint view"),
    }

    // Group2 and independent variables should be unchanged
    match table.get_view(group2_var1).unwrap() {
        View::Constraint(constraint) => {
            assert_eq!(*constraint, constraint::Type::Floating);
        }
        View::Known(_) => panic!("Expected constraint view"),
    }
    match table.get_view(group2_var2).unwrap() {
        View::Constraint(constraint) => {
            assert_eq!(*constraint, constraint::Type::Floating);
        }
        View::Known(_) => panic!("Expected constraint view"),
    }
    match table.get_view(independent_var).unwrap() {
        View::Constraint(constraint) => {
            assert_eq!(*constraint, constraint::Type::UnsignedInteger);
        }
        View::Known(_) => panic!("Expected constraint view"),
    }

    // Now unify group2 variables
    table.unify_infers(group2_var1_id, group2_var2_id).unwrap();

    // Group2 variables should be unified with Floating constraint
    match table.get_view(group2_var1).unwrap() {
        View::Constraint(constraint) => {
            assert_eq!(*constraint, constraint::Type::Floating);
        }
        View::Known(_) => panic!("Expected constraint view"),
    }
    match table.get_view(group2_var2).unwrap() {
        View::Constraint(constraint) => {
            assert_eq!(*constraint, constraint::Type::Floating);
        }
        View::Known(_) => panic!("Expected constraint view"),
    }

    // Group1 should still have Integer constraint and independent should be
    // unchanged
    match table.get_view(group1_var1).unwrap() {
        View::Constraint(constraint) => {
            assert_eq!(*constraint, constraint::Type::Integer);
        }
        View::Known(_) => panic!("Expected constraint view"),
    }
    match table.get_view(group1_var2).unwrap() {
        View::Constraint(constraint) => {
            assert_eq!(*constraint, constraint::Type::Integer);
        }
        View::Known(_) => panic!("Expected constraint view"),
    }
    match table.get_view(independent_var).unwrap() {
        View::Constraint(constraint) => {
            assert_eq!(*constraint, constraint::Type::UnsignedInteger);
        }
        View::Known(_) => panic!("Expected constraint view"),
    }
}

#[test]
fn assignment_to_unified_group_preserves_unrelated_variables() {
    let mut table = TypeTable::default();

    // Create unified group and unrelated variables
    let unified_var1 = create_inference_variable();
    let unified_var2 = create_inference_variable();

    let unrelated_var1 = create_inference_variable();
    let unrelated_var2 = create_inference_variable();

    // Register all variables
    table.register(unified_var1, constraint::Type::All(true));
    table.register(unified_var2, constraint::Type::Number);
    table.register(unrelated_var1, constraint::Type::UnsignedInteger);
    table.register(unrelated_var2, constraint::Type::Floating);

    // Get constraint IDs for unified variables
    let unified_var1_id = match table.get_inference(unified_var1).unwrap() {
        Inference::Inferring(id) => *id,
        Inference::Known(_) => panic!("Expected inferring state"),
    };
    let unified_var2_id = match table.get_inference(unified_var2).unwrap() {
        Inference::Inferring(id) => *id,
        Inference::Known(_) => panic!("Expected inferring state"),
    };

    // Verify initial states of unrelated variables
    match table.get_view(unrelated_var1).unwrap() {
        View::Constraint(constraint) => {
            assert_eq!(*constraint, constraint::Type::UnsignedInteger);
        }
        View::Known(_) => panic!("Expected constraint view"),
    }
    match table.get_view(unrelated_var2).unwrap() {
        View::Constraint(constraint) => {
            assert_eq!(*constraint, constraint::Type::Floating);
        }
        View::Known(_) => panic!("Expected constraint view"),
    }

    // Unify the variables
    table.unify_infers(unified_var1_id, unified_var2_id).unwrap();

    // Assign concrete type to the unified group
    let int32_type = create_int32_type();
    table.assign_known(unified_var1_id, int32_type.clone()).unwrap();

    // Unified variables should be known
    assert_eq!(table.get_view(unified_var1).unwrap(), View::Known(&int32_type));
    assert_eq!(table.get_view(unified_var2).unwrap(), View::Known(&int32_type));

    // Unrelated variables should remain completely unchanged
    match table.get_view(unrelated_var1).unwrap() {
        View::Constraint(constraint) => {
            assert_eq!(*constraint, constraint::Type::UnsignedInteger);
        }
        View::Known(_) => panic!("Expected constraint view"),
    }
    match table.get_view(unrelated_var2).unwrap() {
        View::Constraint(constraint) => {
            assert_eq!(*constraint, constraint::Type::Floating);
        }
        View::Known(_) => panic!("Expected constraint view"),
    }

    // Further assign constraints to unrelated variables to verify they still
    // work independently
    table
        .assign_constraint(unrelated_var1, constraint::Type::UnsignedInteger)
        .unwrap();

    // This should still work and not affect the unified group
    match table.get_view(unrelated_var1).unwrap() {
        View::Constraint(constraint) => {
            assert_eq!(*constraint, constraint::Type::UnsignedInteger);
        }
        View::Known(_) => panic!("Expected constraint view"),
    }

    // Unified variables should still be known to the same type
    assert_eq!(table.get_view(unified_var1).unwrap(), View::Known(&int32_type));
    assert_eq!(table.get_view(unified_var2).unwrap(), View::Known(&int32_type));
}
