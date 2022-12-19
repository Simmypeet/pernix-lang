use core::panic;

use pernix_parser::Parser;
use pernix_project::source_code::SourceCode;

use crate::{
    binding::{
        binder::Binder, bound_expression::BoundExpression,
        bound_statement::BoundStatement,
    },
    control_flow_graph::Terminator,
    symbol::table::{FunctionSymbolTable, TypeSymbolTable},
};

use super::{ControlFlowGraph, Instruction};

#[test]
fn basic_control_flow_graph_test() {
    let source_code = " 
    int32 ReturnOne() {
        let numberOne = 1;
        return numberOne;
    }
    ";
    let source_code =
        SourceCode::new(source_code.to_string(), "test.pnx".to_string());
    let mut parser = Parser::new(&source_code);
    let ast = parser.parse_file().unwrap();

    let type_table = TypeSymbolTable::new();
    let mut function_table = FunctionSymbolTable::new();
    let _ = function_table.populate(&ast, &type_table);

    let bound_function = Binder::bind(
        &function_table.get("ReturnOne").unwrap().value,
        &function_table,
        &type_table,
    )
    .ok()
    .unwrap();

    let cfg = ControlFlowGraph::analyze(bound_function);
    match &cfg.get_entry_block().instructions[0] {
        Instruction::Statement(statement) => {
            assert!(matches!(
                statement,
                BoundStatement::BoundVariableDeclarationStatement(_)
            ));
        }
        Instruction::Terminator(_) => panic!("Expected statement"),
    }
    match &cfg.get_entry_block().instructions[1] {
        Instruction::Statement(_) => panic!("Expected terminator"),
        Instruction::Terminator(terminator) => {
            assert!(matches!(
                terminator,
                super::Terminator::ReturnStatement(_)
            ));
        }
    }

    assert!(cfg.get_entry_block().instructions.len() == 2);
}

#[test]
fn if_without_else_branch() {
    let source_code = " 
    int32 Max(int32 a, int32 b) {
        if (a > b) {
            return a;
        }
        let a = 1;
        return b;
    }
    ";
    let source_code =
        SourceCode::new(source_code.to_string(), "test.pnx".to_string());
    let mut parser = Parser::new(&source_code);
    let ast = parser.parse_file().unwrap();

    let type_table = TypeSymbolTable::new();
    let mut function_table = FunctionSymbolTable::new();
    let _ = function_table.populate(&ast, &type_table);

    let bound_function = Binder::bind(
        &function_table.get("Max").unwrap().value,
        &function_table,
        &type_table,
    )
    .ok()
    .unwrap();

    let cfg = ControlFlowGraph::analyze(bound_function);

    match &cfg.get_entry_block().instructions[0] {
        Instruction::Terminator(terminator) => match terminator {
            super::Terminator::ConditionalJump {
                expression,
                true_block,
                false_block,
            } => {
                assert!(matches!(
                    expression,
                    BoundExpression::BoundBinaryExpression(_)
                ));
                {
                    let true_block = cfg.get_block(*true_block);

                    match &true_block.instructions[0] {
                        Instruction::Statement(_) => {
                            panic!("Expected terminator")
                        }
                        Instruction::Terminator(terminator) => {
                            assert!(matches!(
                                terminator,
                                super::Terminator::ReturnStatement(_)
                            ));
                        }
                    }

                    assert!(true_block.instructions.len() == 1);
                }
                {
                    let false_block = cfg.get_block(*false_block);

                    match &false_block.instructions[0] {
                        Instruction::Statement(statement) => {
                            assert!(matches!(
                                statement,
                                BoundStatement::BoundVariableDeclarationStatement(_)
                            ));
                        }
                        Instruction::Terminator(_) => {
                            panic!("Expected statement")
                        }
                    }

                    match &false_block.instructions[1] {
                        Instruction::Statement(_) => {
                            panic!("Expected terminator")
                        }
                        Instruction::Terminator(terminator) => {
                            assert!(matches!(
                                terminator,
                                super::Terminator::ReturnStatement(_)
                            ));
                        }
                    }

                    assert!(false_block.instructions.len() == 2);
                }
            }
            _ => panic!("Expected conditional jump"),
        },
        _ => panic!("Expected terminator"),
    }

    assert!(cfg.get_entry_block().instructions.len() == 1);
}

#[test]
fn if_with_else_branch() {
    /*
     * entry:
     *     let return_result = 0;
     *     cond_jump a > b -> true_block, false_block;
     *
     * true_block:
     *     return_result = a;
     *     jump end_block;
     *
     * false_block:
     *     return_result = b;
     *     jump end_block;
     *
     * end_block:
     *     return return_result;
     */

    let source_code = " 
    int32 Max(int32 a, int32 b) {
        mutable let return_result = 0;
        if (a > b) {
            return_result = a;
        } else {
            return_result = b;
        }
        return return_result;
    }
    ";
    let source_code =
        SourceCode::new(source_code.to_string(), "test.pnx".to_string());
    let mut parser = Parser::new(&source_code);
    let ast = parser.parse_file().unwrap();

    let type_table = TypeSymbolTable::new();
    let mut function_table = FunctionSymbolTable::new();
    let _ = function_table.populate(&ast, &type_table);

    let bound_function = Binder::bind(
        &function_table.get("Max").unwrap().value,
        &function_table,
        &type_table,
    )
    .ok()
    .unwrap();

    let cfg = ControlFlowGraph::analyze(bound_function);

    let mut end_block_index;

    match &cfg.get_entry_block().instructions[0] {
        Instruction::Statement(statement) => {
            assert!(matches!(
                statement,
                BoundStatement::BoundVariableDeclarationStatement(_)
            ));
        }
        Instruction::Terminator(_) => panic!("Expected statement"),
    }

    match &cfg.get_entry_block().instructions[1] {
        Instruction::Statement(_) => panic!("Expected terminator"),
        Instruction::Terminator(terminator) => match terminator {
            super::Terminator::ConditionalJump {
                expression,
                true_block,
                false_block,
            } => {
                assert!(matches!(
                    expression,
                    BoundExpression::BoundBinaryExpression(_)
                ));
                {
                    let true_block = cfg.get_block(*true_block);

                    match &true_block.instructions[0] {
                        Instruction::Statement(statement) => {
                            assert!(matches!(
                                statement,
                                BoundStatement::BoundExpressionStatement(_)
                            ));
                        }
                        Instruction::Terminator(_) => {
                            panic!("Expected statement")
                        }
                    }

                    match &true_block.instructions[1] {
                        Instruction::Statement(_) => {
                            panic!("Expected terminator")
                        }
                        Instruction::Terminator(terminator) => {
                            assert!(matches!(
                                terminator,
                                super::Terminator::Jump(_)
                            ));
                            end_block_index = match terminator {
                                super::Terminator::Jump(block) => *block,
                                _ => panic!("Expected jump"),
                            };
                        }
                    }

                    assert!(true_block.instructions.len() == 2);
                }
                {
                    let false_block = cfg.get_block(*false_block);

                    match &false_block.instructions[0] {
                        Instruction::Statement(statement) => {
                            assert!(matches!(
                                statement,
                                BoundStatement::BoundExpressionStatement(_)
                            ));
                        }
                        Instruction::Terminator(_) => {
                            panic!("Expected statement")
                        }
                    }

                    match &false_block.instructions[1] {
                        Instruction::Statement(_) => {
                            panic!("Expected terminator")
                        }
                        Instruction::Terminator(terminator) => {
                            assert!(matches!(
                                terminator,
                                super::Terminator::Jump(_)
                            ));
                            end_block_index = match terminator {
                                super::Terminator::Jump(block) => {
                                    assert_eq!(end_block_index, *block, "Expected end_block_index to be equal to the jump block");
                                    *block
                                }
                                _ => panic!("Expected jump"),
                            };
                        }
                    }

                    assert!(false_block.instructions.len() == 2);
                }

                let end_block = cfg.get_block(end_block_index);

                match &end_block.instructions[0] {
                    Instruction::Terminator(Terminator::ReturnStatement(_)) => {
                    }
                    _ => panic!("Expected return terminator"),
                }

                assert!(end_block.instructions.len() == 1);
            }
            _ => panic!("Expected conditional jump"),
        },
    }

    assert!(cfg.get_entry_block().instructions.len() == 2);
}

#[test]
fn nested_if_flow_graph() {
    /*
     * entry:
     *     cond_jump x == 0 -> true_block, false_block;
     *
     * true_block:
     *     cond_jump x == 1 -> true_block1, false_block1;
     *
     * true_block1:
     *     let a = 1;
     *     jump false_block1;
     *
     * false_block1:
     *     let b = 2;
     *     jump end_block;
     *
     * false_block:
     *     let c = 3;
     *     jump end_block;
     *
     * end_block:
     *     let d = 4;
     */

    let source_code = " 
    void Complex(int32 x) 
    {
        if (x == 0) {
            if (x == 1) {
                let a = 1;
            }
            let b = 2;
        } else {
            let c = 3;
        }
        let d = 4;
    }
    ";

    // PLEASE, TO GOD OF COMPILERS AND PROGRAMMING LANGUAGES, MAKE THIS TEST PASS
    // I BEG OF YOU

    let source_code =
        SourceCode::new(source_code.to_string(), "test.pnx".to_string());
    let mut parser = Parser::new(&source_code);
    let ast = parser.parse_file().unwrap();

    let type_table = TypeSymbolTable::new();
    let mut function_table = FunctionSymbolTable::new();
    let _ = function_table.populate(&ast, &type_table);

    let bound_function = Binder::bind(
        &function_table.get("Complex").unwrap().value,
        &function_table,
        &type_table,
    )
    .ok()
    .unwrap();

    let cfg = ControlFlowGraph::analyze(bound_function);

    let end_block_index;
    let true_block_index;
    let false_block_index;
    let true_block1_index;
    let false_block1_index;

    /*
     * entry:
     *    cond_jump x == 0 -> true_block, false_block;
     */

    match &cfg.get_entry_block().instructions[0] {
        Instruction::Terminator(Terminator::ConditionalJump {
            expression,
            true_block,
            false_block,
        }) => {
            assert!(matches!(
                expression,
                BoundExpression::BoundBinaryExpression(_)
            ));
            true_block_index = *true_block;
            false_block_index = *false_block;
        }
        _ => panic!("Expected conditional jump"),
    }

    assert!(cfg.get_entry_block().instructions.len() == 1);

    /*
     * true_block:
     *     cond_jump x == 1 -> true_block1, false_block1;
     */

    {
        let true_block = cfg.get_block(true_block_index);

        match &true_block.instructions[0] {
            Instruction::Terminator(Terminator::ConditionalJump {
                expression,
                true_block,
                false_block,
            }) => {
                assert!(matches!(
                    expression,
                    BoundExpression::BoundBinaryExpression(_)
                ));
                true_block1_index = *true_block;
                false_block1_index = *false_block;
            }
            _ => panic!("Expected conditional jump"),
        }

        assert!(true_block.instructions.len() == 1);
    }

    /*
     * true_block1:
     *     let a = 1;
     *     jump false_block1;
     */

    {
        let true_block1 = cfg.get_block(true_block1_index);

        match &true_block1.instructions[0] {
            Instruction::Statement(statement) => {
                assert!(matches!(
                    statement,
                    BoundStatement::BoundVariableDeclarationStatement(_)
                ));
            }
            Instruction::Terminator(_) => {
                panic!("Expected statement")
            }
        }

        match &true_block1.instructions[1] {
            Instruction::Statement(_) => {
                panic!("Expected terminator")
            }
            Instruction::Terminator(terminator) => {
                match terminator {
                    Terminator::Jump(block) => {
                        assert_eq!(
                            false_block1_index, *block,
                            "Must point to the same block"
                        );
                    }
                    _ => panic!("Expected jump"),
                };
            }
        }

        assert!(true_block1.instructions.len() == 2);
    }

    /*
     * false_block1:
     *    let b = 2;
     *    jump end_block;
     */
    {
        let false_block1 = cfg.get_block(false_block1_index);

        match &false_block1.instructions[0] {
            Instruction::Statement(statement) => {
                assert!(matches!(
                    statement,
                    BoundStatement::BoundVariableDeclarationStatement(_)
                ));
            }
            Instruction::Terminator(_) => {
                panic!("Expected statement")
            }
        }

        match &false_block1.instructions[1] {
            Instruction::Statement(_) => {
                panic!("Expected terminator")
            }
            Instruction::Terminator(terminator) => {
                end_block_index = match terminator {
                    Terminator::Jump(block) => *block,
                    _ => panic!("Expected jump"),
                };
            }
        }

        assert!(false_block1.instructions.len() == 2);
    }

    /*
     * false_block:
     *    let c = 3;
     *    jump end_block;
     */
    {
        let false_block = cfg.get_block(false_block_index);

        match &false_block.instructions[0] {
            Instruction::Statement(statement) => {
                assert!(matches!(
                    statement,
                    BoundStatement::BoundVariableDeclarationStatement(_)
                ));
            }
            Instruction::Terminator(_) => {
                panic!("Expected statement")
            }
        }

        match &false_block.instructions[1] {
            Instruction::Statement(_) => {
                panic!("Expected terminator")
            }
            Instruction::Terminator(terminator) => {
                match terminator {
                    Terminator::Jump(block) => {
                        assert_eq!(
                            *block, end_block_index,
                            "Must point to the same block"
                        );
                    }
                    _ => panic!("Expected jump"),
                };
            }
        }

        assert!(false_block.instructions.len() == 2);
    }

    /*
     * end_block:
     *     let d = 4;
     */
    {
        let end_block = cfg.get_block(end_block_index);

        match &end_block.instructions[0] {
            Instruction::Statement(statement) => {
                assert!(matches!(
                    statement,
                    BoundStatement::BoundVariableDeclarationStatement(_)
                ));
            }
            Instruction::Terminator(_) => {
                panic!("Expected statement")
            }
        }

        assert!(end_block.instructions.len() == 1);
    }
}
