# Semantic Analysis

In this phase, the compiler will perform semantic analysis on the AST. It will check 
for errors in the code that the parser could not detect, as they are not syntax 
errors. Semantic errors include type mismatches, undefined variables, incorrect 
number of arguments, etc.

## Overview

Semantic analysis is quite a complex task, usually done in several passes. In this 
compiler, there are three passes:
 
### **Symbol table construction**
This pass will populate the symbol table with all the declarations in the program. 
It will also check for redeclarations and other errors related to the symbol table. 
In this pass, the compiler checks only for the existence of the symbols, not their 
definition. For example, in this pass, the compiler will not go into the function 
body and analyze the list of statements.

### **Binding** 
This pass will go into the definition of each symbol in the table and attach semantic 
information to them. For example, binding the type information to each expression, 
like `number1 == number2`, will produce a `bool` expression type. Now that the 
expression AST has type information attached to it, the compiler can now perform 
type-checking on them.

### **Control Flow Analysis**: 
This pass will break down all the control flow statements inside the function into a 
**Control Flow Graph**. It lowers the control flow statements, such as if-else and 
while-loop, down into blocks of statements with jump instructions, which is 
beneficial for code generation. It can also detect dead codes and perform some 
optimization on them.