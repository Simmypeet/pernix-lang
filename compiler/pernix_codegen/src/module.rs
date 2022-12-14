use llvm_sys::{
    analysis::{LLVMVerifierFailureAction, LLVMVerifyFunction},
    core::{
        LLVMAppendBasicBlockInContext, LLVMBuildBr, LLVMBuildCall2,
        LLVMBuildCondBr, LLVMBuildLoad2, LLVMBuildRetVoid, LLVMDisposeBuilder,
        LLVMDoubleTypeInContext, LLVMFloatTypeInContext,
        LLVMInt16TypeInContext, LLVMInt1TypeInContext, LLVMInt32TypeInContext,
        LLVMInt64TypeInContext, LLVMInt8TypeInContext, LLVMVoidTypeInContext,
    },
    prelude::LLVMBasicBlockRef,
};
use llvm_sys::{
    core::{
        LLVMAddFunction, LLVMBuildAdd, LLVMBuildAlloca, LLVMBuildAnd,
        LLVMBuildFAdd, LLVMBuildFCmp, LLVMBuildFDiv, LLVMBuildFMul,
        LLVMBuildFPExt, LLVMBuildFPToSI, LLVMBuildFPToUI, LLVMBuildFPTrunc,
        LLVMBuildFRem, LLVMBuildFSub, LLVMBuildICmp, LLVMBuildIntCast2,
        LLVMBuildMul, LLVMBuildNUWAdd, LLVMBuildNUWMul, LLVMBuildNUWSub,
        LLVMBuildNeg, LLVMBuildNot, LLVMBuildOr, LLVMBuildRet, LLVMBuildSDiv,
        LLVMBuildSExt, LLVMBuildSIToFP, LLVMBuildSRem, LLVMBuildStore,
        LLVMBuildSub, LLVMBuildTrunc, LLVMBuildUDiv, LLVMBuildUIToFP,
        LLVMBuildURem, LLVMBuildZExt, LLVMConstInt, LLVMConstIntOfString,
        LLVMConstRealOfString, LLVMCreateBuilderInContext, LLVMDisposeModule,
        LLVMDumpModule, LLVMFunctionType, LLVMGetModuleContext, LLVMGetParam,
        LLVMModuleCreateWithNameInContext, LLVMPositionBuilderAtEnd,
        LLVMSetValueName2,
    },
    prelude::{
        LLVMBool, LLVMBuilderRef, LLVMContextRef, LLVMModuleRef, LLVMTypeRef,
        LLVMValueRef,
    },
    LLVMIntPredicate, LLVMRealPredicate,
};
use pernix_analyzer::{
    binding::{
        bound_declaration::BoundFunction,
        bound_expression::{
            BoundBinaryExpression, BoundCastExpression, BoundExpression,
            BoundFunctionCallExpression, BoundIdentifierExpression,
            BoundLiteralExpression, BoundUnaryExpression,
        },
        bound_statement::{BoundStatement, BoundVariableDeclarationStatement},
    },
    control_flow_graph::{Instruction, Terminator},
    symbol::{FunctionSymbol, PrimitiveType, TypeSymbol, VariableSymbol},
};
use pernix_lexer::token::LiteralConstantToken;
use pernix_parser::abstract_syntax_tree::{BinaryOperator, UnaryOperator};

use std::{
    collections::{hash_map::Entry, HashMap},
    ffi::CString,
    hash::{Hash, Hasher},
    marker::PhantomData,
    ptr,
    sync::Arc,
};

use crate::{context::Context, function::Function};

/// Represent a wrapper around an LLVM module. The module contains all the
/// functions and types that are used in the program and ready to be compiled.
pub struct Module<'ctx, 'table, 'ast> {
    llvm_module: LLVMModuleRef,
    type_map: HashMap<SymbolWrapper<'table, TypeSymbol>, LLVMTypeRef>,
    function_map: HashMap<
        SymbolWrapper<'table, FunctionSymbol<'table, 'ast>>,
        Function<'table, 'ast>,
    >,
    _phantom: PhantomData<&'ctx Context>,
}

struct SymbolWrapper<'table, T> {
    symbol: &'table T,
}

impl<'table, T> SymbolWrapper<'table, T> {
    fn wrap(symbol: &'table T) -> Self {
        Self { symbol }
    }
}

impl<T> PartialEq for SymbolWrapper<'_, T> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.symbol, other.symbol)
    }
}

impl<T> Eq for SymbolWrapper<'_, T> {}

impl<T> std::hash::Hash for SymbolWrapper<'_, T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::ptr::hash(self.symbol, state)
    }
}

impl<'ctx, 'table, 'ast> Drop for Module<'ctx, 'table, 'ast> {
    fn drop(&mut self) {
        // destroy llvm module
        unsafe { LLVMDisposeModule(self.llvm_module) }
    }
}

impl<'ctx: 'table, 'table: 'ast, 'ast> Module<'ctx, 'table, 'ast> {
    /// Create a new LLVM module.
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let cstring = CString::new(module_name).unwrap();

        Self {
            llvm_module: unsafe {
                LLVMModuleCreateWithNameInContext(
                    cstring.as_ptr() as *const i8,
                    context.get_context(),
                )
            },
            type_map: HashMap::new(),
            function_map: HashMap::new(),
            _phantom: PhantomData,
        }
    }

    /// Get the iterator of all the functions in the module.
    pub fn functions(&self) -> impl Iterator<Item = &Function<'table, 'ast>> {
        self.function_map.values()
    }

    /// Get the underlying LLVM module.
    pub unsafe fn get_llvm_module(&self) -> LLVMModuleRef {
        self.llvm_module
    }

    /// Print the LLVM IR dump of the module.
    pub fn print_ir(&self) {
        unsafe {
            LLVMDumpModule(self.llvm_module);
        }
    }

    /// Generate an LLVM type from the given function symbol and add it to the
    /// function map.
    pub fn add_function_symbol(
        &mut self,
        bound_func: BoundFunction<'table, 'ast>,
    ) -> bool {
        match self
            .function_map
            .entry(SymbolWrapper::wrap(bound_func.function_symbol()))
        {
            Entry::Occupied(_) => false,
            Entry::Vacant(_) => unsafe {
                let ret_type = match self
                    .get_type(bound_func.function_symbol().return_type())
                {
                    Some(val) => val,
                    None => return false,
                };

                let mut param_types = Vec::new();
                for param in bound_func.function_symbol().parameters() {
                    match self.get_type(param.variable_type) {
                        Some(val) => param_types.push(val),
                        None => return false,
                    }
                }

                let llvm_type = LLVMFunctionType(
                    ret_type,
                    param_types.as_mut_ptr(),
                    param_types.len() as u32,
                    0,
                );

                let cstring = CString::new(
                    bound_func.function_symbol().full_qualified_name(),
                )
                .unwrap();

                let func = LLVMAddFunction(
                    self.llvm_module,
                    cstring.as_ptr() as *const i8,
                    llvm_type,
                );

                let symbol = bound_func.function_symbol();

                self.function_map.insert(
                    SymbolWrapper::wrap(bound_func.function_symbol()),
                    Function::new(bound_func, llvm_type, func),
                );

                for (i, param) in symbol.parameters().iter().enumerate() {
                    LLVMSetValueName2(
                        LLVMGetParam(func, i as u32),
                        param.name.as_ptr() as *const i8,
                        param.name.len(),
                    );
                }

                true
            },
        }
    }

    /// Generate an LLVM type from a type symbol and add it to the type map.
    pub fn add_type_symbol(&mut self, symbol: &'table TypeSymbol) -> bool {
        match self.type_map.entry(SymbolWrapper::wrap(symbol)) {
            Entry::Occupied(_) => false,
            Entry::Vacant(entry) => {
                unsafe {
                    let llvm_type = match symbol {
                        TypeSymbol::PrimitiveType(PrimitiveType::Void) => {
                            LLVMVoidTypeInContext(LLVMGetModuleContext(
                                self.llvm_module,
                            ))
                        }
                        TypeSymbol::PrimitiveType(PrimitiveType::Int8)
                        | TypeSymbol::PrimitiveType(PrimitiveType::Uint8) => {
                            LLVMInt8TypeInContext(LLVMGetModuleContext(
                                self.llvm_module,
                            ))
                        }
                        TypeSymbol::PrimitiveType(PrimitiveType::Int16)
                        | TypeSymbol::PrimitiveType(PrimitiveType::Uint16) => {
                            LLVMInt16TypeInContext(LLVMGetModuleContext(
                                self.llvm_module,
                            ))
                        }
                        TypeSymbol::PrimitiveType(PrimitiveType::Int32)
                        | TypeSymbol::PrimitiveType(PrimitiveType::Uint32) => {
                            LLVMInt32TypeInContext(LLVMGetModuleContext(
                                self.llvm_module,
                            ))
                        }
                        TypeSymbol::PrimitiveType(PrimitiveType::Int64)
                        | TypeSymbol::PrimitiveType(PrimitiveType::Uint64) => {
                            LLVMInt64TypeInContext(LLVMGetModuleContext(
                                self.llvm_module,
                            ))
                        }
                        TypeSymbol::PrimitiveType(PrimitiveType::Bool) => {
                            LLVMInt1TypeInContext(LLVMGetModuleContext(
                                self.llvm_module,
                            ))
                        }
                        TypeSymbol::PrimitiveType(PrimitiveType::Float32) => {
                            LLVMFloatTypeInContext(LLVMGetModuleContext(
                                self.llvm_module,
                            ))
                        }
                        TypeSymbol::PrimitiveType(PrimitiveType::Float64) => {
                            LLVMDoubleTypeInContext(LLVMGetModuleContext(
                                self.llvm_module,
                            ))
                        }
                    };

                    entry.insert(llvm_type);
                }
                true
            }
        }
    }

    /// Get the LLVM function from the function map
    pub unsafe fn get_function(
        &self,
        symbol: &'table FunctionSymbol,
    ) -> Option<&Function<'table, 'ast>> {
        self.function_map.get(&SymbolWrapper::wrap(symbol))
    }

    /// Get the LLVM type from the type map
    pub unsafe fn get_type(
        &self,
        symbol: &'table TypeSymbol,
    ) -> Option<LLVMTypeRef> {
        match self.type_map.get(&SymbolWrapper::wrap(symbol)) {
            Some(val) => Some(*val),
            None => None,
        }
    }

    /// Get the LLVM context that created this module.
    pub unsafe fn get_context(&self) -> LLVMContextRef {
        unsafe { LLVMGetModuleContext(self.llvm_module) }
    }

    /// Compile the module.
    pub fn compile(&self) {
        for func in self.function_map.values() {
            CodeGenerator::generate(self, func);
        }
    }
}

struct VariableWrapper<'table, 'ast> {
    variable: Arc<VariableSymbol<'table, 'ast>>,
}

impl<'table, 'ast> VariableWrapper<'table, 'ast> {
    fn wrap(
        variable: Arc<VariableSymbol<'table, 'ast>>,
    ) -> VariableWrapper<'table, 'ast> {
        VariableWrapper { variable }
    }
}

impl PartialEq for VariableWrapper<'_, '_> {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.variable, &other.variable)
    }
}

impl Eq for VariableWrapper<'_, '_> {}

impl Hash for VariableWrapper<'_, '_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        ptr::hash(&*self.variable, state)
    }
}

struct CodeGenerator<'modu, 'ctx, 'table, 'ast> {
    module: &'modu Module<'ctx, 'table, 'ast>,
    builder: LLVMBuilderRef,
    variables: HashMap<VariableWrapper<'table, 'ast>, LLVMValueRef>,
    block_vec: Vec<LLVMBasicBlockRef>,
    parameters: HashMap<VariableWrapper<'table, 'ast>, LLVMValueRef>,
}

impl<'modu, 'ctx, 'table, 'ast> Drop
    for CodeGenerator<'modu, 'ctx, 'table, 'ast>
{
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeBuilder(self.builder);
        }
    }
}

impl<'modu: 'table, 'ctx, 'table: 'ast, 'ast>
    CodeGenerator<'modu, 'ctx, 'table, 'ast>
{
    pub fn generate(
        module: &'modu Module<'ctx, 'table, 'ast>,
        function: &'modu Function<'table, 'ast>,
    ) {
        unsafe {
            // set builder to entry basic block
            let mut gen = {
                let builder = LLVMCreateBuilderInContext(module.get_context());
                Self {
                    module,
                    builder,
                    variables: HashMap::new(),
                    block_vec: Vec::new(),
                    parameters: HashMap::new(),
                }
            };

            for (idx, parameter) in function
                .bound_function()
                .function_symbol()
                .parameters()
                .iter()
                .enumerate()
            {
                let llvm_parameter =
                    LLVMGetParam(function.llvm_function(), idx as u32);

                // if mutable, we need to create a local variable
                if parameter.is_mutable {
                    let llvm_type =
                        gen.module.get_type(parameter.variable_type).unwrap();

                    // create cstring for name
                    let cstring = CString::new(parameter.name).unwrap();

                    let llvm_local = LLVMBuildAlloca(
                        gen.builder,
                        llvm_type,
                        cstring.as_ptr() as *const i8,
                    );

                    LLVMBuildStore(gen.builder, llvm_parameter, llvm_local);

                    gen.variables.insert(
                        VariableWrapper::wrap(parameter.clone()),
                        llvm_local,
                    );
                } else {
                    gen.parameters.insert(
                        VariableWrapper::wrap(parameter.clone()),
                        llvm_parameter,
                    );
                }
            }

            for _ in function.control_flow_graph().get_blocks() {
                let block = LLVMAppendBasicBlockInContext(
                    module.get_context(),
                    function.llvm_function(),
                    b"block\0".as_ptr() as *const i8,
                );

                gen.block_vec.push(block);
            }

            for (idx, block) in function
                .control_flow_graph()
                .get_blocks()
                .iter()
                .enumerate()
            {
                // set the builder to the new block
                LLVMPositionBuilderAtEnd(gen.builder, gen.block_vec[idx]);

                // geneerate the instructions
                for instruction in &block.instructions {
                    gen.generate_instruction(instruction);
                }
            }

            LLVMVerifyFunction(
                function.llvm_function(),
                LLVMVerifierFailureAction::LLVMPrintMessageAction,
            );
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    /// INSTRUCTIONS
    ////////////////////////////////////////////////////////////////////////////

    fn generate_instruction(
        &mut self,
        instructions: &'modu Instruction<'table, 'ast>,
    ) {
        match instructions {
            Instruction::Statement(statement) => {
                self.generate_statement(statement);
            }
            Instruction::Terminator(terminator) => match terminator {
                Terminator::Jump(block_idx) => unsafe {
                    LLVMBuildBr(self.builder, self.block_vec[*block_idx]);
                },
                Terminator::ConditionalJump {
                    expression,
                    true_block,
                    false_block,
                } => {
                    let condition = self.generate_expression(expression);

                    unsafe {
                        LLVMBuildCondBr(
                            self.builder,
                            condition,
                            self.block_vec[*true_block],
                            self.block_vec[*false_block],
                        );
                    }
                }
                Terminator::ReturnStatement(return_statement) => {
                    match &return_statement.expression {
                        Some(expr) => {
                            let value = self.generate_expression(&expr);

                            unsafe {
                                LLVMBuildRet(self.builder, value);
                            }
                        }
                        None => unsafe {
                            LLVMBuildRetVoid(self.builder);
                        },
                    }
                }
            },
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    /// STATEMENTS
    ////////////////////////////////////////////////////////////////////////////

    fn generate_statement(
        &mut self,
        statement: &'modu BoundStatement<'table, 'ast>,
    ) {
        match statement {
            BoundStatement::BoundExpressionStatement(expr) => {
                self.generate_expression(expr);
            }
            BoundStatement::BoundVariableDeclarationStatement(statement) => {
                self.generate_variable_declaration_statement(statement)
            }
            _ => {
                unreachable!()
            }
        }
    }
    fn generate_variable_declaration_statement(
        &mut self,
        statement: &'modu BoundVariableDeclarationStatement<'table, 'ast>,
    ) {
        unsafe {
            let llvm_type = self
                .module
                .get_type(statement.expression.get_type().type_symbol)
                .unwrap();

            // create cstring for name
            let cstring =
                CString::new(statement.ast.value.identifier.value).unwrap();

            let llvm_local = LLVMBuildAlloca(
                self.builder,
                llvm_type,
                cstring.as_ptr() as *const i8,
            );

            let value = self.generate_expression(&statement.expression);
            LLVMBuildStore(self.builder, value, llvm_local);

            self.variables.insert(
                VariableWrapper::wrap(statement.variable_symbol.clone()),
                llvm_local,
            );
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    /// EXPRESSION
    ////////////////////////////////////////////////////////////////////////////

    fn generate_expression(
        &mut self,
        expression: &'modu BoundExpression<'table, 'ast>,
    ) -> LLVMValueRef {
        match expression {
            BoundExpression::BoundUnaryExpression(expr) => {
                self.generate_unary_expression(expr)
            }
            BoundExpression::BoundBinaryExpression(expr) => {
                self.generate_binary_expression(expr)
            }
            BoundExpression::BoundIdentifierExpression(expr) => {
                self.generate_identifier_expression(expr)
            }
            BoundExpression::BoundLiteralExpression(expr) => {
                self.generate_literal_expression(expr)
            }
            BoundExpression::BoundFunctionCallExpression(expr) => {
                self.generate_function_call_expression(expr)
            }
            BoundExpression::BoundCastExpression(expr) => {
                self.generate_cast_expression(expr)
            }
        }
    }

    fn generate_cast_expression(
        &mut self,
        cast: &'modu BoundCastExpression<'table, 'ast>,
    ) -> LLVMValueRef {
        let value = self.generate_expression(&cast.operand);

        // floating to numeric
        if cast.operand.get_type().type_symbol.is_floating_point()
            && cast.expression_type.type_symbol.is_numeric()
        {
            let func = if cast.expression_type.type_symbol.is_signed_numeric() {
                LLVMBuildFPToSI
            } else {
                LLVMBuildFPToUI
            };

            unsafe {
                func(
                    self.builder,
                    value,
                    self.module
                        .get_type(cast.expression_type.type_symbol)
                        .unwrap(),
                    b"cast\0" as *const u8 as *const i8,
                )
            }
        }
        // numeric to floating
        else if cast.operand.get_type().type_symbol.is_numeric()
            && cast.expression_type.type_symbol.is_floating_point()
        {
            let func =
                if cast.operand.get_type().type_symbol.is_signed_numeric() {
                    LLVMBuildSIToFP
                } else {
                    LLVMBuildUIToFP
                };

            unsafe {
                func(
                    self.builder,
                    value,
                    self.module
                        .get_type(cast.expression_type.type_symbol)
                        .unwrap(),
                    b"cast\0" as *const u8 as *const i8,
                )
            }
        }
        // floating to floating
        else if cast.operand.get_type().type_symbol.is_floating_point()
            && cast.expression_type.type_symbol.is_floating_point()
        {
            unsafe {
                // extend
                if cast
                    .operand
                    .get_type()
                    .type_symbol
                    .is_primitive_type_of(PrimitiveType::Float32)
                {
                    LLVMBuildFPExt(
                        self.builder,
                        value,
                        self.module
                            .get_type(cast.expression_type.type_symbol)
                            .unwrap(),
                        b"cast\0" as *const u8 as *const i8,
                    )
                } else if cast
                    .operand
                    .get_type()
                    .type_symbol
                    .is_primitive_type_of(PrimitiveType::Float64)
                {
                    LLVMBuildFPTrunc(
                        self.builder,
                        value,
                        self.module
                            .get_type(cast.expression_type.type_symbol)
                            .unwrap(),
                        b"cast\0" as *const u8 as *const i8,
                    )
                } else {
                    unreachable!();
                }
            }
        }
        // integer to integer
        else if cast.operand.get_type().type_symbol.is_integer()
            && cast.expression_type.type_symbol.is_integer()
        {
            fn cal_size(type_symbol: &TypeSymbol) -> u32 {
                match type_symbol {
                    TypeSymbol::PrimitiveType(PrimitiveType::Int8)
                    | TypeSymbol::PrimitiveType(PrimitiveType::Uint8) => 8,
                    TypeSymbol::PrimitiveType(PrimitiveType::Int16)
                    | TypeSymbol::PrimitiveType(PrimitiveType::Uint16) => 16,
                    TypeSymbol::PrimitiveType(PrimitiveType::Int32)
                    | TypeSymbol::PrimitiveType(PrimitiveType::Uint32) => 32,
                    TypeSymbol::PrimitiveType(PrimitiveType::Int64)
                    | TypeSymbol::PrimitiveType(PrimitiveType::Uint64) => 64,
                    _ => unreachable!(),
                }
            }

            let from_size = cal_size(&cast.operand.get_type().type_symbol);
            let to_size = cal_size(&cast.expression_type.type_symbol);

            unsafe {
                if from_size == to_size {
                    LLVMBuildIntCast2(
                        self.builder,
                        value,
                        self.module
                            .get_type(cast.expression_type.type_symbol)
                            .unwrap(),
                        cast.expression_type.type_symbol.is_signed_numeric()
                            as LLVMBool,
                        b"cast\0" as *const u8 as *const i8,
                    )
                }
                // If the target type is larger than the source type, we need to
                // sign-extend or zero-extend the value
                else if to_size > from_size {
                    if cast.expression_type.type_symbol.is_signed_numeric() {
                        LLVMBuildSExt(
                            self.builder,
                            value,
                            self.module
                                .get_type(cast.expression_type.type_symbol)
                                .unwrap(),
                            b"cast\0" as *const u8 as *const i8,
                        )
                    } else {
                        LLVMBuildZExt(
                            self.builder,
                            value,
                            self.module
                                .get_type(cast.expression_type.type_symbol)
                                .unwrap(),
                            b"cast\0" as *const u8 as *const i8,
                        )
                    }
                } else {
                    LLVMBuildTrunc(
                        self.builder,
                        value,
                        self.module
                            .get_type(cast.expression_type.type_symbol)
                            .unwrap(),
                        b"cast\0" as *const u8 as *const i8,
                    )
                }
            }
        } else {
            unreachable!()
        }
    }

    fn generate_identifier_expression(
        &mut self,
        identifier: &BoundIdentifierExpression,
    ) -> LLVMValueRef {
        unsafe {
            match self
                .variables
                .get(&VariableWrapper::wrap(identifier.variable_symbol.clone()))
            {
                Some(ptr) => LLVMBuildLoad2(
                    self.builder,
                    self.module
                        .get_type(identifier.expression_type.type_symbol)
                        .unwrap(),
                    *ptr,
                    b"load\0" as *const u8 as *const i8,
                ),
                None => *self
                    .parameters
                    .get(&VariableWrapper::wrap(
                        identifier.variable_symbol.clone(),
                    ))
                    .expect("expect parameter"),
            }
        }
    }

    fn generate_literal_expression(
        &mut self,
        literal: &BoundLiteralExpression,
    ) -> LLVMValueRef {
        match literal.ast.value.literal_expression {
            LiteralConstantToken::Number { value, .. } => {
                // create a cstring (null terminated) of literal value
                let cstring = CString::new(value).unwrap();

                if literal.expression_type.type_symbol.is_floating_point() {
                    unsafe {
                        LLVMConstRealOfString(
                            self.module
                                .get_type(literal.expression_type.type_symbol)
                                .expect("expect type"),
                            cstring.as_ptr(),
                        )
                    }
                } else if literal.expression_type.type_symbol.is_integer() {
                    unsafe {
                        LLVMConstIntOfString(
                            self.module
                                .get_type(literal.expression_type.type_symbol)
                                .expect("expect type"),
                            cstring.as_ptr(),
                            10,
                        )
                    }
                } else {
                    unreachable!()
                }
            }
            LiteralConstantToken::Boolean(val) => unsafe {
                LLVMConstInt(
                    self.module
                        .get_type(literal.expression_type.type_symbol)
                        .unwrap(),
                    if val { 1 } else { 0 },
                    true as LLVMBool,
                )
            },
        }
    }

    fn generate_function_call_expression(
        &mut self,
        func_call: &'modu BoundFunctionCallExpression<'table, 'ast>,
    ) -> LLVMValueRef {
        unsafe {
            let mut args = Vec::new();

            for arg in &func_call.arguments {
                args.push(self.generate_expression(arg));
            }

            let function = self
                .module
                .get_function(func_call.function)
                .expect("Function not found");

            LLVMBuildCall2(
                self.builder,
                function.llvm_type(),
                function.llvm_function(),
                args.as_mut_ptr(),
                args.len() as u32,
                b"call\0".as_ptr() as *const i8,
            )
        }
    }

    fn generate_unary_expression(
        &mut self,
        expression: &'modu BoundUnaryExpression<'table, 'ast>,
    ) -> LLVMValueRef {
        unsafe {
            let operand = self.generate_expression(&expression.operand);

            match expression.ast.value.operator.value {
                UnaryOperator::Minus => LLVMBuildNeg(
                    self.builder,
                    operand,
                    b"neg\0".as_ptr() as *const i8,
                ),
                UnaryOperator::LogicalNot => LLVMBuildNot(
                    self.builder,
                    operand,
                    b"not\0".as_ptr() as *const i8,
                ),
                UnaryOperator::Plus => operand,
            }
        }
    }

    fn get_llvm_ptr_from_lvalue(
        &self,
        expression: &'modu BoundExpression<'table, 'ast>,
    ) -> LLVMValueRef {
        match expression {
            BoundExpression::BoundIdentifierExpression(expression) => *self
                .variables
                .get(&VariableWrapper::wrap(expression.variable_symbol.clone()))
                .unwrap(),
            _ => {
                todo!();
            }
        }
    }

    fn generate_binary_expression(
        &mut self,
        expression: &'modu BoundBinaryExpression<'table, 'ast>,
    ) -> LLVMValueRef {
        unsafe {
            fn generate_assign_expression(
                gen: &mut CodeGenerator,
                lvalue: &BoundExpression,
                expression: LLVMValueRef,
            ) -> LLVMValueRef {
                let left = gen.get_llvm_ptr_from_lvalue(&lvalue);

                unsafe {
                    LLVMBuildStore(gen.builder, expression, left);

                    LLVMBuildLoad2(
                        gen.builder,
                        gen.module
                            .get_type(lvalue.get_type().type_symbol)
                            .unwrap(),
                        left,
                        b"load\0" as *const u8 as *const i8,
                    )
                }
            }

            let is_assignment = match expression.ast.value.operator.value {
                BinaryOperator::Assignment => true,
                _ => false,
            };

            if is_assignment {
                let right = self.generate_expression(&expression.right);

                return generate_assign_expression(
                    self,
                    &expression.left,
                    right,
                );
            }

            let left = self.generate_expression(&expression.left);
            let right = self.generate_expression(&expression.right);
            let op = expression.ast.value.operator.value;

            if expression.expression_type.type_symbol.is_floating_point() {
                fn generate_arithmetic_expression(
                    gen: &mut CodeGenerator,
                    left: LLVMValueRef,
                    right: LLVMValueRef,
                    op: BinaryOperator,
                ) -> LLVMValueRef {
                    match op {
                        BinaryOperator::Add
                        | BinaryOperator::Subtract
                        | BinaryOperator::Multiply
                        | BinaryOperator::Divide
                        | BinaryOperator::Remainder => {
                            let llvm_op = match op {
                                BinaryOperator::Add => LLVMBuildFAdd,
                                BinaryOperator::Subtract => LLVMBuildFSub,
                                BinaryOperator::Multiply => LLVMBuildFMul,
                                BinaryOperator::Divide => LLVMBuildFDiv,
                                BinaryOperator::Remainder => LLVMBuildFRem,
                                _ => unreachable!(),
                            };

                            unsafe {
                                llvm_op(
                                    gen.builder,
                                    left,
                                    right,
                                    b"bin_val\0".as_ptr() as *const i8,
                                )
                            }
                        }

                        _ => unreachable!(),
                    }
                }

                match op {
                    BinaryOperator::Add
                    | BinaryOperator::Subtract
                    | BinaryOperator::Multiply
                    | BinaryOperator::Divide
                    | BinaryOperator::Remainder => {
                        generate_arithmetic_expression(self, left, right, op)
                    }
                    BinaryOperator::CompoundAddition
                    | BinaryOperator::CompoundSubtraction
                    | BinaryOperator::CompoundMultiplication
                    | BinaryOperator::CompoundDivision
                    | BinaryOperator::CompoundRemainder => {
                        let op = match op {
                            BinaryOperator::CompoundAddition => {
                                BinaryOperator::Add
                            }
                            BinaryOperator::CompoundSubtraction => {
                                BinaryOperator::Subtract
                            }
                            BinaryOperator::CompoundMultiplication => {
                                BinaryOperator::Multiply
                            }
                            BinaryOperator::CompoundDivision => {
                                BinaryOperator::Divide
                            }
                            BinaryOperator::CompoundRemainder => {
                                BinaryOperator::Remainder
                            }
                            _ => unreachable!(),
                        };

                        let result = generate_arithmetic_expression(
                            self, left, right, op,
                        );

                        generate_assign_expression(
                            self,
                            &expression.left,
                            result,
                        )
                    }
                    _ => unreachable!(),
                }
            } else if expression.expression_type.type_symbol.is_numeric() {
                let is_unsigned = expression
                    .expression_type
                    .type_symbol
                    .is_unsigned_numeric();

                fn generate_arithmetic_expression(
                    gen: &mut CodeGenerator,
                    left: LLVMValueRef,
                    right: LLVMValueRef,
                    op: BinaryOperator,
                    is_unsigned: bool,
                ) -> LLVMValueRef {
                    match op {
                        BinaryOperator::Add
                        | BinaryOperator::Subtract
                        | BinaryOperator::Multiply
                        | BinaryOperator::Divide
                        | BinaryOperator::Remainder => {
                            let llvm_op = match op {
                                BinaryOperator::Add => {
                                    if is_unsigned {
                                        LLVMBuildNUWAdd
                                    } else {
                                        LLVMBuildAdd
                                    }
                                }
                                BinaryOperator::Subtract => {
                                    if is_unsigned {
                                        LLVMBuildNUWSub
                                    } else {
                                        LLVMBuildSub
                                    }
                                }
                                BinaryOperator::Multiply => {
                                    if is_unsigned {
                                        LLVMBuildNUWMul
                                    } else {
                                        LLVMBuildMul
                                    }
                                }
                                BinaryOperator::Divide => {
                                    if is_unsigned {
                                        LLVMBuildUDiv
                                    } else {
                                        LLVMBuildSDiv
                                    }
                                }
                                BinaryOperator::Remainder => {
                                    if is_unsigned {
                                        LLVMBuildURem
                                    } else {
                                        LLVMBuildSRem
                                    }
                                }
                                _ => unreachable!(),
                            };

                            unsafe {
                                llvm_op(
                                    gen.builder,
                                    left,
                                    right,
                                    b"bin_val\0".as_ptr() as *const i8,
                                )
                            }
                        }

                        _ => unreachable!(),
                    }
                }

                match op {
                    BinaryOperator::Add
                    | BinaryOperator::Subtract
                    | BinaryOperator::Multiply
                    | BinaryOperator::Divide
                    | BinaryOperator::Remainder => {
                        generate_arithmetic_expression(
                            self,
                            left,
                            right,
                            op,
                            is_unsigned,
                        )
                    }
                    BinaryOperator::CompoundAddition
                    | BinaryOperator::CompoundSubtraction
                    | BinaryOperator::CompoundMultiplication
                    | BinaryOperator::CompoundDivision
                    | BinaryOperator::CompoundRemainder => {
                        let op = match op {
                            BinaryOperator::CompoundAddition => {
                                BinaryOperator::Add
                            }
                            BinaryOperator::CompoundSubtraction => {
                                BinaryOperator::Subtract
                            }
                            BinaryOperator::CompoundMultiplication => {
                                BinaryOperator::Multiply
                            }
                            BinaryOperator::CompoundDivision => {
                                BinaryOperator::Divide
                            }
                            BinaryOperator::CompoundRemainder => {
                                BinaryOperator::Remainder
                            }
                            _ => unreachable!(),
                        };

                        let result = generate_arithmetic_expression(
                            self,
                            left,
                            right,
                            op,
                            is_unsigned,
                        );

                        generate_assign_expression(
                            self,
                            &expression.left,
                            result,
                        )
                    }
                    _ => unreachable!(),
                }
            } else if expression.expression_type.type_symbol.is_bool() {
                match op {
                    BinaryOperator::Equal | BinaryOperator::NotEqual => {
                        let llvm_op = match op {
                            BinaryOperator::Equal => LLVMBuildICmp,
                            BinaryOperator::NotEqual => LLVMBuildICmp,
                            _ => unreachable!(),
                        };

                        llvm_op(
                            self.builder,
                            LLVMIntPredicate::LLVMIntEQ,
                            left,
                            right,
                            b"bin_val\0".as_ptr() as *const i8,
                        )
                    }
                    BinaryOperator::LogicalAnd => LLVMBuildAnd(
                        self.builder,
                        left,
                        right,
                        b"bin_val\0".as_ptr() as *const i8,
                    ),
                    BinaryOperator::LogicalOr => LLVMBuildOr(
                        self.builder,
                        left,
                        right,
                        b"bin_val\0".as_ptr() as *const i8,
                    ),
                    _ => {
                        if expression
                            .left
                            .get_type()
                            .type_symbol
                            .is_floating_point()
                            && expression
                                .right
                                .get_type()
                                .type_symbol
                                .is_floating_point()
                        {
                            let (llvm_op, llvm_predicate) = match op {
                                BinaryOperator::LessThan => (
                                    LLVMBuildFCmp,
                                    LLVMRealPredicate::LLVMRealOLT,
                                ),
                                BinaryOperator::GreaterThan => (
                                    LLVMBuildFCmp,
                                    LLVMRealPredicate::LLVMRealOGT,
                                ),
                                BinaryOperator::LessThanEqual => (
                                    LLVMBuildFCmp,
                                    LLVMRealPredicate::LLVMRealOLE,
                                ),
                                BinaryOperator::GreaterThanEqual => (
                                    LLVMBuildFCmp,
                                    LLVMRealPredicate::LLVMRealOGE,
                                ),
                                BinaryOperator::Equal => (
                                    LLVMBuildFCmp,
                                    LLVMRealPredicate::LLVMRealOEQ,
                                ),
                                BinaryOperator::NotEqual => (
                                    LLVMBuildFCmp,
                                    LLVMRealPredicate::LLVMRealONE,
                                ),
                                _ => unreachable!(),
                            };

                            llvm_op(
                                self.builder,
                                llvm_predicate,
                                left,
                                right,
                                b"bin_val\0".as_ptr() as *const i8,
                            )
                        } else if expression
                            .left
                            .get_type()
                            .type_symbol
                            .is_numeric()
                            && expression
                                .right
                                .get_type()
                                .type_symbol
                                .is_numeric()
                        {
                            let is_unsigned = expression
                                .left
                                .get_type()
                                .type_symbol
                                .is_unsigned_numeric();

                            let (llvm_op, llvm_predicate) = match op {
                                BinaryOperator::LessThan => (
                                    LLVMBuildICmp,
                                    if is_unsigned {
                                        LLVMIntPredicate::LLVMIntULT
                                    } else {
                                        LLVMIntPredicate::LLVMIntSLT
                                    },
                                ),
                                BinaryOperator::GreaterThan => (
                                    LLVMBuildICmp,
                                    if is_unsigned {
                                        LLVMIntPredicate::LLVMIntUGT
                                    } else {
                                        LLVMIntPredicate::LLVMIntSGT
                                    },
                                ),
                                BinaryOperator::LessThanEqual => (
                                    LLVMBuildICmp,
                                    if is_unsigned {
                                        LLVMIntPredicate::LLVMIntULE
                                    } else {
                                        LLVMIntPredicate::LLVMIntSLE
                                    },
                                ),
                                BinaryOperator::GreaterThanEqual => (
                                    LLVMBuildICmp,
                                    if is_unsigned {
                                        LLVMIntPredicate::LLVMIntUGE
                                    } else {
                                        LLVMIntPredicate::LLVMIntSGE
                                    },
                                ),
                                BinaryOperator::Equal => {
                                    (LLVMBuildICmp, LLVMIntPredicate::LLVMIntEQ)
                                }
                                BinaryOperator::NotEqual => {
                                    (LLVMBuildICmp, LLVMIntPredicate::LLVMIntNE)
                                }
                                _ => unreachable!(),
                            };

                            llvm_op(
                                self.builder,
                                llvm_predicate,
                                left,
                                right,
                                b"bin_val\0".as_ptr() as *const i8,
                            )
                        } else {
                            unreachable!()
                        }
                    }
                }
            } else {
                unreachable!()
            }
        }
    }
}
