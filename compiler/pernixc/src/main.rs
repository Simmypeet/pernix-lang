use std::{ffi::CString, mem, process::Command};

use clap::Parser;

use llvm_sys::{
    analysis::{LLVMVerifierFailureAction, LLVMVerifyModule},
    core::{
        LLVMCreatePassManager, LLVMDisposeMessage, LLVMDisposePassManager,
        LLVMPrintModuleToFile, LLVMRunPassManager, LLVMSetDataLayout,
        LLVMSetTarget,
    },
    target::{
        LLVMCopyStringRepOfTargetData, LLVM_InitializeAllAsmParsers,
        LLVM_InitializeAllAsmPrinters, LLVM_InitializeAllTargetInfos,
        LLVM_InitializeAllTargetMCs, LLVM_InitializeAllTargets,
    },
    target_machine::{
        LLVMCodeGenFileType, LLVMCodeGenOptLevel, LLVMCodeModel,
        LLVMCreateTargetDataLayout, LLVMCreateTargetMachine,
        LLVMDisposeTargetMachine, LLVMGetDefaultTargetTriple,
        LLVMGetHostCPUFeatures, LLVMGetTargetFromTriple, LLVMRelocMode,
        LLVMTargetMachineEmitToFile, LLVMTargetRef,
    },
    transforms::{
        instcombine::LLVMAddInstructionCombiningPass,
        ipo::LLVMAddGlobalOptimizerPass,
        scalar::{
            LLVMAddCFGSimplificationPass, LLVMAddGVNPass,
            LLVMAddReassociatePass,
        },
        util::LLVMAddPromoteMemoryToRegisterPass,
    },
};
use pernix_analyzer::{
    binding::bound_declaration::BoundFunction,
    symbol::{
        table::{FunctionSymbolTable, TypeSymbolTable},
        PrimitiveType,
    },
};
use pernix_codegen::{context::Context, module::Module};
use pernix_project::source_code::SourceCode;

mod printing;

#[derive(Parser, Debug)]
#[clap(
    author = "Pernix",
    about = "A compiler for the Pernix programming language",
    version = "0.1.0"
)]
struct CompileArg {
    /// The input files
    file: Vec<String>,

    /// The output file name
    #[clap(long, short)]
    output: Option<String>,

    /// Apply optimizations to the generated code
    #[clap(long)]
    optimize: bool,

    /// Emit the LLVM IR code
    #[clap(long)]
    emit_llvm: bool,
}

fn main() {
    let args = CompileArg::parse();

    if args.file.is_empty() {
        printing::print_error("no input files!");
        std::process::exit(1);
    }

    // get the source codes from the arguments
    let source_codes = {
        let mut source_codes = Vec::new();
        let mut found_error = false;

        for file in args.file {
            // get the file name from the path
            let file_name = match file.split('/').last() {
                Some(file_name) => file_name,
                None => {
                    printing::print_error(
                        format!("`{}` is not a valid path", &file).as_str(),
                    );
                    found_error = true;
                    continue;
                }
            };

            // check if the file exists
            let source_code = match std::fs::read_to_string(&file) {
                Ok(source_code) => source_code,
                Err(_) => {
                    printing::print_error(
                        format!("`{}` does not exist", &file).as_str(),
                    );
                    continue;
                }
            };

            source_codes
                .push(SourceCode::new(source_code, file_name.to_string()));
        }

        if found_error {
            std::process::exit(1);
        } else {
            source_codes
        }
    };

    if source_codes.is_empty() {
        std::process::exit(1);
    }

    let mut ast_vector = Vec::new();
    let type_table = TypeSymbolTable::new();
    let mut function_table = FunctionSymbolTable::new();
    let mut bound_functions = Vec::new();

    for source_code in &source_codes {
        let mut parser = pernix_parser::Parser::new(&source_code);
        let ast = parser.parse_file();

        {
            let parser_errors = parser.pop_errors();

            if !parser_errors.is_empty() {
                for error in parser_errors {
                    printing::print_syntactic_error(source_code, &error);
                }
            } else {
                ast_vector.push(ast);
            }
        }
    }

    for ast in &ast_vector {
        match function_table.populate(&ast, &type_table) {
            Ok(_) => {}
            Err(_) => {}
        }
    }

    for func_sym in function_table.values() {
        match BoundFunction::new(&func_sym.value, &function_table, &type_table)
        {
            Ok(bound_function) => {
                bound_functions.push(bound_function);
            }
            Err(error) => {
                (error);
            }
        }
    }

    let context = Context::new();
    let mut module = Module::new(&context, "pernixc_module");

    for type_sym in type_table.values() {
        module.add_type_symbol(&type_sym.value);
    }

    for bound_function in bound_functions {
        module.add_function_symbol(bound_function);
    }

    module.compile();

    unsafe {
        let mut out_message = mem::zeroed();

        LLVMVerifyModule(
            module.get_llvm_module(),
            LLVMVerifierFailureAction::LLVMPrintMessageAction,
            &mut out_message,
        );

        LLVMDisposeMessage(out_message);
    }

    // expect function "Main"
    let mut found_main = false;
    for function in module.functions() {
        if function
            .bound_function()
            .function_symbol()
            .full_qualified_name()
            == "Main"
        {
            // it should take no arguments and return int32
            if !std::ptr::eq(
                &type_table.get_primitive_type(PrimitiveType::Int32).value,
                function.bound_function().function_symbol().return_type(),
            ) {
                printing::print_error(
                    "the function `Main` should return `int32`",
                );
                std::process::exit(1);
            }

            if function
                .bound_function()
                .function_symbol()
                .parameters()
                .len()
                != 0
            {
                printing::print_error(
                    "the function `Main` should take no arguments",
                );
                std::process::exit(1);
            }

            found_main = true;
        }
    }

    if !found_main {
        printing::print_error("the function `Main` is not defined");
        std::process::exit(1);
    }

    if args.optimize {
        unsafe {
            // optimize the module
            let pass_manager = LLVMCreatePassManager();
            LLVMAddPromoteMemoryToRegisterPass(pass_manager);
            LLVMAddGlobalOptimizerPass(pass_manager);
            LLVMAddInstructionCombiningPass(pass_manager);
            LLVMAddReassociatePass(pass_manager);
            LLVMAddGVNPass(pass_manager);
            LLVMAddCFGSimplificationPass(pass_manager);
            LLVMRunPassManager(pass_manager, module.get_llvm_module());
            LLVMDisposePassManager(pass_manager);
        }
    }

    if args.emit_llvm {
        // emit the module as LLVM IR to <output>.ll
        let output = match &args.output {
            Some(output) => {
                let mut output = output.clone();
                output.push_str(".ll");
                output
            }
            None => "a.ll".to_string(),
        };

        let c_string = CString::new(output).unwrap();

        unsafe {
            LLVMPrintModuleToFile(
                module.get_llvm_module(),
                c_string.as_ptr(),
                &mut mem::zeroed(),
            );
        }
    } else {
        unsafe {
            let output = match &args.output {
                Some(output) => output,
                None => "a",
            };

            // compile to the object file
            {
                // initialize the target
                LLVM_InitializeAllTargetInfos();
                LLVM_InitializeAllTargets();
                LLVM_InitializeAllTargetMCs();
                LLVM_InitializeAllAsmParsers();
                LLVM_InitializeAllAsmPrinters();

                let mut target = mem::zeroed();
                let mut errors = mem::zeroed();
                LLVMGetTargetFromTriple(
                    LLVMGetDefaultTargetTriple(),
                    &mut target as *mut LLVMTargetRef,
                    &mut errors as *mut *mut i8,
                );
                LLVMDisposeMessage(errors);

                let machine = LLVMCreateTargetMachine(
                    target,
                    LLVMGetDefaultTargetTriple(),
                    b"generic\0".as_ptr() as *mut _,
                    LLVMGetHostCPUFeatures(),
                    LLVMCodeGenOptLevel::LLVMCodeGenLevelDefault,
                    LLVMRelocMode::LLVMRelocDefault,
                    LLVMCodeModel::LLVMCodeModelDefault,
                );

                LLVMSetTarget(
                    module.get_llvm_module(),
                    LLVMGetDefaultTargetTriple(),
                );
                let datalayout = LLVMCreateTargetDataLayout(machine);
                let datalayout_str = LLVMCopyStringRepOfTargetData(datalayout);
                LLVMSetDataLayout(module.get_llvm_module(), datalayout_str);
                LLVMDisposeMessage(datalayout_str);

                let output_obj = {
                    let mut output = output.to_string();
                    output.push_str(".o");
                    output
                };

                let output_obj_cstr = CString::new(output_obj.clone()).unwrap();

                LLVMTargetMachineEmitToFile(
                    machine,
                    module.get_llvm_module(),
                    output_obj_cstr.as_ptr() as *mut _,
                    LLVMCodeGenFileType::LLVMObjectFile,
                    &mut errors as *mut *mut i8,
                );

                let output_executable = {
                    #[cfg(target_os = "windows")]
                    let out = output.to_string() + ".exe";
                    #[cfg(not(target_os = "windows"))]
                    let out = output.to_string();

                    out
                };

                // link the object file to the executable
                let command = Command::new("ld")
                    .arg(output_obj)
                    .arg("-e")
                    .arg("Main")
                    .arg("-o")
                    .arg(output_executable)
                    .spawn();

                match command {
                    Ok(mut child) => {
                        let status = child.wait().unwrap();

                        if !status.success() {
                            printing::print_error(
                                "failed to link the object file",
                            );
                        }
                    }
                    Err(e) => {
                        if let std::io::ErrorKind::NotFound = e.kind() {
                            printing::print_error(
                                "pernixc compiler requires `ld` to link the object file",
                            );
                        }
                    }
                }

                LLVMDisposeMessage(errors);
                LLVMDisposeTargetMachine(machine);
            }
        }
    }
}
