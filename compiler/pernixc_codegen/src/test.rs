use std::path::PathBuf;

use inkwell::targets::TargetTriple;

#[test]
fn test_struct() {
    let context = inkwell::context::Context::create();
    let module = context.create_module("test");

    // Create struct type
    let struct_type = context.opaque_struct_type("Tiplet");

    // Define the struct fields
    struct_type.set_body(
        &[
            context.f64_type().into(),
            context.f64_type().into(),
            context.f64_type().into(),
        ],
        false, // not packed
    );

    // Create a function that returns the struct
    let fn_type = struct_type.fn_type(&[], false);
    let function = module.add_function("create_tiplet", fn_type, None);

    // Create a function that returns (0, 0, 0)
    let entry = context.append_basic_block(function, "entry");
    let builder = context.create_builder();
    builder.position_at_end(entry);

    let ptr = builder.build_alloca(struct_type, "tiplet").unwrap();

    let first = builder.build_struct_gep(struct_type, ptr, 0, "first").unwrap();
    let second =
        builder.build_struct_gep(struct_type, ptr, 1, "second").unwrap();
    let third = builder.build_struct_gep(struct_type, ptr, 2, "third").unwrap();

    builder.build_store(first, context.f64_type().const_float(5.5)).unwrap();
    builder.build_store(second, context.f64_type().const_float(6.0)).unwrap();
    builder.build_store(third, context.f64_type().const_float(6.5)).unwrap();

    builder
        .build_return(Some(
            &builder.build_load(struct_type, ptr, "tiplet").unwrap(),
        ))
        .unwrap();

    let main_function = module.add_function(
        "main",
        context.i32_type().fn_type(&[], false),
        None,
    );

    let entry = context.append_basic_block(main_function, "entry");
    let builder = context.create_builder();
    builder.position_at_end(entry);

    let alloca = builder.build_alloca(struct_type, "tiplet").unwrap();
    let tiplet = builder
        .build_call(function, &[], "tiplet")
        .unwrap()
        .try_as_basic_value()
        .left()
        .unwrap();

    let first_gep =
        builder.build_struct_gep(struct_type, alloca, 0, "first").unwrap();

    builder.build_store(first_gep, tiplet).unwrap();

    let load_first = builder
        .build_load(context.f64_type(), first_gep, "first")
        .unwrap()
        .into_float_value();

    let cast_to_i32 = builder
        .build_float_to_signed_int(load_first, context.i32_type(), "cast")
        .unwrap();

    builder.build_return(Some(&cast_to_i32)).unwrap();

    module.set_triple(&TargetTriple::create("arm64-apple-macosx15.0.0"));

    // dump the ir
    dbg!(module.verify().unwrap());

    module.print_to_stderr();
    assert!(module.write_bitcode_to_path(&PathBuf::from("test.bc")));
}
