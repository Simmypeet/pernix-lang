use inkwell::targets::{
    InitializationConfig, RelocMode, Target, TargetMachine,
    TargetMachineOptions,
};

#[test]
fn test_struct() {
    inkwell::targets::Target::initialize_native(
        &InitializationConfig::default(),
    )
    .unwrap();

    let this_tripple = TargetMachine::get_default_triple();
    let target = Target::from_triple(&this_tripple).unwrap();
    let target_machine = target
        .create_target_machine_from_options(
            &this_tripple,
            TargetMachineOptions::default().set_reloc_mode(RelocMode::PIC),
        )
        .unwrap();
    let target_data = target_machine.get_target_data();

    let context = inkwell::context::Context::create();

    let first = context.i8_type();
    let second = context.i32_type();
    let third = context.i64_type();
    let fourth = context.f128_type();

    let struct_ty = context.struct_type(
        &[first.into(), second.into(), third.into(), fourth.into()],
        false,
    );

    println!("{}", target_data.get_abi_size(&struct_ty));
    println!("{}", target_data.get_abi_alignment(&struct_ty));
    println!(
        "{}",
        target_data.get_abi_size(&context.custom_width_int_type(10))
    );
    println!("{}", target_data.get_abi_alignment(&context.bool_type()));
}
