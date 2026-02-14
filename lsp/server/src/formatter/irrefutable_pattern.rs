use pernixc_syntax::pattern;

/// Formats an irrefutable pattern into the given buffer.
pub fn format_pattern(
    buffer: &mut dyn std::fmt::Write,
    irrefutable_pattern: Option<&pattern::Irrefutable>,
) {
    let Some(irrefutable) = irrefutable_pattern else {
        write!(buffer, "..").unwrap();

        return;
    };

    match irrefutable {
        pattern::Irrefutable::Struct(st) => {
            let mut is_first = true;
            write!(buffer, "{{").unwrap();

            for field in st.fields() {
                if !is_first {
                    write!(buffer, ", ").unwrap();
                }
                is_first = false;

                match field {
                    pattern::Field::FieldAssociation(field_association) => {
                        let name = field_association.identifier();

                        if let Some(name) = name {
                            write!(buffer, "{}: ", name.kind).unwrap();
                            format_pattern(
                                buffer,
                                field_association.pattern().as_ref(),
                            );
                        } else {
                            write!(buffer, "..").unwrap();
                        }
                    }

                    pattern::Field::Wildcard(_) => {
                        write!(buffer, "..").unwrap();
                    }

                    pattern::Field::Named(named) => {
                        if let Some(identifier) = named.identifier() {
                            if named.mutable_keyword().is_some() {
                                write!(buffer, "mut ").unwrap();
                            }

                            write!(buffer, "{}", identifier.kind).unwrap();
                        } else {
                            write!(buffer, "..").unwrap();
                        }
                    }
                }
            }

            write!(buffer, "}}").unwrap();
        }

        pattern::Irrefutable::Named(named) => {
            if let Some(identifier) = named.identifier() {
                if named.mutable_keyword().is_some() {
                    write!(buffer, "mut ").unwrap();
                }

                write!(buffer, "{}", identifier.kind).unwrap();
            } else {
                write!(buffer, "..").unwrap();
            }
        }

        pattern::Irrefutable::Tuple(tuple) => {
            let mut is_first = true;

            write!(buffer, "(").unwrap();
            for element in tuple.types() {
                if !is_first {
                    write!(buffer, ", ").unwrap();
                }
                is_first = false;

                if element.ellipsis().is_some() {
                    write!(buffer, "...").unwrap();
                }

                format_pattern(buffer, element.pattern().as_ref());
            }
            write!(buffer, ")").unwrap();
        }

        pattern::Irrefutable::Wildcard(_) => {
            write!(buffer, "..").unwrap();
        }
    }
}
