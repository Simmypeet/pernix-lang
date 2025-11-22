use pernixc_syntax::pattern;

/// Formats an irrefutable pattern into the given buffer.
pub fn format_pattern(
    buffer: &mut dyn std::fmt::Write,
    irrefutable_pattern: Option<&pattern::Irrefutable>,
) -> Result<(), std::fmt::Error> {
    let Some(irrefutable) = irrefutable_pattern else {
        return write!(buffer, "..");
    };

    match irrefutable {
        pattern::Irrefutable::Struct(st) => {
            let mut is_first = true;
            write!(buffer, "{{")?;

            for field in st.fields() {
                if !is_first {
                    write!(buffer, ", ")?;
                }
                is_first = false;

                match field {
                    pattern::Field::FieldAssociation(field_association) => {
                        let name = field_association.identifier();

                        if let Some(name) = name {
                            write!(buffer, "{}: ", name.kind)?;
                            format_pattern(
                                buffer,
                                field_association.pattern().as_ref(),
                            )?;
                        } else {
                            write!(buffer, "..")?;
                        }
                    }

                    pattern::Field::Wildcard(_) => {
                        write!(buffer, "..")?;
                    }

                    pattern::Field::Named(named) => {
                        if let Some(identifier) = named.identifier() {
                            if named.mutable_keyword().is_some() {
                                write!(buffer, "mut ")?;
                            }

                            write!(buffer, "{}", identifier.kind)?;
                        } else {
                            write!(buffer, "..")?;
                        }
                    }
                }
            }

            write!(buffer, "}}")
        }

        pattern::Irrefutable::Named(named) => {
            if let Some(identifier) = named.identifier() {
                if named.mutable_keyword().is_some() {
                    write!(buffer, "mut ")?;
                }

                write!(buffer, "{}", identifier.kind)
            } else {
                write!(buffer, "..")
            }
        }

        pattern::Irrefutable::Tuple(tuple) => {
            let mut is_first = true;

            write!(buffer, "(")?;
            for element in tuple.types() {
                if !is_first {
                    write!(buffer, ", ")?;
                }
                is_first = false;

                if element.ellipsis().is_some() {
                    write!(buffer, "...")?;
                }

                format_pattern(buffer, element.pattern().as_ref())?;
            }
            write!(buffer, ")")
        }

        pattern::Irrefutable::Wildcard(_) => {
            write!(buffer, "..")
        }
    }
}
