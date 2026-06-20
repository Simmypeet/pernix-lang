use pernixc_qbice::TrackedEngine;
use qbice::storage::intern::Interned;

use super::{Application, Constructor, Tuple, TupleShape};
use crate::r#type::Type;

type RegularDestructure<'a> = std::iter::Zip<
    std::iter::Cloned<std::slice::Iter<'a, Interned<Type>>>,
    std::iter::Cloned<std::slice::Iter<'a, Interned<Type>>>,
>;

#[derive(Debug, Clone)]
enum Destructure<'a> {
    Regular(RegularDestructure<'a>),
    OneSidedTuple(OneSidedTupleDestructure<'a>),
}

impl Iterator for Destructure<'_> {
    type Item = (Interned<Type>, Interned<Type>);

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Regular(iterator) => iterator.next(),
            Self::OneSidedTuple(iterator) => iterator.next(),
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
            Self::Regular(iterator) => iterator.size_hint(),
            Self::OneSidedTuple(iterator) => iterator.size_hint(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum OneSidedTupleState {
    Head,
    Middle,
    Tail,
    Done,
}

#[derive(Debug, Clone)]
struct OneSidedTupleDestructure<'a> {
    from_arguments: &'a [Interned<Type>],
    to_arguments: &'a [Interned<Type>],
    unpacked_position: usize,
    to_unpack_range: std::ops::Range<usize>,
    to_unpacked_position: Option<usize>,
    engine: &'a TrackedEngine,
    swap: bool,
    next_head_index: usize,
    next_tail_offset: usize,
    state: OneSidedTupleState,
}

impl OneSidedTupleDestructure<'_> {
    const fn tail_len(&self) -> usize {
        self.from_arguments.len() - self.unpacked_position - 1
    }

    const fn remaining_len(&self) -> usize {
        match self.state {
            OneSidedTupleState::Head => {
                self.unpacked_position - self.next_head_index
                    + 1
                    + self.tail_len()
                    - self.next_tail_offset
            }
            OneSidedTupleState::Middle => {
                1 + self.tail_len() - self.next_tail_offset
            }
            OneSidedTupleState::Tail => self.tail_len() - self.next_tail_offset,
            OneSidedTupleState::Done => 0,
        }
    }
}

impl Iterator for OneSidedTupleDestructure<'_> {
    type Item = (Interned<Type>, Interned<Type>);

    fn next(&mut self) -> Option<Self::Item> {
        match self.state {
            OneSidedTupleState::Head => {
                if self.next_head_index < self.unpacked_position {
                    let index = self.next_head_index;
                    self.next_head_index += 1;

                    return Some(Application::destructured_pair(
                        self.from_arguments[index].clone(),
                        self.to_arguments[index].clone(),
                        self.swap,
                    ));
                }

                self.state = OneSidedTupleState::Middle;
                self.next()
            }
            OneSidedTupleState::Middle => {
                self.state = OneSidedTupleState::Tail;

                Some(Application::destructured_pair(
                    self.from_arguments[self.unpacked_position].clone(),
                    Application::intern_tuple_range(
                        self.to_arguments,
                        self.to_unpack_range.clone(),
                        self.to_unpacked_position,
                        self.engine,
                    ),
                    self.swap,
                ))
            }
            OneSidedTupleState::Tail => {
                if self.next_tail_offset < self.tail_len() {
                    let from_index =
                        self.unpacked_position + 1 + self.next_tail_offset;
                    let to_index = self.to_arguments.len() - self.tail_len()
                        + self.next_tail_offset;
                    self.next_tail_offset += 1;

                    return Some(Application::destructured_pair(
                        self.from_arguments[from_index].clone(),
                        self.to_arguments[to_index].clone(),
                        self.swap,
                    ));
                }

                self.state = OneSidedTupleState::Done;
                None
            }
            OneSidedTupleState::Done => None,
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = self.remaining_len();
        (remaining, Some(remaining))
    }
}

impl Application {
    pub fn destructure<'a>(
        &'a self,
        other: &'a Self,
        engine: &'a TrackedEngine,
    ) -> Option<impl Iterator<Item = (Interned<Type>, Interned<Type>)> + 'a>
    {
        if let (Constructor::Tuple(lhs), Constructor::Tuple(rhs)) =
            (&self.constructor, &other.constructor)
        {
            Self::destructure_tuples(
                lhs,
                &self.arguments,
                rhs,
                &other.arguments,
                engine,
            )
        } else if let (
            Constructor::FunctionPointer(_),
            Constructor::FunctionPointer(_),
        ) = (&self.constructor, &other.constructor)
        {
            Self::destructure_regular(&self.arguments, &other.arguments)
                .map(Destructure::Regular)
        } else if self.constructor == other.constructor {
            Self::destructure_regular(&self.arguments, &other.arguments)
                .map(Destructure::Regular)
        } else {
            None
        }
    }

    fn destructure_regular<'a>(
        lhs: &'a [Interned<Type>],
        rhs: &'a [Interned<Type>],
    ) -> Option<RegularDestructure<'a>> {
        (lhs.len() == rhs.len())
            .then(|| lhs.iter().cloned().zip(rhs.iter().cloned()))
    }

    fn destructure_tuples<'a>(
        lhs_tuple: &Tuple,
        lhs_arguments: &'a [Interned<Type>],
        rhs_tuple: &Tuple,
        rhs_arguments: &'a [Interned<Type>],
        engine: &'a TrackedEngine,
    ) -> Option<Destructure<'a>> {
        let lhs_shape = lhs_tuple.shape()?;
        let rhs_shape = rhs_tuple.shape()?;

        if lhs_arguments.len() == rhs_arguments.len() && lhs_shape == rhs_shape
        {
            return Some(Destructure::Regular(
                lhs_arguments
                    .iter()
                    .cloned()
                    .zip(rhs_arguments.iter().cloned()),
            ));
        }

        match (lhs_shape, rhs_shape) {
            (TupleShape::Regular, TupleShape::Regular) => {
                Self::destructure_regular(lhs_arguments, rhs_arguments)
                    .map(Destructure::Regular)
            }
            (TupleShape::Unpacked(position), rhs_shape) => {
                Self::destructure_one_sided_tuple(
                    lhs_arguments,
                    position,
                    rhs_arguments,
                    rhs_shape,
                    engine,
                    false,
                )
                .map(Destructure::OneSidedTuple)
            }
            (lhs_shape, TupleShape::Unpacked(position)) => {
                Self::destructure_one_sided_tuple(
                    rhs_arguments,
                    position,
                    lhs_arguments,
                    lhs_shape,
                    engine,
                    true,
                )
                .map(Destructure::OneSidedTuple)
            }
        }
    }

    fn destructure_one_sided_tuple<'a>(
        from_arguments: &'a [Interned<Type>],
        unpacked_position: usize,
        to_arguments: &'a [Interned<Type>],
        to_shape: TupleShape,
        engine: &'a TrackedEngine,
        swap: bool,
    ) -> Option<OneSidedTupleDestructure<'a>> {
        if from_arguments.len() > to_arguments.len() + 1 {
            return None;
        }

        let head_range = 0..unpacked_position;
        let from_tail_range = unpacked_position + 1..from_arguments.len();
        let to_tail_range =
            (to_arguments.len() - from_tail_range.len())..to_arguments.len();
        let to_unpack_range = unpacked_position..to_tail_range.start;

        let to_unpacked_position = match to_shape {
            TupleShape::Regular => None,
            TupleShape::Unpacked(position) => Some(position),
        };

        if to_unpacked_position.is_some_and(|position| {
            head_range.contains(&position) || to_tail_range.contains(&position)
        }) {
            return None;
        }

        Some(OneSidedTupleDestructure {
            from_arguments,
            to_arguments,
            unpacked_position,
            to_unpack_range,
            to_unpacked_position,
            engine,
            swap,
            next_head_index: 0,
            next_tail_offset: 0,
            state: OneSidedTupleState::Head,
        })
    }

    fn intern_tuple_range(
        arguments: &[Interned<Type>],
        range: std::ops::Range<usize>,
        unpacked_position: Option<usize>,
        engine: &TrackedEngine,
    ) -> Interned<Type> {
        let unpacked_positions = match unpacked_position {
            Some(position) if range.contains(&position) => {
                engine.intern_unsized(vec![position - range.start])
            }
            Some(_) | None => engine.intern_unsized(Vec::<usize>::new()),
        };

        engine.intern(Type::Application(Self {
            constructor: Constructor::Tuple(Tuple { unpacked_positions }),
            arguments: engine.intern_unsized(arguments[range].to_vec()),
        }))
    }

    const fn destructured_pair(
        lhs: Interned<Type>,
        rhs: Interned<Type>,
        swap: bool,
    ) -> (Interned<Type>, Interned<Type>) {
        if swap { (rhs, lhs) } else { (lhs, rhs) }
    }
}

#[cfg(test)]
mod test;
