#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Frame {
    end: usize,
    key: (u128, u128),
}

/// Represents a write buffer that stores all the values in a single `Vec<u8>`
/// buffer and are separated by `Frame`s. This is used to reduce the number
/// small writes to the database and instead write larger chunks of data at
/// once.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Buffer {
    buffer: Vec<u8>,
    frames: Vec<Frame>,
}

impl Default for Buffer {
    fn default() -> Self {
        Self {
            buffer: Vec::with_capacity(1024 * 1024),
            frames: Vec::with_capacity(50),
        }
    }
}

impl Buffer {
    pub fn write(
        &mut self,
        key: (u128, u128),
        write: impl FnOnce(&mut Vec<u8>) -> bool,
    ) {
        let begin = self.buffer.len();
        let success = write(&mut self.buffer);

        if success {
            let end = self.buffer.len();
            self.frames.push(Frame { end, key });
        } else {
            // truncate the buffer to the beginning
            self.buffer.truncate(begin);
        }
    }

    pub fn flush(&mut self, table: &mut redb::Table<'_, (u128, u128), &[u8]>) {
        let mut begin = 0;

        for frame in &self.frames {
            let end = frame.end;
            let key = frame.key;

            if let Err(e) = table.insert(key, &self.buffer[begin..end]) {
                tracing::error!(
                    "Failed to write frame {frame:?} to database: {e}"
                );
            }

            begin = end;
        }

        // uses clear to keep the capacity of the buffer
        self.frames.clear();
        self.buffer.clear();
    }

    #[must_use]
    pub const fn byte_size(&self) -> usize { self.buffer.len() }
}
