/// A half-open byte span `[start, end)` within a UTF-8 source file.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    /// Creates a new [`Span`] from byte offsets.
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}
