use crate::source::Span;
use std::path::PathBuf;
use std::sync::Arc;

/// A display name for a source (either a filesystem path or a virtual label like "<string>").
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SourceName {
    /// A filesystem-backed source.
    Path(PathBuf),
    /// A virtual source (e.g. `"<string>"`).
    Virtual(String),
}

impl SourceName {
    fn render(&self) -> String {
        match self {
            SourceName::Path(path) => path.display().to_string(),
            SourceName::Virtual(name) => name.clone(),
        }
    }
}

#[derive(Clone, Debug)]
struct SourceFile {
    name: SourceName,
    base_offset: usize,
    src: Arc<str>,
    line_starts: Vec<usize>,
}

impl SourceFile {
    fn len_bytes(&self) -> usize {
        self.src.len()
    }

    fn contains_global_offset(&self, offset: usize) -> bool {
        let end = self.base_offset.saturating_add(self.len_bytes());
        (self.base_offset..=end).contains(&offset)
    }

    fn global_to_local(&self, offset: usize) -> usize {
        offset.saturating_sub(self.base_offset)
    }

    fn local_to_line_col(&self, mut offset: usize) -> LineCol {
        offset = offset.min(self.len_bytes());

        let line_idx = self.line_starts.partition_point(|&start| start <= offset);
        let line_idx = line_idx.saturating_sub(1);
        let line_start = *self.line_starts.get(line_idx).unwrap_or(&0);

        let col = if let Some(prefix) = self.src.get(line_start..offset) {
            prefix.chars().count().saturating_add(1)
        } else {
            offset.saturating_sub(line_start).saturating_add(1)
        };

        LineCol {
            line: line_idx.saturating_add(1),
            col,
        }
    }
}

#[derive(Clone, Debug, Default)]
/// A mapping from global byte offsets to source files and line/column locations.
pub struct SourceMap {
    files: Vec<SourceFile>,
}

impl SourceMap {
    /// Creates an empty [`SourceMap`].
    pub fn new() -> Self {
        Self::default()
    }

    /// Adds a source file to the map at `base_offset`.
    ///
    /// Callers are expected to ensure that `(base_offset..base_offset+src.len())` does not overlap
    /// with previously added sources.
    pub fn add_source(&mut self, name: SourceName, src: Arc<str>, base_offset: usize) {
        let line_starts = compute_line_starts(&src);
        if let Some(prev) = self.files.last() {
            debug_assert!(
                base_offset > prev.base_offset.saturating_add(prev.len_bytes()),
                "source files must have non-overlapping global offset ranges"
            );
        }
        self.files.push(SourceFile {
            name,
            base_offset,
            src,
            line_starts,
        });
    }

    /// Resolves a global [`Span`] to a source file + line/column range.
    pub fn lookup_span(&self, span: Span) -> Option<SourceRange> {
        if self.files.is_empty() {
            return None;
        }

        let start = span.start.min(span.end);
        let end = span.end.max(span.start);

        let file_idx = self.files.partition_point(|file| file.base_offset <= start);
        let file = self.files.get(file_idx.checked_sub(1)?)?;

        if !file.contains_global_offset(start) {
            return None;
        }

        let file_end = file.base_offset.saturating_add(file.len_bytes());
        if end > file_end {
            return None;
        }

        let start_local = file.global_to_local(start);
        let end_local = file.global_to_local(end);

        Some(SourceRange {
            name: file.name.clone(),
            start: file.local_to_line_col(start_local),
            end: file.local_to_line_col(end_local),
        })
    }

    /// Resolves a global [`Span`] to a source file + file-relative byte range.
    pub fn lookup_span_bytes(&self, span: Span) -> Option<SourceByteRange> {
        if self.files.is_empty() {
            return None;
        }

        let start = span.start.min(span.end);
        let end = span.end.max(span.start);

        let file_idx = self.files.partition_point(|file| file.base_offset <= start);
        let file = self.files.get(file_idx.checked_sub(1)?)?;

        if !file.contains_global_offset(start) {
            return None;
        }

        let file_end = file.base_offset.saturating_add(file.len_bytes());
        if end > file_end {
            return None;
        }

        Some(SourceByteRange {
            name: file.name.clone(),
            start: file.global_to_local(start),
            end: file.global_to_local(end),
        })
    }

    /// Returns the source text for `name` if it is present in this map.
    pub fn source_text(&self, name: &SourceName) -> Option<Arc<str>> {
        let file = self.files.iter().find(|f| &f.name == name)?;
        Some(Arc::clone(&file.src))
    }

    /// Returns the list of sources currently tracked by this map.
    pub fn source_names(&self) -> Vec<SourceName> {
        self.files.iter().map(|f| f.name.clone()).collect()
    }

    /// Renders a span into a human-readable `"file: <l:c> - <l:c>"` location string.
    pub fn render_span_location(&self, span: Span) -> Option<String> {
        let range = self.lookup_span(span)?;
        Some(range.render_location())
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
/// A 1-based line/column position within a source file.
pub struct LineCol {
    /// 1-based line number.
    pub line: usize,
    /// 1-based column number (Unicode scalar count within the line prefix).
    pub col: usize,
}

#[derive(Clone, Debug, PartialEq, Eq)]
/// A resolved source range for a [`Span`], including file name and line/column endpoints.
pub struct SourceRange {
    /// Source file identity.
    pub name: SourceName,
    /// Start position (inclusive).
    pub start: LineCol,
    /// End position (exclusive).
    pub end: LineCol,
}

impl SourceRange {
    /// Formats a source range location suitable for diagnostics display.
    pub fn render_location(&self) -> String {
        format!(
            "{}: <{}:{}> - <{}:{}>",
            self.name.render(),
            self.start.line,
            self.start.col,
            self.end.line,
            self.end.col
        )
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
/// A resolved source range for a [`Span`] using file-relative UTF-8 byte offsets.
pub struct SourceByteRange {
    /// Source file identity.
    pub name: SourceName,
    /// Start byte offset within the file (inclusive).
    pub start: usize,
    /// End byte offset within the file (exclusive).
    pub end: usize,
}

fn compute_line_starts(src: &str) -> Vec<usize> {
    let bytes = src.as_bytes();
    let mut line_starts = vec![0];
    let mut i = 0;
    while i < bytes.len() {
        match bytes[i] {
            b'\n' => {
                line_starts.push(i.saturating_add(1));
                i = i.saturating_add(1);
            }
            b'\r' => {
                if bytes.get(i.saturating_add(1)) == Some(&b'\n') {
                    line_starts.push(i.saturating_add(2));
                    i = i.saturating_add(2);
                } else {
                    line_starts.push(i.saturating_add(1));
                    i = i.saturating_add(1);
                }
            }
            _ => i = i.saturating_add(1),
        }
    }
    line_starts
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lookup_span_maps_lf_line_and_column_ranges() {
        let src = Arc::<str>::from("a\nbc\ndef");
        let mut map = SourceMap::new();
        map.add_source(SourceName::Virtual("<string>".to_string()), src, 0);

        let span = Span::new(2, 4); // "bc"
        let range = map.lookup_span(span).expect("range");
        assert_eq!(
            range,
            SourceRange {
                name: SourceName::Virtual("<string>".to_string()),
                start: LineCol { line: 2, col: 1 },
                end: LineCol { line: 2, col: 3 },
            }
        );
    }

    #[test]
    fn lookup_span_maps_crlf_line_and_column_ranges() {
        let src = Arc::<str>::from("a\r\nb\r\nc");
        let mut map = SourceMap::new();
        map.add_source(SourceName::Virtual("<string>".to_string()), src, 0);

        let span = Span::new(3, 4); // "b"
        let range = map.lookup_span(span).expect("range");
        assert_eq!(range.start, LineCol { line: 2, col: 1 });
        assert_eq!(range.end, LineCol { line: 2, col: 2 });
    }

    #[test]
    fn lookup_span_counts_unicode_scalar_columns() {
        let src = Arc::<str>::from("αβ\nγ");
        let mut map = SourceMap::new();
        map.add_source(SourceName::Virtual("<string>".to_string()), src, 0);

        let alpha_len = "α".len();
        let beta_len = "β".len();

        let span_alpha = Span::new(0, alpha_len);
        let alpha = map.lookup_span(span_alpha).expect("range");
        assert_eq!(alpha.start, LineCol { line: 1, col: 1 });
        assert_eq!(alpha.end, LineCol { line: 1, col: 2 });

        let span_beta = Span::new(alpha_len, alpha_len + beta_len);
        let beta = map.lookup_span(span_beta).expect("range");
        assert_eq!(beta.start, LineCol { line: 1, col: 2 });
        assert_eq!(beta.end, LineCol { line: 1, col: 3 });
    }

    #[test]
    fn lookup_span_supports_zero_length_spans() {
        let src = Arc::<str>::from("abc");
        let mut map = SourceMap::new();
        map.add_source(SourceName::Virtual("<string>".to_string()), src, 0);

        let start = map.lookup_span(Span::new(0, 0)).expect("range");
        assert_eq!(start.start, LineCol { line: 1, col: 1 });
        assert_eq!(start.end, LineCol { line: 1, col: 1 });

        let eof = map.lookup_span(Span::new(3, 3)).expect("range");
        assert_eq!(eof.start, LineCol { line: 1, col: 4 });
        assert_eq!(eof.end, LineCol { line: 1, col: 4 });
    }

    #[test]
    fn lookup_span_bytes_returns_file_relative_offsets() {
        let src = Arc::<str>::from("a\nbc");
        let mut map = SourceMap::new();
        map.add_source(
            SourceName::Virtual("<string>".to_string()),
            Arc::clone(&src),
            10,
        );

        let span = Span::new(12, 14); // "bc" (local 2..4)
        let range = map.lookup_span_bytes(span).expect("byte range");
        assert_eq!(
            range,
            SourceByteRange {
                name: SourceName::Virtual("<string>".to_string()),
                start: 2,
                end: 4,
            }
        );
    }

    #[test]
    fn source_text_returns_original_source() {
        let src = Arc::<str>::from("hello");
        let mut map = SourceMap::new();
        let name = SourceName::Virtual("<v>".to_string());
        map.add_source(name.clone(), Arc::clone(&src), 0);

        let got = map.source_text(&name).expect("source");
        assert_eq!(got.as_ref(), "hello");
    }

    #[test]
    fn source_names_lists_tracked_sources() {
        let mut map = SourceMap::new();
        map.add_source(
            SourceName::Virtual("a".to_string()),
            Arc::<str>::from(""),
            0,
        );
        map.add_source(
            SourceName::Virtual("b".to_string()),
            Arc::<str>::from(""),
            10,
        );

        let names = map.source_names();
        assert_eq!(
            names,
            vec![
                SourceName::Virtual("a".to_string()),
                SourceName::Virtual("b".to_string())
            ]
        );
    }
}
