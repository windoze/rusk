use crate::source::Span;
use std::path::PathBuf;
use std::sync::Arc;

/// A display name for a source (either a filesystem path or a virtual label like "<string>").
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum SourceName {
    Path(PathBuf),
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
pub(crate) struct SourceMap {
    files: Vec<SourceFile>,
}

impl SourceMap {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    pub(crate) fn add_source(&mut self, name: SourceName, src: Arc<str>, base_offset: usize) {
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

    pub(crate) fn lookup_span(&self, span: Span) -> Option<SourceRange> {
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

    pub(crate) fn render_span_location(&self, span: Span) -> Option<String> {
        let range = self.lookup_span(span)?;
        Some(range.render_location())
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct LineCol {
    pub(crate) line: usize,
    pub(crate) col: usize,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct SourceRange {
    pub(crate) name: SourceName,
    pub(crate) start: LineCol,
    pub(crate) end: LineCol,
}

impl SourceRange {
    pub(crate) fn render_location(&self) -> String {
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
}
