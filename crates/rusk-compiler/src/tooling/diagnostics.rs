use crate::analysis::{Diagnostic, DiagnosticSeverity};
use crate::source_map::{LineCol, SourceMap, SourceName, SourceRange};

#[derive(Clone, Copy, Debug)]
pub struct RenderOptions {
    pub context_before: usize,
    pub context_after: usize,
}

impl Default for RenderOptions {
    fn default() -> Self {
        Self {
            context_before: 1,
            context_after: 1,
        }
    }
}

pub fn render_human(diag: &Diagnostic, source_map: &SourceMap) -> String {
    render_human_with_options(diag, source_map, &RenderOptions::default())
}

pub fn render_human_with_options(
    diag: &Diagnostic,
    source_map: &SourceMap,
    options: &RenderOptions,
) -> String {
    let mut out = String::new();

    let sev = match diag.severity {
        DiagnosticSeverity::Error => "error",
        DiagnosticSeverity::Warning => "warning",
    };
    if let Some(code) = diag.code.as_deref() {
        out.push_str(&format!("{sev}[{code}]: {}\n", diag.message));
    } else {
        out.push_str(&format!("{sev}: {}\n", diag.message));
    }

    let range = diag
        .range
        .clone()
        .or_else(|| source_map.lookup_span(diag.span));

    let source_name = diag
        .source
        .clone()
        .or_else(|| range.as_ref().map(|r| r.name.clone()))
        .or_else(|| {
            diag.byte_range
                .as_ref()
                .map(|r| r.name.clone())
                .or_else(|| source_map.lookup_span_bytes(diag.span).map(|r| r.name))
        });

    if let (Some(source_name), Some(range)) = (source_name, range) {
        let loc = format_location(&source_name, range.start);
        out.push_str(&format!("  --> {loc}\n"));
        out.push_str("   |\n");

        if let Some(src) = source_map.source_text(&source_name) {
            let src = src.as_ref();
            out.push_str(&render_snippet(src, &range, diag.label.as_deref(), options));
            out.push_str("   |\n");
        }
    }

    if let Some(help) = diag.help.as_deref() {
        out.push_str(&format!("   = help: {help}\n"));
    }
    for note in &diag.notes {
        out.push_str(&format!("   = note: {note}\n"));
    }

    out
}

fn format_location(name: &SourceName, pos: LineCol) -> String {
    format!("{}:{}:{}", render_source_name(name), pos.line, pos.col)
}

fn render_source_name(name: &SourceName) -> String {
    match name {
        SourceName::Path(path) => path.display().to_string(),
        SourceName::Virtual(label) => label.clone(),
    }
}

fn render_snippet(
    src: &str,
    range: &SourceRange,
    label: Option<&str>,
    options: &RenderOptions,
) -> String {
    let mut out = String::new();

    let line_starts = compute_line_starts(src);
    let total_lines = line_starts.len();
    let start_line = range.start.line.saturating_sub(1);
    let end_line = range
        .end
        .line
        .saturating_sub(1)
        .min(total_lines.saturating_sub(1));

    let first = start_line.saturating_sub(options.context_before);
    let last = (end_line.saturating_add(options.context_after)).min(total_lines.saturating_sub(1));

    let width = (last.saturating_add(1)).to_string().len().max(1);

    for line_idx in first..=last {
        let line_no = line_idx + 1;
        let line = get_line(src, &line_starts, line_idx);
        out.push_str(&format!("{line_no:>width$} | {line}\n"));

        if line_idx == start_line {
            let caret = render_caret_line(range, width, label);
            out.push_str(&caret);
        }
    }

    out
}

fn render_caret_line(range: &SourceRange, width: usize, label: Option<&str>) -> String {
    let start_col = range.start.col.saturating_sub(1);
    let end_col_excl = if range.start.line == range.end.line {
        range
            .end
            .col
            .saturating_sub(1)
            .max(start_col.saturating_add(1))
    } else {
        start_col.saturating_add(1)
    };
    let spaces = " ".repeat(start_col);

    let caret_len = end_col_excl.saturating_sub(start_col).max(1);
    let carets = "^".repeat(caret_len);

    let mut out = String::new();
    out.push_str(&format!("{:>width$} | {spaces}{carets}", ""));
    if let Some(msg) = label
        && !msg.is_empty()
    {
        out.push(' ');
        out.push_str(msg);
    }
    out.push('\n');
    out
}

fn compute_line_starts(src: &str) -> Vec<usize> {
    let mut starts = vec![0usize];
    for (idx, b) in src.bytes().enumerate() {
        if b == b'\n' {
            starts.push(idx + 1);
        }
    }
    if starts.is_empty() {
        starts.push(0);
    }
    starts
}

fn get_line(src: &str, line_starts: &[usize], line_idx: usize) -> String {
    let start = *line_starts.get(line_idx).unwrap_or(&0);
    let end = *line_starts.get(line_idx + 1).unwrap_or(&src.len());
    let end = end.min(src.len());
    let mut line = src[start..end].to_string();
    if line.ends_with('\n') {
        line.pop();
        if line.ends_with('\r') {
            line.pop();
        }
    } else if line.ends_with('\r') {
        line.pop();
    }
    line
}
