use crate::analysis::{Diagnostic, DiagnosticSeverity};
use crate::lexer::{FStringPart, LexItem, Lexer, Token, TokenKind, TriviaKind};
use crate::parser::{ParseError, Parser};
use crate::source::Span;
use crate::source_map::{SourceMap, SourceName};
use std::path::Path;
use std::sync::Arc;

#[derive(Clone, Copy, Debug)]
pub struct FormatOptions {
    pub indent_width: usize,
}

impl Default for FormatOptions {
    fn default() -> Self {
        Self { indent_width: 4 }
    }
}

#[derive(Clone, Debug)]
pub struct FormatOutput {
    pub formatted: Option<String>,
    pub diagnostics: Vec<Diagnostic>,
    pub source_map: SourceMap,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum BlockKind {
    Module,
    Struct,
    Enum,
    Interface,
    Impl,
    Function,
    Control(ControlKind),
    Other,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ControlKind {
    If,
    Match,
    For,
    While,
    Loop,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ItemGroup {
    Mod,
    Use,
}

pub fn format_source(source_name: SourceName, src: &str, options: &FormatOptions) -> FormatOutput {
    let source = Arc::<str>::from(src);
    let mut source_map = SourceMap::new();
    source_map.add_source(source_name.clone(), Arc::clone(&source), 0);

    if let Err(err) = parse_eof(src) {
        return FormatOutput {
            formatted: None,
            diagnostics: vec![diagnostic_from_parse_error(
                err,
                &source_map,
                Some(source_name),
            )],
            source_map,
        };
    }

    let formatted = match format_tokens(src, options) {
        Ok(s) => s,
        Err(err) => {
            return FormatOutput {
                formatted: None,
                diagnostics: vec![diagnostic_from_parse_error(
                    err,
                    &source_map,
                    Some(source_name),
                )],
                source_map,
            };
        }
    };

    FormatOutput {
        formatted: Some(formatted),
        diagnostics: Vec::new(),
        source_map,
    }
}

pub fn format_file(path: &Path, options: &FormatOptions) -> FormatOutput {
    let src = match std::fs::read_to_string(path) {
        Ok(s) => s,
        Err(err) => {
            let mut source_map = SourceMap::new();
            let name = SourceName::Path(path.to_path_buf());
            source_map.add_source(name.clone(), Arc::<str>::from(""), 0);
            let diag = Diagnostic {
                message: format!("failed to read file: {err}"),
                severity: DiagnosticSeverity::Error,
                code: Some("io_error".to_string()),
                span: Span::new(0, 0),
                label: None,
                help: None,
                notes: Vec::new(),
                source: Some(name.clone()),
                range: None,
                byte_range: None,
            };
            return FormatOutput {
                formatted: None,
                diagnostics: vec![diag],
                source_map,
            };
        }
    };
    format_source(SourceName::Path(path.to_path_buf()), &src, options)
}

fn parse_eof(src: &str) -> Result<(), ParseError> {
    let mut parser = Parser::with_base_offset(src, 0)?;
    let _ = parser.parse_program()?;
    Ok(())
}

fn diagnostic_from_parse_error(
    err: ParseError,
    source_map: &SourceMap,
    source_name: Option<SourceName>,
) -> Diagnostic {
    let span = err.span;
    Diagnostic {
        message: err.message,
        severity: DiagnosticSeverity::Error,
        code: Some("parse_error".to_string()),
        span,
        label: None,
        help: None,
        notes: Vec::new(),
        source: source_name,
        range: source_map.lookup_span(span),
        byte_range: source_map.lookup_span_bytes(span),
    }
}

fn format_tokens(src: &str, options: &FormatOptions) -> Result<String, ParseError> {
    let mut lexer = Lexer::with_base_offset(src, 0);
    let mut items: Vec<LexItem> = Vec::new();
    loop {
        let item = lexer.next_item()?;
        let is_eof = matches!(
            &item,
            LexItem::Token(Token {
                kind: TokenKind::Eof,
                ..
            })
        );
        items.push(item);
        if is_eof {
            break;
        }
    }

    let mut out = String::new();
    let mut indent = 0usize;
    let mut at_line_start = true;
    let mut generic_depth = 0usize;
    let mut paren_depth = 0usize;
    let mut bracket_depth = 0usize;
    let mut expect_generic_lt = false;
    let mut expect_item_name = false;
    let mut block_stack: Vec<BlockKind> = Vec::new();
    let mut pending_block_kind: Option<BlockKind> = None;
    let mut current_item_group: Option<ItemGroup> = None;
    let mut control_stmt: Option<(ControlKind, usize)> = None;

    let mut prev_token: Option<Token> = None;

    let mut i = 0usize;
    while i < items.len() {
        match &items[i] {
            LexItem::Trivia(trivia) => {
                if trivia.kind == TriviaKind::Whitespace {
                    if at_line_start && trivia.text.chars().filter(|&ch| ch == '\n').count() >= 2 {
                        let next = peek_next_token_kind(&items, i + 1);
                        let prev_kind = prev_token.as_ref().map(|t| &t.kind);
                        if !matches!(prev_kind, Some(TokenKind::LBrace))
                            && !matches!(next, Some(TokenKind::RBrace))
                        {
                            blank_line(&mut out, &mut at_line_start);
                        }
                    }
                    i += 1;
                    continue;
                }

                // Comments / shebang.
                if !at_line_start && !out.ends_with([' ', '\n']) {
                    out.push(' ');
                } else {
                    push_indent(&mut out, indent, options);
                    at_line_start = false;
                }
                let text = strip_trailing_newlines(&trivia.text);
                out.push_str(text);
                newline(&mut out, &mut at_line_start);
                i += 1;
                continue;
            }
            LexItem::Token(tok) => {
                let kind = &tok.kind;
                if matches!(kind, TokenKind::Eof) {
                    break;
                }

                if expect_item_name {
                    if matches!(kind, TokenKind::Ident(_)) {
                        expect_item_name = false;
                        expect_generic_lt = true;
                    } else if !matches!(kind, TokenKind::KwPub) {
                        expect_item_name = false;
                    }
                } else if expect_generic_lt && !matches!(kind, TokenKind::Lt) {
                    // We were expecting `...<...>` but the next non-whitespace token wasn't `<`.
                    expect_generic_lt = false;
                }

                if matches!(
                    kind,
                    TokenKind::KwFn
                        | TokenKind::KwStruct
                        | TokenKind::KwEnum
                        | TokenKind::KwInterface
                        | TokenKind::KwType
                        | TokenKind::KwMod
                ) {
                    expect_item_name = true;
                }
                if matches!(kind, TokenKind::KwImpl) {
                    expect_generic_lt = true;
                }

                let in_item_container = block_stack.last().is_none()
                    || matches!(block_stack.last(), Some(BlockKind::Module));
                let in_stmt_block = block_stack
                    .iter()
                    .any(|k| matches!(k, BlockKind::Function | BlockKind::Control(_)));

                let prev_kind = prev_token.as_ref().map(|t| &t.kind);
                let stmt_start = at_line_start
                    && matches!(
                        prev_kind,
                        None | Some(TokenKind::LBrace)
                            | Some(TokenKind::Semi)
                            | Some(TokenKind::RBrace)
                    );

                if stmt_start {
                    current_item_group = None;
                }

                if in_item_container && current_item_group.is_none() {
                    if matches!(kind, TokenKind::KwMod) {
                        current_item_group = Some(ItemGroup::Mod);
                    } else if matches!(kind, TokenKind::KwUse) {
                        current_item_group = Some(ItemGroup::Use);
                    }
                }

                if in_stmt_block && stmt_start {
                    let control_kind = match kind {
                        TokenKind::KwIf => Some(ControlKind::If),
                        TokenKind::KwMatch => Some(ControlKind::Match),
                        TokenKind::KwFor => Some(ControlKind::For),
                        TokenKind::KwWhile => Some(ControlKind::While),
                        TokenKind::KwLoop => Some(ControlKind::Loop),
                        _ => None,
                    };
                    if let Some(control_kind) = control_kind {
                        // Blank line before control blocks, except at the start of the containing
                        // block.
                        if !matches!(prev_kind, None | Some(TokenKind::LBrace)) {
                            blank_line(&mut out, &mut at_line_start);
                        }
                        control_stmt = Some((control_kind, indent));
                        if pending_block_kind.is_none() {
                            pending_block_kind = Some(BlockKind::Control(control_kind));
                        }
                    }
                }

                if pending_block_kind.is_none() {
                    pending_block_kind = match kind {
                        TokenKind::KwMod => Some(BlockKind::Module),
                        TokenKind::KwStruct => Some(BlockKind::Struct),
                        TokenKind::KwEnum => Some(BlockKind::Enum),
                        TokenKind::KwInterface => Some(BlockKind::Interface),
                        TokenKind::KwImpl => Some(BlockKind::Impl),
                        TokenKind::KwFn => Some(BlockKind::Function),
                        _ => None,
                    };
                }

                if in_stmt_block && matches!(kind, TokenKind::KwElse) {
                    let next = peek_next_token_kind(&items, i + 1);
                    if next.is_some_and(|k| matches!(k, TokenKind::LBrace))
                        && pending_block_kind.is_none()
                    {
                        pending_block_kind = Some(BlockKind::Control(ControlKind::If));
                    }
                }

                // Combine `{}`.
                if matches!(kind, TokenKind::LBrace) {
                    let empty = next_non_ws_is_rbrace(&items, i + 1);
                    let block_kind = pending_block_kind.take().unwrap_or(BlockKind::Other);
                    write_token(
                        &mut out,
                        &mut at_line_start,
                        indent,
                        options,
                        prev_token.as_ref(),
                        tok,
                        generic_depth,
                    );
                    prev_token = Some(tok.clone());
                    if empty {
                        let _ = block_kind;
                        // Skip whitespace trivia between `{` and `}`.
                        let mut j = i + 1;
                        while j < items.len()
                            && matches!(&items[j], LexItem::Trivia(t) if t.kind == TriviaKind::Whitespace)
                        {
                            j += 1;
                        }
                        if j < items.len()
                            && matches!(
                                &items[j],
                                LexItem::Token(Token {
                                    kind: TokenKind::RBrace,
                                    ..
                                })
                            )
                        {
                            // Print `}` directly.
                            let rbrace = match &items[j] {
                                LexItem::Token(t) => t,
                                _ => unreachable!(),
                            };
                            write_raw(&mut out, "}", &mut at_line_start, indent, options, false);
                            prev_token = Some(rbrace.clone());
                            i = j + 1;
                            continue;
                        }
                    } else {
                        indent = indent.saturating_add(1);
                        block_stack.push(block_kind);
                        newline(&mut out, &mut at_line_start);
                        i += 1;
                        continue;
                    }
                }

                // `}` decreases indent first.
                if matches!(kind, TokenKind::RBrace) {
                    let closed_kind = block_stack.pop().unwrap_or(BlockKind::Other);
                    indent = indent.saturating_sub(1);
                    no_trailing_blank_line(&mut out);
                    if !at_line_start {
                        newline(&mut out, &mut at_line_start);
                    }
                    push_indent(&mut out, indent, options);
                    at_line_start = false;
                    out.push('}');
                    prev_token = Some(tok.clone());

                    let next = peek_next_token_kind(&items, i + 1);
                    match next.as_ref() {
                        Some(TokenKind::KwElse) => {
                            out.push(' ');
                            at_line_start = false;
                        }
                        Some(
                            TokenKind::Comma
                            | TokenKind::Semi
                            | TokenKind::RParen
                            | TokenKind::RBracket,
                        ) => {}
                        Some(_) => {
                            newline(&mut out, &mut at_line_start);
                            let in_item_container = block_stack.last().is_none()
                                || matches!(block_stack.last(), Some(BlockKind::Module));
                            let in_member_container = matches!(
                                block_stack.last(),
                                Some(BlockKind::Impl) | Some(BlockKind::Interface)
                            );

                            let next_is_item = in_item_container && next_starts_item(&items, i + 1);
                            let next_is_method =
                                in_member_container && next_starts_fn_like_member(&items, i + 1);

                            let closed_is_item = matches!(
                                closed_kind,
                                BlockKind::Module
                                    | BlockKind::Struct
                                    | BlockKind::Enum
                                    | BlockKind::Interface
                                    | BlockKind::Impl
                                    | BlockKind::Function
                            );
                            if closed_is_item && next_is_item {
                                blank_line(&mut out, &mut at_line_start);
                            }
                            if closed_kind == BlockKind::Function && next_is_method {
                                blank_line(&mut out, &mut at_line_start);
                            }

                            if let Some((control_kind, stmt_indent)) = control_stmt {
                                if closed_kind == BlockKind::Control(control_kind)
                                    && indent == stmt_indent
                                {
                                    if !matches!(next.as_ref(), Some(TokenKind::RBrace)) {
                                        blank_line(&mut out, &mut at_line_start);
                                    }
                                    control_stmt = None;
                                }
                            }
                        }
                        None => newline(&mut out, &mut at_line_start),
                    }

                    i += 1;
                    continue;
                }

                // Shift operators: `<<`, `>>`, `>>>` (must be adjacent in the source to be valid).
                if matches!(kind, TokenKind::Lt)
                    && let Some(op_len) = adjacent_repeat_count(&items, i, TokenKind::Lt, 3)
                    && op_len >= 2
                {
                    let op = if op_len == 2 { "<<" } else { "<<<" };
                    write_operator(
                        &mut out,
                        &mut at_line_start,
                        indent,
                        options,
                        prev_token.as_ref(),
                        op,
                    );
                    // Consume N tokens.
                    i += op_len;
                    prev_token = Some(tok.clone());
                    continue;
                }
                if matches!(kind, TokenKind::Gt)
                    && generic_depth == 0
                    && let Some(op_len) = adjacent_repeat_count(&items, i, TokenKind::Gt, 3)
                    && op_len >= 2
                {
                    let after = peek_nth_token_kind(&items, i, op_len);
                    if after.is_some_and(|kind| is_expr_start_token(&kind)) {
                        let op = if op_len == 2 { ">>" } else { ">>>" };
                        write_operator(
                            &mut out,
                            &mut at_line_start,
                            indent,
                            options,
                            prev_token.as_ref(),
                            op,
                        );
                        i += op_len;
                        prev_token = Some(tok.clone());
                        continue;
                    }
                }

                // Generic angle tracking (heuristic).
                if matches!(kind, TokenKind::Lt)
                    && (expect_generic_lt || should_treat_lt_as_generic(prev_token.as_ref()))
                {
                    write_raw(&mut out, "<", &mut at_line_start, indent, options, false);
                    generic_depth = generic_depth.saturating_add(1);
                    expect_generic_lt = false;
                    prev_token = Some(tok.clone());
                    i += 1;
                    continue;
                }
                if matches!(kind, TokenKind::Gt) && generic_depth > 0 {
                    write_raw(&mut out, ">", &mut at_line_start, indent, options, false);
                    generic_depth = generic_depth.saturating_sub(1);
                    prev_token = Some(tok.clone());
                    i += 1;
                    continue;
                }

                // Commas at the "brace list" level (e.g., match arms, struct fields, use groups)
                // become line breaks, but commas inside `(...)` and `[...]` stay inline.
                if matches!(kind, TokenKind::Comma) {
                    write_raw(&mut out, ",", &mut at_line_start, indent, options, false);
                    prev_token = Some(tok.clone());
                    if generic_depth == 0 && indent > 0 && paren_depth == 0 && bracket_depth == 0 {
                        newline(&mut out, &mut at_line_start);
                    }
                    i += 1;
                    continue;
                }

                // Semicolons always end the line.
                if matches!(kind, TokenKind::Semi) {
                    write_raw(&mut out, ";", &mut at_line_start, indent, options, false);
                    prev_token = Some(tok.clone());
                    newline(&mut out, &mut at_line_start);

                    if let Some((_control_kind, stmt_indent)) = control_stmt {
                        if indent == stmt_indent {
                            let next = peek_next_token_kind(&items, i + 1);
                            if next.is_some_and(|k| !matches!(k, TokenKind::RBrace)) {
                                blank_line(&mut out, &mut at_line_start);
                            }
                            control_stmt = None;
                        }
                    }

                    let in_item_container = block_stack.last().is_none()
                        || matches!(block_stack.last(), Some(BlockKind::Module));
                    let in_member_container = matches!(
                        block_stack.last(),
                        Some(BlockKind::Impl) | Some(BlockKind::Interface)
                    );

                    let next_group = next_item_group(&items, i + 1);
                    if in_item_container
                        && matches!(current_item_group, Some(ItemGroup::Mod))
                        && next_group != Some(ItemGroup::Mod)
                    {
                        blank_line(&mut out, &mut at_line_start);
                    }
                    if in_item_container
                        && matches!(current_item_group, Some(ItemGroup::Use))
                        && next_group != Some(ItemGroup::Use)
                    {
                        blank_line(&mut out, &mut at_line_start);
                    }

                    let next_starts_method =
                        in_member_container && next_starts_fn_like_member(&items, i + 1);
                    if next_starts_method {
                        blank_line(&mut out, &mut at_line_start);
                    }

                    current_item_group = None;
                    pending_block_kind = None;
                    i += 1;
                    continue;
                }

                write_token(
                    &mut out,
                    &mut at_line_start,
                    indent,
                    options,
                    prev_token.as_ref(),
                    tok,
                    generic_depth,
                );
                prev_token = Some(tok.clone());
                match kind {
                    TokenKind::LParen => paren_depth = paren_depth.saturating_add(1),
                    TokenKind::RParen => paren_depth = paren_depth.saturating_sub(1),
                    TokenKind::LBracket => bracket_depth = bracket_depth.saturating_add(1),
                    TokenKind::RBracket => bracket_depth = bracket_depth.saturating_sub(1),
                    _ => {}
                }
                i += 1;
            }
        }
    }

    // Ensure a single trailing newline.
    while out.ends_with('\n') {
        out.pop();
    }
    out.push('\n');
    Ok(out)
}

fn push_indent(out: &mut String, indent: usize, options: &FormatOptions) {
    for _ in 0..indent.saturating_mul(options.indent_width) {
        out.push(' ');
    }
}

fn newline(out: &mut String, at_line_start: &mut bool) {
    if !out.ends_with('\n') {
        out.push('\n');
    }
    // Cap at a single blank line.
    while out.ends_with("\n\n\n") {
        out.pop();
    }
    *at_line_start = true;
}

fn blank_line(out: &mut String, at_line_start: &mut bool) {
    if out.is_empty() {
        *at_line_start = true;
        return;
    }
    if !out.ends_with('\n') {
        out.push('\n');
    }
    if !out.ends_with("\n\n") {
        out.push('\n');
    }
    while out.ends_with("\n\n\n") {
        out.pop();
    }
    *at_line_start = true;
}

fn no_trailing_blank_line(out: &mut String) {
    while out.ends_with("\n\n") {
        out.pop();
    }
}

fn strip_trailing_newlines(s: &str) -> &str {
    s.trim_end_matches(['\n', '\r'])
}

fn write_operator(
    out: &mut String,
    at_line_start: &mut bool,
    indent: usize,
    options: &FormatOptions,
    prev: Option<&Token>,
    op: &str,
) {
    if *at_line_start {
        push_indent(out, indent, options);
        *at_line_start = false;
    }
    let _ = prev;
    if !out.ends_with([' ', '\n']) {
        out.push(' ');
    }
    out.push_str(op);
    out.push(' ');
    *at_line_start = false;
}

fn write_raw(
    out: &mut String,
    raw: &str,
    at_line_start: &mut bool,
    indent: usize,
    options: &FormatOptions,
    ensure_space_before: bool,
) {
    if *at_line_start {
        push_indent(out, indent, options);
        *at_line_start = false;
    } else if ensure_space_before && !out.ends_with([' ', '\n']) {
        out.push(' ');
    }
    out.push_str(raw);
}

fn write_token(
    out: &mut String,
    at_line_start: &mut bool,
    indent: usize,
    options: &FormatOptions,
    prev: Option<&Token>,
    tok: &Token,
    generic_depth: usize,
) {
    if *at_line_start {
        push_indent(out, indent, options);
        *at_line_start = false;
    }

    let raw = token_text(&tok.kind);
    let need_space = needs_space(prev.map(|t| &t.kind), Some(&tok.kind), generic_depth > 0);
    if need_space && !out.ends_with([' ', '\n']) {
        out.push(' ');
    }
    out.push_str(&raw);
    *at_line_start = false;
}

fn token_text(kind: &TokenKind) -> String {
    match kind {
        TokenKind::Eof => String::new(),
        TokenKind::Ident(s) => s.clone(),
        TokenKind::Int(v) => v.to_string(),
        TokenKind::Float(v) => v.to_string(),
        TokenKind::Char(c) => format!("'{}'", escape_char(*c)),
        TokenKind::String(s) => format!("\"{}\"", escape_string(s)),
        TokenKind::Bytes(bytes) => format!("b\"{}\"", escape_bytes(bytes)),
        TokenKind::FString(parts) => format!("f\"{}\"", escape_fstring(parts)),

        TokenKind::KwPub => "pub".to_string(),
        TokenKind::KwUse => "use".to_string(),
        TokenKind::KwMod => "mod".to_string(),
        TokenKind::KwAs => "as".to_string(),
        TokenKind::KwIs => "is".to_string(),
        TokenKind::KwFn => "fn".to_string(),
        TokenKind::KwIntrinsic => "intrinsic".to_string(),
        TokenKind::KwCont => "cont".to_string(),
        TokenKind::KwLet => "let".to_string(),
        TokenKind::KwConst => "const".to_string(),
        TokenKind::KwReadonly => "readonly".to_string(),
        TokenKind::KwStatic => "static".to_string(),
        TokenKind::KwDerive => "derive".to_string(),
        TokenKind::KwStruct => "struct".to_string(),
        TokenKind::KwEnum => "enum".to_string(),
        TokenKind::KwInterface => "interface".to_string(),
        TokenKind::KwImpl => "impl".to_string(),
        TokenKind::KwType => "type".to_string(),
        TokenKind::KwIf => "if".to_string(),
        TokenKind::KwElse => "else".to_string(),
        TokenKind::KwMatch => "match".to_string(),
        TokenKind::KwReturn => "return".to_string(),
        TokenKind::KwLoop => "loop".to_string(),
        TokenKind::KwWhile => "while".to_string(),
        TokenKind::KwFor => "for".to_string(),
        TokenKind::KwIn => "in".to_string(),
        TokenKind::KwBreak => "break".to_string(),
        TokenKind::KwContinue => "continue".to_string(),

        TokenKind::LParen => "(".to_string(),
        TokenKind::RParen => ")".to_string(),
        TokenKind::LBrace => "{".to_string(),
        TokenKind::RBrace => "}".to_string(),
        TokenKind::LBracket => "[".to_string(),
        TokenKind::RBracket => "]".to_string(),
        TokenKind::Comma => ",".to_string(),
        TokenKind::Colon => ":".to_string(),
        TokenKind::Semi => ";".to_string(),
        TokenKind::Dot => ".".to_string(),
        TokenKind::DotDot => "..".to_string(),
        TokenKind::ColonColon => "::".to_string(),
        TokenKind::Turbofish => "::<".to_string(),

        TokenKind::Arrow => "->".to_string(),
        TokenKind::FatArrow => "=>".to_string(),
        TokenKind::Assign => "=".to_string(),
        TokenKind::Plus => "+".to_string(),
        TokenKind::Minus => "-".to_string(),
        TokenKind::Star => "*".to_string(),
        TokenKind::Slash => "/".to_string(),
        TokenKind::Percent => "%".to_string(),
        TokenKind::EqEq => "==".to_string(),
        TokenKind::NotEq => "!=".to_string(),
        TokenKind::Lt => "<".to_string(),
        TokenKind::LtEq => "<=".to_string(),
        TokenKind::Gt => ">".to_string(),
        TokenKind::GtEq => ">=".to_string(),
        TokenKind::AndAnd => "&&".to_string(),
        TokenKind::OrOr => "||".to_string(),
        TokenKind::Amp => "&".to_string(),
        TokenKind::Caret => "^".to_string(),
        TokenKind::Bang => "!".to_string(),
        TokenKind::Question => "?".to_string(),
        TokenKind::At => "@".to_string(),
        TokenKind::Pipe => "|".to_string(),
    }
}

fn escape_char(c: char) -> String {
    match c {
        '\\' => "\\\\".to_string(),
        '\'' => "\\'".to_string(),
        '\n' => "\\n".to_string(),
        '\r' => "\\r".to_string(),
        '\t' => "\\t".to_string(),
        '\0' => "\\0".to_string(),
        other => other.to_string(),
    }
}

fn escape_string(s: &str) -> String {
    let mut out = String::new();
    for ch in s.chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            '\0' => out.push_str("\\0"),
            other => out.push(other),
        }
    }
    out
}

fn escape_bytes(bytes: &[u8]) -> String {
    let mut out = String::new();
    for &b in bytes {
        match b {
            b'\\' => out.push_str("\\\\"),
            b'"' => out.push_str("\\\""),
            b'\n' => out.push_str("\\n"),
            b'\r' => out.push_str("\\r"),
            b'\t' => out.push_str("\\t"),
            b'\0' => out.push_str("\\0"),
            0x20..=0x7E => out.push(b as char),
            _ => out.push_str(&format!("\\x{b:02X}")),
        }
    }
    out
}

fn escape_fstring(parts: &[FStringPart]) -> String {
    let mut out = String::new();
    for part in parts {
        match part {
            FStringPart::Text(text) => {
                for ch in text.chars() {
                    match ch {
                        '{' => out.push_str("{{"),
                        '}' => out.push_str("}}"),
                        '\\' => out.push_str("\\\\"),
                        '"' => out.push_str("\\\""),
                        '\n' => out.push_str("\\n"),
                        '\r' => out.push_str("\\r"),
                        '\t' => out.push_str("\\t"),
                        '\0' => out.push_str("\\0"),
                        other => out.push(other),
                    }
                }
            }
            FStringPart::Expr { src, .. } => {
                out.push('{');
                out.push_str(src.trim());
                out.push('}');
            }
        }
    }
    out
}

fn needs_space(prev: Option<&TokenKind>, curr: Option<&TokenKind>, in_generic: bool) -> bool {
    let Some(curr) = curr else {
        return false;
    };

    if prev.is_none() {
        return false;
    }
    let prev = prev.unwrap();

    if matches!(
        curr,
        TokenKind::Comma
            | TokenKind::Semi
            | TokenKind::Dot
            | TokenKind::RParen
            | TokenKind::RBracket
            | TokenKind::RBrace
            | TokenKind::Colon
            | TokenKind::ColonColon
    ) {
        return false;
    }
    if matches!(
        prev,
        TokenKind::LParen
            | TokenKind::LBracket
            | TokenKind::Dot
            | TokenKind::ColonColon
            | TokenKind::At
            | TokenKind::Turbofish
    ) {
        return false;
    }

    // `as?` should not have whitespace between `as` and `?`.
    if matches!(prev, TokenKind::KwAs) && matches!(curr, TokenKind::Question) {
        return false;
    }

    // Tight generics: `Foo<T>`.
    if in_generic {
        if matches!(curr, TokenKind::Gt) {
            return false;
        }
        if matches!(prev, TokenKind::Lt) {
            return false;
        }
    }

    // Space after commas, except before a closing delimiter.
    if matches!(prev, TokenKind::Comma) {
        return !matches!(
            curr,
            TokenKind::RParen | TokenKind::RBracket | TokenKind::RBrace | TokenKind::Gt
        );
    }

    // Space after `:`, e.g. `x: int`, `T: Iterator`.
    if matches!(prev, TokenKind::Colon) {
        return true;
    }

    // Space around arrows and fat arrows and assignment.
    if is_binary_op(prev) || is_binary_op(curr) {
        return true;
    }

    // Keywords generally want a space before the following token.
    if is_keyword(prev) {
        return true;
    }

    // Words next to words.
    if is_word(prev) && is_word(curr) {
        return true;
    }

    // `)` followed by word, e.g. `fn f() -> ...`.
    if matches!(prev, TokenKind::RParen | TokenKind::RBracket) && is_word(curr) {
        return true;
    }

    // Word before `{` / `[` often wants a space.
    if is_word(prev) && matches!(curr, TokenKind::LBrace) {
        return true;
    }
    if matches!(curr, TokenKind::LBrace) && matches!(prev, TokenKind::RParen | TokenKind::RBracket)
    {
        return true;
    }

    false
}

fn is_word(kind: &TokenKind) -> bool {
    matches!(
        kind,
        TokenKind::Ident(_)
            | TokenKind::Int(_)
            | TokenKind::Float(_)
            | TokenKind::Char(_)
            | TokenKind::String(_)
            | TokenKind::Bytes(_)
            | TokenKind::FString(_)
    ) || is_keyword(kind)
}

fn is_keyword(kind: &TokenKind) -> bool {
    matches!(
        kind,
        TokenKind::KwPub
            | TokenKind::KwUse
            | TokenKind::KwMod
            | TokenKind::KwFn
            | TokenKind::KwIntrinsic
            | TokenKind::KwLet
            | TokenKind::KwConst
            | TokenKind::KwReadonly
            | TokenKind::KwStatic
            | TokenKind::KwDerive
            | TokenKind::KwStruct
            | TokenKind::KwEnum
            | TokenKind::KwInterface
            | TokenKind::KwImpl
            | TokenKind::KwType
            | TokenKind::KwIf
            | TokenKind::KwElse
            | TokenKind::KwMatch
            | TokenKind::KwReturn
            | TokenKind::KwLoop
            | TokenKind::KwWhile
            | TokenKind::KwFor
            | TokenKind::KwIn
            | TokenKind::KwBreak
            | TokenKind::KwContinue
    )
}

fn is_binary_op(kind: &TokenKind) -> bool {
    matches!(
        kind,
        TokenKind::Arrow
            | TokenKind::FatArrow
            | TokenKind::Assign
            | TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Star
            | TokenKind::Slash
            | TokenKind::Percent
            | TokenKind::EqEq
            | TokenKind::NotEq
            | TokenKind::Lt
            | TokenKind::LtEq
            | TokenKind::Gt
            | TokenKind::GtEq
            | TokenKind::AndAnd
            | TokenKind::OrOr
            | TokenKind::Amp
            | TokenKind::Caret
            | TokenKind::Pipe
            | TokenKind::KwAs
            | TokenKind::KwIs
    )
}

fn is_expr_start_token(kind: &TokenKind) -> bool {
    matches!(
        kind,
        TokenKind::Ident(_)
            | TokenKind::Int(_)
            | TokenKind::Float(_)
            | TokenKind::Char(_)
            | TokenKind::String(_)
            | TokenKind::Bytes(_)
            | TokenKind::FString(_)
            | TokenKind::LParen
            | TokenKind::LBracket
            | TokenKind::LBrace
            | TokenKind::KwIf
            | TokenKind::KwMatch
            | TokenKind::KwLoop
            | TokenKind::KwWhile
            | TokenKind::KwFor
            | TokenKind::Pipe
            | TokenKind::At
            | TokenKind::Bang
            | TokenKind::Minus
    )
}

fn should_treat_lt_as_generic(prev: Option<&Token>) -> bool {
    let Some(prev) = prev else { return false };
    let TokenKind::Ident(name) = &prev.kind else {
        return false;
    };
    name.chars()
        .next()
        .is_some_and(|ch| ch.is_ascii_uppercase())
}

fn adjacent_repeat_count(
    items: &[LexItem],
    start_idx: usize,
    kind: TokenKind,
    max: usize,
) -> Option<usize> {
    let LexItem::Token(first) = &items[start_idx] else {
        return None;
    };
    if first.kind != kind {
        return None;
    }
    let mut count = 1usize;
    let mut prev_span_end = first.span.end;
    let mut idx = start_idx + 1;
    while count < max {
        let Some(next_tok) = peek_next_token(items, idx) else {
            break;
        };
        if next_tok.kind != kind {
            break;
        }
        if next_tok.span.start != prev_span_end {
            break;
        }
        prev_span_end = next_tok.span.end;
        count += 1;
        idx += 1;
    }
    Some(count)
}

fn peek_next_token(items: &[LexItem], mut idx: usize) -> Option<&Token> {
    while idx < items.len() {
        match &items[idx] {
            LexItem::Trivia(t) if t.kind == TriviaKind::Whitespace => idx += 1,
            LexItem::Trivia(_) => return None,
            LexItem::Token(t) => return Some(t),
        }
    }
    None
}

fn peek_next_token_kind(items: &[LexItem], idx: usize) -> Option<TokenKind> {
    peek_next_token(items, idx).map(|t| t.kind.clone())
}

fn peek_nth_token_kind(items: &[LexItem], start: usize, n: usize) -> Option<TokenKind> {
    let mut idx = start;
    let mut seen = 0usize;
    while idx < items.len() {
        match &items[idx] {
            LexItem::Trivia(t) if t.kind == TriviaKind::Whitespace => idx += 1,
            LexItem::Trivia(_) => return None,
            LexItem::Token(t) => {
                if seen == n {
                    return Some(t.kind.clone());
                }
                seen += 1;
                idx += 1;
            }
        }
    }
    None
}

fn next_non_ws_is_rbrace(items: &[LexItem], mut idx: usize) -> bool {
    while idx < items.len() {
        match &items[idx] {
            LexItem::Trivia(t) if t.kind == TriviaKind::Whitespace => idx += 1,
            LexItem::Trivia(_) => return false,
            LexItem::Token(Token {
                kind: TokenKind::RBrace,
                ..
            }) => return true,
            _ => return false,
        }
    }
    false
}

fn next_starts_item(items: &[LexItem], mut idx: usize) -> bool {
    while idx < items.len() {
        match &items[idx] {
            LexItem::Trivia(t) if t.kind == TriviaKind::Whitespace => idx += 1,
            LexItem::Trivia(_) => return false,
            LexItem::Token(t) => {
                return matches!(
                    t.kind,
                    TokenKind::KwPub
                        | TokenKind::KwFn
                        | TokenKind::KwIntrinsic
                        | TokenKind::KwStruct
                        | TokenKind::KwEnum
                        | TokenKind::KwInterface
                        | TokenKind::KwImpl
                        | TokenKind::KwMod
                        | TokenKind::KwUse
                        | TokenKind::KwDerive
                );
            }
        }
    }
    false
}

fn next_item_group(items: &[LexItem], idx: usize) -> Option<ItemGroup> {
    let first = peek_next_token_kind(items, idx)?;
    match first {
        TokenKind::KwMod => Some(ItemGroup::Mod),
        TokenKind::KwUse => Some(ItemGroup::Use),
        TokenKind::KwPub => {
            let second = peek_nth_token_kind(items, idx, 1)?;
            match second {
                TokenKind::KwMod => Some(ItemGroup::Mod),
                TokenKind::KwUse => Some(ItemGroup::Use),
                _ => None,
            }
        }
        _ => None,
    }
}

fn next_starts_fn_like_member(items: &[LexItem], idx: usize) -> bool {
    let mut n = 0usize;
    // Skip a few common modifiers (pub/readonly/static/intrinsic) and then look for `fn`.
    for _ in 0..6 {
        let Some(kind) = peek_nth_token_kind(items, idx, n) else {
            return false;
        };
        match kind {
            TokenKind::KwPub
            | TokenKind::KwReadonly
            | TokenKind::KwStatic
            | TokenKind::KwIntrinsic => {
                n += 1;
                continue;
            }
            TokenKind::KwFn => return true,
            _ => return false,
        }
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;

    fn fmt(src: &str) -> String {
        let options = FormatOptions::default();
        let out = format_source(SourceName::Virtual("<test>".to_string()), src, &options);
        assert!(
            out.diagnostics.is_empty(),
            "expected no diagnostics, got {:#?}",
            out.diagnostics
        );
        out.formatted.expect("expected formatted output")
    }

    #[test]
    fn keeps_commas_inline_in_parens_and_brackets() {
        let formatted = fmt("fn foo(a:int,b:int,c:int)->int{a+b+c}\n\
             fn main()->int{let xs=[1,2,3];foo(1,2,3)}\n");

        assert!(formatted.contains("fn main() -> int {"));
        assert!(formatted.contains("[1, 2, 3]"));
        assert!(formatted.contains("foo(1, 2, 3)"));
        assert!(!formatted.contains("foo(1,\n"));
        assert!(!formatted.contains("[1,\n"));
    }

    #[test]
    fn breaks_match_arms_after_commas() {
        let formatted = fmt("fn main()->int{match 0{0=>1,_=>2,}}\n");
        assert!(formatted.contains("0 => 1,\n        _ => 2,"));
    }

    #[test]
    fn tight_impl_generics() {
        let formatted = fmt("interface Iterator { fn next() -> int; }\n\
             struct ZipIterator<T1:Iterator,T2:Iterator>{a:T1,b:T2,}\n\
             impl<T1:Iterator,T2:Iterator> ZipIterator<T1,T2>{fn foo()->int{0}}\n\
             fn main()->int{0}\n");
        assert!(formatted.contains("impl<T1: Iterator, T2: Iterator> ZipIterator<T1, T2>"));
        assert!(!formatted.contains("impl <"));
        assert!(!formatted.contains("Iterator >"));
    }

    #[test]
    fn blank_lines_between_use_group_and_items() {
        let formatted = fmt("use a::b;\nuse c::d;\nstruct S{x:int}\nfn main()->int{0}\n");
        assert!(formatted.contains("use a::b;\nuse c::d;\n\nstruct S"));
        assert!(!formatted.contains("use a::b;\n\nuse c::d;"));
        assert!(formatted.contains("}\n\nfn main()"));
    }

    #[test]
    fn blank_lines_between_methods_in_interface_and_impl() {
        let formatted = fmt("interface I{fn a()->int;fn b()->int;}\n\
             struct S{}\n\
             impl I for S{fn a()->int{1}fn b()->int{2}}\n\
             fn main()->int{0}\n");
        assert!(formatted.contains("fn a() -> int;\n\n    fn b() -> int;"));
        assert!(formatted.contains("}\n\n    fn b() -> int"));
    }

    #[test]
    fn blank_lines_around_control_blocks_and_compress_multiple_blank_lines() {
        let formatted = fmt("fn main()->int{\n\
             let x=0;\n\
             \n\
             \n\
             if x==0{1}else{2};\n\
             \n\
             let y=1;\n\
             y\n\
             }\n");
        assert!(formatted.contains("{\n    let x = 0;\n\n    if x == 0 {"));
        assert!(formatted.contains("};\n\n    let y = 1;"));
        assert!(!formatted.contains("\n\n\n"));
        assert!(!formatted.contains("{\n\n"));
        assert!(!formatted.contains("y\n\n}"));
    }
}
