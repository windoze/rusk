use crate::source::Span;
use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind {
    Eof,

    Ident(String),
    Int(i64),
    Float(f64),
    String(String),
    Bytes(Vec<u8>),
    FString(Vec<FStringPart>),

    // Keywords.
    KwPub,
    KwUse,
    KwMod,
    KwAs,
    KwIs,
    KwFn,
    KwCont,
    KwLet,
    KwConst,
    KwReadonly,
    KwStatic,
    KwStruct,
    KwEnum,
    KwInterface,
    KwImpl,
    KwIf,
    KwElse,
    KwMatch,
    KwReturn,
    KwLoop,
    KwWhile,
    KwFor,
    KwIn,
    KwBreak,
    KwContinue,

    // Delimiters.
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Comma,
    Colon,
    Semi,
    Dot,
    DotDot,
    ColonColon,

    // Operators.
    Arrow,
    FatArrow,
    Assign,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    EqEq,
    NotEq,
    Lt,
    LtEq,
    Gt,
    GtEq,
    AndAnd,
    OrOr,
    Bang,
    Question,
    At,
    Pipe,
}

#[derive(Clone, Debug, PartialEq)]
pub enum FStringPart {
    Text(String),
    Expr { src: String, base_offset: usize },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LexError {
    pub message: String,
    pub span: Span,
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} at {}..{}",
            self.message, self.span.start, self.span.end
        )
    }
}

impl std::error::Error for LexError {}

pub struct Lexer<'a> {
    src: &'a str,
    pos: usize,
    base_offset: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            src,
            pos: 0,
            base_offset: 0,
        }
    }

    pub fn with_base_offset(src: &'a str, base_offset: usize) -> Self {
        Self {
            src,
            pos: 0,
            base_offset,
        }
    }

    pub fn next_token(&mut self) -> Result<Token, LexError> {
        self.skip_ws_and_comments()?;

        let start = self.pos;
        let Some(ch) = self.peek_char() else {
            return Ok(Token {
                kind: TokenKind::Eof,
                span: self.span(start, start),
            });
        };

        // Bytes literal: b"..."
        if ch == 'b' && self.peek_next_char() == Some('"') {
            return self.lex_bytes();
        }

        // Formatted string literal: f"..."
        if ch == 'f' && self.peek_next_char() == Some('"') {
            return self.lex_fstring();
        }

        // Identifier or keyword.
        if is_ident_start(ch) {
            return Ok(self.lex_ident_or_keyword());
        }

        // Number.
        if ch.is_ascii_digit() {
            return self.lex_number();
        }

        // String literal.
        if ch == '"' {
            return self.lex_string();
        }

        // Punctuation / operators.
        let kind = match ch {
            '(' => {
                self.bump_char();
                TokenKind::LParen
            }
            ')' => {
                self.bump_char();
                TokenKind::RParen
            }
            '{' => {
                self.bump_char();
                TokenKind::LBrace
            }
            '}' => {
                self.bump_char();
                TokenKind::RBrace
            }
            '[' => {
                self.bump_char();
                TokenKind::LBracket
            }
            ']' => {
                self.bump_char();
                TokenKind::RBracket
            }
            ',' => {
                self.bump_char();
                TokenKind::Comma
            }
            ';' => {
                self.bump_char();
                TokenKind::Semi
            }
            '.' => {
                self.bump_char();
                if self.peek_char() == Some('.') {
                    self.bump_char();
                    TokenKind::DotDot
                } else {
                    TokenKind::Dot
                }
            }
            ':' => {
                self.bump_char();
                if self.peek_char() == Some(':') {
                    self.bump_char();
                    TokenKind::ColonColon
                } else {
                    TokenKind::Colon
                }
            }
            '-' => {
                self.bump_char();
                if self.peek_char() == Some('>') {
                    self.bump_char();
                    TokenKind::Arrow
                } else {
                    TokenKind::Minus
                }
            }
            '=' => {
                self.bump_char();
                match self.peek_char() {
                    Some('=') => {
                        self.bump_char();
                        TokenKind::EqEq
                    }
                    Some('>') => {
                        self.bump_char();
                        TokenKind::FatArrow
                    }
                    _ => TokenKind::Assign,
                }
            }
            '!' => {
                self.bump_char();
                if self.peek_char() == Some('=') {
                    self.bump_char();
                    TokenKind::NotEq
                } else {
                    TokenKind::Bang
                }
            }
            '?' => {
                self.bump_char();
                TokenKind::Question
            }
            '<' => {
                self.bump_char();
                if self.peek_char() == Some('=') {
                    self.bump_char();
                    TokenKind::LtEq
                } else {
                    TokenKind::Lt
                }
            }
            '>' => {
                self.bump_char();
                if self.peek_char() == Some('=') {
                    self.bump_char();
                    TokenKind::GtEq
                } else {
                    TokenKind::Gt
                }
            }
            '+' => {
                self.bump_char();
                TokenKind::Plus
            }
            '*' => {
                self.bump_char();
                TokenKind::Star
            }
            '/' => {
                self.bump_char();
                TokenKind::Slash
            }
            '%' => {
                self.bump_char();
                TokenKind::Percent
            }
            '&' => {
                self.bump_char();
                if self.peek_char() == Some('&') {
                    self.bump_char();
                    TokenKind::AndAnd
                } else {
                    return Err(self.error_here("unexpected `&` (expected `&&`)"));
                }
            }
            '|' => {
                self.bump_char();
                if self.peek_char() == Some('|') {
                    self.bump_char();
                    TokenKind::OrOr
                } else {
                    TokenKind::Pipe
                }
            }
            '@' => {
                self.bump_char();
                TokenKind::At
            }
            _ => {
                return Err(LexError {
                    message: format!("unexpected character `{ch}`"),
                    span: self.span(start, self.pos + ch.len_utf8()),
                });
            }
        };

        Ok(Token {
            kind,
            span: self.span(start, self.pos),
        })
    }

    fn lex_ident_or_keyword(&mut self) -> Token {
        let start = self.pos;
        self.bump_char();
        while let Some(ch) = self.peek_char() {
            if is_ident_continue(ch) {
                self.bump_char();
            } else {
                break;
            }
        }
        let text = &self.src[start..self.pos];
        let kind = match text {
            "pub" => TokenKind::KwPub,
            "use" => TokenKind::KwUse,
            "mod" => TokenKind::KwMod,
            "as" => TokenKind::KwAs,
            "is" => TokenKind::KwIs,
            "fn" => TokenKind::KwFn,
            "cont" => TokenKind::KwCont,
            "let" => TokenKind::KwLet,
            "const" => TokenKind::KwConst,
            "readonly" => TokenKind::KwReadonly,
            "static" => TokenKind::KwStatic,
            "struct" => TokenKind::KwStruct,
            "enum" => TokenKind::KwEnum,
            "interface" => TokenKind::KwInterface,
            "impl" => TokenKind::KwImpl,
            "if" => TokenKind::KwIf,
            "else" => TokenKind::KwElse,
            "match" => TokenKind::KwMatch,
            "return" => TokenKind::KwReturn,
            "loop" => TokenKind::KwLoop,
            "while" => TokenKind::KwWhile,
            "for" => TokenKind::KwFor,
            "in" => TokenKind::KwIn,
            "break" => TokenKind::KwBreak,
            "continue" => TokenKind::KwContinue,
            _ => TokenKind::Ident(text.to_string()),
        };
        Token {
            kind,
            span: self.span(start, self.pos),
        }
    }

    fn lex_number(&mut self) -> Result<Token, LexError> {
        let start = self.pos;

        // Base prefix.
        let mut base = 10u32;
        let mut prefix_len = 0usize;
        if self.peek_char() == Some('0') {
            match self.peek_nth_char(1) {
                Some('x') | Some('X') => {
                    base = 16;
                    self.pos += 2;
                    prefix_len = 2;
                }
                Some('o') | Some('O') => {
                    base = 8;
                    self.pos += 2;
                    prefix_len = 2;
                }
                Some('b') | Some('B') => {
                    base = 2;
                    self.pos += 2;
                    prefix_len = 2;
                }
                _ => {}
            }
        }

        // Read digits/underscores for the integer part.
        let mut seen_digit = false;
        while let Some(ch) = self.peek_char() {
            if ch == '_' {
                self.bump_char();
                continue;
            }
            if digit_value(ch, base).is_some() {
                seen_digit = true;
                self.bump_char();
                continue;
            }
            break;
        }

        if !seen_digit {
            return Err(LexError {
                message: "expected digits after numeric base prefix".to_string(),
                span: self.span(start, self.pos),
            });
        }

        // Float only for base-10 literals.
        if base == 10 {
            let is_float = match (self.peek_char(), self.peek_nth_char(1)) {
                (Some('.'), Some(next)) if next.is_ascii_digit() => true,
                (Some('e') | Some('E'), _) => true,
                _ => false,
            };
            if is_float {
                // Continue consuming float tail.
                if self.peek_char() == Some('.') {
                    self.bump_char();
                    while let Some(ch) = self.peek_char() {
                        if ch.is_ascii_digit() || ch == '_' {
                            self.bump_char();
                        } else {
                            break;
                        }
                    }
                }
                if matches!(self.peek_char(), Some('e') | Some('E')) {
                    self.bump_char();
                    if matches!(self.peek_char(), Some('+') | Some('-')) {
                        self.bump_char();
                    }
                    let mut has_exp_digit = false;
                    while let Some(ch) = self.peek_char() {
                        if ch.is_ascii_digit() {
                            has_exp_digit = true;
                            self.bump_char();
                        } else if ch == '_' {
                            self.bump_char();
                        } else {
                            break;
                        }
                    }
                    if !has_exp_digit {
                        return Err(self.error_here("expected exponent digits"));
                    }
                }

                let raw = &self.src[start..self.pos];
                let cooked: String = raw.chars().filter(|c| *c != '_').collect();
                let value: f64 = cooked.parse().map_err(|_| LexError {
                    message: "invalid float literal".to_string(),
                    span: self.span(start, self.pos),
                })?;
                return Ok(Token {
                    kind: TokenKind::Float(value),
                    span: self.span(start, self.pos),
                });
            }
        }

        let raw_digits = &self.src[start + prefix_len..self.pos];
        let digits: String = raw_digits.chars().filter(|c| *c != '_').collect();

        let value = i64::from_str_radix(&digits, base).map_err(|_| LexError {
            message: "invalid integer literal".to_string(),
            span: self.span(start, self.pos),
        })?;

        Ok(Token {
            kind: TokenKind::Int(value),
            span: self.span(start, self.pos),
        })
    }

    fn lex_string(&mut self) -> Result<Token, LexError> {
        let start = self.pos;
        let value = self.scan_string()?;
        Ok(Token {
            kind: TokenKind::String(value),
            span: self.span(start, self.pos),
        })
    }

    fn lex_bytes(&mut self) -> Result<Token, LexError> {
        let start = self.pos;
        // Consume the `b`.
        self.bump_char();
        let value = self.scan_bytes()?;
        Ok(Token {
            kind: TokenKind::Bytes(value),
            span: self.span(start, self.pos),
        })
    }

    fn lex_fstring(&mut self) -> Result<Token, LexError> {
        let start = self.pos;
        // Consume the `f`.
        self.bump_char();
        let parts = self.scan_fstring_parts()?;
        Ok(Token {
            kind: TokenKind::FString(parts),
            span: self.span(start, self.pos),
        })
    }

    fn scan_string(&mut self) -> Result<String, LexError> {
        // Assumes the current char is `"`.
        let quote_start = self.pos;
        let Some('"') = self.peek_char() else {
            return Err(self.error_here("expected `\"`"));
        };
        self.bump_char();

        let mut out = String::new();
        loop {
            let Some(ch) = self.peek_char() else {
                return Err(LexError {
                    message: "unterminated string literal".to_string(),
                    span: self.span(quote_start, self.pos),
                });
            };
            if ch == '"' {
                self.bump_char();
                break;
            }
            if ch == '\\' {
                self.bump_char();
                let Some(esc) = self.peek_char() else {
                    return Err(self.error_here("unterminated escape"));
                };
                self.bump_char();
                match esc {
                    '\\' => out.push('\\'),
                    '"' => out.push('"'),
                    'n' => out.push('\n'),
                    'r' => out.push('\r'),
                    't' => out.push('\t'),
                    '0' => out.push('\0'),
                    'u' => {
                        if self.peek_char() != Some('{') {
                            return Err(self.error_here("expected `u{...}` escape"));
                        }
                        self.bump_char();
                        let hex_start = self.pos;
                        while let Some(h) = self.peek_char() {
                            if h == '}' {
                                break;
                            }
                            if h.is_ascii_hexdigit() {
                                self.bump_char();
                            } else {
                                return Err(self.error_here("invalid unicode escape"));
                            }
                        }
                        if self.peek_char() != Some('}') {
                            return Err(self.error_here("unterminated unicode escape"));
                        }
                        let hex = &self.src[hex_start..self.pos];
                        self.bump_char();
                        let code = u32::from_str_radix(hex, 16)
                            .map_err(|_| self.error_here("invalid unicode escape"))?;
                        let Some(scalar) = char::from_u32(code) else {
                            return Err(self.error_here("invalid unicode scalar value"));
                        };
                        out.push(scalar);
                    }
                    _ => {
                        return Err(LexError {
                            message: format!("unknown escape `\\{esc}`"),
                            span: self.span(self.pos.saturating_sub(2), self.pos),
                        });
                    }
                }
                continue;
            }

            out.push(ch);
            self.bump_char();
        }

        Ok(out)
    }

    fn scan_bytes(&mut self) -> Result<Vec<u8>, LexError> {
        // Assumes the current char is `"`.
        let quote_start = self.pos;
        let Some('"') = self.peek_char() else {
            return Err(self.error_here("expected `\"`"));
        };
        self.bump_char();

        let mut out = Vec::new();
        loop {
            let Some(ch) = self.peek_char() else {
                return Err(LexError {
                    message: "unterminated bytes literal".to_string(),
                    span: self.span(quote_start, self.pos),
                });
            };
            if ch == '"' {
                self.bump_char();
                break;
            }
            if ch == '\\' {
                self.bump_char();
                let Some(esc) = self.peek_char() else {
                    return Err(self.error_here("unterminated escape"));
                };
                self.bump_char();
                let byte = match esc {
                    '\\' => b'\\',
                    '"' => b'"',
                    'n' => b'\n',
                    'r' => b'\r',
                    't' => b'\t',
                    '0' => b'\0',
                    'x' => {
                        let hi = self
                            .peek_char()
                            .ok_or_else(|| self.error_here("unterminated \\x escape"))?;
                        let lo = self
                            .peek_nth_char(1)
                            .ok_or_else(|| self.error_here("unterminated \\x escape"))?;
                        if !hi.is_ascii_hexdigit() || !lo.is_ascii_hexdigit() {
                            return Err(self.error_here("invalid \\xHH escape"));
                        }
                        self.bump_char();
                        self.bump_char();
                        let mut buf = [0u8; 2];
                        buf[0] = hi as u8;
                        buf[1] = lo as u8;
                        u8::from_str_radix(std::str::from_utf8(&buf).expect("ascii hex"), 16)
                            .map_err(|_| self.error_here("invalid \\xHH escape"))?
                    }
                    _ => return Err(self.error_here("unknown escape in bytes literal")),
                };
                out.push(byte);
                continue;
            }

            let v = ch as u32;
            let ok_ascii = (0x20..=0x7E).contains(&v);
            if !ok_ascii || ch == '"' || ch == '\\' {
                return Err(self.error_here("non-ASCII character in bytes literal"));
            }
            out.push(ch as u8);
            self.bump_char();
        }

        Ok(out)
    }

    fn scan_fstring_parts(&mut self) -> Result<Vec<FStringPart>, LexError> {
        // Assumes the current char is `"`.
        let quote_start = self.pos;
        if self.peek_char() != Some('"') {
            return Err(self.error_here("expected `\"` after `f`"));
        }
        self.bump_char();

        let mut parts = Vec::new();
        let mut text = String::new();
        loop {
            let Some(ch) = self.peek_char() else {
                return Err(LexError {
                    message: "unterminated formatted string literal".to_string(),
                    span: self.span(quote_start, self.pos),
                });
            };

            if ch == '"' {
                self.bump_char();
                break;
            }

            if ch == '\\' {
                // Reuse UTF-8 string escape rules.
                self.bump_char();
                let Some(esc) = self.peek_char() else {
                    return Err(self.error_here("unterminated escape"));
                };
                self.bump_char();
                match esc {
                    '\\' => text.push('\\'),
                    '"' => text.push('"'),
                    'n' => text.push('\n'),
                    'r' => text.push('\r'),
                    't' => text.push('\t'),
                    '0' => text.push('\0'),
                    'u' => {
                        if self.peek_char() != Some('{') {
                            return Err(self.error_here("expected `u{...}` escape"));
                        }
                        self.bump_char();
                        let hex_start = self.pos;
                        while let Some(h) = self.peek_char() {
                            if h == '}' {
                                break;
                            }
                            if h.is_ascii_hexdigit() {
                                self.bump_char();
                            } else {
                                return Err(self.error_here("invalid unicode escape"));
                            }
                        }
                        if self.peek_char() != Some('}') {
                            return Err(self.error_here("unterminated unicode escape"));
                        }
                        let hex = &self.src[hex_start..self.pos];
                        self.bump_char();
                        let code = u32::from_str_radix(hex, 16)
                            .map_err(|_| self.error_here("invalid unicode escape"))?;
                        let Some(scalar) = char::from_u32(code) else {
                            return Err(self.error_here("invalid unicode scalar value"));
                        };
                        text.push(scalar);
                    }
                    _ => return Err(self.error_here("unknown escape in formatted string")),
                }
                continue;
            }

            if ch == '{' {
                if self.peek_nth_char(1) == Some('{') {
                    self.pos += 2;
                    text.push('{');
                    continue;
                }

                // Start interpolation.
                if !text.is_empty() {
                    parts.push(FStringPart::Text(mem::take(&mut text)));
                }
                self.bump_char(); // consume '{'

                let expr_base_offset = self.base_offset + self.pos;
                let expr_start = self.pos;
                self.scan_fstring_interpolation()?;
                let expr_end = self.pos;
                let expr_src = self.src[expr_start..expr_end].to_string();

                if self.peek_char() != Some('}') {
                    return Err(self.error_here("unterminated `{...}` in formatted string"));
                }
                self.bump_char(); // consume '}'

                parts.push(FStringPart::Expr {
                    src: expr_src,
                    base_offset: expr_base_offset,
                });
                continue;
            }

            if ch == '}' {
                if self.peek_nth_char(1) == Some('}') {
                    self.pos += 2;
                    text.push('}');
                    continue;
                }
                return Err(self.error_here("unmatched `}` in formatted string"));
            }

            text.push(ch);
            self.bump_char();
        }

        if !text.is_empty() {
            parts.push(FStringPart::Text(text));
        }

        Ok(parts)
    }

    fn scan_fstring_interpolation(&mut self) -> Result<(), LexError> {
        let mut brace_depth: i64 = 0;
        loop {
            let Some(ch) = self.peek_char() else {
                return Err(self.error_here("unterminated `{...}` in formatted string"));
            };

            if brace_depth == 0 && ch == '}' {
                return Ok(());
            }

            // Skip comments.
            if self.peek_str("//") {
                self.skip_line_comment();
                continue;
            }
            if self.peek_str("/*") {
                self.skip_block_comment()?;
                continue;
            }

            // Skip string / bytes / formatted strings within the interpolation.
            if ch == '"' {
                self.skip_string_like(StringKind::Utf8)?;
                continue;
            }
            if ch == 'b' && self.peek_next_char() == Some('"') {
                self.bump_char(); // 'b'
                self.skip_string_like(StringKind::Bytes)?;
                continue;
            }
            if ch == 'f' && self.peek_next_char() == Some('"') {
                self.bump_char(); // 'f'
                self.skip_fstring_like()?;
                continue;
            }

            if ch == '{' {
                brace_depth += 1;
                self.bump_char();
                continue;
            }
            if ch == '}' {
                brace_depth -= 1;
                if brace_depth < 0 {
                    return Err(self.error_here("unexpected `}` in interpolation"));
                }
                self.bump_char();
                continue;
            }

            self.bump_char();
        }
    }

    fn skip_string_like(&mut self, kind: StringKind) -> Result<(), LexError> {
        // Assumes current char is `"`.
        self.bump_char();
        loop {
            let Some(ch) = self.peek_char() else {
                return Err(self.error_here("unterminated string literal"));
            };
            if ch == '"' {
                self.bump_char();
                return Ok(());
            }
            if ch == '\\' {
                self.bump_char();
                let Some(esc) = self.peek_char() else {
                    return Err(self.error_here("unterminated escape"));
                };
                self.bump_char();
                if esc == 'u' && self.peek_char() == Some('{') {
                    self.bump_char(); // '{'
                    while let Some(h) = self.peek_char() {
                        self.bump_char();
                        if h == '}' {
                            break;
                        }
                    }
                } else if esc == 'x' && kind == StringKind::Bytes {
                    // Skip two hex digits if present.
                    if self.peek_char().is_some() {
                        self.bump_char();
                    }
                    if self.peek_char().is_some() {
                        self.bump_char();
                    }
                }
                continue;
            }
            self.bump_char();
        }
    }

    fn skip_fstring_like(&mut self) -> Result<(), LexError> {
        // Assumes current char is `"`.
        if self.peek_char() != Some('"') {
            return Err(self.error_here("expected `\"` after `f`"));
        }
        self.bump_char();

        let mut brace_depth: i64 = 0;
        loop {
            let Some(ch) = self.peek_char() else {
                return Err(self.error_here("unterminated formatted string literal"));
            };
            if ch == '"' && brace_depth == 0 {
                self.bump_char();
                return Ok(());
            }

            if ch == '\\' {
                self.bump_char();
                if self.peek_char().is_some() {
                    self.bump_char();
                }
                continue;
            }

            if ch == '{' {
                if self.peek_nth_char(1) == Some('{') {
                    self.pos += 2;
                    continue;
                }
                self.bump_char();
                brace_depth += 1;
                self.scan_fstring_interpolation()?;
                if self.peek_char() != Some('}') {
                    return Err(self.error_here("unterminated `{...}` in nested formatted string"));
                }
                self.bump_char();
                brace_depth -= 1;
                continue;
            }

            if ch == '}' {
                if self.peek_nth_char(1) == Some('}') {
                    self.pos += 2;
                    continue;
                }
                return Err(self.error_here("unmatched `}` in nested formatted string"));
            }

            self.bump_char();
        }
    }

    fn skip_ws_and_comments(&mut self) -> Result<(), LexError> {
        loop {
            while self.peek_char().is_some_and(|c| c.is_whitespace()) {
                self.bump_char();
            }
            if self.peek_str("//") {
                self.skip_line_comment();
                continue;
            }
            if self.peek_str("/*") {
                self.skip_block_comment()?;
                continue;
            }
            break;
        }
        Ok(())
    }

    fn skip_line_comment(&mut self) {
        debug_assert!(self.peek_str("//"));
        self.pos += 2;
        while let Some(ch) = self.peek_char() {
            self.bump_char();
            if ch == '\n' {
                break;
            }
        }
    }

    fn skip_block_comment(&mut self) -> Result<(), LexError> {
        debug_assert!(self.peek_str("/*"));
        let start = self.pos;
        self.pos += 2;
        let mut depth = 1usize;
        while depth > 0 {
            if self.pos >= self.src.len() {
                return Err(LexError {
                    message: "unterminated block comment".to_string(),
                    span: self.span(start, self.pos),
                });
            }
            if self.peek_str("/*") {
                self.pos += 2;
                depth += 1;
                continue;
            }
            if self.peek_str("*/") {
                self.pos += 2;
                depth -= 1;
                continue;
            }
            self.bump_char();
        }
        Ok(())
    }

    fn bump_char(&mut self) {
        let Some(ch) = self.peek_char() else {
            return;
        };
        self.pos += ch.len_utf8();
    }

    fn peek_char(&self) -> Option<char> {
        self.src[self.pos..].chars().next()
    }

    fn peek_next_char(&self) -> Option<char> {
        self.peek_nth_char(1)
    }

    fn peek_nth_char(&self, n: usize) -> Option<char> {
        self.src[self.pos..].chars().nth(n)
    }

    fn peek_str(&self, s: &str) -> bool {
        self.src[self.pos..].starts_with(s)
    }

    fn span(&self, start: usize, end: usize) -> Span {
        Span::new(self.base_offset + start, self.base_offset + end)
    }

    fn error_here(&self, message: impl Into<String>) -> LexError {
        LexError {
            message: message.into(),
            span: self.span(self.pos, self.pos),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum StringKind {
    Utf8,
    Bytes,
}

fn digit_value(ch: char, base: u32) -> Option<u32> {
    let v = ch.to_digit(base)?;
    Some(v)
}

fn is_ident_start(ch: char) -> bool {
    #[cfg(feature = "unicode")]
    {
        ch == '_' || unicode_ident::is_xid_start(ch)
    }
    #[cfg(not(feature = "unicode"))]
    {
        ch == '_' || ch.is_ascii_alphabetic()
    }
}

fn is_ident_continue(ch: char) -> bool {
    #[cfg(feature = "unicode")]
    {
        ch == '_' || unicode_ident::is_xid_continue(ch)
    }
    #[cfg(not(feature = "unicode"))]
    {
        ch == '_' || ch.is_ascii_alphanumeric()
    }
}

use std::mem;
