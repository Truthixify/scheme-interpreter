use miette::{Diagnostic, Error, LabeledSpan};
use thiserror::Error;

#[derive(Diagnostic, Debug, Error)]
#[error("Unexpected EOF")]
pub struct Eof;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Token<'a> {
    pub slice: &'a str,
    pub offset: usize,
    pub kind: TokenKind,
}

impl<'a> std::fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.slice)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind {
    LeftParen,
    RightParen,
    Comma,
    Dot,
    Plus,
    Minus,
    Star,
    Slash,
    Semicolon,
    Not,
    Equal,
    LessEqual,
    GreaterEqual,
    Less,
    Greater,
    String,
    Ident,
    Number(f64),
    And,
    Or,
    If,
    Cond,
    True,
    False,
    Display,
    Nil,
    Quote,
}

pub struct Lexer<'a> {
    source: &'a str,
    rest: &'a str,
    byte: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            source: input,
            rest: input,
            byte: 0,
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let mut chars = self.rest.chars();
            let c = chars.next()?;
            let offset = self.byte;
            let slice = &self.rest[..c.len_utf8()];
            let c_onwards = self.rest;
            self.rest = chars.as_str();
            self.byte += c.len_utf8();

            enum Started {
                String,
                Number,
                Ident,
                IfEqualElse(TokenKind, TokenKind),
                Semicolon,
                Hashtag
            }

            let make_token = |kind: TokenKind| {
                Some(Ok(Token {
                    slice,
                    kind,
                    offset,
                }))
            };

            let started = match c {
                '(' => return make_token(TokenKind::LeftParen),
                ')' => return make_token(TokenKind::RightParen),
                ',' => return make_token(TokenKind::Comma),
                '.' => return make_token(TokenKind::Dot),
                '-' => return make_token(TokenKind::Minus),
                '+' => return make_token(TokenKind::Plus),
                '*' => return make_token(TokenKind::Star),
                '/' => return make_token(TokenKind::Slash),
                '=' => return make_token(TokenKind::Equal),
                '\'' => return make_token(TokenKind::Quote),
                '#' => Started::Hashtag,
                ';' => Started::Semicolon,
                '<' => Started::IfEqualElse(TokenKind::LessEqual, TokenKind::Less),
                '>' => Started::IfEqualElse(TokenKind::GreaterEqual, TokenKind::Greater),
                '"' => Started::String,
                '0'..='9' => Started::Number,
                'a'..='z' | 'A'..='Z' | '_' => Started::Ident,
                c if c.is_whitespace() => continue,
                _ => return Some(Err(miette::miette! {
                    labels = vec![
                        LabeledSpan::at(self.byte - c.len_utf8()..self.byte, "this input character"),
                    ],
                    "unexpected token {c}",
                }
                .with_source_code(self.source.to_string())))
            };

            match started {
                Started::Ident => {
                    let first_non_ident = c_onwards
                        .find(|c| !matches!(c, 'a'..='z' | 'A'..='Z' | '_' | '0'..='9' | '?'))
                        .unwrap_or_else(|| c_onwards.len());

                    let literal = &c_onwards[..first_non_ident];

                    let extra_byte = literal.len() - c.len_utf8();
                    self.byte += extra_byte;
                    self.rest = &self.rest[extra_byte..];

                    let kind = match literal {
                        "not" => TokenKind::Not,
                        "and" => TokenKind::And,
                        "if" => TokenKind::If,
                        "nil" => TokenKind::Nil,
                        "or" => TokenKind::Or,
                        "true" => TokenKind::True,
                        "false" => TokenKind::False,
                        _ => TokenKind::Ident,
                    };

                    return Some(Ok(Token {
                        slice: literal,
                        offset,
                        kind,
                    }));
                }
                Started::String => {
                    if let Some(end) = self.rest.find('"') {
                        let literal = &c_onwards[..end + 1 + 1];
                        self.byte += end + 1;
                        self.rest = &self.rest[end + 1..];
                        return Some(Ok(Token {
                            slice: literal,
                            offset,
                            kind: TokenKind::String,
                        }));
                    } else {
                        let err = Err(miette::miette! {
                            labels = vec![
                                LabeledSpan::at(self.byte - c.len_utf8()..self.byte, "this string literal"),
                            ],
                            "unterminated string",
                        }
                        .with_source_code(self.source.to_string()));

                        self.byte += self.rest.len();
                        self.rest = &self.rest[self.rest.len()..];

                        return Some(err);
                    }
                }
                Started::Number => {
                    let end = c_onwards
                        .find(|c| matches!(c, ' ' | ')'))
                        .unwrap_or_else(|| c_onwards.len());
                    let literal = &c_onwards[..end];
                    let extra_byte = literal.len() - c.len_utf8();
                    self.byte += extra_byte;
                    self.rest = &self.rest[extra_byte..];

                    let n = match literal.parse() {
                        Ok(n) => n,
                        Err(err) => return Some(Err(miette::miette! {
                            labels = vec![
                                LabeledSpan::at(self.byte - literal.len()..self.byte, "this numeric literal"),
                            ],
                            "{err}",
                        }
                        .with_source_code(self.source.to_string()))),
                    };

                    return Some(Ok(Token {
                        slice: literal,
                        offset,
                        kind: TokenKind::Number(n),
                    }));
                }
                Started::IfEqualElse(yes, no) => {
                    self.rest = self.rest.trim_start();
                    let trimmed = c_onwards.len() - self.rest.len() - 1;
                    self.byte += trimmed;
                    if self.rest.trim_start().starts_with('=') {
                        let span = &c_onwards[..c.len_utf8() + trimmed + 1];
                        self.rest = &self.rest[1..];
                        self.byte += 1;
                        return Some(Ok(Token {
                            slice: span,
                            offset,
                            kind: yes,
                        }));
                    } else {
                        return Some(Ok(Token {
                            slice,
                            offset,
                            kind: no,
                        }));
                    }
                }
                Started::Semicolon => {
                    let line_end = self.rest.find('\n').unwrap_or_else(|| self.rest.len());
                    self.byte += line_end;
                    self.rest = &self.rest[line_end..];
                    continue;
                }
                Started::Hashtag => {
                    let end = self.rest.find(|c| matches!(c, ' ' | ')')).unwrap_or_else(|| self.rest.len());
                    let literal = &c_onwards[..end + 1];
                    self.byte += end;
                    self.rest = &self.rest[end..];
                        
                    match literal {
                        "#t" => return Some(Ok(Token {
                            slice: literal,
                            offset,
                            kind: TokenKind::True
                        })),
                        "#f" => return Some(Ok(Token {
                            slice: literal,
                            offset,
                            kind: TokenKind::True
                        })),
                        _ => return Some(Err(miette::miette! {
                            labels = vec![
                                LabeledSpan::at(self.byte - literal.len()..self.byte, "this literal"),
                            ],
                            "expected #t or #f",
                        }
                        .with_source_code(self.source.to_string())))
                    }
                }
            }
        }
    }
}