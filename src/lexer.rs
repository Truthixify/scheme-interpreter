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
    Expt,
    Semicolon,
    Not,
    Equal,
    LessEqual,
    GreaterEqual,
    Less,
    Greater,
    String,
    Symbol,
    Number,
    And,
    Or,
    If,
    Cond,
    True,
    False,
    Display,
    Nil,
    Quote,
    Print,
    Define,
    Lambda,
    Let,
    Begin,
}

#[derive(Debug)]
pub struct Lexer<'a> {
    source: &'a str,
    rest: &'a str,
    byte: usize,
    pub peeked: Option<Result<Token<'a>, miette::Error>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            source: input,
            rest: input,
            byte: 0,
            peeked: None,
        }
    }

    pub fn peek(&mut self) -> Option<&Result<Token<'a>, miette::Error>> {
        if self.peeked.is_some() {
            return self.peeked.as_ref();
        }

        self.peeked = self.next();
        self.peeked.as_ref()
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(next) = self.peeked.take() {
            return Some(next);
        }

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
                IfNextIsNumberElse(TokenKind, TokenKind),
                Symbol,
                Number,
                IfEqualElse(TokenKind, TokenKind),
                Semicolon,
                Hashtag,
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
                '.' => Started::IfNextIsNumberElse(TokenKind::Number, TokenKind::Dot),
                '-' => Started::IfNextIsNumberElse(TokenKind::Number, TokenKind::Minus),
                '+' => Started::IfNextIsNumberElse(TokenKind::Number, TokenKind::Plus),
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
                'a'..='z' | 'A'..='Z' | '_' => Started::Symbol,
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
                Started::Symbol => {
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
                        "expt" => TokenKind::Expt,
                        "cond" => TokenKind::Cond,
                        "display" => TokenKind::Display,
                        "print" => TokenKind::Print,
                        "define" => TokenKind::Define,
                        "lambda" => TokenKind::Lambda,
                        "let" => TokenKind::Let,
                        "begin" => TokenKind::Begin,
                        _ => TokenKind::Symbol,
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
                        .find(|c: char| c.is_whitespace() || matches!(c, ')'))
                        .unwrap_or_else(|| c_onwards.len());
                    let literal = &c_onwards[..end];
                    let extra_byte = literal.len() - c.len_utf8();
                    self.byte += extra_byte;
                    self.rest = &self.rest[extra_byte..];

                    match literal.parse::<f64>() {
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
                        kind: TokenKind::Number,
                    }));
                }
                Started::IfNextIsNumberElse(yes, no) => match self.rest.chars().nth(0) {
                    Some(c) if ('0'..='9').contains(&c) => {
                        let end = c_onwards
                            .find(|c: char| c.is_whitespace() || matches!(c, ')'))
                            .unwrap_or_else(|| c_onwards.len());
                        let literal = &c_onwards[..end];
                        let extra_byte = literal.len() - c.len_utf8();
                        self.byte += extra_byte;
                        self.rest = &self.rest[extra_byte..];

                        match literal.parse::<f64>() {
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
                            kind: yes,
                        }));
                    }
                    Some(_) => {
                        return Some(Ok(Token {
                            slice,
                            offset,
                            kind: no,
                        }))
                    }
                    None => {
                        return Some(Ok(Token {
                            slice,
                            offset,
                            kind: no,
                        }))
                    }
                },
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
                    let end = self
                        .rest
                        .find(|c| matches!(c, ' ' | ')'))
                        .unwrap_or_else(|| self.rest.len());
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
                            kind: TokenKind::False
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parentheses() {
        let input = "( )";
        let mut lexer = Lexer::new(input);

        let expected_tokens = vec![
            Token {
                slice: "(",
                offset: 0,
                kind: TokenKind::LeftParen,
            },
            Token {
                slice: ")",
                offset: 2,
                kind: TokenKind::RightParen,
            },
        ];

        for expected_token in expected_tokens.into_iter() {
            assert_eq!(lexer.next().unwrap().unwrap(), expected_token);
        }
    }

    #[test]
    fn test_numbers() {
        let input = "42 3.14 -100";
        let mut lexer = Lexer::new(input);

        let expected_tokens = vec![
            Token {
                slice: "42",
                offset: 0,
                kind: TokenKind::Number,
            },
            Token {
                slice: "3.14",
                offset: 3,
                kind: TokenKind::Number,
            },
            Token {
                slice: "-100",
                offset: 8,
                kind: TokenKind::Number,
            },
        ];

        for expected_token in expected_tokens.into_iter() {
            assert_eq!(lexer.next().unwrap().unwrap(), expected_token);
        }
    }

    #[test]
    fn test_identifiers_and_keywords() {
        let input = "define x and or if";
        let mut lexer = Lexer::new(input);

        let expected_tokens = vec![
            Token {
                slice: "define",
                offset: 0,
                kind: TokenKind::Define,
            },
            Token {
                slice: "x",
                offset: 7,
                kind: TokenKind::Symbol,
            },
            Token {
                slice: "and",
                offset: 9,
                kind: TokenKind::And,
            },
            Token {
                slice: "or",
                offset: 13,
                kind: TokenKind::Or,
            },
            Token {
                slice: "if",
                offset: 16,
                kind: TokenKind::If,
            },
        ];

        for expected_token in expected_tokens.into_iter() {
            assert_eq!(lexer.next().unwrap().unwrap(), expected_token);
        }
    }

    #[test]
    fn test_strings() {
        let input = r#""hello" "world""#;
        let mut lexer = Lexer::new(input);

        let expected_tokens = vec![
            Token {
                slice: "\"hello\"",
                offset: 0,
                kind: TokenKind::String,
            },
            Token {
                slice: "\"world\"",
                offset: 8,
                kind: TokenKind::String,
            },
        ];

        for expected_token in expected_tokens.into_iter() {
            assert_eq!(lexer.next().unwrap().unwrap(), expected_token);
        }
    }

    #[test]
    fn test_operators() {
        let input = "+ - * / = < >";
        let mut lexer = Lexer::new(input);

        let expected_tokens = vec![
            Token {
                slice: "+",
                offset: 0,
                kind: TokenKind::Plus,
            },
            Token {
                slice: "-",
                offset: 2,
                kind: TokenKind::Minus,
            },
            Token {
                slice: "*",
                offset: 4,
                kind: TokenKind::Star,
            },
            Token {
                slice: "/",
                offset: 6,
                kind: TokenKind::Slash,
            },
            Token {
                slice: "=",
                offset: 8,
                kind: TokenKind::Equal,
            },
            Token {
                slice: "<",
                offset: 10,
                kind: TokenKind::Less,
            },
            Token {
                slice: ">",
                offset: 12,
                kind: TokenKind::Greater,
            },
        ];

        for expected_token in expected_tokens.into_iter() {
            assert_eq!(lexer.next().unwrap().unwrap(), expected_token);
        }
    }

    #[test]
    fn test_booleans() {
        let input = "#t #f";
        let mut lexer = Lexer::new(input);

        let expected_tokens = vec![
            Token {
                slice: "#t",
                offset: 0,
                kind: TokenKind::True,
            },
            Token {
                slice: "#f",
                offset: 3,
                kind: TokenKind::False,
            },
        ];

        for expected_token in expected_tokens.into_iter() {
            assert_eq!(lexer.next().unwrap().unwrap(), expected_token);
        }
    }

    #[test]
    fn test_nested_expressions() {
        let input = "(+ (* 2 3) (/ 6 2))";
        let mut lexer = Lexer::new(input);

        let expected_tokens = vec![
            Token {
                slice: "(",
                offset: 0,
                kind: TokenKind::LeftParen,
            },
            Token {
                slice: "+",
                offset: 1,
                kind: TokenKind::Plus,
            },
            Token {
                slice: "(",
                offset: 3,
                kind: TokenKind::LeftParen,
            },
            Token {
                slice: "*",
                offset: 4,
                kind: TokenKind::Star,
            },
            Token {
                slice: "2",
                offset: 6,
                kind: TokenKind::Number,
            },
            Token {
                slice: "3",
                offset: 8,
                kind: TokenKind::Number,
            },
            Token {
                slice: ")",
                offset: 9,
                kind: TokenKind::RightParen,
            },
            Token {
                slice: "(",
                offset: 11,
                kind: TokenKind::LeftParen,
            },
            Token {
                slice: "/",
                offset: 12,
                kind: TokenKind::Slash,
            },
            Token {
                slice: "6",
                offset: 14,
                kind: TokenKind::Number,
            },
            Token {
                slice: "2",
                offset: 16,
                kind: TokenKind::Number,
            },
            Token {
                slice: ")",
                offset: 17,
                kind: TokenKind::RightParen,
            },
            Token {
                slice: ")",
                offset: 18,
                kind: TokenKind::RightParen,
            },
        ];

        for expected_token in expected_tokens.into_iter() {
            assert_eq!(lexer.next().unwrap().unwrap(), expected_token);
        }
    }
}
