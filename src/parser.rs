use miette::{Diagnostic, Error, LabeledSpan};
use thiserror::Error;
use crate::{
    lexer::{Token, TokenKind},
    Lexer,
};

#[derive(Diagnostic, Debug, Error)]
#[error("unexpected EOF")]
pub struct Eof;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Op {
    Minus,
    Plus,
    Star,
    Not,
    Equal,
    LessEqual,
    GreaterEqual,
    Less,
    Greater,
    Slash,
}

impl std::fmt::Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Op::Minus => "-",
                Op::Plus => "+",
                Op::Star => "*",
                Op::Equal => "=",
                Op::LessEqual => "<=",
                Op::GreaterEqual => ">=",
                Op::Less => "<",
                Op::Greater => ">",
                Op::Slash => "/",
                Op::Not => "not",
            }
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Atom<'a> {
    String(&'a str),
    Number(f64),
    Nil,
    Bool(bool),
    Ident(&'a str),
    Op(Op)
}

impl std::fmt::Display for Atom<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Atom::Bool(b) => write!(f, "{}", if *b { "#t" } else { "#f" }),
            Atom::Ident(id) => write!(f, "{}", id),
            Atom::Number(n) => write!(f, "{}", n),
            Atom::String(s) => write!(f, "{}", s),
            Atom::Nil => write!(f, "()"),
            Atom::Op(op) => write!(f, "{}", op),
        }
    }
}

#[derive(Clone, PartialEq)]
pub enum Pair<'a> {
    Atom(Atom<'a>),
    Cons(Box<Pair<'a>>, Box<Pair<'a>>),
}

impl std::fmt::Debug for Pair<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Pair::Atom(Atom::Nil) => write!(f, "nil"),
            Pair::Atom(atom) => write!(f, "{}", atom),
            Pair::Cons(p1, p2) => write!(f, "Pair({:?}, {:?})", p1, p2),
        }
    }
}

impl<'a> std::fmt::Display for Pair<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Pair::Atom(atom) => write!(f, "{}", atom),
            Pair::Cons(left, right) => {
                write!(f, "(")?;
                write!(f, "{}", left)?;

                let mut current = right;
                loop {
                    match **current {
                        Pair::Atom(Atom::Nil) => break,
                        Pair::Cons(ref car, ref cdr) => {
                            write!(f, " {}", car)?;
                            current = cdr;
                        }
                        _ => {
                            write!(f, " . {}", current)?;
                            break;
                        }
                    }
                }

                write!(f, ")")
            }
        }
    }
}

pub struct Parser<'a> {
    source: &'a str,
    lexer: Lexer<'a>,
}

impl <'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            source: input,
            lexer: Lexer::new(input),
        }
    }

    pub fn parse(&mut self) -> Result<Pair<'a>, Error> {
        self.parse_head()
    }

    fn parse_head(&mut self) -> Result<Pair<'a>, Error> {
        match self.lexer.next() {
            Some(Ok(Token {
                kind: TokenKind::Nil,
                ..
            })) => Ok(Pair::Atom(Atom::Nil)),
            Some(Ok(Token {
                kind: TokenKind::String,
                slice,
                ..
            })) => Ok(Pair::Atom(Atom::String(slice))),
            Some(Ok(Token {
                kind: TokenKind::Number,
                slice,
                ..
            })) => Ok(Pair::Atom(Atom::Number(slice.parse().unwrap()))),
            Some(Ok(Token {
                kind: TokenKind::Ident,
                slice,
                ..
            })) => Ok(Pair::Atom(Atom::Ident(slice))),
            Some(Ok(Token {
                kind: TokenKind::True,
                ..
            })) => Ok(Pair::Atom(Atom::Bool(true))),
            Some(Ok(Token {
                kind: TokenKind::False,
                ..
            })) => Ok(Pair::Atom(Atom::Bool(false))),
            Some(Ok(Token {
                kind: TokenKind::Plus,
                ..
            })) => Ok(Pair::Atom(Atom::Op(Op::Plus))),
            Some(Ok(Token {
                kind: TokenKind::LeftParen,
                ..
            })) => self.parse_tail(),
            Some(Ok(Token {
                kind: TokenKind::Quote,
                ..
            })) => Ok(Pair::Cons(Box::new(Pair::Atom(Atom::Ident("quote"))), Box::new(self.parse_head()?))),
            Some(Ok(token)) => return Err(miette::miette! {
                labels = vec![
                    LabeledSpan::at(token.offset..token.offset + token.slice.len(), "here"),
                ],
                help = format!("unexpected {:?}", token.kind),
                "unexpected token",
            }
            .with_source_code(self.source.to_string())),
            Some(Err(err)) => return  Err(err),
            None => return Err(Eof.into())
        }
    }

    fn parse_tail(&mut self) -> Result<Pair<'a>, Error> {
        match self.lexer.peek() {
            Some(Ok(Token {
                kind: TokenKind::RightParen,
                ..
            })) => {
                self.lexer.next();
                return Ok(Pair::Atom(Atom::Nil));
            }
            Some(Ok(Token {
                kind: TokenKind::Dot,
                ..
            })) => {
                self.lexer.next();
                match self.lexer.next() {
                    Some(Ok(Token {
                        kind: TokenKind::LeftParen,
                        ..
                    })) => Ok(self.parse_tail()?),
                    Some(Ok(Token {
                        kind: TokenKind::Number,
                        slice,
                        ..
                    })) => Ok(Pair::Atom(Atom::Number(slice.parse().unwrap()))),
                    Some(Ok(token)) => Ok(Pair::Atom(Atom::Ident(token.slice))),
                    Some(Err(err)) => return Err(err),
                    None => return Err(Eof.into())
                }
            }
            Some(Ok(_)) => return Ok(Pair::Cons(Box::new(self.parse_head()?), Box::new(self.parse_tail()?))),
            Some(Err(err)) => {
                eprintln!("{}", err);
                todo!()
            }
            None => return Err(Eof.into())
        }
    }

}