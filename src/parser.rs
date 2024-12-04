use crate::{
    lexer::{Token, TokenKind},
    Lexer,
};
use miette::{Diagnostic, Error, LabeledSpan};
use thiserror::Error;

#[derive(Diagnostic, Debug, Error)]
#[error("unexpected EOF")]
pub struct Eof;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Op {
    Minus,
    Plus,
    Star,
    Slash,
    Expt,
    Equal,
    LessEqual,
    GreaterEqual,
    Less,
    Greater,
    Not,
    And,
    Or,
    If,
    Cond,
    Display,
    Print,
    Define,
    Lambda,
    Let,
    Begin,
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
                Op::And => "and",
                Op::Or => "or",
                Op::Display => "display",
                Op::Print => "print",
                Op::Define => "define",
                Op::Lambda => "lambda",
                Op::If => "if",
                Op::Cond => "cond",
                Op::Expt => "expt",
                Op::Let => "let",
                Op::Begin => "begin"
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
    Symbol(&'a str),
    Op(Op),
}

impl std::fmt::Display for Atom<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Atom::Bool(b) => write!(f, "{}", if *b { "#t" } else { "#f" }),
            Atom::Symbol(sym) => write!(f, "{}", sym),
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

impl<'a> Pair<'a> {
    pub fn car(&self) -> Pair<'a> {
        match self {
            Pair::Atom(atom) => Pair::Atom(atom.clone()),
            Pair::Cons(p1, _p2) => *p1.clone(),
        }
    }

    pub fn cdr(&self) -> Option<Pair<'a>> {
        match self {
            Pair::Atom(_) => None,
            Pair::Cons(_p1, p2) => Some(*p2.clone())
        }
    }

    pub fn is_nil(&self) -> bool {
        matches!(self, Pair::Atom(Atom::Nil))
    }

    pub fn from_vec(numbers: Vec<f64>) -> Pair<'a> {
        let mut pair = Pair::Atom(Atom::Nil);
        for &num in numbers.iter().rev() {
            pair = Pair::Cons(Box::new(Pair::Atom(Atom::Number(num))), Box::new(pair));
        }
        pair
    }

    pub fn to_vec(&self) -> Vec<Pair<'a>> {
        let mut result = vec![];
        let mut current = self;
        loop {
            match current {
                Pair::Cons(car, cdr) => {
                    result.push(*car.clone());
                    current = cdr;
                }
                Pair::Atom(Atom::Nil) => break,
                _ => {
                    result.push(current.clone());
                    break;
                }
            }
        }
        result
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Pair::Atom(Atom::Bool(b)) => *b,
            Pair::Atom(Atom::Nil) => false,
            _ => true,
        }
    }

    pub fn len(&self) -> usize {
        let length = 0;
        
        if !self.is_nil() {
            if let Some(cdr) = self.cdr() {
                return 1 + cdr.len();
            }
        }

        length
    }

    pub fn map<F>(self, f: F) -> Pair<'a> 
    where 
        F: Fn(Pair) -> Pair,
    {
        if let Some(cdr) = self.cdr() {
            return Pair::Cons(Box::new(f(self.car())), Box::new(cdr.map(f)));
        } else {
            todo!()
        }
    }
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

impl<'a> Parser<'a> {
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
                kind: TokenKind::Symbol,
                slice,
                ..
            })) => Ok(Pair::Atom(Atom::Symbol(slice))),
            Some(Ok(Token {
                kind: TokenKind::True,
                ..
            })) => Ok(Pair::Atom(Atom::Bool(true))),
            Some(Ok(Token {
                kind: TokenKind::False,
                ..
            })) => Ok(Pair::Atom(Atom::Bool(false))),

            // operators
            Some(Ok(Token {
                kind: TokenKind::Not,
                ..
            })) => Ok(Pair::Atom(Atom::Op(Op::Not))),
            Some(Ok(Token {
                kind: TokenKind::Plus,
                ..
            })) => Ok(Pair::Atom(Atom::Op(Op::Plus))),
            Some(Ok(Token {
                kind: TokenKind::Minus,
                ..
            })) => Ok(Pair::Atom(Atom::Op(Op::Minus))),
            Some(Ok(Token {
                kind: TokenKind::Star,
                ..
            })) => Ok(Pair::Atom(Atom::Op(Op::Star))),
            Some(Ok(Token {
                kind: TokenKind::Slash,
                ..
            })) => Ok(Pair::Atom(Atom::Op(Op::Slash))),
            Some(Ok(Token {
                kind: TokenKind::Expt,
                ..
            })) => Ok(Pair::Atom(Atom::Op(Op::Expt))),
            Some(Ok(Token {
                kind: TokenKind::Less,
                ..
            })) => Ok(Pair::Atom(Atom::Op(Op::Less))),
            Some(Ok(Token {
                kind: TokenKind::LessEqual,
                ..
            })) => Ok(Pair::Atom(Atom::Op(Op::LessEqual))),
            Some(Ok(Token {
                kind: TokenKind::Greater,
                ..
            })) => Ok(Pair::Atom(Atom::Op(Op::Greater))),
            Some(Ok(Token {
                kind: TokenKind::GreaterEqual,
                ..
            })) => Ok(Pair::Atom(Atom::Op(Op::GreaterEqual))),
            Some(Ok(Token {
                kind: TokenKind::Display,
                ..
            })) => Ok(Pair::Atom(Atom::Op(Op::Display))),
            Some(Ok(Token {
                kind: TokenKind::Define,
                ..
            })) => Ok(Pair::Atom(Atom::Op(Op::Define))),
            Some(Ok(Token {
                kind: TokenKind::Lambda,
                ..
            })) => Ok(Pair::Atom(Atom::Op(Op::Lambda))),
            Some(Ok(Token {
                kind: TokenKind::Print,
                ..
            })) => Ok(Pair::Atom(Atom::Op(Op::Print))),
            Some(Ok(Token {
                kind: TokenKind::If,
                ..
            })) => Ok(Pair::Atom(Atom::Op(Op::If))),
            Some(Ok(Token {
                kind: TokenKind::Cond,
                ..
            })) => Ok(Pair::Atom(Atom::Op(Op::Cond))),
            Some(Ok(Token {
                kind: TokenKind::And,
                ..
            })) => Ok(Pair::Atom(Atom::Op(Op::And))),
            Some(Ok(Token {
                kind: TokenKind::Or,
                ..
            })) => Ok(Pair::Atom(Atom::Op(Op::Or))),

            // parenthesis
            Some(Ok(Token {
                kind: TokenKind::LeftParen,
                ..
            })) => self.parse_tail(),
            Some(Ok(Token {
                kind: TokenKind::Quote,
                ..
            })) => Ok(Pair::Cons(
                Box::new(Pair::Atom(Atom::Symbol("quote"))),
                Box::new(self.parse_head()?),
            )),
            Some(Ok(token)) => {
                return Err(miette::miette! {
                    labels = vec![
                        LabeledSpan::at(token.offset..token.offset + token.slice.len(), "here"),
                    ],
                    help = format!("unexpected {:?}", token.slice),
                    "unexpected token",
                }
                .with_source_code(self.source.to_string()))
            }
            Some(Err(err)) => return Err(err),
            None => return Err(Eof.into()),
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
                    Some(Ok(token)) => Ok(Pair::Atom(Atom::Symbol(token.slice))),
                    Some(Err(err)) => return Err(err),
                    None => return Err(Eof.into()),
                }
            }
            Some(Ok(_)) => {
                return Ok(Pair::Cons(
                    Box::new(self.parse_head()?),
                    Box::new(self.parse_tail()?),
                ))
            }
            Some(Err(err)) => {
                eprintln!("{}", err);
                todo!()
            }
            None => return Err(Eof.into()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_input(input: &str) -> Result<Pair, Error> {
        let mut parser = Parser::new(input);
        parser.parse()
    }

    #[test]
    fn test_parse_nil() {
        let input = "()";
        let result = parse_input(input).unwrap();
        assert_eq!(result, Pair::Atom(Atom::Nil));
    }

    #[test]
    fn test_parse_number() {
        let input = "42";
        let result = parse_input(input).unwrap();
        assert_eq!(result, Pair::Atom(Atom::Number(42.0)));
    }

    #[test]
    fn test_parse_string() {
        let input = "\"hello\"";
        let result = parse_input(input).unwrap();
        assert_eq!(result, Pair::Atom(Atom::String("\"hello\"")));
    }

    #[test]
    fn test_parse_boolean() {
        let input_true = "#t";
        let input_false = "#f";

        let result_true = parse_input(input_true).unwrap();
        let result_false = parse_input(input_false).unwrap();

        assert_eq!(result_true, Pair::Atom(Atom::Bool(true)));
        assert_eq!(result_false, Pair::Atom(Atom::Bool(false)));
    }

    #[test]
    fn test_parse_identifier() {
        let input = "foo";
        let result = parse_input(input).unwrap();
        assert_eq!(result, Pair::Atom(Atom::Symbol("foo")));
    }

    #[test]
    fn test_parse_operator() {
        let input = "+";
        let result = parse_input(input).unwrap();
        assert_eq!(result, Pair::Atom(Atom::Op(Op::Plus)));
    }

    #[test]
    fn test_parse_simple_list() {
        let input = "(1 2 3)";
        let result = parse_input(input).unwrap();
        let expected = Pair::Cons(
            Box::new(Pair::Atom(Atom::Number(1.0))),
            Box::new(Pair::Cons(
                Box::new(Pair::Atom(Atom::Number(2.0))),
                Box::new(Pair::Cons(
                    Box::new(Pair::Atom(Atom::Number(3.0))),
                    Box::new(Pair::Atom(Atom::Nil)),
                )),
            )),
        );
        assert_eq!(result, expected);
    }

    #[test]
    fn test_parse_nested_list() {
        let input = "(1 (2 3) 4)";
        let result = parse_input(input).unwrap();
        let expected = Pair::Cons(
            Box::new(Pair::Atom(Atom::Number(1.0))),
            Box::new(Pair::Cons(
                Box::new(Pair::Cons(
                    Box::new(Pair::Atom(Atom::Number(2.0))),
                    Box::new(Pair::Cons(
                        Box::new(Pair::Atom(Atom::Number(3.0))),
                        Box::new(Pair::Atom(Atom::Nil)),
                    )),
                )),
                Box::new(Pair::Cons(
                    Box::new(Pair::Atom(Atom::Number(4.0))),
                    Box::new(Pair::Atom(Atom::Nil)),
                )),
            )),
        );
        assert_eq!(result, expected);
    }

    #[test]
    fn test_parse_improper_list() {
        let input = "(1 2 . 3)";
        let result = parse_input(input).unwrap();
        let expected = Pair::Cons(
            Box::new(Pair::Atom(Atom::Number(1.0))),
            Box::new(Pair::Cons(
                Box::new(Pair::Atom(Atom::Number(2.0))),
                Box::new(Pair::Atom(Atom::Number(3.0))),
            )),
        );
        assert_eq!(result, expected);
    }

    #[test]
    fn test_parse_quote() {
        let input = "'foo";
        let result = parse_input(input).unwrap();
        let expected = Pair::Cons(
            Box::new(Pair::Atom(Atom::Symbol("quote"))),
            Box::new(Pair::Atom(Atom::Symbol("foo"))),
        );
        assert_eq!(result, expected);
    }

    #[test]
    fn test_parse_empty_input() {
        let input = "";
        let result = parse_input(input);
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_unexpected_token() {
        let input = ".";
        let result = parse_input(input);
        assert!(result.is_err());
    }
}
