use std::{cell::RefCell, rc::Rc};
use crate::{env::Environment, parser::{Atom, Op, Pair}};

pub struct Evaluator<'a> {
    env: Rc<RefCell<Environment<'a>>>,
}

impl<'a> Evaluator<'a> {
    pub fn new() -> Self {
        Self {
            env: Environment::new(),
        }
    }

    pub fn with_env(env: Environment<'a>) -> Self {
        Self {
            env: Rc::new(RefCell::new(Environment::with_parent(env)))
        }
    }

    pub fn eval(&self, expr: Pair<'a>) -> Result<Pair<'a>, String> {
        match expr {
            // Return atomic values directly.
            Pair::Atom(Atom::Number(_))
            | Pair::Atom(Atom::String(_))
            | Pair::Atom(Atom::Bool(_))
            | Pair::Atom(Atom::Nil)
            | Pair::Atom(Atom::Op(_)) => Ok(expr),

            // Evaluate symbols by looking them up in the environment.
            Pair::Atom(Atom::Symbol(sym)) => self
                .env
                .borrow()
                .lookup(sym)
                .ok_or_else(|| format!("unbound symbol: {}", sym)),

            // Special form: define
            Pair::Cons(head, rest) if *head == Pair::Atom(Atom::Op(Op::Define)) => {
                if let Pair::Cons(var, value) = *rest {
                    if let Pair::Atom(Atom::Symbol(name)) = *var {
                        let value = self.eval(value.car())?;
                        self.env.borrow_mut().define(name, value.clone());
                        return Ok(value);
                    }
                }
                Err("Invalid define syntax".to_string())
            }

            // Special form: if
            Pair::Cons(head, rest) if *head == Pair::Atom(Atom::Op(Op::If)) => {
                if let Some(Pair::Cons(cond, rest)) = rest.cdr() {
                    if let Some(Pair::Cons(then_branch, else_branch)) = rest.cdr() {
                        let cond_value = self.eval(*cond)?;
                        if let Pair::Atom(Atom::Bool(true)) = cond_value {
                            return self.eval(*then_branch);
                        } else {
                            return self.eval(*else_branch);
                        }
                    }
                }
                Err("Invalid if syntax".to_string())
            }

            // Function application
            Pair::Cons(head, args) => {
                let func = self.eval(*head)?;
                let arg_values = self.eval_list(*args)?;
                self.apply(func, arg_values)
            }

            _ => Err("Unexpected expression".to_string()),
        }
    }

    fn eval_list(&self, list: Pair<'a>) -> Result<Vec<Pair<'a>>, String> {
        let mut result = vec![];
        let mut current = list;
        while let Pair::Cons(head, tail) = current {
            result.push(self.eval(*head)?);
            current = *tail;
        }
        Ok(result)
    }

    fn apply(&self, func: Pair<'a>, args: Vec<Pair<'a>>) -> Result<Pair<'a>, String> {
        match func {
            // Handle primitive functions (e.g., +, -, *, /).
            Pair::Atom(Atom::Op(op)) => match op {
                Op::Plus => self.apply_numeric_op(args, |a, b| a + b),
                Op::Minus => self.apply_numeric_op(args, |a, b| a - b),
                Op::Star => self.apply_numeric_op(args, |a, b| a * b),
                Op::Slash => self.apply_numeric_op(args, |a, b| a / b),
                _ => Err(format!("Unknown operator: {}", op)),
            },
            _ => Err("Expected a function".to_string()),
        }
    }

    fn apply_numeric_op<F>(&self, args: Vec<Pair<'a>>, op: F) -> Result<Pair<'a>, String>
    where
        F: Fn(f64, f64) -> f64,
    {
        let mut numbers = args
            .into_iter()
            .map(|arg| {
                if let Pair::Atom(Atom::Number(num)) = arg {
                    Ok(num)
                } else {
                    Err("Expected a number".to_string())
                }
            })
            .collect::<Result<Vec<f64>, String>>()?;

        if numbers.len() < 2 {
            return Err("Not enough arguments".to_string());
        }

        let result = numbers.drain(..).reduce(op).unwrap();
        Ok(Pair::Atom(Atom::Number(result)))
    }
}