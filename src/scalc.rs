use crate::parser::*;

/// Reduces a list of numbers using a binary operation.
/// 
/// # Arguments
/// - `f`: A function that takes two `f64` arguments and returns an `f64`.
/// - `pair`: A `Pair` representing a list of numbers.
/// - `start`: The initial value for the reduction.
/// 
/// # Returns
/// - `Ok(f64)`: The result of reducing the list.
/// - `Err(&str)`: An error message if the list is malformed or contains invalid elements.
pub fn reduce<'a, F>(f: &F, pair: Pair<'a>, start: f64) -> Result<f64, &'static str>
where 
    F: Fn(f64, f64) -> f64
{
    if pair.is_nil() {
        // Base case: if the list is empty, return the starting value.
        return Ok(start);
    } else {
        // Extract the first element (car) of the list.
        if let Pair::Atom(Atom::Number(n)) = pair.car() {
            // Recursively reduce the rest of the list (cdr).
            if let Some(cdr) = pair.cdr() {
                return reduce(f, cdr, f(start, n));
            } else {
                return Err("malformed list".into());
            }
        } else {
            // Return an error if the car is not a number.
            return Err("not a number".into());
        }
    }
}

/// Applies a given operator to a list of arguments.
/// 
/// # Arguments
/// - `op`: The operator to apply (`Op` enum).
/// - `pair`: A `Pair` representing the arguments.
/// 
/// # Returns
/// - `Ok(f64)`: The result of applying the operator.
/// - `Err(&str)`: An error message if the operation fails.
fn calc_apply(op: Op, pair: Pair<'_>) -> Result<f64, &'static str> {
    match op {
        // Addition: reduce with the `add` function, starting at 0.
        Op::Plus => reduce(&add, pair, 0.0),
        // Subtraction: handle single and multiple arguments.
        Op::Minus => {
            if pair.is_nil() {
                Err("- requires at least 1 argument".into())
            } else if let Pair::Atom(Atom::Number(first)) = pair.car() {
                if let Some(cdr) = pair.cdr() {
                    if matches!(cdr, Pair::Atom(Atom::Nil)) {
                        Ok(minus(0.0, first)) // Single argument case.
                    } else {
                        reduce(&minus, cdr, first) // Multiple arguments.
                    }
                } else {
                    return Err("malformed list".into());
                }
            } else {
                Err("not a number".into())
            }
        }
        // Multiplication: reduce with the `mult` function, starting at 1.
        Op::Star => reduce(&mult, pair, 1.0),
        // Division: handle single and multiple arguments with error checks.
        Op::Slash => {
            if pair.is_nil() {
                Err("/ requires at least 1 argument".into())
            } else if let Pair::Atom(Atom::Number(first)) = pair.car() {
                if let Some(cdr) = pair.cdr() {
                    if matches!(cdr, Pair::Atom(Atom::Nil)) {
                        Ok(div(1.0, first)) // Single argument case.
                    } else {
                        reduce(&div, cdr, first) // Multiple arguments.
                    }
                } else {
                    return Err("malformed list".into());
                }
            } else {
                Err("not a number".into())
            }
        }
        // Exponentiation: requires exactly two arguments.
        Op::Expt => {
            if pair.is_nil() {
                Err("expt requires at least 2 arguments".into())
            } else if let Pair::Atom(Atom::Number(first)) = pair.car() {
                if let Some(cdr) = pair.cdr() {
                    if matches!(cdr, Pair::Atom(Atom::Nil)) {
                        Err("pow requires exactly 2 arguments".into())
                    } else {
                        reduce(&expt, cdr, first)
                    }
                } else {
                    return Err("malformed list".into());
                }
            } else {
                Err("not a number".into())
            }
        }
        // Default case: invalid operator.
        _ => Err("invalid operator".into()),
    }
}

/// Evaluates an expression represented as a `Pair`.
/// 
/// # Arguments
/// - `input`: The input expression in `Pair` format.
/// 
/// # Returns
/// - `Ok(f64)`: The result of evaluating the expression.
/// - `Err(&str)`: An error message if evaluation fails.
pub fn calc_eval(input: Pair<'_>) -> Result<f64, &'static str> {
    // Extract the operator from the first element (car).
    let op = match input.car() {
        Pair::Atom(Atom::Op(op)) => op,
        _ => return Err("invalid operator".into()),
    };

    // Process the arguments (cdr).
    if let Some(mut cdr) = input.cdr() {
        let mut results = vec![];

        // Evaluate each argument, collecting results.
        while !cdr.is_nil() {
            let car = cdr.car();
            match car {
                Pair::Cons(_, _) => {
                    results.push(calc_eval(car)?); // Recursively evaluate nested expressions.
                }
                Pair::Atom(Atom::Number(n)) => results.push(n), // Add numbers to the results.
                _ => return Err("invalid argument".into()),
            }

            cdr = cdr.cdr().ok_or("malformed list")?;
        }

        // Convert evaluated arguments into a new `Pair` and apply the operator.
        let reduced_pair = Pair::from_vec(results);
        calc_apply(op, reduced_pair)
    } else {
        Err("no arguments provided".into())
    }
}

// Basic arithmetic operations.
fn add(a: f64, b: f64) -> f64 {
    a + b
}

fn minus(a: f64, b: f64) -> f64 {
    a - b
}

fn mult(a: f64, b: f64) -> f64 {
    a * b
}

fn div(a: f64, b: f64) -> f64 {
    assert_ne!(b, 0.0, "zero division error");
    a / b
}

fn expt(a: f64, b: f64) -> f64 {
    a.powf(b)
}


#[cfg(test)]
mod tests {
    use super::*;

    fn parse_input(input: &str) -> Pair {
        let mut parser = Parser::new(input);
        parser.parse().unwrap()
    }

    #[test]
    fn test_add() {
        let pair = parse_input("(+ 12 331 10)");
        assert_eq!(Ok(353.0), calc_eval(pair));
    }

    #[test]
    fn test_minus() {
        let pair = parse_input("(- 50 20 10)");
        assert_eq!(Ok(20.0), calc_eval(pair));
    }

    #[test]
    fn test_minus_single_arg() {
        let pair = parse_input("(- 50)");
        assert_eq!(Ok(-50.0), calc_eval(pair));
    }

    #[test]
    fn test_mult() {
        let pair = parse_input("(* 2 3 4)");
        assert_eq!(Ok(24.0), calc_eval(pair));
    }

    #[test]
    fn test_mult_single_arg() {
        let pair = parse_input("(* 5)");
        assert_eq!(Ok(5.0), calc_eval(pair));
    }

    #[test]
    fn test_div() {
        let pair = parse_input("(/ 100 2 5)");
        assert_eq!(Ok(10.0), calc_eval(pair));
    }

    #[test]
    fn test_div_single_arg() {
        let pair = parse_input("(/ 10)");
        assert_eq!(Ok(0.1), calc_eval(pair));
    }

    #[test]
    fn test_nested_operations() {
        let pair = parse_input("(+ 10 (* 2 3) (/ 20 5))");
        assert_eq!(Ok(20.0), calc_eval(pair));
    }

    #[test]
    fn test_invalid_operator() {
        let pair = parse_input("(pow 2 3)");
        let result = calc_eval(pair);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), "invalid operator");
    }

    #[test]
    fn test_empty_list() {
        let pair = parse_input("()");
        let result = calc_eval(pair);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), "invalid operator");
    }

    #[test]
    fn test_invalid_syntax() {
        let pair = parse_input("(+ 1 2 . 3)");
        let result = calc_eval(pair);
        assert!(result.is_err());
    }

    #[test]
    fn test_non_number_arguments() {
        let pair = parse_input("(+ 1 \"string\")");
        let result = calc_eval(pair);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), "invalid argument");
    }

    #[test]
    fn test_pow() {
        let pair = parse_input("(expt 2 3)");
        assert_eq!(Ok(8.0), calc_eval(pair));
    }

    #[test]
    fn test_pow_with_floats() {
        let pair = parse_input("(expt 4.0 0.5)");
        assert_eq!(Ok(2.0), calc_eval(pair));
    }

    #[test]
    fn test_nested_pow() {
        let pair = parse_input("(expt 2 (expt 3 2))");
        assert_eq!(Ok(512.0), calc_eval(pair));
    }

    #[test]
    fn test_pow_negative_base() {
        let pair = parse_input("(expt -2 3)");
        assert_eq!(Ok(-8.0), calc_eval(pair));
    }

    #[test]
    fn test_pow_negative_exponent() {
        let pair = parse_input("(expt 2 -1)");
        assert_eq!(Ok(0.5), calc_eval(pair));
    }

    #[test]
    fn test_invalid_pow() {
        let pair = parse_input("(expt 2)");
        let result = calc_eval(pair);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), "pow requires exactly 2 arguments");
    }
}
