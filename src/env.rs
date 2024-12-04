use std::collections::HashMap;
use crate::parser::Pair;
use std::rc::Rc;
use std::cell::RefCell;

#[derive(Debug, Clone, PartialEq)]
pub struct Environment<'a> {
    bindings: HashMap<&'a str, Pair<'a>>,
    parent: Option<Rc<RefCell<Environment<'a>>>>,
}

impl<'a> Environment<'a> {
    pub fn new() -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            bindings: HashMap::new(),
            parent: None,
        }))
    }

    pub fn with_parent(parent: Environment<'a>) -> Self {
        Self {
            bindings: HashMap::new(),
            parent: Some(Rc::new(RefCell::new(parent))),
        }
    }

    pub fn lookup(&self, name: &str) -> Option<Pair<'a>> {
        if let Some(val) = self.bindings.get(name) {
            return Some(val.clone());
        }
        if let Some(parent) = &self.parent {
            return parent.borrow().lookup(name);
        }
        None
    }

    pub fn define(&mut self, binding: &'a str, val: Pair<'a>) {
        self.bindings.insert(binding, val);
    }

    // pub fn make_child_frame(self, mut formals: Pair<'a>, mut vals: Pair<'a>) -> Result<Self, Error> {
    //     let mut frame = Frame::new(self);

    //     if formals.len() != vals.len() {
    //         return Err(miette::miette!("too many or two few arguments have been given"));
    //     }

    //     while !matches!(formals, Pair::Atom(Atom::Nil)) && !matches!(vals, Pair::Atom(Atom::Nil)) {
    //         frame.define(formals.car(), vals.car());
    //         if let Some(f_cdr) = formals.cdr() {
    //             formals = f_cdr;
    //         }
    //         if let Some(v_cdr) = formals.cdr() {
    //             vals = v_cdr;
    //         }
    //     }

    //     Ok(frame)
    // }
}

impl std::fmt::Display for Environment<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.parent {
            None => write!(f, "<Global Frame>"),
            Some(parent) => {
                let mut s = String::new();
                for (key, val) in &self.bindings {
                    s = format!("{}: {}", key, val);
                };

                write!(f, "<{{{}}} -> {}>", s, parent.borrow())
            }
        }
    }
}