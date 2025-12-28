use crate::semant::env::Symbol;
use std::cell::RefCell;
use std::fmt::{Debug, Display, Formatter};
use std::rc::Rc;

pub type TypeRef = Rc<RefCell<Type>>;

#[derive(Clone, PartialEq, Eq)]
pub enum Type {
    Int,
    String,
    Record(Vec<(Symbol, TypeRef)>, u32),
    Array(TypeRef, u32),
    Nil,
    Void,
    Name(Symbol, Option<TypeRef>),
}

impl Debug for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, "Int"),
            Type::String => write!(f, "String"),
            Type::Nil => write!(f, "Nil"),
            Type::Void => write!(f, "Void"),

            Type::Name(sym, _) => write!(f, "Name({sym})"),

            Type::Record(_, id) => write!(f, "Record(id={id})"),

            Type::Array(_, id) => write!(f, "Array(id={id})"),
        }
    }
}
impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Clone)]
pub enum ValueEnvEntry {
    Var(Type),
    Fun(Vec<Type>, Type),
}
