use crate::semant::env::Symbol;

#[derive(Debug, PartialEq, Eq)]
#[derive(Clone)]
pub enum Type {
    Int,
    String,
    Record(Vec<(Symbol, Type)>, u32),
    Array(Box<Type>, u32),
    Nil,
    Void,
}

#[derive(Clone)]
pub enum ValueEnvEntry {
    Var(Type),
    Fun(Vec<Type>, Type)
}
