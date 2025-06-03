use crate::semant::env::Symbol;

#[derive(Debug)]
pub enum Type {
    Int,
    String,
    Record(Vec<(Symbol, Type)>, u32),
    Array(Box<Type>, u32),
    Nil,
    Void
}