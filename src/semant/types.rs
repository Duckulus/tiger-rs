use crate::semant::env::Symbol;

#[derive(Debug, PartialEq, Eq)]
#[derive(Clone)]
pub enum TypeEnvEntry {
    Int,
    String,
    Record(Vec<(Symbol, TypeEnvEntry)>, u32),
    Array(Box<TypeEnvEntry>, u32),
    Nil,
    Void,
}

#[derive(Clone)]
pub enum ValueEnvEntry {
    Var(TypeEnvEntry),
    Fun(Vec<TypeEnvEntry>, TypeEnvEntry)
}
