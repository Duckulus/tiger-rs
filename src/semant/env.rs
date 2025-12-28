use crate::semant::types::{Type, TypeRef, ValueEnvEntry};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub type Symbol = String;

pub type ValueEnv = SymbolTable<ValueEnvEntry>;
pub type TypeEnv = SymbolTable<TypeRef>;

pub struct SymbolTable<T> {
    table: HashMap<Symbol, Vec<T>>,
    undo_stack: Vec<UndoStackEntry>,
    builtins: HashMap<Symbol, T>,
}

enum UndoStackEntry {
    BeginMarker,
    Entry(Symbol),
}

impl<T: Clone> SymbolTable<T> {
    pub fn empty() -> Self {
        SymbolTable {
            table: HashMap::new(),
            undo_stack: Vec::new(),
            builtins: HashMap::new(),
        }
    }

    pub fn enter(&mut self, symbol: Symbol, value: T) {
        if !self.table.contains_key(&symbol) {
            self.table.insert(symbol.clone(), Vec::new());
        }
        self.table.get_mut(&symbol).unwrap().push(value);
        self.undo_stack.push(UndoStackEntry::Entry(symbol));
    }

    pub fn begin_scope(&mut self) {
        self.undo_stack.push(UndoStackEntry::BeginMarker);
    }

    pub fn end_scope(&mut self) {
        if self.undo_stack.is_empty() {
            return;
        }
        while let UndoStackEntry::Entry(symbol) = self.undo_stack.last().unwrap() {
            self.table.get_mut(symbol).unwrap().pop();
            self.undo_stack.pop();
        }
        if !self.undo_stack.is_empty() {
            self.undo_stack.pop();
        }
    }

    pub fn lookup(&self, symbol: &Symbol) -> Option<T> {
        self.table
            .get(symbol)
            .and_then(|bucket| bucket.last().cloned())
            .or_else(|| self.builtins.get(symbol).cloned())
    }
}

pub fn base_value_env() -> ValueEnv {
    SymbolTable::empty()
}

pub fn base_type_env() -> TypeEnv {
    let mut table = SymbolTable::empty();
    table
        .builtins
        .insert("int".to_string(), Rc::new(RefCell::new(Type::Int)));
    table
        .builtins
        .insert("string".to_string(), Rc::new(RefCell::new(Type::Int)));
    table
}
