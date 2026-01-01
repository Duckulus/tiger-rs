use crate::semant::types::{Type, TypeRef, ValueEnvEntry};
use crate::trans::frame::Frame;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub type Symbol = String;

pub type ValueEnv<F> = SymbolTable<ValueEnvEntry<F>>;
pub type TypeEnv = SymbolTable<TypeRef>;

pub struct SymbolTable<T> {
    table: RefCell<HashMap<Symbol, Vec<T>>>,
    undo_stack: RefCell<Vec<UndoStackEntry>>,
    builtins: HashMap<Symbol, T>,
}

enum UndoStackEntry {
    BeginMarker,
    Entry(Symbol),
}

impl<T: Clone> SymbolTable<T> {
    pub fn empty() -> Self {
        SymbolTable {
            table: RefCell::new(HashMap::new()),
            undo_stack: RefCell::new(Vec::new()),
            builtins: HashMap::new(),
        }
    }

    pub fn enter(&self, symbol: Symbol, value: T) {
        let mut table = self.table.borrow_mut();
        if !table.contains_key(&symbol) {
            table.insert(symbol.clone(), Vec::new());
        }
        table.get_mut(&symbol).unwrap().push(value);
        let mut undo_stack = self.undo_stack.borrow_mut();
        undo_stack.push(UndoStackEntry::Entry(symbol));
    }

    pub fn begin_scope(&self) {
        let mut undo_stack = self.undo_stack.borrow_mut();
        undo_stack.push(UndoStackEntry::BeginMarker);
    }

    pub fn end_scope(&self) {
        let mut undo_stack = self.undo_stack.borrow_mut();
        if undo_stack.is_empty() {
            return;
        }

        let mut table = self.table.borrow_mut();
        while let UndoStackEntry::Entry(symbol) = undo_stack.last().unwrap() {
            table.get_mut(symbol).unwrap().pop();
            undo_stack.pop();
        }
        if !undo_stack.is_empty() {
            undo_stack.pop();
        }
    }

    pub fn lookup(&self, symbol: &Symbol) -> Option<T> {
        self.table
            .borrow_mut()
            .get(symbol)
            .and_then(|bucket| bucket.last().cloned())
            .or_else(|| self.builtins.get(symbol).cloned())
    }
}

pub fn base_value_env<F: Frame>() -> ValueEnv<F> {
    SymbolTable::empty()
}

pub fn base_type_env() -> TypeEnv {
    let mut table = SymbolTable::empty();
    table
        .builtins
        .insert("int".to_string(), Rc::new(RefCell::new(Type::Int)));
    table
        .builtins
        .insert("string".to_string(), Rc::new(RefCell::new(Type::String)));
    table
}
