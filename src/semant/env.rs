use std::collections::HashMap;
use std::rc::Rc;

pub type Symbol = String;

pub struct SymbolTable<T> {
    table: HashMap<Symbol, Vec<Rc<T>>>,
    undo_stack: Vec<UndoStackEntry>,
}

enum UndoStackEntry {
    BeginMarker,
    Entry(Symbol),
}

impl<T> SymbolTable<T> {
    pub fn empty() -> Self {
        SymbolTable {
            table: HashMap::new(),
            undo_stack: Vec::new(),
        }
    }

    pub fn enter(&mut self, symbol: Symbol, value: T) {
        if !self.table.contains_key(&symbol) {
            self.table.insert(symbol.clone(), Vec::new());
        }
        self.table.get_mut(&symbol).unwrap().push(Rc::new(value));
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

    pub fn lookup(&self, symbol: Symbol) -> Option<Rc<T>> {
        self.table
            .get(&symbol)
            .and_then(|bucket| bucket.last().cloned())
    }
}
