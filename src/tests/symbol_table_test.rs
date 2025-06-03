use crate::semant::env::SymbolTable;

#[test]
pub fn test_symbol_table() {
    let mut table = SymbolTable::empty();
    
    assert!(table.lookup("hi".to_string()).is_none());
    
    table.begin_scope();
    table.enter("hi".to_string(), 10);
    table.enter("bye".to_string(), 20);
    
    assert_eq!(*table.lookup("hi".to_string()).unwrap(), 10);
    assert_eq!(*table.lookup("bye".to_string()).unwrap(), 20);
    
    table.begin_scope();
    table.enter("hi".to_string(), 11);

    assert_eq!(*table.lookup("hi".to_string()).unwrap(), 11);
    assert_eq!(*table.lookup("bye".to_string()).unwrap(), 20);
    
    table.end_scope();

    assert_eq!(*table.lookup("hi".to_string()).unwrap(), 10);
    assert_eq!(*table.lookup("bye".to_string()).unwrap(), 20);
    
    table.end_scope();
    assert!(table.lookup("hi".to_string()).is_none());
    assert!(table.lookup("bye".to_string()).is_none());
}