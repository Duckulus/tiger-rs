#![allow(unused)]

use crate::lexer::Spanned;

pub type Symbol = String;

pub type TypSymbol = Symbol;

pub type Program = Exp;

#[derive(Debug, Clone, PartialEq)]
pub enum Var {
    Simple(Symbol),
    Field(Box<Var>, Symbol),
    Subscript(Box<Var>, Box<Spanned<Exp>>),
}

impl Var {
    pub fn simple(name: Symbol) -> Self {
        Var::Simple(name)
    }
    pub fn field(var: Var, name: Symbol) -> Self {
        Var::Field(Box::new(var), name)
    }
    pub fn subscript(var: Var, exp: Spanned<Exp>) -> Self {
        Var::Subscript(Box::new(var), Box::new(exp))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Oper {
    Plus,
    Minus,
    Times,
    Divide,
    Eq, Neq, Lt, Le, Gt, Ge
}


#[derive(Debug, Clone, PartialEq)]
pub struct EField {
    pub name: Symbol,
    pub value: Spanned<Exp>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub name: Symbol,
    pub typ: TypSymbol,
}



#[derive(Debug, Clone, PartialEq)]
pub enum Exp {
    Var(Box<Var>),
    Nil,
    Int(i32),
    String(String),
    Call(Symbol, Vec<Spanned<Exp>>),
    Op(Oper, Box<Spanned<Exp>>, Box<Spanned<Exp>>),
    Record(TypSymbol, Vec<EField>),
    Seq(Vec<Spanned<Exp>>),
    Assign(Box<Var>, Box<Spanned<Exp>>),
    If {cond: Box<Spanned<Exp>>, then: Box<Spanned<Exp>>, elsee: Option<Box<Spanned<Exp>>>},
    While {cond: Box<Spanned<Exp>>, body: Box<Spanned<Exp>>},
    Break,
    For {var: Symbol, lo: Box<Spanned<Exp>>, hi: Box<Spanned<Exp>>, body: Box<Spanned<Exp>>},
    Let(Vec<Dec>, Vec<Spanned<Exp>>),
    Array{typ: TypSymbol, size: Box<Spanned<Exp>>, init: Box<Spanned<Exp>>}
}

impl Exp {
    pub fn var(var: Var) -> Self {
        Exp::Var(Box::new(var))
    }
    
    pub fn nil() -> Self {
        Exp::Nil
    }
    
    pub fn int(i: i32) -> Self {
        Exp::Int(i)
    }
    
    pub fn string(s: String) -> Self {
        Exp::String(s)
    }
    
    pub fn call(name: Symbol, args: Vec<Spanned<Exp>>) -> Self {
        Exp::Call(name, args)
    }
    
    pub fn op(op: Oper, e1: Spanned<Exp>, e2: Spanned<Exp>) -> Self {
        Exp::Op(op, Box::new(e1), Box::new(e2))
    }
    
    pub fn record(typ: TypSymbol, fields: Vec<EField>) -> Self {
        Exp::Record(typ, fields)
    }
    
    pub fn seq(exps: Vec<Spanned<Exp>>) -> Self {
        Exp::Seq(exps)
    }
    
    pub fn assign(var: Var, exp: Spanned<Exp>) -> Self {
        Exp::Assign(Box::new(var), Box::new(exp))
    }
    
    pub fn iff(cond: Spanned<Exp>, then: Spanned<Exp>, elsee: Option<Spanned<Exp>>) -> Self {
        Exp::If {cond: Box::new(cond), then: Box::new(then), elsee: elsee.map(Box::new)}
    }
    
    pub fn whilee(cond: Spanned<Exp>, body: Spanned<Exp>) -> Self {
        Exp::While {cond: Box::new(cond), body: Box::new(body)}
    }
    
    pub fn breakk() -> Self {
        Exp::Break
    }
    
    pub fn forr(var: Symbol, lo: Spanned<Exp>, hi: Spanned<Exp>, body: Spanned<Exp>) -> Self {
        Exp::For {var, lo: Box::new(lo), hi: Box::new(hi), body: Box::new(body)}
    }
    
    pub fn lett(decs: Vec<Dec>, body: Vec<Spanned<Exp>>) -> Self {
        Exp::Let(decs, body)
    }
    
    pub fn array(typ: TypSymbol, size: Spanned<Exp>, init: Spanned<Exp>) -> Self {
        Exp::Array{typ, size: Box::new(size), init: Box::new(init)}
    }
}


#[derive(Debug, Clone, PartialEq)]
pub struct FunDec {
    pub name: Symbol,
    pub params: Vec<Field>,
    pub result: Option<TypSymbol>,
    pub body: Spanned<Exp>
}


pub type NamedType = (TypSymbol, Type);

#[derive(Debug, Clone, PartialEq)]
pub enum Dec {
    Function(Vec<FunDec>),
    Var(Symbol, Option<TypSymbol>, Box<Spanned<Exp>>),
    Type(Vec<NamedType>),
}

impl Dec {
    pub fn function(funs: Vec<FunDec>) -> Self {
        Dec::Function(funs)
    }

    pub fn var(name: Symbol, typ: Option<TypSymbol>, init: Spanned<Exp>) -> Self {
        Dec::Var(name, typ, Box::new(init))
    }

    pub fn typee(types: Vec<NamedType>) -> Self {
        Dec::Type(types)
    }
}





#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Name(TypSymbol),
    Record(Vec<Field>),
    Array(TypSymbol),
}
