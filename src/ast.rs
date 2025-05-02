#![allow(unused)]
type Symbol = String;

type TypSymbol = Symbol;

pub type Program = Exp;

#[derive(Debug, Clone, PartialEq)]
pub enum Var {
    Simple(Symbol),
    Field(Box<Var>, Symbol),
    Subscript(Box<Var>, Box<Exp>),
}

impl Var {
    pub fn simple(name: Symbol) -> Self {
        Var::Simple(name)
    }
    pub fn field(var: Var, name: Symbol) -> Self {
        Var::Field(Box::new(var), name)
    }
    pub fn subscript(var: Var, exp: Exp) -> Self {
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
    pub value: Exp,
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
    Call(Symbol, Vec<Exp>),
    Op(Oper, Box<Exp>, Box<Exp>),
    Record(TypSymbol, Vec<EField>),
    Seq(Vec<Exp>),
    Assign(Box<Var>, Box<Exp>),
    If {cond: Box<Exp>, then: Box<Exp>, elsee: Option<Box<Exp>>},
    While {cond: Box<Exp>, body: Box<Exp>},
    Break,
    For {var: Symbol, lo: Box<Exp>, hi: Box<Exp>, body: Box<Exp>},
    Let(Vec<Dec>, Vec<Exp>),
    Array{typ: TypSymbol, size: Box<Exp>, init: Box<Exp>}
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
    
    pub fn call(name: Symbol, args: Vec<Exp>) -> Self {
        Exp::Call(name, args)
    }
    
    pub fn op(op: Oper, e1: Exp, e2: Exp) -> Self {
        Exp::Op(op, Box::new(e1), Box::new(e2))
    }
    
    pub fn record(typ: TypSymbol, fields: Vec<EField>) -> Self {
        Exp::Record(typ, fields)
    }
    
    pub fn seq(exps: Vec<Exp>) -> Self {
        Exp::Seq(exps)
    }
    
    pub fn assign(var: Var, exp: Exp) -> Self {
        Exp::Assign(Box::new(var), Box::new(exp))
    }
    
    pub fn iff(cond: Exp, then: Exp, elsee: Option<Exp>) -> Self {
        Exp::If {cond: Box::new(cond), then: Box::new(then), elsee: elsee.map(Box::new)}
    }
    
    pub fn whilee(cond: Exp, body: Exp) -> Self {
        Exp::While {cond: Box::new(cond), body: Box::new(body)}
    }
    
    pub fn breakk() -> Self {
        Exp::Break
    }
    
    pub fn forr(var: Symbol, lo: Exp, hi: Exp, body: Exp) -> Self {
        Exp::For {var, lo: Box::new(lo), hi: Box::new(hi), body: Box::new(body)}
    }
    
    pub fn lett(decs: Vec<Dec>, body: Vec<Exp>) -> Self {
        Exp::Let(decs, body)
    }
    
    pub fn array(typ: TypSymbol, size: Exp, init: Exp) -> Self {
        Exp::Array{typ, size: Box::new(size), init: Box::new(init)}
    }
}


#[derive(Debug, Clone, PartialEq)]
pub struct FunDec {
    pub name: Symbol,
    pub params: Vec<Field>,
    pub result: Option<TypSymbol>,
    pub body: Exp
}


pub type NamedType = (TypSymbol, Type);

#[derive(Debug, Clone, PartialEq)]
pub enum Dec {
    Function(Vec<FunDec>),
    Var(Symbol, Option<TypSymbol>, Box<Exp>),
    Type(Vec<NamedType>),
}

impl Dec {
    pub fn function(funs: Vec<FunDec>) -> Self {
        Dec::Function(funs)
    }

    pub fn var(name: Symbol, typ: Option<TypSymbol>, init: Exp) -> Self {
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
