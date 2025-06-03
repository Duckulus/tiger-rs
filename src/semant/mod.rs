use crate::parse::ast::{Exp, Oper};
use crate::parse::lexer::{Span, Spanned};
use crate::semant::env::SymbolTable;
use crate::semant::types::Type;

pub mod env;

pub mod types;

type TypedExp = (Spanned<Exp>, Type);

pub fn trans_exp(exp: Spanned<Exp>) -> Result<TypedExp, TypeError> {
    let mut value_env = SymbolTable::empty();
    let mut type_env = SymbolTable::empty();
    trans_exp_rec(&mut value_env, &mut type_env, exp)
}

fn trans_exp_rec(value_env: &mut SymbolTable<Type>, type_env: &mut SymbolTable<Type>, exp: Spanned<Exp>) -> Result<TypedExp, TypeError> {
    match exp.0.clone() {
        Exp::Var(var) => {
           unimplemented!() 
        }
        Exp::Nil => {
            Ok((exp, Type::Nil))
        }
        Exp::Int(_) => {
            Ok((exp, Type::Int))
        }
        Exp::String(_) => {
            Ok((exp, Type::String))
        }
        Exp::Call(_, _) => {
            unimplemented!()
        }
        Exp::Op(op, lhs, rhs) => {
           match op {
               Oper::Plus | Oper::Minus | Oper::Times | Oper::Divide=> {
                   let (_, left_type)= trans_exp_rec(value_env, type_env, *lhs.clone())?;
                   if !matches!(left_type, Type::Int){
                       return Err(TypeError::new(*lhs, "Expected int"));
                   }
                   let (_, right_type)= trans_exp_rec(value_env, type_env, *rhs.clone())?;
                   if !matches!(right_type, Type::Int){
                       return Err(TypeError::new(*rhs, "Expected int"));
                   }
                   Ok((exp, Type::Int))
               }
               _ => {
                   unimplemented!()
               }
               // Oper::Eq => {}
               // Oper::Neq => {}
               // Oper::Lt => {}
               // Oper::Le => {}
               // Oper::Gt => {}
               // Oper::Ge => {}
           } 
        }
        Exp::Record(_, _) => {
            unimplemented!()
        }
        Exp::Seq(_) => {
            unimplemented!()
        }
        Exp::Assign(_, _) => {
            unimplemented!()
        }
        Exp::If { .. } => {
            unimplemented!()
        }
        Exp::While { .. } => {
            unimplemented!()
        }
        Exp::Break => {
            unimplemented!()
        }
        Exp::For { .. } => {
            unimplemented!()
        }
        Exp::Let(_, _) => {
            unimplemented!()
        }
        Exp::Array { .. } => {
            unimplemented!()
        }
    }
}

pub struct TypeError {
    exp: Spanned<Exp>,
    message: String
}

impl TypeError {
    pub fn new(exp: Spanned<Exp>, message: &str) -> Self {
        Self {
            exp,
            message: message.to_string()
        }
    }
    
    pub fn span(&self) -> Span {
        self.exp.1
    }
    
    pub fn message(&self) -> String {
        self.message.clone()
    }
}