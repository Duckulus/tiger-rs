use crate::parse::ast::{Dec, Exp, Oper, Var};
use crate::parse::lexer::{Span, Spanned};
use crate::semant::env::{base_type_env, base_value_env, Symbol, SymbolTable};
use crate::semant::types::{TypeEnvEntry, ValueEnvEntry};
use crate::semant::TypeErrorKind::Mismatch;

pub mod env;

pub mod types;

pub type TypedExp = (Spanned<Exp>, TypeEnvEntry);

pub fn trans_exp(exp: Spanned<Exp>) -> Result<TypedExp, TypeError> {
    let mut value_env = base_value_env();
    let mut type_env = base_type_env();
    trans_exp_rec(&mut value_env, &mut type_env, exp)
}

fn trans_exp_rec(
    value_env: &mut SymbolTable<ValueEnvEntry>,
    type_env: &mut SymbolTable<TypeEnvEntry>,
    exp: Spanned<Exp>,
) -> Result<TypedExp, TypeError> {
    match exp.0.clone() {
        Exp::Var(var) => match *var {
            Var::Simple(symb) => {
                let entry = value_env.lookup(&symb).ok_or_else(|| {
                    TypeError::new(exp.clone().1, TypeErrorKind::UnknownIdentifier(symb))
                })?;

                match entry {
                    ValueEnvEntry::Var(typ) => Ok((exp, typ)),
                    ValueEnvEntry::Fun(_, _) => {
                        Err(TypeError::new(exp.1, TypeErrorKind::InvalidIdentifier))
                    }
                }
            }
            Var::Field(_, _) => {
                unimplemented!()
            }
            Var::Subscript(_, _) => {
                unimplemented!()
            }
        },
        Exp::Nil => Ok((exp, TypeEnvEntry::Nil)),
        Exp::Int(_) => Ok((exp, TypeEnvEntry::Int)),
        Exp::String(_) => Ok((exp, TypeEnvEntry::String)),
        Exp::Call(fun_name, args) => {
            // let fun_type = value_env.lookup(&fun_name).ok_or_else(|| Err(TypeError::new(fun_name.)));
            unimplemented!()
        }
        Exp::Op(op, lhs, rhs) => {
            match op {
                Oper::Plus | Oper::Minus | Oper::Times | Oper::Divide => {
                    let (_, left_type) = trans_exp_rec(value_env, type_env, *lhs.clone())?;
                    if !matches!(left_type, TypeEnvEntry::Int) {
                        return Err(TypeError::new(
                            lhs.1,
                            Mismatch {
                                expected: TypeEnvEntry::Int,
                                found: left_type,
                            },
                        ));
                    }
                    let (_, right_type) = trans_exp_rec(value_env, type_env, *rhs.clone())?;
                    if !matches!(right_type, TypeEnvEntry::Int) {
                        return Err(TypeError::new(
                            rhs.1,
                            Mismatch {
                                expected: TypeEnvEntry::Int,
                                found: right_type,
                            },
                        ));
                    }
                    Ok((exp, TypeEnvEntry::Int))
                }
                _ => {
                    unimplemented!()
                } // Oper::Eq => {}
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
        Exp::Let(decs, exps) => {
            value_env.begin_scope();
            type_env.begin_scope();
            for dec in decs {
                trans_dec(value_env, type_env, dec)?;
            }
            let mut last = None;
            for exp in exps {
                last = Some(trans_exp_rec(value_env, type_env, exp)?);
            }
            type_env.end_scope();
            value_env.end_scope();
            Ok((exp, last.unwrap().1))
        }
        Exp::Array { .. } => {
            unimplemented!()
        }
    }
}

fn trans_dec(
    value_env: &mut SymbolTable<ValueEnvEntry>,
    type_env: &mut SymbolTable<TypeEnvEntry>,
    dec: Dec,
) -> Result<(), TypeError> {
    match dec {
        Dec::Function(funcs) => {
            for func in funcs {
                let mut param_types = Vec::new();
                value_env.begin_scope();
                for param in func.params {
                    let param_type = trans_typ_symbol(type_env, param.typ)?;
                    param_types.push(param_type.clone());
                    value_env.enter(param.name, ValueEnvEntry::Var(param_type));
                }
                let return_exp = trans_exp_rec(value_env, type_env, func.body.clone())?;
                value_env.end_scope();

                if let Some(return_type_symbol) = func.result {
                    let return_type = trans_typ_symbol(type_env, return_type_symbol)?;
                    if return_type != return_exp.1 {
                        return Err(TypeError {
                            span: func.body.1,
                            kind: TypeErrorKind::Mismatch {
                                expected: return_type,
                                found: return_exp.1,
                            },
                        });
                    }
                }
                // value_env.enter(func.name, ValueEnvEntry::Fun(param_types, return_exp.1));
            }
            Ok(())
        }
        Dec::Var(symb, type_annotation, exp) => {
            let exp_ty = trans_exp_rec(value_env, type_env, *exp.clone())?;
            if let Some(typ_sym) = type_annotation {
                let typ = trans_typ_symbol(type_env, typ_sym)?;
                if typ != exp_ty.1 {
                    return Err(TypeError::new(
                        exp.1,
                        TypeErrorKind::Mismatch {
                            expected: typ,
                            found: exp_ty.1,
                        },
                    ));
                }
            }
            value_env.enter(symb.0, ValueEnvEntry::Var(exp_ty.1.clone()));
            Ok(())
        }
        Dec::Type(_) => {
            unimplemented!()
        }
    }
}

fn trans_typ_symbol(
    type_env: &mut SymbolTable<TypeEnvEntry>,
    symbol: Spanned<Symbol>,
) -> Result<TypeEnvEntry, TypeError> {
    if let Some(typ) = type_env.lookup(&symbol.0) {
        Ok(typ)
    } else {
        Err(TypeError::new(
            symbol.1,
            TypeErrorKind::UnknownIdentifier(symbol.0),
        ))
    }
}

#[derive(Debug)]
pub struct TypeError {
    span: Span,
    kind: TypeErrorKind,
}

#[derive(Debug, Clone)]
pub enum TypeErrorKind {
    UnknownIdentifier(Symbol),
    Mismatch {
        expected: TypeEnvEntry,
        found: TypeEnvEntry,
    },
    InvalidIdentifier,
}

impl TypeError {
    pub fn new(span: Span, kind: TypeErrorKind) -> Self {
        Self { span, kind }
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn kind(&self) -> TypeErrorKind {
        self.kind.clone()
    }
}
