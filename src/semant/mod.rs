use crate::parse::ast::{Dec, Exp, Oper, TypeDecl, Var};
use crate::parse::lexer::{Span, Spanned};
use crate::semant::env::{Symbol, SymbolTable, base_type_env, base_value_env};
use crate::semant::types::{Type, ValueEnvEntry};
use chumsky::span::SimpleSpan;
use std::sync::atomic::{AtomicU32, Ordering};

pub mod env;

pub mod types;

pub type TypedExp = (Spanned<Exp>, Type);

pub fn trans_exp(exp: Spanned<Exp>) -> Result<TypedExp, TypeError> {
    let mut value_env = base_value_env();
    let mut type_env = base_type_env();
    trans_exp_rec(&mut value_env, &mut type_env, exp)
}

fn trans_exp_rec(
    value_env: &mut SymbolTable<ValueEnvEntry>,
    type_env: &mut SymbolTable<Type>,
    exp: Spanned<Exp>,
) -> Result<TypedExp, TypeError> {
    match exp.0.clone() {
        Exp::Var(var) => {
            let typ = trans_var(value_env, type_env, var)?;
            Ok((exp, typ))
        }
        Exp::Nil => Ok((exp, Type::Nil)),
        Exp::Int(_) => Ok((exp, Type::Int)),
        Exp::String(_) => Ok((exp, Type::String)),
        Exp::Call(fun_name, args) => {
            let fun_type = value_env_lookup(value_env, fun_name.clone())?;
            if let ValueEnvEntry::Fun(arg_types, return_type) = fun_type {
                if args.len() != arg_types.len() {
                    let args_span = if args.len() == 0 {
                        fun_name.1
                    } else {
                        SimpleSpan::from(args.first().unwrap().1.start..args.last().unwrap().1.end)
                    };
                    return Err(TypeError::new(
                        args_span,
                        TypeErrorKind::ArgCountMismatch {
                            expected: arg_types.len(),
                            found: args.len(),
                        },
                    ));
                }
                for (arg, expected_type) in args.into_iter().zip(arg_types) {
                    let arg_span = arg.1.clone();
                    let (_, arg_type) = trans_exp_rec(value_env, type_env, arg)?;
                    if arg_type != expected_type {
                        return Err(TypeError::new(
                            arg_span,
                            TypeErrorKind::TypeMismatch {
                                expected: expected_type,
                                found: arg_type,
                            },
                        ));
                    }
                }
                Ok((exp, return_type))
            } else {
                Err(TypeError::new(fun_name.1, TypeErrorKind::InvalidIdentifier))
            }
        }
        Exp::Op(op, lhs, rhs) => {
            let (_, left_type) = trans_exp_rec(value_env, type_env, *lhs.clone())?;
            let (right_exp, right_type) = trans_exp_rec(value_env, type_env, *rhs.clone())?;
            match op.0 {
                Oper::Plus | Oper::Minus | Oper::Times | Oper::Divide => {
                    if !matches!(left_type, Type::Int) {
                        return Err(TypeError::new(
                            lhs.1,
                            TypeErrorKind::TypeMismatch {
                                expected: Type::Int,
                                found: left_type,
                            },
                        ));
                    }
                    if !matches!(right_type, Type::Int) {
                        return Err(TypeError::new(
                            rhs.1,
                            TypeErrorKind::TypeMismatch {
                                expected: Type::Int,
                                found: right_type,
                            },
                        ));
                    }
                    Ok((exp, Type::Int))
                }
                Oper::Lt | Oper::Le | Oper::Gt | Oper::Ge => match left_type {
                    Type::Int | Type::String => {
                        if right_type == left_type {
                            Ok((exp, Type::Int))
                        } else {
                            Err(TypeError::new(
                                right_exp.1,
                                TypeErrorKind::TypeMismatch {
                                    expected: left_type,
                                    found: right_type,
                                },
                            ))
                        }
                    }
                    _ => Err(TypeError::new(op.1, TypeErrorKind::InvalidOperator)),
                },
                Oper::Eq | Oper::Neq => match left_type {
                    Type::Int | Type::String | Type::Record(_, _) | Type::Array(_, _) => {
                        if right_type == left_type {
                            Ok((exp, Type::Int))
                        } else {
                            Err(TypeError::new(
                                right_exp.1,
                                TypeErrorKind::TypeMismatch {
                                    expected: left_type,
                                    found: right_type,
                                },
                            ))
                        }
                    }
                    _ => Err(TypeError::new(op.1, TypeErrorKind::InvalidOperator)),
                },
            }
        }
        Exp::Record(typ_symbol, efields) => {
            let typ_symbol_span = typ_symbol.1.clone();
            let ty = type_env_lookup(type_env, typ_symbol)?;
            if let Type::Record(ref fields, _) = ty {
                let mut fields = fields.clone();
                for efield in efields {
                    let exp_span = efield.value.1.clone();
                    let (_, given_type) = trans_exp_rec(value_env, type_env, efield.value)?;
                    if let Some(pos) = fields.iter().position(|f| f.0 == efield.name.0) {
                        let (_, expected_type) = fields.get(pos).unwrap();
                        if given_type != *expected_type {
                            return Err(TypeError {
                                span: exp_span,
                                kind: TypeErrorKind::TypeMismatch {
                                    found: given_type,
                                    expected: expected_type.clone(),
                                },
                            });
                        }
                        fields.remove(pos);
                    } else {
                        return Err(TypeError {
                            span: efield.name.1,
                            kind: TypeErrorKind::UnexpectedRecordField(efield.name.0),
                        });
                    }
                }
                if !fields.is_empty() {
                    return Err(TypeError {
                        span: typ_symbol_span,
                        kind: TypeErrorKind::MissingRecordFields {
                            missing: fields.iter().map(|f| f.0.clone()).collect(),
                        },
                    });
                }
                Ok((exp, ty))
            } else {
                Err(TypeError {
                    span: typ_symbol_span,
                    kind: TypeErrorKind::NotARecordType,
                })
            }
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
        Exp::Array { typ, size, init } => {
            let typ_span = typ.1.clone();
            let ty = type_env_lookup(type_env, typ)?;
            if let Type::Array(ref elem_type, _) = ty {
                let (_, size_type) = trans_exp_rec(value_env, type_env, *size.clone())?;
                if size_type != Type::Int {
                    return Err(TypeError {
                        span: size.1,
                        kind: TypeErrorKind::TypeMismatch {
                            expected: Type::Int,
                            found: size_type,
                        },
                    });
                }
                let (_, init_type) = trans_exp_rec(value_env, type_env, *init.clone())?;
                if init_type != **elem_type {
                    return Err(TypeError {
                        span: init.1,
                        kind: TypeErrorKind::TypeMismatch {
                            expected: *elem_type.clone(),
                            found: init_type,
                        },
                    });
                }
                Ok((exp, ty))
            } else {
                Err(TypeError {
                    span: typ_span,
                    kind: TypeErrorKind::NotAnArrayType,
                })
            }
        }
    }
}

fn trans_var(
    value_env: &mut SymbolTable<ValueEnvEntry>,
    type_env: &mut SymbolTable<Type>,
    var: Box<Var>,
) -> Result<Type, TypeError> {
    match *var {
        Var::Simple(symb) => {
            let symb_span = symb.1.clone();
            let entry = value_env_lookup(value_env, symb)?;

            match entry {
                ValueEnvEntry::Var(typ) => Ok(typ),
                ValueEnvEntry::Fun(_, _) => {
                    Err(TypeError::new(symb_span, TypeErrorKind::InvalidIdentifier))
                }
            }
        }
        Var::Field(var, symb) => {
            let var_typ = trans_var(value_env, type_env, var)?;
            if let Type::Record(fields, _) = var_typ {
                let field = fields.iter().find(|f| f.0 == symb.0);
                if let Some((_, field_type)) = field {
                    Ok(field_type.clone())
                } else {
                    Err(TypeError::new(
                        symb.1,
                        TypeErrorKind::UnexpectedRecordField(symb.0),
                    ))
                }
            } else {
                Err(TypeError::new(symb.1, TypeErrorKind::UnexpectedFieldAccess))
            }
        }
        Var::Subscript(var, exp) => {
            let exp_span = exp.1.clone();
            let (_, index_type) = trans_exp_rec(value_env, type_env, *exp)?;
            if index_type != Type::Int {
                return Err(TypeError::new(
                    exp_span,
                    TypeErrorKind::TypeMismatch {
                        expected: Type::Int,
                        found: index_type,
                    },
                ));
            }
            let var_typ = trans_var(value_env, type_env, var)?;
            if let Type::Array(elem_type, _) = var_typ {
                Ok(*elem_type)
            } else {
                Err(TypeError::new(exp_span, TypeErrorKind::UnexpectedSubscript))
            }
        }
    }
}

fn trans_dec(
    value_env: &mut SymbolTable<ValueEnvEntry>,
    type_env: &mut SymbolTable<Type>,
    dec: Dec,
) -> Result<(), TypeError> {
    match dec {
        Dec::Function(funcs) => {
            for func in funcs {
                let mut param_types = Vec::new();
                value_env.begin_scope();
                for param in func.params {
                    let param_type = type_env_lookup(type_env, param.typ)?;
                    param_types.push(param_type.clone());
                    value_env.enter(param.name, ValueEnvEntry::Var(param_type));
                }
                let return_exp = trans_exp_rec(value_env, type_env, func.body.clone())?;
                value_env.end_scope();

                if let Some(return_type_symbol) = func.result {
                    let return_type = type_env_lookup(type_env, return_type_symbol)?;
                    if return_type != return_exp.1 {
                        return Err(TypeError {
                            span: func.body.1,
                            kind: TypeErrorKind::TypeMismatch {
                                expected: return_type,
                                found: return_exp.1,
                            },
                        });
                    }
                }
                value_env.enter(func.name, ValueEnvEntry::Fun(param_types, return_exp.1));
            }
            Ok(())
        }
        Dec::Var(symb, type_annotation, exp) => {
            let exp_ty = trans_exp_rec(value_env, type_env, *exp.clone())?;
            if let Some(typ_sym) = type_annotation {
                let typ = type_env_lookup(type_env, typ_sym)?;
                if typ != exp_ty.1 {
                    return Err(TypeError::new(
                        exp.1,
                        TypeErrorKind::TypeMismatch {
                            expected: typ,
                            found: exp_ty.1,
                        },
                    ));
                }
            }
            value_env.enter(symb.0, ValueEnvEntry::Var(exp_ty.1.clone()));
            Ok(())
        }
        Dec::Type(named_types) => {
            for (name, type_decl) in named_types {
                let typ = trans_type_decl(type_env, type_decl)?;
                type_env.enter(name, typ)
            }
            Ok(())
        }
    }
}

fn trans_type_decl(
    type_env: &mut SymbolTable<Type>,
    type_decl: TypeDecl,
) -> Result<Type, TypeError> {
    match type_decl {
        TypeDecl::Name(name) => type_env_lookup(type_env, name),
        TypeDecl::Record(fields) => Ok(Type::Record(
            fields
                .into_iter()
                .map(|field| {
                    let ty = type_env_lookup(type_env, field.typ)?;
                    Ok((field.name, ty))
                })
                .collect::<Result<Vec<_>, TypeError>>()?,
            next_type_id(),
        )),
        TypeDecl::Array(typ) => {
            let ty = type_env_lookup(type_env, typ)?;
            Ok(Type::Array(Box::from(ty), next_type_id()))
        }
    }
}

fn type_env_lookup(
    type_env: &mut SymbolTable<Type>,
    symbol: Spanned<Symbol>,
) -> Result<Type, TypeError> {
    if let Some(typ) = type_env.lookup(&symbol.0) {
        Ok(typ)
    } else {
        Err(TypeError::new(
            symbol.1,
            TypeErrorKind::UnknownIdentifier(symbol.0),
        ))
    }
}

fn value_env_lookup(
    value_env: &mut SymbolTable<ValueEnvEntry>,
    symbol: Spanned<Symbol>,
) -> Result<ValueEnvEntry, TypeError> {
    if let Some(typ) = value_env.lookup(&symbol.0) {
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
    TypeMismatch { expected: Type, found: Type },
    ArgCountMismatch { expected: usize, found: usize },
    InvalidIdentifier,
    NotAnArrayType,
    NotARecordType,
    MissingRecordFields { missing: Vec<Symbol> },
    UnexpectedRecordField(Symbol),
    UnexpectedFieldAccess,
    UnexpectedSubscript,
    InvalidOperator,
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

static CURRENT_TYPE_ID: AtomicU32 = AtomicU32::new(0);
fn next_type_id() -> u32 {
    CURRENT_TYPE_ID.fetch_add(1, Ordering::SeqCst)
}
