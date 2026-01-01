use crate::parse::ast::{Dec, Exp, Oper, TypeDecl, Var};
use crate::parse::lexer::{Span, Spanned};
use crate::semant::env::{Symbol, SymbolTable, TypeEnv, ValueEnv, base_type_env, base_value_env};
use crate::semant::types::{Type, TypeRef, ValueEnvEntry};
use crate::trans::frame::Frame;
use crate::trans::temp::Label;
use crate::trans::{Level, TrExp, Translator, trans_array_subscript, trans_binary_arithmetic, trans_field_access, trans_if, trans_if_else, trans_int, trans_nil, trans_rel_op, trans_simple_var, trans_record, trans_array};
use chumsky::span::SimpleSpan;
use std::cell::RefCell;
use std::rc::Rc;
use std::sync::atomic::{AtomicU32, Ordering};

pub mod env;

pub mod escape;
pub mod types;

pub type TranslatedExp = (TrExp, Type);

pub fn trans_exp<F: Frame>(exp: Spanned<Exp>) -> Result<TranslatedExp, TypeError> {
    let outermost = Rc::new(Level::new_outermost());
    let mut value_env = base_value_env::<F>();
    let mut type_env = base_type_env();
    let mut translator = Translator::new();
    trans_exp_rec(
        &mut value_env,
        &mut type_env,
        &mut translator,
        outermost,
        exp,
        false,
    )
}

fn trans_exp_rec<F: Frame>(
    value_env: &mut ValueEnv<F>,
    type_env: &mut TypeEnv,
    translator: &mut Translator,
    level: Rc<Level<F>>,
    exp: Spanned<Exp>,
    in_loop: bool,
) -> Result<TranslatedExp, TypeError> {
    match exp.0.clone() {
        Exp::Var(var) => Ok(trans_var(
            value_env, type_env, translator, level, *var, in_loop,
        )?),
        Exp::Nil => Ok((trans_nil(), Type::Nil)),
        Exp::Int(value) => Ok((trans_int(value), Type::Int)),
        Exp::String(s) => Ok((translator.trans_string(s), Type::String)),
        Exp::Call(fun_name, args) => {
            let fun_type = env_lookup(value_env, fun_name.clone())?;
            if let ValueEnvEntry::Fun(arg_types, return_type, fun_level, fun_label) = fun_type {
                if args.len() != arg_types.len() {
                    let args_span = if args.is_empty() {
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
                    let arg_span = arg.1;
                    let (_, arg_type) = trans_exp_rec(
                        value_env,
                        type_env,
                        translator,
                        level.clone(),
                        arg,
                        in_loop,
                    )?;
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
                unimplemented!();
                //Ok((exp, return_type))
            } else {
                Err(TypeError::new(fun_name.1, TypeErrorKind::InvalidIdentifier))
            }
        }
        Exp::Op(op, lhs, rhs) => {
            let (left_exp, left_type) = trans_exp_rec(
                value_env,
                type_env,
                translator,
                level.clone(),
                *lhs.clone(),
                in_loop,
            )?;
            let (right_exp, right_type) = trans_exp_rec(
                value_env,
                type_env,
                translator,
                level,
                *rhs.clone(),
                in_loop,
            )?;
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
                    Ok((
                        trans_binary_arithmetic(op.0, left_exp, right_exp),
                        Type::Int,
                    ))
                }
                Oper::Lt | Oper::Le | Oper::Gt | Oper::Ge => match left_type {
                    Type::Int | Type::String => {
                        if right_type == left_type {
                            Ok((
                                trans_rel_op(op.0, left_type, left_exp, right_exp),
                                Type::Int,
                            ))
                        } else {
                            Err(TypeError::new(
                                rhs.1,
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
                        if right_type == left_type
                            || (matches!(left_type, Type::Record(_, _)) && right_type == Type::Nil)
                        {
                            Ok((
                                trans_rel_op(op.0, left_type, left_exp, right_exp),
                                Type::Int,
                            ))
                        } else {
                            Err(TypeError::new(
                                rhs.1,
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
            let typ_symbol_span = typ_symbol.1;
            let ty = env_lookup(type_env, typ_symbol)?;
            if let Type::Record(ref fields, _) = actual_type(ty.clone()) {
                let mut fields = fields.clone();
                let mut trans_fields = Vec::new();
                for efield in efields {
                    let exp_span = efield.value.1;
                    let (trans_field, given_type) = trans_exp_rec(
                        value_env,
                        type_env,
                        translator,
                        level.clone(),
                        efield.value,
                        in_loop,
                    )?;
                    trans_fields.push(trans_field);
                    if let Some(pos) = fields.iter().position(|f| f.0 == efield.name.0) {
                        let (_, expected_type) = fields.get(pos).unwrap();
                        let expected_type = actual_type(expected_type.clone());

                        if given_type != expected_type
                            && !(matches!(expected_type, Type::Record(_, _))
                                && given_type == Type::Nil)
                        // nil is part of every record type
                        {
                            return Err(TypeError {
                                span: exp_span,
                                kind: TypeErrorKind::TypeMismatch {
                                    found: given_type,
                                    expected: expected_type,
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
                Ok((trans_record::<F>(trans_fields), actual_type(ty)))
            } else {
                Err(TypeError {
                    span: typ_symbol_span,
                    kind: TypeErrorKind::NotARecordType,
                })
            }
        }
        Exp::Seq(exps) => {
            let (_, ty) = trans_exp_rec(
                value_env,
                type_env,
                translator,
                level,
                exps.last().unwrap().clone(),
                in_loop,
            )?;
            unimplemented!();
            //Ok((exp, ty))
        }
        Exp::Assign(var, exp) => {
            let (var_exp, var_ty) = trans_var(
                value_env,
                type_env,
                translator,
                level.clone(),
                *var,
                in_loop,
            )?;
            check_type(
                value_env,
                type_env,
                translator,
                level,
                var_ty,
                *exp.clone(),
                in_loop,
            )?;
            // Ok((*exp, Type::Void))

            unimplemented!();
        }
        Exp::If { cond, then, elsee } => {
            let cond_span = cond.1;
            let (cond_exp, cond_type) = trans_exp_rec(
                value_env,
                type_env,
                translator,
                level.clone(),
                *cond,
                in_loop,
            )?;
            if !matches!(cond_type, Type::Int) {
                return Err(TypeError {
                    span: cond_span,
                    kind: TypeErrorKind::TypeMismatch {
                        expected: Type::Int,
                        found: cond_type,
                    },
                });
            }
            let then_span = then.1;
            let (then_exp, then_type) = trans_exp_rec(
                value_env,
                type_env,
                translator,
                level.clone(),
                *then,
                in_loop,
            )?;
            if let Some(elsee) = elsee {
                let else_span = elsee.1;
                let (else_exp, else_type) =
                    trans_exp_rec(value_env, type_env, translator, level, *elsee, in_loop)?;
                if then_type != else_type {
                    Err(TypeError {
                        span: else_span,
                        kind: TypeErrorKind::TypeMismatch {
                            expected: then_type,
                            found: else_type,
                        },
                    })
                } else {
                    Ok((trans_if_else(cond_exp, then_exp, else_exp), then_type))
                }
            } else if !matches!(then_type, Type::Void) {
                // then must be void if there is no else
                Err(TypeError {
                    span: then_span,
                    kind: TypeErrorKind::TypeMismatch {
                        expected: Type::Void,
                        found: then_type,
                    },
                })
            } else {
                Ok((trans_if(cond_exp, then_exp), then_type))
            }
        }
        Exp::While { cond, body } => {
            check_type(
                value_env,
                type_env,
                translator,
                level.clone(),
                Type::Int,
                *cond,
                in_loop,
            )?;
            check_type(
                value_env,
                type_env,
                translator,
                level,
                Type::Void,
                *body,
                true,
            )?;
            //Ok((exp, Type::Void))
            unimplemented!()
        }
        Exp::Break(span) => {
            if in_loop {
                // Ok((exp, Type::Void))
                unimplemented!()
            } else {
                Err(TypeError::new(span, TypeErrorKind::BreakOutsideLoop))
            }
        }
        Exp::For {
            var,
            escaping: escape,
            lo,
            hi,
            body,
        } => {
            check_type(
                value_env,
                type_env,
                translator,
                level.clone(),
                Type::Int,
                *lo,
                in_loop,
            )?;
            check_type(
                value_env,
                type_env,
                translator,
                level.clone(),
                Type::Int,
                *hi,
                in_loop,
            )?;

            value_env.begin_scope();
            let access = level.alloc_local(*escape.borrow());
            value_env.enter(var, ValueEnvEntry::Var(Type::Int, access));
            check_type(
                value_env,
                type_env,
                translator,
                level,
                Type::Void,
                *body,
                true,
            )?;
            value_env.end_scope();

            // Ok((exp, Type::Void))
            unimplemented!()
        }
        Exp::Let(decs, exps) => {
            value_env.begin_scope();
            type_env.begin_scope();
            for dec in decs {
                trans_dec(value_env, type_env, translator, level.clone(), dec, in_loop)?;
            }
            let mut last = None;
            for exp in exps {
                last = Some(trans_exp_rec(
                    value_env,
                    type_env,
                    translator,
                    level.clone(),
                    exp,
                    in_loop,
                )?);
            }
            type_env.end_scope();
            value_env.end_scope();
            // Ok((exp, last.unwrap().1))
            unimplemented!()
        }
        Exp::Array { typ, size, init } => {
            let typ_span = typ.1;
            let ty = actual_type(env_lookup(type_env, typ)?);
            if let Type::Array(ref elem_type, _) = ty {
                let (size_exp, size_type) = trans_exp_rec(
                    value_env,
                    type_env,
                    translator,
                    level.clone(),
                    *size.clone(),
                    in_loop,
                )?;
                if size_type != Type::Int {
                    return Err(TypeError {
                        span: size.1,
                        kind: TypeErrorKind::TypeMismatch {
                            expected: Type::Int,
                            found: size_type,
                        },
                    });
                }
                let (init_exp, init_type) = trans_exp_rec(
                    value_env,
                    type_env,
                    translator,
                    level,
                    *init.clone(),
                    in_loop,
                )?;
                if init_type != *elem_type.borrow() {
                    return Err(TypeError {
                        span: init.1,
                        kind: TypeErrorKind::TypeMismatch {
                            expected: elem_type.borrow().clone(),
                            found: init_type,
                        },
                    });
                }
                    Ok((trans_array(size_exp, init_exp), ty))
            } else {
                Err(TypeError {
                    span: typ_span,
                    kind: TypeErrorKind::NotAnArrayType,
                })
            }
        }
    }
}

fn trans_var<F: Frame>(
    value_env: &mut ValueEnv<F>,
    type_env: &mut TypeEnv,
    translator: &mut Translator,
    level: Rc<Level<F>>,
    var: Var,
    in_loop: bool,
) -> Result<TranslatedExp, TypeError> {
    match var {
        Var::Simple(symb) => {
            let symb_span = symb.1;
            let entry = env_lookup(value_env, symb)?;

            match entry {
                ValueEnvEntry::Var(typ, access) => Ok((trans_simple_var(access, level), typ)),
                ValueEnvEntry::Fun(_, _, _, _) => {
                    Err(TypeError::new(symb_span, TypeErrorKind::InvalidIdentifier))
                }
            }
        }
        Var::Field(var, symb) => {
            let (var_exp, var_typ) =
                trans_var(value_env, type_env, translator, level, *var, in_loop)?;
            if let Type::Record(fields, _) = var_typ {
                let field_index = fields.iter().position(|f| f.0 == symb.0);
                if let Some(position) = field_index {
                    let (_, field_type) = fields.get(position).unwrap();
                    Ok((
                        trans_field_access::<F>(var_exp, position),
                        actual_type(field_type.clone()),
                    ))
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
            let exp_span = exp.1;
            let (index_exp, index_type) = trans_exp_rec(
                value_env,
                type_env,
                translator,
                level.clone(),
                *exp,
                in_loop,
            )?;
            if index_type != Type::Int {
                return Err(TypeError::new(
                    exp_span,
                    TypeErrorKind::TypeMismatch {
                        expected: Type::Int,
                        found: index_type,
                    },
                ));
            }
            let (var_exp, var_typ) =
                trans_var(value_env, type_env, translator, level, *var, in_loop)?;
            if let Type::Array(elem_type, _) = var_typ {
                Ok((
                    trans_array_subscript::<F>(var_exp, index_exp),
                    elem_type.borrow().clone(),
                ))
            } else {
                Err(TypeError::new(exp_span, TypeErrorKind::UnexpectedSubscript))
            }
        }
    }
}

fn trans_dec<F: Frame>(
    value_env: &mut ValueEnv<F>,
    type_env: &mut TypeEnv,
    translator: &mut Translator,
    level: Rc<Level<F>>,
    dec: Dec,
    in_loop: bool,
) -> Result<(), TypeError> {
    match dec {
        Dec::Function(funcs) => {
            let mut func_levels = Vec::new();
            for func in &funcs {
                let mut param_types = Vec::new();
                let mut param_escaping = Vec::new();
                for param in &func.params {
                    let param_type = actual_type(env_lookup(type_env, param.typ.clone())?);
                    param_types.push(param_type.clone());
                    param_escaping.push(*param.escaping.borrow());
                }
                let return_type = func
                    .result
                    .clone()
                    .map(|t| env_lookup(type_env, t).map(actual_type))
                    .transpose()?
                    .unwrap_or(Type::Void);

                let label = Label::new_unnamed();
                let new_level = Rc::new(Level::new_inner(
                    level.clone(),
                    label.clone(),
                    param_escaping,
                ));
                func_levels.push(new_level.clone());
                value_env.enter(
                    func.name.clone(),
                    ValueEnvEntry::Fun(param_types, return_type, new_level, label),
                );
            }
            for (func, func_level) in funcs.into_iter().zip(func_levels) {
                value_env.begin_scope();
                for (param, access) in func.params.into_iter().zip(func_level.formals()) {
                    let param_type = actual_type(env_lookup(type_env, param.typ)?);
                    value_env.enter(param.name, ValueEnvEntry::Var(param_type, access));
                }
                let return_exp = trans_exp_rec(
                    value_env,
                    type_env,
                    translator,
                    func_level,
                    func.body.clone(),
                    false,
                )?;
                value_env.end_scope();

                if let Some(return_type_symbol) = func.result {
                    let return_type = actual_type(env_lookup(type_env, return_type_symbol)?);
                    if return_type != return_exp.1 {
                        return Err(TypeError {
                            span: func.body.1,
                            kind: TypeErrorKind::TypeMismatch {
                                expected: return_type,
                                found: return_exp.1,
                            },
                        });
                    }
                } else if return_exp.1 != Type::Void {
                    return Err(TypeError {
                        span: func.body.1,
                        kind: TypeErrorKind::TypeMismatch {
                            expected: Type::Void,
                            found: return_exp.1,
                        },
                    });
                }
            }
            Ok(())
        }
        Dec::Var(symb, type_annotation, exp, escaping) => {
            let exp_ty = trans_exp_rec(
                value_env,
                type_env,
                translator,
                level.clone(),
                *exp.clone(),
                in_loop,
            )?;
            if let Some(typ_sym) = type_annotation {
                let typ = actual_type(env_lookup(type_env, typ_sym)?);
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
            let access = level.alloc_local(*escaping.borrow());
            value_env.enter(symb.0, ValueEnvEntry::Var(exp_ty.1.clone(), access));
            Ok(())
        }
        Dec::Type(named_types) => {
            for ((name, _), _) in &named_types {
                type_env.enter(
                    name.clone(),
                    Rc::new(RefCell::new(Type::Name(name.clone(), None))),
                );
            }
            let mut refs = Vec::new();
            for ((name, span), type_decl) in &named_types {
                let typ = trans_type_decl(type_env, type_decl.clone())?;
                let placeholder_type = type_env.lookup(name).unwrap();
                refs.push((placeholder_type.clone(), span));
                let Type::Name(_, ref mut type_ref) = *placeholder_type.borrow_mut() else {
                    panic!("encountered non name type where it was expected")
                };
                *type_ref = Some(typ);
            }
            for (type_ref, span) in refs {
                if is_cyclic(type_ref) {
                    return Err(TypeError::new(*span, TypeErrorKind::IllegalCycle));
                }
            }
            Ok(())
        }
    }
}

fn is_cyclic(type_ref: TypeRef) -> bool {
    let mut typ = type_ref.borrow().clone();
    while let Type::Name(_, other) = typ {
        if other.is_none() {
            panic!("encountered empty name type");
        }
        let other = other.unwrap();
        if other == type_ref {
            return true;
        }
        let next = other.borrow().clone();
        typ = next;
    }
    false
}

fn check_type<F: Frame>(
    value_env: &mut ValueEnv<F>,
    type_env: &mut TypeEnv,
    translator: &mut Translator,
    level: Rc<Level<F>>,
    expected_type: Type,
    exp: Spanned<Exp>,
    in_loop: bool,
) -> Result<(), TypeError> {
    let span = exp.1;
    let (_, typ) = trans_exp_rec(value_env, type_env, translator, level, exp, in_loop)?;
    if typ != expected_type {
        Err(TypeError {
            span,
            kind: TypeErrorKind::TypeMismatch {
                expected: expected_type,
                found: typ,
            },
        })
    } else {
        Ok(())
    }
}

fn trans_type_decl(type_env: &mut TypeEnv, type_decl: TypeDecl) -> Result<TypeRef, TypeError> {
    match type_decl {
        TypeDecl::Name(name) => env_lookup(type_env, name),
        TypeDecl::Record(fields) => Ok(Rc::new(RefCell::new(Type::Record(
            fields
                .into_iter()
                .map(|field| {
                    let ty = env_lookup(type_env, field.typ)?;
                    Ok((field.name, ty))
                })
                .collect::<Result<Vec<_>, TypeError>>()?,
            next_type_id(),
        )))),
        TypeDecl::Array(typ) => {
            let ty = env_lookup(type_env, typ)?;
            Ok(Rc::new(RefCell::new(Type::Array(ty, next_type_id()))))
        }
    }
}

fn actual_type(mut typ: TypeRef) -> Type {
    loop {
        match typ.clone().borrow().clone() {
            Type::Name(_, Some(inner)) => {
                typ = inner.clone();
            }
            _ => return typ.borrow().clone(),
        }
    }
}

fn env_lookup<T: Clone>(
    type_env: &mut SymbolTable<T>,
    symbol: Spanned<Symbol>,
) -> Result<T, TypeError> {
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
    IllegalType,
    IllegalCycle,
    BreakOutsideLoop,
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
