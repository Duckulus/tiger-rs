use crate::parse::ast::{Dec, Exp, Var};
use crate::semant::env::SymbolTable;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Clone)]
struct EscapeEnvEntry {
    depth: usize,
    ptr: Rc<RefCell<bool>>,
}

type EscapeEnv = SymbolTable<EscapeEnvEntry>;

pub fn find_escape(exp: &Exp) {
    let mut env = SymbolTable::empty();
    traverse_exp(&mut env, 0, exp);
}

fn traverse_exp(env: &mut EscapeEnv, depth: usize, exp: &Exp) {
    match exp {
        Exp::Var(var) => traverse_var(env, depth, var.as_ref()),
        Exp::Nil => {}
        Exp::Int(_) => {}
        Exp::String(_) => {}
        Exp::Call(_, args) => {
            for arg in args {
                traverse_exp(env, depth, &arg.0);
            }
        }
        Exp::Op(_, l, r) => {
            traverse_exp(env, depth, &l.as_ref().0);
            traverse_exp(env, depth, &r.as_ref().0);
        }
        Exp::Record(_, fields) => {
            for field in fields {
                traverse_exp(env, depth, &field.value.0);
            }
        }
        Exp::Seq(exps) => {
            for exp in exps {
                traverse_exp(env, depth, &exp.0);
            }
        }
        Exp::Assign(var, exp) => {
            traverse_var(env, depth, var.as_ref());
            traverse_exp(env, depth, &exp.0);
        }
        Exp::If { cond, then, elsee } => {
            traverse_exp(env, depth, &cond.0);
            traverse_exp(env, depth, &then.0);
            if let Some(elsee) = elsee {
                traverse_exp(env, depth, &elsee.0);
            }
        }
        Exp::While { cond, body } => {
            traverse_exp(env, depth, &cond.0);
            traverse_exp(env, depth, &body.0);
        }
        Exp::Break(_) => {}
        Exp::For {
            body,
            var,
            escaping,
            lo,
            hi,
        } => {
            traverse_exp(env, depth, &lo.0);
            traverse_exp(env, depth, &hi.0);
            env.begin_scope();
            env.enter(
                var.clone(),
                EscapeEnvEntry {
                    ptr: escaping.clone(),
                    depth,
                },
            );
            traverse_exp(env, depth, &body.0);
            env.end_scope();
        }
        Exp::Let(decs, exps) => {
            env.begin_scope();
            for dec in decs {
                traverse_dec(env, depth, dec)
            }
            for exp in exps {
                traverse_exp(env, depth, &exp.0);
            }
            env.end_scope();
        }
        Exp::Array { init, size, .. } => {
            traverse_exp(env, depth, &size.0);
            traverse_exp(env, depth, &init.0);
        }
    }
}

fn traverse_dec(env: &mut EscapeEnv, depth: usize, dec: &Dec) {
    match dec {
        Dec::Function(fun_decs) => {
            for fun_dec in fun_decs {
                let new_depth = depth + 1;
                env.begin_scope();
                for formal in &fun_dec.params {
                    env.enter(
                        formal.name.clone(),
                        EscapeEnvEntry {
                            ptr: formal.escaping.clone(),
                            depth: new_depth,
                        },
                    )
                }
                traverse_exp(env, new_depth, &fun_dec.body.0);
                env.end_scope();
            }
        }
        Dec::Var(name, _, exp, escaping) => {
            traverse_exp(env, depth, &exp.0);

            env.enter(
                name.0.clone(),
                EscapeEnvEntry {
                    ptr: escaping.clone(),
                    depth,
                },
            );
        }
        Dec::Type(_) => {}
    }
}

fn traverse_var(env: &mut EscapeEnv, depth: usize, var: &Var) {
    match var {
        Var::Simple(name) => {
            if let Some(entry) = env.lookup(&name.0) {
                if depth > entry.depth {
                    *entry.ptr.borrow_mut() = true;
                }
            }
        }
        Var::Field(var, _) => {
            traverse_var(env, depth, var);
        }
        Var::Subscript(var, exp) => {
            traverse_var(env, depth, var);
            traverse_exp(env, depth, &exp.0);
        }
    }
}
