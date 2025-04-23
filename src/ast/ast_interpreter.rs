#![allow(unused)]

use crate::ast::BinOp::{Minus, Plus, Times};
use crate::ast::{BinOp, Exp, Stm};
use std::cmp::max;
use std::collections::HashMap;


fn max_args(stm: &Stm) -> usize {
    match stm {
        Stm::Compound(stm1, stm2) => max(max_args(stm1), max_args(stm2)),
        Stm::Assign(_, exp) => max_args_in_exp(exp.as_ref()),
        Stm::Print(exps) => {
            let inner = exps
                .iter()
                .map(max_args_in_exp)
                .max()
                .unwrap_or(0);
            max(exps.len(), inner)
        }
    }
}

fn max_args_in_exp(exp: &Exp) -> usize {
    match exp {
        Exp::Id(_) => 0,
        Exp::Num(_) => 0,
        Exp::Op(left, _, right) => max(
            max_args_in_exp(left.as_ref()),
            max_args_in_exp(right.as_ref()),
        ),
        Exp::Eseq(stm, exp) => max(max_args(stm.as_ref()), max_args_in_exp(exp.as_ref())),
    }
}

fn interp(stm: &Stm) {
    let map = HashMap::new();
    interp_stm(stm, &map);
}

fn interp_stm(stm: &Stm, table: &HashMap<String, i32>) -> HashMap<String, i32> {
    match stm {
        Stm::Compound(stm1, stm2) => {
            let t1 = interp_stm(stm1.as_ref(), table);
            interp_stm(stm2.as_ref(), &t1)
        }
        Stm::Assign(id, exp) => {
            let (val, t1) = interp_exp(exp.as_ref(), table);
            let mut new_table = t1.clone();
            new_table.insert(id.clone(), val);
            new_table
        }
        Stm::Print(exps) => {
            let mut t = table.clone();
            let mut values = Vec::with_capacity(exps.len());
            for exp in exps {
                let (val, t2) = interp_exp(exp, &t);
                values.push(val);
                t = t2;
            }
            println!(
                "{}",
                values
                    .iter()
                    .map(|i| i.to_string())
                    .collect::<Vec<_>>()
                    .join(" ")
            );
            t
        }
    }
}

fn interp_exp(exp: &Exp, table: &HashMap<String, i32>) -> (i32, HashMap<String, i32>) {
    match exp {
        Exp::Id(id) => (*table.get(id).unwrap(), table.clone()),
        Exp::Num(num) => (*num, table.clone()),
        Exp::Op(left, op, right) => {
            let t1 = table.clone();
            let (l, t2) = interp_exp(left.as_ref(), &t1);
            let (r, t3) = interp_exp(right.as_ref(), &t2);
            let result = match op {
                Plus => l + r,
                Minus => l - r,
                Times => l * r,
                BinOp::Div => l / r,
            };
            (result, t3)
        }
        Exp::Eseq(stm, exp) => {
            let t1 = table.clone();
            interp_exp(exp.as_ref(), &interp_stm(stm.as_ref(), &t1))
        }
    }
}

fn prog() -> Stm {
    Stm::compound(
        Stm::assign("a", Exp::op(Exp::num(5), Plus, Exp::num(3))),
        Stm::compound(
            Stm::assign(
                "b",
                Exp::eseq(
                    Stm::print(vec![
                        Exp::id("a"),
                        Exp::op(Exp::id("a"), crate::ast::BinOp::Minus, Exp::num(1)),
                    ]),
                    Exp::op(Exp::num(10), Times, Exp::id("a")),
                ),
            ),
            Stm::print(vec![Exp::id("b")]),
        ),
    )
}

#[test]
pub fn test_max_args() {
    assert_eq!(2, max_args(&prog()));
}

#[test]
pub fn test_interpreter() {
    let table = interp_stm(&prog(), &HashMap::new());
    assert_eq!(8, *table.get("a").unwrap());
    assert_eq!(80, *table.get("b").unwrap())
}

#[test]
pub fn map_clone_test() {
    let mut map1 = HashMap::new();
    map1.insert("a", 1);
    let mut map2 = map1.clone();
    assert_eq!(1, *map2.get("a").unwrap());
    map2.insert("a", 2);
    assert_eq!(1, *map1.get("a").unwrap());
}
