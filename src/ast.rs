
mod ast_interpreter;

enum Stm {
    Compound(Box<Stm>, Box<Stm>),
    Assign(String, Box<Exp>),
    Print(Vec<Exp>)
}

impl Stm {
    fn compound(stm1: Stm, stm2: Stm) -> Self {
       Self::Compound(Box::from(stm1), Box::from(stm2))
    }

    fn assign(id: &str, exp: Exp) -> Self {
       Self::Assign(id.to_string(), Box::from(exp))
    }

    fn print(exps: Vec<Exp>) -> Self {
        Self::Print(exps)
    }
}

enum Exp {
    Id(String),
    Num(i32),
    Op(Box<Exp>, BinOp, Box<Exp>),
    Eseq(Box<Stm>, Box<Exp>),
}

impl Exp {
    fn id(id: &str) -> Self {
       Self::Id(id.to_string())
    }

    fn num(num: i32) -> Self {
        Self::Num(num)
    }

    fn op(left: Exp, op: BinOp, right: Exp) -> Self {
        Self::Op(Box::from(left), op, Box::from(right))
    }

    fn eseq(stm: Stm, exp: Exp) -> Self {
        Self::Eseq(Box::from(stm), Box::from(exp))
    }
}

enum BinOp {
    Plus,
    Minus,
    Times,
    Div
}