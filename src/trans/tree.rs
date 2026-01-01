use crate::trans::temp::{Label, Temp};
use std::cell::RefCell;
use std::rc::Rc;

pub type Patch = Rc<RefCell<Option<Label>>>;

#[derive(Debug)]
pub enum TreeStm {
    Seq(Box<TreeStm>, Box<TreeStm>),
    Label(Label),
    Jump {
        target: Box<TreeExp>,
        possible_targets: Vec<Label>,
    },
    CJump {
        op: TreeRelOp,
        left: Box<TreeExp>,
        right: Box<TreeExp>,
        truel: Patch,
        falsel: Patch,
    },
    Move {
        dest: Box<TreeExp>,
        source: Box<TreeExp>,
    },
    Exp(Box<TreeExp>),
}

impl TreeStm {
    pub fn seq(s1: TreeStm, s2: TreeStm) -> TreeStm {
        TreeStm::Seq(Box::new(s1), Box::new(s2))
    }

    pub fn label(l: Label) -> TreeStm {
        TreeStm::Label(l)
    }

    pub fn jump(target: TreeExp, possible_targets: Vec<Label>) -> TreeStm {
        TreeStm::Jump {
            target: Box::new(target),
            possible_targets,
        }
    }

    pub fn cjump(
        op: TreeRelOp,
        left: TreeExp,
        right: TreeExp,
        truel: Patch,
        falsel: Patch,
    ) -> TreeStm {
        TreeStm::CJump {
            op,
            left: Box::new(left),
            right: Box::new(right),
            truel,
            falsel,
        }
    }

    pub fn movee(dest: TreeExp, source: TreeExp) -> TreeStm {
        TreeStm::Move {
            dest: Box::new(dest),
            source: Box::new(source),
        }
    }

    pub fn exp(exp: TreeExp) -> TreeStm {
        TreeStm::Exp(Box::new(exp))
    }
}

#[derive(Debug)]
pub enum TreeExp {
    BinOp(TreeBinOp, Box<TreeExp>, Box<TreeExp>),
    Mem(Box<TreeExp>),
    Temp(Temp),
    Eseq(Box<TreeStm>, Box<TreeExp>),
    Name(Label),
    Const(i32),
    Call(Box<TreeExp>, Vec<TreeExp>),
}

impl TreeExp {
    pub fn eseq(stm: TreeStm, exp: TreeExp) -> TreeExp {
        TreeExp::Eseq(Box::new(stm), Box::new(exp))
    }

    pub fn constt(val: i32) -> TreeExp {
        TreeExp::Const(val)
    }

    pub fn bin_op(op: TreeBinOp, left: TreeExp, right: TreeExp) -> TreeExp {
        TreeExp::BinOp(op, Box::new(left), Box::new(right))
    }

    pub fn mem(exp: TreeExp) -> TreeExp {
        TreeExp::Mem(Box::new(exp))
    }

    pub fn temp(t: Temp) -> TreeExp {
        TreeExp::Temp(t)
    }

    pub fn name(label: Label) -> TreeExp {
        TreeExp::Name(label)
    }

    pub fn call(func: TreeExp, args: Vec<TreeExp>) -> TreeExp {
        TreeExp::Call(Box::new(func), args)
    }

    pub fn plus(left: TreeExp, right: TreeExp) -> TreeExp {
        TreeExp::BinOp(TreeBinOp::Plus, Box::new(left), Box::new(right))
    }

    pub fn mul(left: TreeExp, right: TreeExp) -> TreeExp {
        TreeExp::BinOp(TreeBinOp::Mul, Box::new(left), Box::new(right))
    }
}

#[derive(Debug)]
pub enum TreeBinOp {
    Plus,
    Minus,
    Mul,
    Div,
    And,
    Or,
    LShift,
    RShift,
    ArShift,
    Xor,
}

#[derive(Debug)]
pub enum TreeRelOp {
    EQ,
    NE,
    LT,
    GT,
    LE,
    GE,
    ULT,
    ULE,
    UGT,
    UGE,
}
