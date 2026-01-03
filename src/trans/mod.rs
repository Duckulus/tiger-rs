use crate::parse::ast::Oper;
use crate::semant::types::Type;
use crate::trans::fragment::Fragment;
use crate::trans::frame::Frame;
use crate::trans::temp::{Label, Temp};
use crate::trans::tree::{Patch, TreeBinOp, TreeExp, TreeRelOp, TreeStm, to_seq};
use std::cell::RefCell;
use std::rc::Rc;

mod fragment;
pub mod frame;
pub mod mips_frame;
pub mod temp;
mod tree;

pub type TrAccess<F> = (<F as Frame>::Access, Level<F>);

#[derive(Clone, Debug)]
pub struct Level<F: Frame> {
    parent: Option<Rc<Level<F>>>,
    frame: RefCell<F>,
    depth: usize,
}

impl<F: Frame> Level<F> {
    pub fn new_inner(parent: Rc<Level<F>>, label: Label, mut formals: Vec<bool>) -> Level<F> {
        formals.insert(0, true); // insert static link as pseudo-parameter
        let frame = Frame::new_frame(label, formals);
        let depth = parent.depth + 1;
        Self {
            parent: Some(parent),
            frame: RefCell::new(frame),
            depth,
        }
    }

    pub fn new_outermost() -> Level<F> {
        let frame = Frame::new_frame(Label::new_unnamed(), Vec::new());
        Self {
            parent: None,
            frame: RefCell::new(frame),
            depth: 0,
        }
    }

    pub fn alloc_local(&self, escape: bool) -> TrAccess<F> {
        let access = self.frame.borrow_mut().alloc_local(escape);
        (access, self.clone())
    }

    pub fn formals(&self) -> Vec<TrAccess<F>> {
        self.frame
            .borrow()
            .formals()
            .iter()
            .skip(1) // Skip static link
            .map(|acc| (acc.clone(), self.clone()))
            .collect()
    }

    /// returns static link unless this is the outermost level
    pub fn static_link(&self) -> Option<TrAccess<F>> {
        self.frame
            .borrow()
            .formals()
            .first()
            .map(|acc| (acc.clone(), self.clone()))
    }
}

#[derive(Debug)]
pub struct Cx {
    trues: Vec<Patch>,
    falses: Vec<Patch>,
    stm: TreeStm,
}

#[derive(Debug)]
pub enum TrExp {
    Ex(TreeExp),
    Nx(TreeStm),
    Cx(Cx),
}

pub fn patch(cx: &Cx, true_label: Label, false_label: Label) {
    for patch in &cx.trues {
        *patch.borrow_mut() = Some(true_label.clone());
    }
    for patch in &cx.falses {
        *patch.borrow_mut() = Some(false_label.clone());
    }
}

pub fn un_ex(exp: TrExp) -> TreeExp {
    match exp {
        TrExp::Ex(exp) => exp,
        TrExp::Nx(stm) => TreeExp::eseq(stm, TreeExp::constt(0)),
        TrExp::Cx(cx) => {
            let true_label = Label::new_unnamed();
            let false_label = Label::new_unnamed();
            patch(&cx, true_label.clone(), false_label.clone());
            let r = Temp::new();
            TreeExp::eseq(
                TreeStm::seq(
                    TreeStm::movee(TreeExp::temp(r.clone()), TreeExp::constt(1)),
                    TreeStm::seq(
                        cx.stm,
                        TreeStm::seq(
                            TreeStm::label(false_label),
                            TreeStm::seq(
                                TreeStm::movee(TreeExp::temp(r.clone()), TreeExp::constt(0)),
                                TreeStm::Label(true_label),
                            ),
                        ),
                    ),
                ),
                TreeExp::temp(r),
            )
        }
    }
}

pub fn un_nx(exp: TrExp) -> TreeStm {
    match exp {
        TrExp::Ex(t_ex) => TreeStm::exp(t_ex),
        TrExp::Nx(stm) => stm,
        TrExp::Cx(cx) => {
            let end_label = Label::new_unnamed();
            patch(&cx, end_label.clone(), end_label.clone());
            TreeStm::seq(cx.stm, TreeStm::label(end_label))
        }
    }
}

pub fn un_cx(exp: TrExp) -> Cx {
    match exp {
        TrExp::Ex(exp) => {
            let t = Rc::new(RefCell::new(None));
            let f = Rc::new(RefCell::new(None));
            let stm = TreeStm::cjump(TreeRelOp::Gt, exp, TreeExp::Const(0), t.clone(), f.clone());
            Cx {
                stm,
                trues: vec![t],
                falses: vec![f],
            }
        }
        TrExp::Nx(_) => {
            panic!("un_cx called with Nx")
        }
        TrExp::Cx(cx) => cx,
    }
}

pub struct Translator<F: Frame> {
    fragments: RefCell<Vec<Fragment<F>>>,
}

impl<F: Frame> Default for Translator<F> {
    fn default() -> Self {
        Self::new()
    }
}

impl<F: Frame> Translator<F> {
    pub fn new() -> Self {
        Translator {
            fragments: RefCell::new(Vec::new()),
        }
    }
    pub fn trans_string(&self, string: String) -> TrExp {
        let label = Label::new_unnamed();
        let frag = Fragment::String(label.clone(), string);
        self.fragments.borrow_mut().push(frag);
        TrExp::Ex(TreeExp::Name(label))
    }

    pub fn proc_entry_exit(&self, level: Rc<Level<F>>, body: TrExp) {
        let frag = Fragment::Proc(un_nx(body), level.frame.borrow().clone());
        self.fragments.borrow_mut().push(frag);
    }

    pub fn get_result(&self) -> Vec<Fragment<F>> {
        self.fragments.borrow().clone()
    }
}

pub fn trans_simple_var<F: Frame>(access: TrAccess<F>, used_level: Rc<Level<F>>) -> TrExp {
    let (access, level) = access;
    let mut current_level = used_level;
    let mut current_level_fp = TreeExp::temp(F::fp());
    while level.depth < current_level.depth {
        let (static_link, _) = current_level
            .static_link()
            .expect("Inner level should have static link");
        let previous_fp = F::build_exp(static_link.clone(), current_level_fp);

        current_level = current_level.parent.as_ref().unwrap().clone();
        current_level_fp = previous_fp;
    }
    TrExp::Ex(F::build_exp(access, current_level_fp))
}

pub fn trans_array_subscript<F: Frame>(array: TrExp, index: TrExp) -> TrExp {
    // TODO bounds and nil checking
    TrExp::Ex(TreeExp::mem(TreeExp::plus(
        un_ex(array),
        TreeExp::mul(un_ex(index), TreeExp::constt(F::word_size())),
    )))
}

pub fn trans_field_access<F: Frame>(value: TrExp, field_index: usize) -> TrExp {
    // TODO nil checking
    TrExp::Ex(TreeExp::mem(TreeExp::plus(
        un_ex(value),
        TreeExp::mul(
            TreeExp::constt(field_index as i32),
            TreeExp::constt(F::word_size()),
        ),
    )))
}

pub fn trans_nil() -> TrExp {
    TrExp::Ex(TreeExp::constt(0))
}

pub fn trans_int(value: i32) -> TrExp {
    TrExp::Ex(TreeExp::constt(value))
}

pub fn trans_binary_arithmetic(op: Oper, lhs: TrExp, rhs: TrExp) -> TrExp {
    let oper = match op {
        Oper::Plus => TreeBinOp::Plus,
        Oper::Minus => TreeBinOp::Minus,
        Oper::Times => TreeBinOp::Mul,
        Oper::Divide => TreeBinOp::Div,
        _ => panic!("Expected arithmetic operator found {:?}", op),
    };
    TrExp::Ex(TreeExp::bin_op(oper, un_ex(lhs), un_ex(rhs)))
}

pub fn trans_rel_op(op: Oper, typ: Type, lhs: TrExp, rhs: TrExp) -> TrExp {
    let oper = match op {
        Oper::Eq => TreeRelOp::Eq,
        Oper::Neq => TreeRelOp::Ne,
        Oper::Lt => TreeRelOp::Lt,
        Oper::Le => TreeRelOp::Ule,
        Oper::Gt => TreeRelOp::Ugt,
        Oper::Ge => TreeRelOp::Uge,
        _ => panic!("Expected rel operator found {:?}", op),
    };

    let (final_lhs, final_rhs) = if typ == Type::String {
        (
            TreeExp::call(
                TreeExp::name(Label::new("stringCompare".to_string())),
                vec![un_ex(lhs), un_ex(rhs)],
            ),
            TreeExp::constt(0),
        )
    } else {
        (un_ex(lhs), un_ex(rhs))
    };

    let t = Rc::new(RefCell::new(None));
    let f = Rc::new(RefCell::new(None));
    TrExp::Cx(Cx {
        trues: vec![t.clone()],
        falses: vec![f.clone()],
        stm: TreeStm::cjump(oper, final_lhs, final_rhs, t, f),
    })
}

pub fn trans_if_else(cond: TrExp, then: TrExp, elsee: TrExp) -> TrExp {
    let true_label = Label::new_unnamed();
    let false_label = Label::new_unnamed();

    let cx = un_cx(cond);
    patch(&cx, true_label.clone(), false_label.clone());

    let r = Temp::new();
    let join_label = Label::new_unnamed();
    // TODO optimize the case that then and else return void
    TrExp::Ex(TreeExp::eseq(
        TreeStm::seq(
            cx.stm,
            TreeStm::seq(
                TreeStm::label(true_label),
                TreeStm::seq(
                    TreeStm::movee(TreeExp::temp(r.clone()), un_ex(then)),
                    TreeStm::seq(
                        TreeStm::jump(TreeExp::Name(join_label.clone()), vec![join_label.clone()]),
                        TreeStm::seq(
                            TreeStm::label(false_label),
                            TreeStm::seq(
                                TreeStm::movee(TreeExp::temp(r.clone()), un_ex(elsee)),
                                TreeStm::label(join_label),
                            ),
                        ),
                    ),
                ),
            ),
        ),
        TreeExp::temp(r),
    ))
}

pub fn trans_if(cond: TrExp, then: TrExp) -> TrExp {
    let true_label = Label::new_unnamed();
    let false_label = Label::new_unnamed();

    let cx = un_cx(cond);
    patch(&cx, true_label.clone(), false_label.clone());

    TrExp::Nx(TreeStm::seq(
        cx.stm,
        TreeStm::seq(
            TreeStm::label(true_label),
            TreeStm::seq(un_nx(then), TreeStm::label(false_label)),
        ),
    ))
}

pub fn trans_record<F: Frame>(fields: Vec<TrExp>) -> TrExp {
    let mut moves = None;
    let r = Temp::new();
    let n_fields = fields.len();
    for (i, field) in fields.into_iter().enumerate().rev() {
        let offset = i as i32 * F::word_size();
        let node = TreeStm::movee(
            TreeExp::mem(TreeExp::plus(
                TreeExp::temp(r.clone()),
                TreeExp::constt(offset),
            )),
            un_ex(field),
        );
        if let Some(exp) = moves {
            moves = Some(TreeStm::seq(node, exp))
        } else {
            moves = Some(node)
        }
    }
    let mut moves = moves.unwrap_or_else(|| TreeStm::exp(TreeExp::constt(0)));

    moves = TreeStm::seq(
        TreeStm::movee(
            TreeExp::temp(r.clone()),
            TreeExp::call(
                TreeExp::name(Label::new("initRecord".to_string())),
                vec![TreeExp::constt(n_fields as i32 * F::word_size())],
            ),
        ),
        moves,
    );
    TrExp::Ex(TreeExp::eseq(moves, TreeExp::temp(r)))
}

pub fn trans_array(size: TrExp, init_value: TrExp) -> TrExp {
    let r = Temp::new();

    let alloc = TreeStm::movee(
        TreeExp::temp(r.clone()),
        TreeExp::call(
            TreeExp::name(Label::new("initArray".to_string())),
            vec![un_ex(size), un_ex(init_value)],
        ),
    );
    TrExp::Ex(TreeExp::eseq(alloc, TreeExp::temp(r)))
}

pub fn trans_while(condition: TrExp, body: TrExp, end_label: Label) -> TrExp {
    let test_label = Label::new_unnamed();
    let body_label = Label::new_unnamed();
    let cx = un_cx(condition);
    patch(&cx, body_label.clone(), end_label.clone());
    TrExp::Nx(TreeStm::seq(
        TreeStm::label(test_label.clone()),
        TreeStm::seq(
            cx.stm,
            TreeStm::seq(
                TreeStm::Label(body_label),
                TreeStm::seq(
                    un_nx(body),
                    TreeStm::seq(
                        TreeStm::jump(TreeExp::name(test_label.clone()), vec![test_label]),
                        TreeStm::label(end_label),
                    ),
                ),
            ),
        ),
    ))
}

pub fn trans_break(end_label: Label) -> TrExp {
    TrExp::Nx(TreeStm::jump(
        TreeExp::name(end_label.clone()),
        vec![end_label],
    ))
}

pub fn trans_assign(var: TrExp, value: TrExp) -> TrExp {
    TrExp::Nx(TreeStm::movee(un_ex(var), un_ex(value)))
}

pub fn trans_seq(mut exps: Vec<TrExp>) -> TrExp {
    let last = exps.pop().unwrap();

    TrExp::Ex(TreeExp::eseq(
        to_seq(exps.into_iter().map(un_nx).collect()),
        un_ex(last),
    ))
}

pub fn trans_fun_call<F: Frame>(
    label: Label,
    args: Vec<TrExp>,
    fun_level: Rc<Level<F>>,
    level: Rc<Level<F>>,
) -> TrExp {
    let mut static_link_exp;

    if fun_level.depth > level.depth {
        // calling child function -> pass own fp
        static_link_exp = TreeExp::temp(F::fp());
    } else {
        // calling sibling or parent function -> climb from level to the level where is fun_level in
        let mut current_level = level.clone();
        static_link_exp = TreeExp::temp(F::fp());
        while current_level.depth > fun_level.depth - 1 {
            let (sl_access, _) = current_level
                .static_link()
                .expect("Inner level should have static link");
            let previous_fp = F::build_exp(sl_access, static_link_exp);

            current_level = current_level.parent.as_ref().unwrap().clone();
            static_link_exp = previous_fp;
        }
    }

    let mut vec = vec![static_link_exp];
    vec.extend(&mut args.into_iter().map(un_ex));
    TrExp::Ex(TreeExp::call(TreeExp::name(label), vec))
}

pub fn trans_var_dec<F: Frame>(access: TrAccess<F>, value_exp: TrExp) -> TrExp {
    let dest = F::build_exp(access.0, TreeExp::Temp(F::fp()));
    TrExp::Nx(TreeStm::movee(dest, un_ex(value_exp)))
}

pub fn noop() -> TrExp {
    TrExp::Nx(TreeStm::exp(TreeExp::constt(0)))
}

pub fn trans_let(decs: Vec<TrExp>, exps: Vec<TrExp>) -> TrExp {
    let decs = to_seq(decs.into_iter().map(un_nx).collect());
    TrExp::Ex(TreeExp::eseq(decs, un_ex(trans_seq(exps))))
}
