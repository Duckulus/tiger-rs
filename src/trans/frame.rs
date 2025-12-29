use crate::trans::temp::Label;

pub trait Frame<A> {
    fn new_frame(name: Label, formals: Vec<bool>) -> Self;

    fn name(&self) -> Label;

    fn formals(&self) -> Vec<A>;

    fn alloc_local(&mut self, escape: bool) -> A;
}
