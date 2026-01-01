use crate::trans::temp::{Label, Temp};
use crate::trans::tree::TreeExp;
use std::fmt::Debug;

pub trait Frame: Clone + Debug {
    type Access: Clone + Debug;

    fn new_frame(name: Label, formals: Vec<bool>) -> Self;

    fn name(&self) -> Label;

    fn formals(&self) -> Vec<Self::Access>;

    fn alloc_local(&mut self, escape: bool) -> Self::Access;

    fn fp() -> Temp;

    fn build_exp(access: Self::Access, fp: TreeExp) -> TreeExp;

    fn word_size() -> i32;
}
