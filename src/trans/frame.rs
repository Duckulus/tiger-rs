use std::fmt::Debug;
use crate::trans::temp::Label;

pub trait Frame: Clone + Debug {
    type Access: Clone + Debug;

    fn new_frame(name: Label, formals: Vec<bool>) -> Self;

    fn name(&self) -> Label;

    fn formals(&self) -> Vec<Self::Access>;

    fn alloc_local(&mut self, escape: bool) -> Self::Access;
}
