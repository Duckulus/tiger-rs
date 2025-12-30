use crate::trans::frame::Frame;
use crate::trans::temp::Label;
use std::cell::RefCell;
use std::rc::Rc;

pub mod frame;
pub mod mips_frame;
pub mod temp;

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
}
