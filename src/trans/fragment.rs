use crate::trans::frame::Frame;
use crate::trans::temp::Label;
use crate::trans::tree::TreeStm;

#[derive(Debug, Clone)]
pub enum Fragment<F: Frame> {
    String(Label, String),
    Proc(TreeStm, F),
}
