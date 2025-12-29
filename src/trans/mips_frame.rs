use crate::trans::frame::Frame;
use crate::trans::temp::{Label, Temp};

const WORD_SIZE: i32 = 4;

#[derive(Clone)]
enum MipsAccess {
    InFrame(i32),
    InReg(Temp),
}

struct MipsFrame {
    label: Label,
    formals: Vec<MipsAccess>,
    locals: Vec<MipsAccess>,
    local_offset: i32,
}

// First 4 Args passed in $a0-$a3
//
// HIGH ADDRESSES
//
//     incoming args
// $fp static link (treated as formal)
//     locals
//     return address
//     temporaries
//     saved registers
//     outgoing args
// $sp static link
//
// LOW ADDRESSES
impl Frame<MipsAccess> for MipsFrame {
    fn new_frame(name: Label, formals: Vec<bool>) -> Self {
        let mut formal_accesses = Vec::with_capacity(formals.len());
        let mut local_offset = 0;
        for (i, escaping) in formals.into_iter().enumerate() {
            if i < 4 {
                if escaping {
                    formal_accesses.push(MipsAccess::InFrame(local_offset));
                    local_offset -= WORD_SIZE;
                } else {
                    formal_accesses.push(MipsAccess::InReg(Temp::new()));
                }
            } else {
                const SHADOW_SPACE: i32 = 4 * WORD_SIZE; // mips always reserves space for first 4 args even if unused
                let spilled_arg_index = i as i32 - 4;
                formal_accesses.push(MipsAccess::InFrame(
                    SHADOW_SPACE + spilled_arg_index * WORD_SIZE,
                ));
            }
        }
        Self {
            label: name,
            formals: formal_accesses,
            locals: Vec::new(),
            local_offset,
        }
    }

    fn name(&self) -> Label {
        self.label.clone()
    }

    fn formals(&self) -> Vec<MipsAccess> {
        self.formals.clone()
    }

    fn alloc_local(&mut self, escape: bool) -> MipsAccess {
        let local = if escape {
            let local = MipsAccess::InFrame(self.local_offset);
            self.local_offset -= WORD_SIZE;
            local
        } else {
            MipsAccess::InReg(Temp::new())
        };
        self.locals.push(local.clone());
        local
    }
}
