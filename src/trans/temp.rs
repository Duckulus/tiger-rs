use std::sync::atomic::{AtomicU32, Ordering};

type TempID = u32;

// temp ids 0-99 are reserved for special registers
const FIRST_VIRTUAL_REG_ID: u32 = 100;
static NEXT_TEMP_ID: AtomicU32 = AtomicU32::new(FIRST_VIRTUAL_REG_ID);

#[derive(Clone, Debug)]
pub struct Temp(TempID);

impl Temp {
    pub fn new() -> Self {
        Self(NEXT_TEMP_ID.fetch_add(1, Ordering::SeqCst))
    }

    pub fn with_id(id: TempID) -> Self {
        Self(id)
    }

    pub fn id(&self) -> TempID {
        self.0
    }
}

type LabelID = u32;
static NEXT_LABEL_ID: AtomicU32 = AtomicU32::new(0);

#[derive(Clone, Debug)]
pub enum Label {
    Unnamed(LabelID),
    Named(LabelID, String),
}

impl Label {
    pub fn new_unnamed() -> Self {
        Self::Unnamed(NEXT_LABEL_ID.fetch_add(1, Ordering::SeqCst))
    }

    pub fn new(name: String) -> Self {
        Self::Named(NEXT_LABEL_ID.fetch_add(1, Ordering::SeqCst), name)
    }

    pub fn id(&self) -> LabelID {
        match self {
            Label::Unnamed(id) => *id,
            Label::Named(id, _) => *id,
        }
    }
}
