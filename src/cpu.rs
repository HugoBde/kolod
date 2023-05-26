/*** CONSTANTS ***/
const STATE_REGISTERS_NUM: usize = 6;

/*** ENUMS ***/
enum CPU_STATE {
    ARM,
    THUMB,
}

/*** STRUCTS ***/
pub struct CPU {
    state: CPU_STATE,
    status_registers: [u32; STATE_REGISTERS_NUM],
}

impl CPU {
    pub fn new() -> CPU {
        CPU {
            state: CPU_STATE::ARM,
            status_registers: [0; STATE_REGISTERS_NUM],
        }
    }
}
