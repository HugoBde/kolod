use std::cell::RefCell;
use std::rc::Rc;

use crate::memory::Memory;

const STATE_REGISTERS_NUM: usize = 6;

const GEN_PURPOSE_REGISTERS_NUM: usize = 31;

const SAVED_PROGRAM_STATUS_REGISTERS_NUM: usize = 5;

const STACK_POINTER_REGISTER_INDEX: u32 = 13;

const LINK_REGISTER_INDEX: u32 = 14;

const PROGRAM_COUNTER_INDEX: u32 = 15;

#[derive(PartialEq)]

enum CpuMode {
    ARM,
    THUMB,
}

enum CpuState {
    User      = 0b10000,
    FIQ       = 0b10001,
    IRQ       = 0b10010,
    Super     = 0b10011,
    Abort     = 0b10111,
    Undefined = 0b11011,
    System    = 0b11111,
}

pub struct CPU {
    mode:  CpuMode,
    state: CpuState,

    gen_registers:    [u32; GEN_PURPOSE_REGISTERS_NUM],
    status_registers: [u32; STATE_REGISTERS_NUM],
    cpsr_register:    u32,
    spsr_register:    [u32; SAVED_PROGRAM_STATUS_REGISTERS_NUM],
    mem:              Rc<RefCell<Memory>>,
}

enum Flag {
    N = 1 << 31,
    Z = 1 << 30,
    C = 1 << 29,
    V = 1 << 28,
    Q = 1 << 27,
}

#[allow(dead_code)]
impl CPU {
    pub fn new(mem: Rc<RefCell<Memory>>) -> CPU {

        CPU {
            mode: CpuMode::ARM,
            state: CpuState::User,
            gen_registers: [0; GEN_PURPOSE_REGISTERS_NUM],
            status_registers: [0; STATE_REGISTERS_NUM],
            cpsr_register: 0,
            spsr_register: [0; SAVED_PROGRAM_STATUS_REGISTERS_NUM],
            mem,
        }
    }

    fn get_flag(&self, flag: Flag) -> u32 {

        if self.cpsr_register & flag as u32 != 0 {

            1
        } else {

            0
        }
    }

    fn read_pc(&self) -> u32 {

        self.gen_registers[PROGRAM_COUNTER_INDEX as usize]
    }

    fn write_pc(&mut self, val: u32) {

        self.gen_registers[PROGRAM_COUNTER_INDEX as usize] = val;
    }

    fn add_pc(&mut self, offset: i32) {

        if offset < 0 {

            self.gen_registers[PROGRAM_COUNTER_INDEX as usize] -= offset as u32;
        } else {

            self.gen_registers[PROGRAM_COUNTER_INDEX as usize] += offset as u32;
        }
    }

    // Gen registers map:
    // System/User FIQ       Supervisor Abort     IRQ       Undefined
    // --------------------------------------------------------------
    // R0          R0        R0         R0        R0        R0
    // R1          R1        R1         R1        R1        R1
    // R2          R2        R2         R2        R2        R2
    // R3          R3        R3         R3        R3        R3
    // R4          R4        R4         R4        R4        R4
    // R5          R5        R5         R5        R5        R5
    // R6          R6        R6         R6        R6        R6
    // R7          R7        R7         R7        R7        R7
    // --------------------------------------------------------------
    // R8          R8_fiq    R8         R8        R8        R8
    // R9          R9_fiq    R9         R9        R9        R9
    // R10         R10_fiq   R10        R10       R10       R10
    // R11         R11_fiq   R11        R11       R11       R11
    // R12         R12_fiq   R12        R12       R12       R12
    // R13 (SP)    R13_fiq   R13_svc    R13_abt   R13_irq   R13_und
    // R14 (LR)    R14_fiq   R14_svc    R14_abt   R14_irq   R14_und
    // R15 (PC)    R15       R15        R15       R15       R15
    // --------------------------------------------------------------
    // CPSR        CPSR      CPSR       CPSR      CPSR      CPSR
    // --          SPSR_fiq  SPSR_svc   SPSR_abt  SPSR_irq  SPSR_und
    // --------------------------------------------------------------
    //
    // register  | index |  State
    // ----------+-------+--------
    //    R0     |   0   |
    //    R1     |   1   |
    //    R2     |   2   |
    //    R3     |   3   |
    //    R4     |   4   |
    //    R5     |   5   |
    //    R6     |   6   |
    //    R7     |   7   |  USER/SYSTEM
    //    R8     |   8   |
    //    R9     |   9   |
    //   R10     |   10  |
    //   R11     |   11  |
    //   R12     |   12  |
    //  R13 (SP) |   13  |
    //  R14 (LR) |   14  |
    //  R15 (PC) |   15  |
    // ----------+-------+-------
    //  R8_fiq   |   16  |
    //  R9_fiq   |   17  |
    //  R10_fiq  |   18  |
    //  R11_fiq  |   19  |    FIQ
    //  R12_fiq  |   20  |
    //  R13_fiq  |   21  |
    //  R14_fiq  |   22  |
    // ----------+-------+-------
    //  R13_svc  |   23  |  Supervisor
    //  R14_svc  |   24  |
    // ----------+-------+-------
    //  R13_abt  |   25  |    Abort
    //  R14_abt  |   26  |
    // ----------+-------+-------
    //  R13_irq  |   27  |  IRQ
    //  R14_irq  |   28  |
    // ----------+-------+-------
    //  R13_und  |   29  |  Undefined
    //  R14_und  |   30  |

    fn write_reg(&mut self, reg: u32, val: u32) {

        let reg = reg as usize;

        match &self.state {
            CpuState::User => self.gen_registers[reg] = val,
            CpuState::System => self.gen_registers[reg] = val,
            CpuState::FIQ => {
                if 7 < reg && reg < 15 {

                    self.gen_registers[reg + 8] = val;
                } else {

                    self.gen_registers[reg] = val;
                }
            }
            CpuState::Super => {
                if reg == 13 || reg == 14 {

                    self.gen_registers[reg + 10] = val;
                } else {

                    self.gen_registers[reg] = val;
                }
            }
            CpuState::Abort => {
                if reg == 13 || reg == 14 {

                    self.gen_registers[reg + 12] = val;
                } else {

                    self.gen_registers[reg] = val;
                }
            }
            CpuState::IRQ => {
                if reg == 13 || reg == 14 {

                    self.gen_registers[reg + 14] = val;
                } else {

                    self.gen_registers[reg] = val;
                }
            }
            CpuState::Undefined => {
                if reg == 13 || reg == 14 {

                    self.gen_registers[reg + 16] = val;
                } else {

                    self.gen_registers[reg] = val;
                }
            }
        }
    }

    fn read_reg(&self, reg: u32) -> u32 {

        let reg = reg as usize;

        match &self.state {
            CpuState::User => self.gen_registers[reg],
            CpuState::System => self.gen_registers[reg],
            CpuState::FIQ => {
                if 7 < reg && reg < 15 {

                    self.gen_registers[reg + 8]
                } else {

                    self.gen_registers[reg]
                }
            }
            CpuState::Super => {
                if reg == 13 || reg == 14 {

                    self.gen_registers[reg + 10]
                } else {

                    self.gen_registers[reg]
                }
            }
            CpuState::Abort => {
                if reg == 13 || reg == 14 {

                    self.gen_registers[reg + 12]
                } else {

                    self.gen_registers[reg]
                }
            }
            CpuState::IRQ => {
                if reg == 13 || reg == 14 {

                    self.gen_registers[reg + 14]
                } else {

                    self.gen_registers[reg]
                }
            }
            CpuState::Undefined => {
                if reg == 13 || reg == 14 {

                    self.gen_registers[reg + 16]
                } else {

                    self.gen_registers[reg]
                }
            }
        }
    }

    fn barrel_shifter(&self, val: u32, shift_op: u32) -> u32 {

        let shift_amount = if shift_op & 0x1 != 0 {

            // Shift by bottom byte of register
            self.read_reg(shift_op >> 4) & 0xff
        } else {

            // Shift by 5 bit unsigned integer
            shift_op >> 3
        };

        match (shift_op >> 1) & 0b11 {
            // Logical feft shift
            0b00 => val << shift_amount,

            // Logical right shift
            0b01 => val >> shift_amount,

            // Arithmetic right shift
            0b10 => ((val as i32) >> shift_amount) as u32,

            // Rotate right
            0b11 => val.rotate_right(shift_amount),
            _ => panic!("Literally how the fk did I get here??!?!"),
        }
    }

    //  ALL INSTRUCTIONS HAVE ALREADY BEEN CHECKED FOR CONDITIONS BEFORE CALLING
    //

    fn arm_data_proc(&mut self, opcode: u32) {

        let op1 = self.read_reg((opcode >> 15) & 0xf);

        // If bit 25 is set, operand 2 is an immediate value, else, it's a register
        let op2 = if opcode & 1 << 25 != 0 {

            // Immediate value with Right Rotate
            let imm = opcode & 0xff;

            let rot = (opcode >> 8) & 0x7;

            imm.rotate_right(rot)
        } else {

            // Shifted register
            let val = self.read_reg(opcode & 0xf);

            let shift_op = (opcode >> 4) & 0xff;

            self.barrel_shifter(val, shift_op)
        };

        let dest_reg = (opcode >> 12) & 0xf;

        let res = match (opcode >> 21) & 0xf {
            0b0000 => self.arm_AND(op1, op2),
            0b0001 => self.arm_EOR(op1, op2),
            0b0010 => self.arm_SUB(op1, op2),
            0b0011 => self.arm_RSB(op1, op2),
            0b0100 => self.arm_ADD(op1, op2),
            0b0101 => self.arm_ADC(op1, op2),
            0b0110 => self.arm_SBC(op1, op2),
            0b0111 => self.arm_RSC(op1, op2),
            0b1000 => return self.arm_TST(op1, op2),
            0b1001 => return self.arm_TEQ(op1, op2),
            0b1010 => return self.arm_CMP(op1, op2),
            0b1011 => return self.arm_CMN(op1, op2),
            0b1100 => self.arm_ORR(op1, op2),
            0b1101 => self.arm_MOV(op2),
            0b1110 => self.arm_BIC(op1, op2),
            0b1111 => self.arm_MVN(op2),
            _ => panic!("LITERALLY HOW TF DID I GET HERE!??!"),
        };

        self.write_reg(dest_reg, res);
        // Get Op1, Op2 and destination register
        // give Op1 and Op2 to operaiton, store result in destination register
    }

    fn arm_ADC(&mut self, op1: u32, op2: u32) -> u32 {

        op1 + op2 + self.get_flag(Flag::C)
    }

    fn arm_ADD(&mut self, op1: u32, op2: u32) -> u32 {

        op1 + op2
    }

    fn arm_AND(&mut self, op1: u32, op2: u32) -> u32 {

        op1 & op2
    }

    fn arm_B(&mut self, opcode: u32) {

        let offset = ((opcode & 0xffffff) << 2) as i32;

        self.add_pc(offset);
    }

    fn arm_BIC(&mut self, op1: u32, op2: u32) -> u32 {

        op1 & (!op2)
    }

    fn arm_BL(&mut self, opcode: u32) {

        self.write_reg(LINK_REGISTER_INDEX, self.read_pc());

        let offset = ((opcode & 0xffffff) << 2) as i32;

        self.add_pc(offset);
    }

    fn arm_BX(&mut self, opcode: u32) {

        self.write_pc(self.read_reg(opcode & 0xf));

        let new_mode = if opcode & 0x1 == 0 { CpuMode::ARM } else { CpuMode::THUMB };

        if new_mode != self.mode {

            self.mode = new_mode;

            todo!("Flush pipeline");
        }
    }

    fn arm_CDP(&mut self, opcode: u32) {}

    fn arm_CMN(&mut self, op1: u32, op2: u32) {}

    fn arm_CMP(&mut self, op1: u32, op2: u32) {}

    fn arm_EOR(&mut self, op1: u32, op2: u32) -> u32 {

        op1 ^ op2
    }

    fn arm_LDC(&mut self, opcode: u32) {}

    fn arm_LDM(&mut self, opcode: u32) {}

    fn arm_LDR(&mut self, opcode: u32) {}

    fn arm_MCR(&mut self, opcode: u32) {}

    fn arm_MLA(&mut self, opcode: u32) {}

    fn arm_MOV(&mut self, op2: u32) -> u32 {

        op2
    }

    fn arm_MRC(&mut self, opcode: u32) {}

    fn arm_MRS(&mut self, opcode: u32) {}

    fn arm_MSR(&mut self, opcode: u32) {}

    fn arm_PSR(&mut self, opcode: u32) {}

    fn arm_MUL(&mut self, opcode: u32) {}

    fn arm_MVN(&mut self, op2: u32) -> u32 {

        !op2
    }

    fn arm_ORR(&mut self, op1: u32, op2: u32) -> u32 {

        op1 | op2
    }

    fn arm_RSB(&mut self, op1: u32, op2: u32) -> u32 {

        op2 - op1
    }

    fn arm_RSC(&mut self, op1: u32, op2: u32) -> u32 {

        op2 - op1 + self.get_flag(Flag::C) - 1
    }

    fn arm_SBC(&mut self, op1: u32, op2: u32) -> u32 {

        op1 - op2 + self.get_flag(Flag::C) - 1
    }

    fn arm_STC(&mut self, opcode: u32) {}

    fn arm_STM(&mut self, opcode: u32) {}

    fn arm_STR(&mut self, opcode: u32) {}

    fn arm_SUB(&mut self, op1: u32, op2: u32) -> u32 {

        op1 - op2
    }

    fn arm_SWI(&mut self, opcode: u32) {}

    fn arm_SWP(&mut self, opcode: u32) {}

    fn arm_TEQ(&mut self, op1: u32, op2: u32) {}

    fn arm_TST(&mut self, op1: u32, op2: u32) {}
}
