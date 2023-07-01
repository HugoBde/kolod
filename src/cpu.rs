#![allow(non_snake_case)]
#![cfg_attr(debug_assertions, allow(dead_code))]
#![cfg_attr(debug_assertions, allow(unused_variables))]

use std::io::Write;
use std::rc::Rc;
use std::thread::sleep;
use std::{cell::RefCell, io};

use num_derive::FromPrimitive;
use num_traits::FromPrimitive;

use crate::memory::{self, Memory};

const STATE_REGISTERS_NUM: usize = 6;

const GEN_PURPOSE_REGISTERS_NUM: usize = 31;

const SAVED_PROGRAM_STATUS_REGISTERS_NUM: usize = 5;

const STACK_POINTER_REGISTER_INDEX: u32 = 13;

const LINK_REGISTER_INDEX: u32 = 14;

const PROGRAM_COUNTER_INDEX: u32 = 15;

#[derive(PartialEq, Debug)]

enum CpuState {
    ARM,
    THUMB,
}

#[derive(Debug)]

enum CpuMode {
    User = 0b10000,
    FIQ = 0b10001,
    IRQ = 0b10010,
    Super = 0b10011,
    Abort = 0b10111,
    Undefined = 0b11011,
    System = 0b11111,
}

#[derive(FromPrimitive)]

enum InstructionCondition {
    //              |          desc           |  flags         |
    //              +-------------------------+----------------+
    EQ = 0b0000, // | equal                   | Z              |
    NE = 0b0001, // | not equal               | ~Z             |
    CS = 0b0010, // | unsigned higher or same | C              |
    CC = 0b0011, // | unsigned lower          | ~C             |
    MI = 0b0100, // | negative                | N              |
    PL = 0b0101, // | positive or zero        | ~N             |
    VS = 0b0110, // | overflow                | V              |
    VC = 0b0111, // | no overflow             | ~V             |
    HI = 0b1000, // | unsigned higher         | C && ~Z        |
    LS = 0b1001, // | unsigned lower or same  | ~C || Z        |
    GE = 0b1010, // | greater or equal        | N == V         |
    LT = 0b1011, // | less than               | N != V         |
    GT = 0b1100, // | greater than            | ~Z && (N == V) |
    LE = 0b1101, // | less than or equal      | Z || (N != V)  |
    AL = 0b1110, // | always                  | ---            |
}

pub struct CPU {
    mode: CpuMode,
    state: CpuState,

    gen_registers: [u32; GEN_PURPOSE_REGISTERS_NUM],
    status_registers: [u32; STATE_REGISTERS_NUM],
    cpsr_register: u32,
    spsr_register: [u32; SAVED_PROGRAM_STATUS_REGISTERS_NUM],
    mem: Rc<RefCell<Memory>>,

    arm_lut: [(fn(&mut CPU, u32), &'static str); 4096],
    thumb_lut: [(fn(&mut CPU, u16), &'static str); 256],
}

enum Flag {
    N = 1 << 31,
    Z = 1 << 30,
    C = 1 << 29,
    V = 1 << 28,
    Q = 1 << 27,
}

#[allow(dead_code)]

impl std::fmt::Debug for CPU {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CPU")
            .field("mode", &self.mode)
            .field("state", &self.state)
            .field("gen_reg", &self.gen_registers)
            .field("status reg", &self.status_registers)
            .field("cpsr_reg", &self.cpsr_register)
            .field("spsr_reg", &self.spsr_register)
            .finish()
    }
}

impl CPU {
    pub fn new(mem: &Rc<RefCell<Memory>>) -> CPU {
        let mut cpu = CPU {
            mode: CpuMode::User,
            state: CpuState::ARM,
            gen_registers: [0; GEN_PURPOSE_REGISTERS_NUM],
            status_registers: [0; STATE_REGISTERS_NUM],
            cpsr_register: 0,
            spsr_register: [0; SAVED_PROGRAM_STATUS_REGISTERS_NUM],
            mem: mem.clone(),

            arm_lut: [(CPU::arm_undefined, "arm und"); 4096],
            thumb_lut: [(CPU::thumb_undefined, "thumb und"); 256],
        };

        // let mut i = 0;
        //
        //        while i < 256 {
        //
        //            cpu.thumb_lut[i] = if i & 0b11110000 == 0b11110000 {
        //
        //                (CPU::thumb_BL, "thumb bl")
        //            } else if i & 0b111110000 == 0b11100000 {
        //
        //                (CPU::thumb_B, "thumb b")
        //            } else if i & 0b11111111 == 0b11011111 {
        //
        //                (CPU::thumb_SWI, "thumb swi")
        //            } else if i & 0b11110000 == 0b11010000 {
        //
        //                (CPU::thumb_B_cond, "thumb b cond")
        //            } else if i & 0b11111000 == 0b11001000 {
        //
        //                CPU::thumb_LDMIA
        //            } else if i & 0b11111000 == 0b11000000 {
        //
        //                CPU::thumb_STMIA
        //            } else if i & 0b11111110 == 0b10111100 {
        //
        //                CPU::thumb_POP
        //            } else if i & 0b11111110 == 0b10110100 {
        //
        //                CPU::thumb_PUSH
        //            } else if i & 0b11111111 == 0b10110000 {
        //
        //                CPU::thumb_ADD_SP
        //            } else if i & 0b11110000 == 0b10100000 {
        //
        //                CPU::thumb_LOAD_ADDR
        //            } else if i & 0b11111000 == 0b10011000 {
        //
        //                CPU::thumb_LOAD_SP
        //            } else if i & 0b11111000 == 0b10010000 {
        //
        //                CPU::thumb_STORE_SP
        //            } else if i & 0b11111000 == 0b10001000 {
        //
        //                CPU::thumb_LDRH
        //            } else if i & 0b11111000 == 0b10000000 {
        //
        //                CPU::thumb_STRH
        //            } else if i & 0b11101000 == 0b01101000 {
        //
        //                CPU::thumb_LDR_imm
        //            } else {
        //
        //                CPU::thumb_undefined
        //            };
        //
        //            i += 1;
        //        }

        let mut i = 0;

        while i < 4096 {
            cpu.arm_lut[i] = if i & 0b111100000000 == 0b111100000000 {
                (CPU::arm_SWI, "arm swi")
            } else if i & 0b111100010001 == 0b111000010001 {
                (CPU::arm_MRC, "arm mrc")
            } else if i & 0b111100010001 == 0b111000000001 {
                (CPU::arm_MCR, "arm mcr")
            } else if i & 0b111100000001 == 0b111000000000 {
                (CPU::arm_CDP, "arm cdp")
            } else if i & 0b111000010000 == 0b110000010000 {
                (CPU::arm_LDC, "arm ldc")
            } else if i & 0b111000010000 == 0b110000000000 {
                (CPU::arm_STC, "arm stc")
            } else if i & 0b111100000000 == 0b101000000000 {
                (CPU::arm_BL, "arm bl")
            } else if i & 0b111100000000 == 0b100000000000 {
                (CPU::arm_B, "arm b")
            } else if i & 0b111000010000 == 0b100000010000 {
                (CPU::arm_LDM, "arm ldm")
            } else if i & 0b111000010000 == 0b100000000000 {
                (CPU::arm_STM, "arm stm")
            } else if i & 0b111000010000 == 0b011000010000 {
                (CPU::arm_undefined, "arm_und")
            } else if i & 0b110000010000 == 0b010000010000 {
                (CPU::arm_LDR, "arm ldr")
            } else if i & 0b110000010000 == 0b010000000000 {
                (CPU::arm_STR, "arm str")
            } else if i & 0b111000011001 == 0b000000011001 {
                (CPU::arm_LDRH, "arm ldrh")
            } else if i & 0b111000011001 == 0b000000001001 {
                (CPU::arm_STRH, "arm strh")
            } else if i & 0b111111111111 == 0b000100100001 {
                (CPU::arm_BX, "arm bx")
            } else if i & 0b111110111111 == 0b000100001001 {
                (CPU::arm_SWP, "arm swp")
            } else if i & 0b111110001111 == 0b000010001001 {
                (CPU::arm_MULL, "arm mull")
            } else if i & 0b111110001111 == 0b000000001001 {
                (CPU::arm_MUL, "arm mul")
            } else if i & 01100000000000 == 0b000000000000 {
                (CPU::arm_data_proc, "arm data proc")
            } else {
                (CPU::arm_undefined, "arm und")
            };

            i += 1;
        }

        cpu.write_pc(memory::GAME_PAK_OFFSET as u32);

        cpu
    }

    pub fn clock(&mut self) {
        sleep(std::time::Duration::from_millis(100));

        match self.state {
            CpuState::ARM => self.clock_arm(),
            CpuState::THUMB => self.clock_thumb(),
        }
    }

    pub fn clock_arm(&mut self) {
        // FETCH
        let opcode = self.fetch_arm_opcode();

        // DECODE
        let opcode_important_bits = CPU::arm_opcode_get_bits(opcode);

        let (instruction, instruction_name) = self.arm_lut[CPU::arm_opcode_get_bits(opcode)];

        println!("{}", instruction_name);
        io::stdout().flush().unwrap();

        if self.arm_check_cond(opcode) {
            // EXECUTE
            instruction(self, opcode);
        }

        self.add_pc(4);
    }

    pub fn clock_thumb(&mut self) {
        // FETCH
        let opcode = self.fetch_thumb_opcode();

        // DECODE
        let (instruction, instruction_name) = self.thumb_lut[CPU::thumb_opcode_get_bits(opcode)];

        println!("{}", instruction_name);

        // EXECUTE
        instruction(self, opcode);

        self.add_pc(2);
    }

    fn fetch_arm_opcode(&self) -> u32 {
        let addr = self.read_pc();

        self.mem.borrow().read_word(addr)
    }

    fn fetch_thumb_opcode(&self) -> u16 {
        let addr = self.read_pc();

        // Do the thing about the bit saying if you should read the upper two bytes or the lower two bytes
        self.mem.borrow().read_half(addr)
    }

    fn arm_opcode_get_bits(opcode: u32) -> usize {
        let opcode = opcode as usize;

        // Bits 27 to 20
        let upper_bits = (opcode >> 16) & 0xff0;

        // Bits 7 to 4
        let lower_bits = (opcode >> 4) & 0xf;

        upper_bits | lower_bits
    }

    fn thumb_opcode_get_bits(opcode: u16) -> usize {
        let opcode = opcode as usize;

        opcode
    }

    fn arm_check_cond(&self, opcode: u32) -> bool {
        let condition: InstructionCondition = InstructionCondition::from_u32(opcode >> 28).unwrap();

        match condition {
            InstructionCondition::EQ => self.get_flag(Flag::Z),
            InstructionCondition::NE => !self.get_flag(Flag::Z),
            InstructionCondition::CS => self.get_flag(Flag::C),
            InstructionCondition::CC => !self.get_flag(Flag::C),
            InstructionCondition::MI => self.get_flag(Flag::N),
            InstructionCondition::PL => !self.get_flag(Flag::N),
            InstructionCondition::VS => self.get_flag(Flag::V),
            InstructionCondition::VC => !self.get_flag(Flag::V),
            InstructionCondition::HI => self.get_flag(Flag::C) && !self.get_flag(Flag::Z),
            InstructionCondition::LS => !self.get_flag(Flag::C) || self.get_flag(Flag::Z),
            InstructionCondition::GE => self.get_flag(Flag::N) == self.get_flag(Flag::V),
            InstructionCondition::LT => self.get_flag(Flag::N) != self.get_flag(Flag::V),
            InstructionCondition::GT => {
                !self.get_flag(Flag::Z) && (self.get_flag(Flag::N) == self.get_flag(Flag::V))
            }
            InstructionCondition::LE => {
                self.get_flag(Flag::Z) || (self.get_flag(Flag::N) != self.get_flag(Flag::V))
            }
            InstructionCondition::AL => true,
        }
    }

    fn thumb_check_cond(&self, opcode: u16) -> bool {
        todo!()
    }

    fn get_flag(&self, flag: Flag) -> bool {
        self.cpsr_register & flag as u32 != 0
    }

    fn set_flag(&mut self, flag: Flag) {
        self.cpsr_register |= flag as u32;
    }

    fn unset_flag(&mut self, flag: Flag) {
        self.cpsr_register &= !(flag as u32);
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

        match &self.mode {
            CpuMode::User => self.gen_registers[reg] = val,
            CpuMode::System => self.gen_registers[reg] = val,
            CpuMode::FIQ => {
                if 7 < reg && reg < 15 {
                    self.gen_registers[reg + 8] = val;
                } else {
                    self.gen_registers[reg] = val;
                }
            }
            CpuMode::Super => {
                if reg == 13 || reg == 14 {
                    self.gen_registers[reg + 10] = val;
                } else {
                    self.gen_registers[reg] = val;
                }
            }
            CpuMode::Abort => {
                if reg == 13 || reg == 14 {
                    self.gen_registers[reg + 12] = val;
                } else {
                    self.gen_registers[reg] = val;
                }
            }
            CpuMode::IRQ => {
                if reg == 13 || reg == 14 {
                    self.gen_registers[reg + 14] = val;
                } else {
                    self.gen_registers[reg] = val;
                }
            }
            CpuMode::Undefined => {
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

        match &self.mode {
            CpuMode::User => self.gen_registers[reg],
            CpuMode::System => self.gen_registers[reg],
            CpuMode::FIQ => {
                if 7 < reg && reg < 15 {
                    self.gen_registers[reg + 8]
                } else {
                    self.gen_registers[reg]
                }
            }
            CpuMode::Super => {
                if reg == 13 || reg == 14 {
                    self.gen_registers[reg + 10]
                } else {
                    self.gen_registers[reg]
                }
            }
            CpuMode::Abort => {
                if reg == 13 || reg == 14 {
                    self.gen_registers[reg + 12]
                } else {
                    self.gen_registers[reg]
                }
            }
            CpuMode::IRQ => {
                if reg == 13 || reg == 14 {
                    self.gen_registers[reg + 14]
                } else {
                    self.gen_registers[reg]
                }
            }
            CpuMode::Undefined => {
                if reg == 13 || reg == 14 {
                    self.gen_registers[reg + 16]
                } else {
                    self.gen_registers[reg]
                }
            }
        }
    }

    // There are many errors in the barrel shifter logic but i can't be fucked sorting that out now
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
        op1 + op2 + self.get_flag(Flag::C) as u32
    }

    fn arm_ADD(&mut self, op1: u32, op2: u32) -> u32 {
        op1 + op2
    }

    fn arm_AND(&mut self, op1: u32, op2: u32) -> u32 {
        op1 & op2
    }

    // note: could probably merge arm_B and arm_BL and
    // save the PC if the link bit is set
    // note: the PC value saved should be one after the current
    // instruction due to some instruction prefetching
    // stuff idk this is confusing
    fn arm_B(&mut self, opcode: u32) {
        let offset = ((opcode & 0xffffff) << 2) as i32;

        self.add_pc(offset + 8);

        println!("ARM_B jumping to {:#x}", self.read_pc());
    }

    fn arm_BIC(&mut self, op1: u32, op2: u32) -> u32 {
        op1 & (!op2)
    }

    fn arm_BL(&mut self, opcode: u32) {
        self.write_reg(LINK_REGISTER_INDEX, self.read_pc() + 4);

        let offset = ((opcode & 0xffffff) << 2) as i32;

        self.add_pc(offset + 8);

        println!("ARM_BL jumping to {:#x}", self.read_pc());
    }

    fn arm_BX(&mut self, opcode: u32) {
        self.write_pc(self.read_reg(opcode & 0xf));

        let new_state = if opcode & 0x1 == 0 {
            CpuState::ARM
        } else {
            CpuState::THUMB
        };

        if new_state != self.state {
            self.state = new_state;

            // todo!("Flush pipeline");
        }
    }

    fn arm_CDP(&mut self, opcode: u32) {}

    fn arm_CMN(&mut self, op1: u32, op2: u32) {
        let res = op1 + op2;
        let res = op1.overflowing_add(op2);
        if res == 0 {
            self.set_flag(Flag::Z);
        } else {
            self.unset_flag(Flag::Z);
        }

        if res 
    }

    fn arm_CMP(&mut self, op1: u32, op2: u32) {}

    fn arm_EOR(&mut self, op1: u32, op2: u32) -> u32 {
        op1 ^ op2
    }

    fn arm_LDC(&mut self, opcode: u32) {}

    fn arm_LDM(&mut self, opcode: u32) {}

    fn arm_LDR(&mut self, opcode: u32) {}

    fn arm_LDRH(&mut self, opcode: u32) {}

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

    fn arm_MULL(&mut self, opcode: u32) {}

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
        op2 - op1 + self.get_flag(Flag::C) as u32 - 1
    }

    fn arm_SBC(&mut self, op1: u32, op2: u32) -> u32 {
        op1 - op2 + self.get_flag(Flag::C) as u32 - 1
    }

    fn arm_STC(&mut self, opcode: u32) {}

    fn arm_STM(&mut self, opcode: u32) {}

    fn arm_STR(&mut self, opcode: u32) {}

    fn arm_STRH(&mut self, opcode: u32) {}

    fn arm_SUB(&mut self, op1: u32, op2: u32) -> u32 {
        op1 - op2
    }

    fn arm_SWI(&mut self, opcode: u32) {}

    fn arm_SWP(&mut self, opcode: u32) {}

    fn arm_TEQ(&mut self, op1: u32, op2: u32) {}

    fn arm_TST(&mut self, op1: u32, op2: u32) {}

    fn thumb_ADC(&mut self, opcode: u16) {}

    fn thumb_ADD(&mut self, opcode: u16) {}

    fn thumb_ADD_SP(&mut self, opcode: u16) {}

    fn thumb_AND(&mut self, opcode: u16) {}

    fn thumb_ASR(&mut self, opcode: u16) {}

    fn thumb_B(&mut self, opcode: u16) {}

    fn thumb_B_cond(&mut self, opcode: u16) {}

    fn thumb_BIC(&mut self, opcode: u16) {}

    fn thumb_BL(&mut self, opcode: u16) {}

    fn thumb_BX(&mut self, opcode: u16) {}

    fn thumb_CMN(&mut self, opcode: u16) {}

    fn thumb_CMP(&mut self, opcode: u16) {}

    fn thumb_EOR(&mut self, opcode: u16) {}

    fn thumb_LDMIA(&mut self, opcode: u16) {}

    fn thumb_LDR_reg_off(&mut self, opcode: u16) {}

    fn thumb_LDR_imm(&mut self, opcode: u16) {}

    fn thumb_LDRB(&mut self, opcode: u16) {}

    fn thumb_LDRH(&mut self, opcode: u16) {}

    fn thumb_LOAD_ADDR(&mut self, opcode: u16) {}

    fn thumb_LOAD_SP(&mut self, opcode: u16) {}

    fn thumb_LSL(&mut self, opcode: u16) {}

    fn thumb_LDSB(&mut self, opcode: u16) {}

    fn thumb_LDSH(&mut self, opcode: u16) {}

    fn thumb_LSR(&mut self, opcode: u16) {}

    fn thumb_MOV(&mut self, opcode: u16) {}

    fn thumb_MUL(&mut self, opcode: u16) {}

    fn thumb_MVN(&mut self, opcode: u16) {}

    fn thumb_NEG(&mut self, opcode: u16) {}

    fn thumb_ORR(&mut self, opcode: u16) {}

    fn thumb_POP(&mut self, opcode: u16) {}

    fn thumb_PUSH(&mut self, opcode: u16) {}

    fn thumb_ROR(&mut self, opcode: u16) {}

    fn thumb_SBC(&mut self, opcode: u16) {}

    fn thumb_STMIA(&mut self, opcode: u16) {}

    fn thumb_STORE_SP(&mut self, opcode: u16) {}

    fn thumb_STR(&mut self, opcode: u16) {}

    fn thumb_STRB(&mut self, opcode: u16) {}

    fn thumb_STRH(&mut self, opcode: u16) {}

    fn thumb_SWI(&mut self, opcode: u16) {}

    fn thumb_SUB(&mut self, opcode: u16) {}

    fn thumb_TST(&mut self, opcode: u16) {}

    fn arm_undefined(&mut self, opcode: u32) {
        panic!("ARM UNDEFINED {:#x}", opcode);
    }

    fn thumb_undefined(&mut self, opcode: u16) {
        panic!("THUMB UNDEFINED {:#x}", opcode);
    }
}
