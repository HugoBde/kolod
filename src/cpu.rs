#![allow(non_snake_case)]
#![cfg_attr(debug_assertions, allow(dead_code))]
#![cfg_attr(debug_assertions, allow(unused_variables))]

use std::cell::RefCell;
use std::io;
use std::io::Write;
use std::rc::Rc;
use std::thread::sleep;

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
    User      = 0b10000,
    FIQ       = 0b10001,
    IRQ       = 0b10010,
    Super     = 0b10011,
    Abort     = 0b10111,
    Undefined = 0b11011,
    System    = 0b11111,
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

type ARMInstruction = fn(&mut CPU, u32);

type THUMBInstruction = fn(&mut CPU, u16);

pub struct CPU {
    mode:  CpuMode,
    state: CpuState,

    gen_registers:    [u32; GEN_PURPOSE_REGISTERS_NUM],
    status_registers: [u32; STATE_REGISTERS_NUM],
    cpsr_register:    u32,
    spsr_register:    [u32; SAVED_PROGRAM_STATUS_REGISTERS_NUM],
    mem:              Rc<RefCell<Memory>>,

    arm_lut:   [(ARMInstruction, &'static str); 4096],
    thumb_lut: [(THUMBInstruction, &'static str); 256],
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
            mode:             CpuMode::User,
            state:            CpuState::ARM,
            gen_registers:    [0; GEN_PURPOSE_REGISTERS_NUM],
            status_registers: [0; STATE_REGISTERS_NUM],
            cpsr_register:    0,
            spsr_register:    [0; SAVED_PROGRAM_STATUS_REGISTERS_NUM],
            mem:              mem.clone(),

            arm_lut:   CPU::build_arm_lut(),
            thumb_lut: [(CPU::thumb_undefined, "thumb und"); 256],
        };

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
            InstructionCondition::GT => !self.get_flag(Flag::Z) && (self.get_flag(Flag::N) == self.get_flag(Flag::V)),
            InstructionCondition::LE => self.get_flag(Flag::Z) || (self.get_flag(Flag::N) != self.get_flag(Flag::V)),
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

        0
    }

    //  ALL INSTRUCTIONS HAVE ALREADY BEEN CHECKED FOR CONDITIONS BEFORE CALLING
    //

    fn arm_data_proc(&mut self, opcode: u32) {

        todo!()
    }

    fn arm_multiply(&mut self, opcode: u32) {

        todo!()
    }

    fn arm_multiply_long(&mut self, opcode: u32) {

        todo!()
    }

    fn arm_single_data_swap(&mut self, opcode: u32) {

        todo!()
    }

    fn arm_branch_exchange(&mut self, opcode: u32) {

        todo!()
    }

    fn arm_halfword_data_transfer(&mut self, opcode: u32) {

        todo!()
    }

    fn arm_single_data_transfer(&mut self, opcopde: u32) {

        todo!()
    }

    fn arm_block_data_transfer(&mut self, opcode: u32) {

        todo!()
    }

    fn arm_branch(&mut self, opcode: u32) {

        todo!()
    }

    fn arm_coproc_data_transfer(&mut self, opcode: u32) {

        todo!()
    }

    fn arm_coproc_data_operation(&mut self, opcode: u32) {

        todo!()
    }

    fn arm_coproc_register_transfer(&mut self, opcopde: u32) {

        todo!()
    }

    fn arm_software_interrupt(&mut self, opcopde: u32) {

        todo!()
    }

    fn arm_undefined(&mut self, opcode: u32) {

        panic!("ARM UNDEFINED {:#x}", opcode);
    }

    fn thumb_move_shifted_register(&mut self, opcode: u16) {

        todo!()
    }

    fn thumb_add_sub(&mut self, opcode: u16) {

        todo!()
    }

    fn thumb_move_compare_add_sub_imm(&mut self, opcode: u16) {

        todo!()
    }

    fn thumb_alu_op(&mut self, opcode: u16) {

        todo!()
    }

    fn thumb_hi_reg_op_branch_exchange(&mut self, opcode: u16) {

        todo!()
    }

    fn thumb_pc_relative_load(&mut self, opcode: u16) {

        todo!()
    }

    fn thumb_load_store_reg_off(&mut self, opcode: u16) {

        todo!()
    }

    fn thumb_load_store_sign_ext_byte_halfword(&mut self, opcode: u16) {

        todo!()
    }

    fn thumb_load_store_imm_off(&mut self, opcode: u16) {

        todo!()
    }

    fn thumb_load_store_halfword(&mut self, opcode: u16) {

        todo!()
    }

    fn thumb_sp_relative_load_store(&mut self, opcode: u16) {

        todo!()
    }

    fn thumb_load_addr(&mut self, opcode: u16) {

        todo!()
    }

    fn thumb_add_offset_to_sp(&mut self, opcode: u16) {

        todo!()
    }

    fn thumb_push_pop_reg(&mut self, opcode: u16) {

        todo!()
    }

    fn thumb_multiple_load_store(&mut self, opcode: u16) {

        todo!()
    }

    fn thumb_cond_branch(&mut self, opcode: u16) {

        todo!()
    }

    fn thumb_software_interrupt(&mut self, opcode: u16) {

        todo!()
    }

    fn thumb_uncond_branch(&mut self, opcode: u16) {

        todo!()
    }

    fn thumb_long_branch_link(&mut self, opcode: u16) {

        todo!()
    }

    fn thumb_undefined(&mut self, opcode: u16) {

        panic!("THUMB UNDEFINED {:#x}", opcode);
    }

    fn build_arm_lut() -> [(fn(&mut CPU, u32), &'static str); 4096] {

        let mut lut: [(ARMInstruction, &'static str); 4096] = [(CPU::arm_undefined, "ARM - undefined"); 4096];

        for i in 0..4096 {

            lut[i] = if i & 0b111100000000 == 0b111100000000 {

                (CPU::arm_software_interrupt, "ARM - software interrupt")
            } else if i & 0b111100000001 == 0b111000000001 {

                (CPU::arm_coproc_register_transfer, "ARM - coproc reg transfer")
            } else if i & 0b111100000001 == 0b111000000000 {

                (CPU::arm_coproc_data_operation, "ARM - coproc data op")
            } else if i & 0b111000000000 == 0b110000000000 {

                (CPU::arm_coproc_data_transfer, "ARM - coproc data transfer")
            } else if i & 0b111000000000 == 0b101000000000 {

                (CPU::arm_branch, "ARM - branch")
            } else if i & 0b111000000000 == 0b100000000000 {

                (CPU::arm_block_data_transfer, "ARM - block data transfer")
            } else if i & 0b111000000001 == 0b011000000001 {

                (CPU::arm_undefined, "ARM - undefined")
            } else if i & 0b110000000000 == 0b010000000000 {

                (CPU::arm_single_data_transfer, "ARM - single data transfer")
            } else if i & 0b111000001001 == 0b000000001001 {

                (CPU::arm_halfword_data_transfer, "ARM - halfword data transfer")
            } else if i & 0b111111111111 == 0b000100100001 {

                (CPU::arm_branch_exchange, "ARM - branch exchange")
            } else if i & 0b111110111111 == 0b000100001001 {

                (CPU::arm_single_data_swap, "ARM - single data swap")
            } else if i & 0b111110001111 == 0b000010001001 {

                (CPU::arm_multiply_long, "ARM - multiply long")
            } else if i & 0b111111001111 == 0b000000001001 {

                (CPU::arm_multiply, "ARM - multiply")
            } else if i & 0b110000000000 == 0b000000000000 {

                (CPU::arm_data_proc, "ARM - data proc")
            } else {

                (CPU::arm_undefined, "ARM - HAAAAAAAA")
            }
        }

        lut
    }

    fn build_thumb_lut() -> [(fn(&mut CPU, u16), &'static str); 256] {

        let mut lut: [(fn(&mut CPU, u16), &'static str); 256] = [(CPU::thumb_undefined, "THUMB - undefined"); 256];

        for i in 0..256 {

            lut[i] = if i & 0b11110000 == 0b11110000 {

                (CPU::thumb_long_branch_link, "THUMB - long branch link")
            } else if i & 0b11111000 == 0b11100000 {

                (CPU::thumb_uncond_branch, "THUMB - uncond branch")
            } else if i & 0b11111111 == 0b11011111 {

                (CPU::thumb_software_interrupt, "THUMB - software interrupt")
            } else if i & 0b11110000 == 0b11010000 {

                (CPU::thumb_cond_branch, "THUMB - cond branch")
            } else if i & 0b11110000 == 0b11000000 {

                (CPU::thumb_multiple_load_store, "THUMB - multiple load store")
            } else if i & 0b11110110 == 0b10110100 {

                (CPU::thumb_push_pop_reg, "THUMB - push pop reg")
            } else if i & 0b11111111 == 0b10110000 {

                (CPU::thumb_add_offset_to_sp, "THUMB - add off to sp")
            } else if i & 0b11110000 == 0b10100000 {

                (CPU::thumb_load_addr, "THUMB - load addr")
            } else if i & 0b11110000 == 0b10010000 {

                (CPU::thumb_sp_relative_load_store, "THUMB - sp relative load store")
            } else if i & 0b11110000 == 0b10000000 {

                (CPU::thumb_load_store_halfword, "THUMB - load store halfword")
            } else if i & 0b11100000 == 0b01100000 {

                (CPU::thumb_load_store_imm_off, "THUMB - load store imm off")
            } else if i & 0b11110010 == 0b01010010 {

                (CPU::thumb_load_store_sign_ext_byte_halfword, "THUMB - load store sign ext byte halfword")
            } else if i & 0b11110010 == 0b01010000 {

                (CPU::thumb_load_store_reg_off, "THUMB - load store reg off")
            } else if i & 0b11111000 == 0b10010000 {

                (CPU::thumb_pc_relative_load, "THUMB - pc relative load")
            } else if i & 0b11111100 == 0b01000100 {

                (CPU::thumb_hi_reg_op_branch_exchange, "THUMB - hi reg op branch exchange")
            } else if i & 0b11111100 == 0b01000000 {

                (CPU::thumb_alu_op, "THUMB - alu op")
            } else if i & 0b11100000 == 0b00100000 {

                (CPU::thumb_move_compare_add_sub_imm, "THUMB - move compare add sub imm")
            } else if i & 0b11111000 == 0b00011000 {

                (CPU::thumb_add_sub, "THUMB - add sub")
            } else if i & 0111000000 == 0b00000000 {

                (CPU::thumb_move_shifted_register, "THUMB - move shifted reg")
            } else {

                (CPU::thumb_undefined, "THUMB - undefined (it's bad)")
            }
        }

        lut
    }
}
