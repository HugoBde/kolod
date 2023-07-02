#![allow(non_snake_case)]
#![cfg_attr(debug_assertions, allow(dead_code))]
#![cfg_attr(debug_assertions, allow(unused_variables))]

use std::cell::RefCell;
use std::fmt::Display;
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

#[derive(Clone, Copy, PartialEq)]

enum Opcode {
    ARM(Option<ARMInstruction>, Option<u32>),
    THUMB(Option<THUMBInstruction>, Option<u16>),
}

#[derive(Clone, Copy, PartialEq)]

pub struct ARMInstruction {
    op: fn(&mut CPU, u32),
    desc: &'static str,
}

impl Display for ARMInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.desc)
    }
}

#[derive(Clone, Copy, PartialEq)]

pub struct THUMBInstruction {
    op: fn(&mut CPU, u16),
    desc: &'static str,
}

impl Display for THUMBInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.desc)
    }
}

pub struct CPU {
    mode: CpuMode,
    state: CpuState,

    gen_registers: [u32; GEN_PURPOSE_REGISTERS_NUM],
    status_registers: [u32; STATE_REGISTERS_NUM],
    cpsr_register: u32,
    spsr_register: [u32; SAVED_PROGRAM_STATUS_REGISTERS_NUM],
    mem: Rc<RefCell<Memory>>,

    arm_lut: [ARMInstruction; 4096],
    thumb_lut: [THUMBInstruction; 256],

    fetch_opcode: Opcode,
    decode_opcode: Opcode,
    exec_opcode: Opcode,
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

            arm_lut: CPU::build_arm_lut(),
            thumb_lut: CPU::build_thumb_lut(),

            fetch_opcode: Opcode::ARM(None, None),
            decode_opcode: Opcode::ARM(None, None),
            exec_opcode: Opcode::ARM(None, None),
        };

        cpu.write_pc(memory::GAME_PAK_OFFSET as u32);

        cpu
    }

    fn arm_pipeline_clear(&mut self) {
        self.exec_opcode = Opcode::ARM(None, None);

        self.decode_opcode = Opcode::ARM(None, None);

        self.fetch_opcode = Opcode::ARM(None, None);
    }

    fn thumb_pipeline_clear(&mut self) {
        self.exec_opcode = Opcode::THUMB(None, None);

        self.decode_opcode = Opcode::THUMB(None, None);

        self.fetch_opcode = Opcode::THUMB(None, None);
    }

    pub fn clock(&mut self) {
        sleep(std::time::Duration::from_millis(100));

        print!("tick ");

        match self.state {
            CpuState::ARM => self.clock_arm(),
            CpuState::THUMB => self.clock_thumb(),
        }
    }

    pub fn clock_arm(&mut self) {
        print!("ARM ");

        // Execute the opcode we last decoded
        self.exec_opcode = self.decode_opcode;

        if let Opcode::ARM(Some(instruction), Some(opcode)) = self.exec_opcode {
            if self.arm_cond_check(opcode) {
                print!("exec: {} | ", instruction.desc);

                (instruction.op)(self, opcode);
            } else {
                print!("no exec: {} | ", instruction);
            }
        }

        // Decode the opcode we last fetched
        self.decode_opcode = self.fetch_opcode;

        if let Opcode::ARM(_, Some(opcode)) = self.decode_opcode {
            self.decode_opcode = self.arm_opcode_decode(opcode);

            if let Opcode::ARM(Some(instruction), _) = self.decode_opcode {
                print!("decode: {:#x} -> {} | ", opcode, instruction);
            }
        }

        // Fetch a new opcode
        self.fetch_opcode = self.arm_opcode_fetch();

        println!("fetch: {:#x}", self.read_pc());

        // Move PC forward
        self.add_pc(4);
    }

    fn arm_opcode_decode(&self, opcode: u32) -> Opcode {
        let important_bits = CPU::arm_opcode_important_bits_get(opcode);

        let instruction = self.arm_lut[important_bits];

        Opcode::ARM(Some(instruction), Some(opcode))
    }

    fn arm_opcode_fetch(&self) -> Opcode {
        let addr = self.read_pc();

        Opcode::ARM(None, Some(self.mem.borrow().read_word(addr)))
    }

    fn arm_opcode_important_bits_get(opcode: u32) -> usize {
        let opcode = opcode as usize;

        // Bits 27 to 20
        let upper_bits = (opcode >> 16) & 0xff0;

        // Bits 7 to 4
        let lower_bits = (opcode >> 4) & 0xf;

        upper_bits | lower_bits
    }

    pub fn clock_thumb(&mut self) {
        print!("THUMB ");

        // Execute the opcode we last decoded
        self.exec_opcode = self.decode_opcode;

        if let Opcode::THUMB(Some(instruction), Some(opcode)) = self.exec_opcode {
            if self.thumb_cond_check(opcode) {
                print!("exec: {}", instruction);

                (instruction.op)(self, opcode);
            } else {
                print!("no exec: {}", instruction);
            }
        }

        // Decode the opcode we last fetched
        self.decode_opcode = self.fetch_opcode;

        if let Opcode::THUMB(_, Some(opcode)) = self.decode_opcode {
            self.decode_opcode = self.thumb_opcode_decode(opcode);

            if let Opcode::THUMB(Some(instruction), _) = self.decode_opcode {
                print!("decode: {:#x} -> {}", opcode, instruction);
            }
        }

        // Fetch a new opcode
        self.fetch_opcode = self.arm_opcode_fetch();

        println!("fetch: {:#x}", self.read_pc());

        // Move PC forward
        self.add_pc(2);
    }

    fn thumb_opcode_decode(&self, opcode: u16) -> Opcode {
        let important_bits = CPU::thumb_opcode_important_bits_get(opcode);

        let instruction = self.thumb_lut[important_bits];

        Opcode::THUMB(Some(instruction), Some(opcode))
    }

    fn thumb_opcode_fetch(&self) -> Opcode {
        let addr = self.read_pc();

        // Do the thing about the bit saying if you should read the upper two bytes or the lower two bytes
        Opcode::THUMB(None, Some(self.mem.borrow().read_half(addr)))
    }

    fn thumb_opcode_important_bits_get(opcode: u16) -> usize {
        let opcode = (opcode >> 8) as usize;

        opcode
    }

    fn arm_cond_check(&self, opcode: u32) -> bool {
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

    fn thumb_cond_check(&self, opcode: u16) -> bool {
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
    fn barrel_shifter(val: u32, shift_op: u32) -> u32 {
        0
    }

    //  ALL INSTRUCTIONS HAVE ALREADY BEEN CHECKED FOR CONDITIONS BEFORE CALLING
    //

    fn arm_data_proc(&mut self, opcode: u32) {
        let operand_1 = {
            let reg_n = (opcode >> 16) & 0xf;
            self.read_reg(reg_n);
        };

        let operand_2 = {
            let operand_2 = opcode & 0xfff;
            // Bit 25 specifies whether operand 2 is an immediate value or not
            if opcode & (1 << 25) != 0 {
                let imm = opcode & 0xff;
                let shift = (opcode >> 8) & 0xf;
                // apply shift to imm
                0
            } else {
                let reg_m = opcode & 0xf;
                let shift = (opcode >> 4) & 0xff;
                // apply shift to value stored in reg_m
                1
            }
        };
        let inner_opcode = (opcode >> 21) & 0xf;

        match inner_opcode {
            0b0000 => todo!(),
            0b0001 => todo!(),
            0b0010 => todo!(),
            0b0011 => todo!(),
            0b0100 => todo!(),
            0b0101 => todo!(),
            0b0110 => todo!(),
            0b0111 => todo!(),
            0b1000 => todo!(),
            0b1001 => todo!(),
            0b1010 => todo!(),
            0b1011 => todo!(),
            0b1100 => todo!(),
            0b1101 => todo!(),
            0b1110 => todo!(),
            0b1111 => todo!(),

            _ => panic!("how did we get here???"),
        }

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
        // If link bit is set
        if opcode & (1 << 24) != 0 {
            // do the link stuff
            todo!();
        }

        // Offset is stored in bottom 24 bits as signed 2's complement
        let offset = ((opcode as i32) & 0xffffff) << 2;

        // Sign extend from 24 + 2 = 26 bits to 32 bits
        let offset = offset.wrapping_shl(6).wrapping_shr(6);

        self.add_pc(offset);

        self.arm_pipeline_clear();
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

    fn build_arm_lut() -> [ARMInstruction; 4096] {
        let mut lut: [ARMInstruction; 4096] = [ARMInstruction {
            op: CPU::arm_undefined,
            desc: "ARM - undefined",
        }; 4096];

        for i in 0..4096 {
            lut[i] = if i & 0b111100000000 == 0b111100000000 {
                ARMInstruction {
                    op: CPU::arm_software_interrupt,
                    desc: "ARM - software interrupt",
                }
            } else if i & 0b111100000001 == 0b111000000001 {
                ARMInstruction {
                    op: CPU::arm_coproc_register_transfer,
                    desc: "ARM - coproc reg transfer",
                }
            } else if i & 0b111100000001 == 0b111000000000 {
                ARMInstruction {
                    op: CPU::arm_coproc_data_operation,
                    desc: "ARM - coproc data op",
                }
            } else if i & 0b111000000000 == 0b110000000000 {
                ARMInstruction {
                    op: CPU::arm_coproc_data_transfer,
                    desc: "ARM - coproc data transfer",
                }
            } else if i & 0b111000000000 == 0b101000000000 {
                ARMInstruction {
                    op: CPU::arm_branch,
                    desc: "ARM - branch",
                }
            } else if i & 0b111000000000 == 0b100000000000 {
                ARMInstruction {
                    op: CPU::arm_block_data_transfer,
                    desc: "ARM - block data transfer",
                }
            } else if i & 0b111000000001 == 0b011000000001 {
                ARMInstruction {
                    op: CPU::arm_undefined,
                    desc: "ARM - undefined",
                }
            } else if i & 0b110000000000 == 0b010000000000 {
                ARMInstruction {
                    op: CPU::arm_single_data_transfer,
                    desc: "ARM - single data transfer",
                }
            } else if i & 0b111000001001 == 0b000000001001 {
                ARMInstruction {
                    op: CPU::arm_halfword_data_transfer,
                    desc: "ARM - halfword data transfer",
                }
            } else if i & 0b111111111111 == 0b000100100001 {
                ARMInstruction {
                    op: CPU::arm_branch_exchange,
                    desc: "ARM - branch exchange",
                }
            } else if i & 0b111110111111 == 0b000100001001 {
                ARMInstruction {
                    op: CPU::arm_single_data_swap,
                    desc: "ARM - single data swap",
                }
            } else if i & 0b111110001111 == 0b000010001001 {
                ARMInstruction {
                    op: CPU::arm_multiply_long,
                    desc: "ARM - multiply long",
                }
            } else if i & 0b111111001111 == 0b000000001001 {
                ARMInstruction {
                    op: CPU::arm_multiply,
                    desc: "ARM - multiply",
                }
            } else if i & 0b110000000000 == 0b000000000000 {
                ARMInstruction {
                    op: CPU::arm_data_proc,
                    desc: "ARM - data proc",
                }
            } else {
                ARMInstruction {
                    op: CPU::arm_undefined,
                    desc: "ARM - HAAAAAAAA",
                }
            }
        }

        lut
    }

    fn build_thumb_lut() -> [THUMBInstruction; 256] {
        let mut lut: [THUMBInstruction; 256] = [THUMBInstruction {
            op: CPU::thumb_undefined,
            desc: "THUMB - undefined",
        }; 256];

        for i in 0..256 {
            lut[i] = if i & 0b11110000 == 0b11110000 {
                THUMBInstruction {
                    op: CPU::thumb_long_branch_link,
                    desc: "THUMB - long branch link",
                }
            } else if i & 0b11111000 == 0b11100000 {
                THUMBInstruction {
                    op: CPU::thumb_uncond_branch,
                    desc: "THUMB - uncond branch",
                }
            } else if i & 0b11111111 == 0b11011111 {
                THUMBInstruction {
                    op: CPU::thumb_software_interrupt,
                    desc: "THUMB - software interrupt",
                }
            } else if i & 0b11110000 == 0b11010000 {
                THUMBInstruction {
                    op: CPU::thumb_cond_branch,
                    desc: "THUMB - cond branch",
                }
            } else if i & 0b11110000 == 0b11000000 {
                THUMBInstruction {
                    op: CPU::thumb_multiple_load_store,
                    desc: "THUMB - multiple load store",
                }
            } else if i & 0b11110110 == 0b10110100 {
                THUMBInstruction {
                    op: CPU::thumb_push_pop_reg,
                    desc: "THUMB - push pop reg",
                }
            } else if i & 0b11111111 == 0b10110000 {
                THUMBInstruction {
                    op: CPU::thumb_add_offset_to_sp,
                    desc: "THUMB - add off to sp",
                }
            } else if i & 0b11110000 == 0b10100000 {
                THUMBInstruction {
                    op: CPU::thumb_load_addr,
                    desc: "THUMB - load addr",
                }
            } else if i & 0b11110000 == 0b10010000 {
                THUMBInstruction {
                    op: CPU::thumb_sp_relative_load_store,
                    desc: "THUMB - sp relative load store",
                }
            } else if i & 0b11110000 == 0b10000000 {
                THUMBInstruction {
                    op: CPU::thumb_load_store_halfword,
                    desc: "THUMB - load store halfword",
                }
            } else if i & 0b11100000 == 0b01100000 {
                THUMBInstruction {
                    op: CPU::thumb_load_store_imm_off,
                    desc: "THUMB - load store imm off",
                }
            } else if i & 0b11110010 == 0b01010010 {
                THUMBInstruction {
                    op: CPU::thumb_load_store_sign_ext_byte_halfword,
                    desc: "THUMB - load store sign ext byte halfword",
                }
            } else if i & 0b11110010 == 0b01010000 {
                THUMBInstruction {
                    op: CPU::thumb_load_store_reg_off,
                    desc: "THUMB - load store reg off",
                }
            } else if i & 0b11111000 == 0b10010000 {
                THUMBInstruction {
                    op: CPU::thumb_pc_relative_load,
                    desc: "THUMB - pc relative load",
                }
            } else if i & 0b11111100 == 0b01000100 {
                THUMBInstruction {
                    op: CPU::thumb_hi_reg_op_branch_exchange,
                    desc: "THUMB - hi reg op branch exchange",
                }
            } else if i & 0b11111100 == 0b01000000 {
                THUMBInstruction {
                    op: CPU::thumb_alu_op,
                    desc: "THUMB - alu op",
                }
            } else if i & 0b11100000 == 0b00100000 {
                THUMBInstruction {
                    op: CPU::thumb_move_compare_add_sub_imm,
                    desc: "THUMB - move compare add sub imm",
                }
            } else if i & 0b11111000 == 0b00011000 {
                THUMBInstruction {
                    op: CPU::thumb_add_sub,
                    desc: "THUMB - add sub",
                }
            } else if i & 0111000000 == 0b00000000 {
                THUMBInstruction {
                    op: CPU::thumb_move_shifted_register,
                    desc: "THUMB - move shifted reg",
                }
            } else {
                THUMBInstruction {
                    op: CPU::thumb_undefined,
                    desc: "THUMB - undefined (it's bad)",
                }
            }
        }

        lut
    }
}
