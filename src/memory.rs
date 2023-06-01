use std::fs::File;
use std::io::{self, BufReader, Read};

pub const BIOS_SIZE: usize = 0x4000; // 16 kibibytes
pub const BIOS_OFFSET: usize = 0x0;

pub const BOARD_WRAM_SIZE: usize = 0x40000; // 256 kibibytes
pub const BOARD_WRAM_OFFSET: usize = 0x2000000;

pub const CHIP_WRAM_SIZE: usize = 0x8000; // 32 kibibytes
pub const CHIP_WRAM_OFFSET: usize = 0x3000000;

pub const IO_REG_SIZE: usize = 0x3ff; // 1023 bytes
pub const IO_REG_OFFSET: usize = 0x4000000;

pub const PALETTE_RAM_SIZE: usize = 0x400; // 1 kibibyte
pub const PALETTE_RAM_OFFSET: usize = 0x5000000;

pub const VRAM_SIZE: usize = 0x18000; // 96 kibibytes
pub const VRAM_OFFSET: usize = 0x6000000;

pub const OAM_SIZE: usize = 0x400; // 1 kibibyte
pub const OAM_OFFSET: usize = 0x7000000;

pub const GAME_PAK_SIZE: usize = 0x2000000; // 32 mebibytes
pub const GAME_PAK_OFFSET: usize = 0x8000000;

pub const GAME_PAK_SRAM_SIZE: usize = 0x10000; // 64 kibibytes
pub const GAME_PAK_SRAM_OFFSET: usize = 0xe000000;

pub const MEM_END: usize = 0xe010000;

pub struct Memory {
    bios:          [u8; BIOS_SIZE],
    board_wram:    [u8; BOARD_WRAM_SIZE],
    chip_wram:     [u8; CHIP_WRAM_SIZE],
    io_reg:        [u8; IO_REG_SIZE],
    pram:          [u8; PALETTE_RAM_SIZE],
    vram:          [u8; VRAM_SIZE],
    oam:           [u8; OAM_SIZE],
    game_pak:      Vec<u8>,
    game_pak_size: usize,
    game_pak_sram: [u8; GAME_PAK_SRAM_SIZE],
}

impl Memory {
    pub fn new() -> Memory {

        Memory {
            bios:          [0; BIOS_SIZE],
            board_wram:    [0; BOARD_WRAM_SIZE],
            chip_wram:     [0; CHIP_WRAM_SIZE],
            io_reg:        [0; IO_REG_SIZE],
            pram:          [0; PALETTE_RAM_SIZE],
            vram:          [0; VRAM_SIZE],
            oam:           [0; OAM_SIZE],
            game_pak:      Vec::with_capacity(GAME_PAK_SIZE),
            game_pak_size: 0,
            game_pak_sram: [0; GAME_PAK_SRAM_SIZE],
        }
    }

    pub fn load_game(&mut self, file_name: &str) -> io::Result<()> {

        log::debug!("Loading file {}", file_name);

        let reader = BufReader::new(File::open(file_name)?);

        self.game_pak_size = reader.take(GAME_PAK_SIZE as u64).read(&mut self.game_pak)?;

        Ok(())
    }

    pub fn read_byte(&self, addr: u32) -> u8 {

        let addr = addr as usize;

        match addr {
            0x00000000..=0x00003fff => return self.bios[addr - BIOS_OFFSET],
            0x02000000..=0x0203ffff => return self.board_wram[addr - BOARD_WRAM_OFFSET],
            0x03000000..=0x03007fff => return self.chip_wram[addr - CHIP_WRAM_OFFSET],
            0x04000000..=0x040003fe => return self.io_reg[addr - IO_REG_OFFSET],
            0x05000000..=0x050003ff => return self.pram[addr - PALETTE_RAM_OFFSET],
            0x06000000..=0x06017fff => return self.vram[addr - VRAM_OFFSET],
            0x07000000..=0x070003ff => return self.oam[addr - OAM_OFFSET],
            0x08000000..=0x09ffffff => return self.game_pak[addr - GAME_PAK_OFFSET],
            0x0e000000..=0x0e00ffff => return self.game_pak_sram[addr - GAME_PAK_OFFSET],

            _ => panic!("Out of bound byte read @ {:#x}", addr),
        }
    }

    pub fn read_half(&self, addr: u32) -> u16 {

        let addr = addr as usize;

        if addr & 0x1 != 0 {

            panic!("Misaligned halfword read @ {:#x}", addr);
        }

        match addr {
            0x00000000..=0x00003fff => return u16::from_le_bytes(self.bios[addr - BIOS_OFFSET..addr - BIOS_OFFSET + 2].try_into().unwrap()),
            0x02000000..=0x0203ffff => return u16::from_le_bytes(self.board_wram[addr - BOARD_WRAM_OFFSET..addr - BOARD_WRAM_OFFSET + 2].try_into().unwrap()),
            0x03000000..=0x03007fff => return u16::from_le_bytes(self.chip_wram[addr - CHIP_WRAM_OFFSET..addr - CHIP_WRAM_OFFSET + 2].try_into().unwrap()),
            0x04000000..=0x040003fe => return u16::from_le_bytes(self.io_reg[addr - IO_REG_OFFSET..addr - IO_REG_OFFSET + 2].try_into().unwrap()),
            0x05000000..=0x050003ff => return u16::from_le_bytes(self.pram[addr - PALETTE_RAM_OFFSET..addr - PALETTE_RAM_OFFSET + 2].try_into().unwrap()),
            0x06000000..=0x06017fff => return u16::from_le_bytes(self.vram[addr - VRAM_OFFSET..addr - VRAM_OFFSET + 2].try_into().unwrap()),
            0x07000000..=0x070003ff => return u16::from_le_bytes(self.oam[addr - OAM_OFFSET..addr - OAM_OFFSET + 2].try_into().unwrap()),
            0x08000000..=0x09ffffff => return u16::from_le_bytes(self.game_pak[addr - GAME_PAK_OFFSET..addr - GAME_PAK_OFFSET + 2].try_into().unwrap()),
            0x0e000000..=0x0e00ffff => return u16::from_le_bytes(self.game_pak_sram[addr - GAME_PAK_SRAM_OFFSET..addr - GAME_PAK_SRAM_OFFSET + 2].try_into().unwrap()),

            _ => panic!("Out of bound halfword read @ {:#x}", addr),
        }
    }

    pub fn read_word(&self, addr: u32) -> u32 {

        let addr = addr as usize;

        if addr & 0x11 != 0 {

            panic!("Misaligned word read @ {:#x}", addr);
        }

        match addr {
            0x00000000..=0x00003fff => return u32::from_le_bytes(self.bios[addr - BIOS_OFFSET..addr - BIOS_OFFSET + 4].try_into().unwrap()),
            0x02000000..=0x0203ffff => return u32::from_le_bytes(self.board_wram[addr - BOARD_WRAM_OFFSET..addr - BOARD_WRAM_OFFSET + 4].try_into().unwrap()),
            0x03000000..=0x03007fff => return u32::from_le_bytes(self.chip_wram[addr - CHIP_WRAM_OFFSET..addr - CHIP_WRAM_OFFSET + 4].try_into().unwrap()),
            0x04000000..=0x040003fe => return u32::from_le_bytes(self.io_reg[addr - IO_REG_OFFSET..addr - IO_REG_OFFSET + 4].try_into().unwrap()),
            0x05000000..=0x050003ff => return u32::from_le_bytes(self.pram[addr - PALETTE_RAM_OFFSET..addr - PALETTE_RAM_OFFSET + 4].try_into().unwrap()),
            0x06000000..=0x06017fff => return u32::from_le_bytes(self.vram[addr - VRAM_OFFSET..addr - VRAM_OFFSET + 4].try_into().unwrap()),
            0x07000000..=0x070003ff => return u32::from_le_bytes(self.oam[addr - OAM_OFFSET..addr - OAM_OFFSET + 4].try_into().unwrap()),
            0x08000000..=0x09ffffff => return u32::from_le_bytes(self.game_pak[addr - GAME_PAK_OFFSET..addr - GAME_PAK_OFFSET + 4].try_into().unwrap()),
            0x0e000000..=0x0e00ffff => return u32::from_le_bytes(self.game_pak_sram[addr - GAME_PAK_SRAM_OFFSET..addr - GAME_PAK_SRAM_OFFSET + 4].try_into().unwrap()),

            _ => panic!("Out of bound word read @ {:#x}", addr),
        }
    }

    pub fn read_slice(&self, addr: usize, len: usize) -> &[u8] {

        match addr {
            0x00000000..=0x00003fff => return &self.bios[addr - BIOS_OFFSET..addr - BIOS_OFFSET + len],
            0x02000000..=0x0203ffff => return &self.board_wram[addr - BOARD_WRAM_OFFSET..addr - BOARD_WRAM_OFFSET + len],
            0x03000000..=0x03007fff => return &self.chip_wram[addr - CHIP_WRAM_OFFSET..addr - CHIP_WRAM_OFFSET + len],
            0x04000000..=0x040003fe => return &self.io_reg[addr - IO_REG_OFFSET..addr - IO_REG_OFFSET + len],
            0x05000000..=0x050003ff => return &self.pram[addr - PALETTE_RAM_OFFSET..addr - PALETTE_RAM_OFFSET + len],
            0x06000000..=0x06017fff => return &self.vram[addr - VRAM_OFFSET..addr - VRAM_OFFSET + len],
            0x07000000..=0x070003ff => return &self.oam[addr - OAM_OFFSET..addr - OAM_OFFSET + len],
            0x08000000..=0x09ffffff => return &self.game_pak[addr - GAME_PAK_OFFSET..addr - GAME_PAK_OFFSET + len],
            0x0e000000..=0x0e00ffff => return &self.game_pak_sram[addr - GAME_PAK_SRAM_OFFSET..addr - GAME_PAK_SRAM_OFFSET + len],

            _ => panic!("Out of bound slice read @ {:#x}", addr),
        }
    }

    pub fn write_byte(&mut self, addr: usize, val: u8) {

        match addr {
            0x00000000..=0x00003fff => self.bios[addr - BIOS_OFFSET] = val,
            0x02000000..=0x0203ffff => self.board_wram[addr - BOARD_WRAM_OFFSET] = val,
            0x03000000..=0x03007fff => self.chip_wram[addr - CHIP_WRAM_OFFSET] = val,
            0x04000000..=0x040003fe => self.io_reg[addr - IO_REG_OFFSET] = val,
            0x05000000..=0x050003ff => self.pram[addr - PALETTE_RAM_OFFSET] = val,
            0x06000000..=0x06017fff => self.vram[addr - VRAM_OFFSET] = val,
            0x07000000..=0x070003ff => self.oam[addr - OAM_OFFSET] = val,
            0x08000000..=0x09ffffff => self.game_pak[addr - GAME_PAK_OFFSET] = val,
            0x0e000000..=0x0e00ffff => self.game_pak_sram[addr - GAME_PAK_OFFSET] = val,

            _ => panic!("Out of bound byte write @ {:#x}", addr),
        }
    }

    pub fn write_half(&mut self, addr: usize, val: u16) {

        if addr & 0x1 != 0 {

            panic!("Misaligned halfword write @ {:#x}", addr);
        }

        match addr {
            0x00000000..=0x00003fff => {

                self.bios[addr - BIOS_OFFSET] = val as u8;

                self.bios[addr - BIOS_OFFSET + 1] = (val >> 8) as u8;
            }
            0x02000000..=0x0203ffff => {

                self.board_wram[addr - BOARD_WRAM_OFFSET] = val as u8;

                self.board_wram[addr - BOARD_WRAM_OFFSET + 1] = (val >> 8) as u8;
            }
            0x03000000..=0x03007fff => {

                self.chip_wram[addr - CHIP_WRAM_OFFSET] = val as u8;

                self.chip_wram[addr - CHIP_WRAM_OFFSET + 1] = (val >> 8) as u8;
            }
            0x04000000..=0x040003fe => {

                self.io_reg[addr - IO_REG_OFFSET] = val as u8;

                self.io_reg[addr - IO_REG_OFFSET + 1] = (val >> 8) as u8;
            }
            0x05000000..=0x050003ff => {

                self.pram[addr - PALETTE_RAM_OFFSET] = val as u8;

                self.pram[addr - PALETTE_RAM_OFFSET + 1] = (val >> 8) as u8;
            }
            0x06000000..=0x06017fff => {

                self.vram[addr - VRAM_OFFSET] = val as u8;

                self.vram[addr - VRAM_OFFSET + 1] = (val >> 8) as u8;
            }
            0x07000000..=0x070003ff => {

                self.oam[addr - OAM_OFFSET] = val as u8;

                self.oam[addr - OAM_OFFSET + 1] = (val >> 8) as u8;
            }
            0x08000000..=0x09ffffff => {

                self.game_pak[addr - GAME_PAK_OFFSET] = val as u8;

                self.game_pak[addr - GAME_PAK_OFFSET + 1] = (val >> 8) as u8;
            }
            0x0e000000..=0x0e00ffff => {

                self.game_pak_sram[addr - GAME_PAK_OFFSET] = val as u8;

                self.game_pak_sram[addr - GAME_PAK_OFFSET + 1] = (val >> 8) as u8;
            }

            _ => panic!("Out of bound halfword write @ {:#x}", addr),
        }
    }

    pub fn write_word(&mut self, addr: usize, val: u32) {

        if addr & 0x11 != 0 {

            panic!("Misaligned word write @ {:#x}", addr);
        }

        match addr {
            0x00000000..=0x00003fff => {

                self.bios[addr - BIOS_OFFSET] = val as u8;

                self.bios[addr - BIOS_OFFSET + 1] = (val >> 8) as u8;

                self.bios[addr - BIOS_OFFSET + 2] = (val >> 16) as u8;

                self.bios[addr - BIOS_OFFSET + 3] = (val >> 24) as u8;
            }
            0x02000000..=0x0203ffff => {

                self.board_wram[addr - BOARD_WRAM_OFFSET] = val as u8;

                self.board_wram[addr - BOARD_WRAM_OFFSET + 1] = (val >> 8) as u8;

                self.board_wram[addr - BOARD_WRAM_OFFSET + 2] = (val >> 16) as u8;

                self.board_wram[addr - BOARD_WRAM_OFFSET + 3] = (val >> 24) as u8;
            }
            0x03000000..=0x03007fff => {

                self.chip_wram[addr - CHIP_WRAM_OFFSET] = val as u8;

                self.chip_wram[addr - CHIP_WRAM_OFFSET + 1] = (val >> 8) as u8;

                self.chip_wram[addr - CHIP_WRAM_OFFSET + 2] = (val >> 16) as u8;

                self.chip_wram[addr - CHIP_WRAM_OFFSET + 3] = (val >> 24) as u8;
            }
            0x04000000..=0x040003fe => {

                self.io_reg[addr - IO_REG_OFFSET] = val as u8;

                self.io_reg[addr - IO_REG_OFFSET + 1] = (val >> 8) as u8;

                self.io_reg[addr - IO_REG_OFFSET + 2] = (val >> 16) as u8;

                self.io_reg[addr - IO_REG_OFFSET + 3] = (val >> 24) as u8;
            }
            0x05000000..=0x050003ff => {

                self.pram[addr - PALETTE_RAM_OFFSET] = val as u8;

                self.pram[addr - PALETTE_RAM_OFFSET + 1] = (val >> 8) as u8;

                self.pram[addr - PALETTE_RAM_OFFSET + 2] = (val >> 16) as u8;

                self.pram[addr - PALETTE_RAM_OFFSET + 3] = (val >> 24) as u8;
            }
            0x06000000..=0x06017fff => {

                self.vram[addr - VRAM_OFFSET] = val as u8;

                self.vram[addr - VRAM_OFFSET + 1] = (val >> 8) as u8;

                self.vram[addr - VRAM_OFFSET + 2] = (val >> 16) as u8;

                self.vram[addr - VRAM_OFFSET + 3] = (val >> 24) as u8;
            }
            0x07000000..=0x070003ff => {

                self.oam[addr - OAM_OFFSET] = val as u8;

                self.oam[addr - OAM_OFFSET + 1] = (val >> 8) as u8;

                self.oam[addr - OAM_OFFSET + 2] = (val >> 16) as u8;

                self.oam[addr - OAM_OFFSET + 3] = (val >> 24) as u8;
            }
            0x08000000..=0x09ffffff => {

                self.game_pak[addr - GAME_PAK_OFFSET] = val as u8;

                self.game_pak[addr - GAME_PAK_OFFSET + 1] = (val >> 8) as u8;

                self.game_pak[addr - GAME_PAK_OFFSET + 2] = (val >> 16) as u8;

                self.game_pak[addr - GAME_PAK_OFFSET + 3] = (val >> 24) as u8;
            }
            0x0e000000..=0x0e00ffff => {

                self.game_pak_sram[addr - GAME_PAK_OFFSET] = val as u8;

                self.game_pak_sram[addr - GAME_PAK_OFFSET + 1] = (val >> 8) as u8;

                self.game_pak_sram[addr - GAME_PAK_OFFSET + 2] = (val >> 16) as u8;

                self.game_pak_sram[addr - GAME_PAK_OFFSET + 3] = (val >> 24) as u8;
            }

            _ => panic!("Out of bound word write @ {:#x}", addr),
        }
    }
}
