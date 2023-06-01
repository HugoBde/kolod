use std::cell::RefCell;
use std::io;
use std::rc::Rc;

use crate::cpu::CPU;
use crate::memory::Memory;
use crate::ppu::PPU;

const PPU_FLIP_PERIOD: usize = 16667;

pub struct GBA {
    // Components
    cpu: CPU,
    mem: Rc<RefCell<Memory>>,
    ppu: PPU,

    // Misc
    game_loaded: bool,
}

impl GBA {
    pub fn new() -> GBA {

        let mem = RefCell::new(Memory::new());

        let mem_rc = Rc::new(mem);

        GBA {
            cpu: CPU::new(&mem_rc),
            ppu: PPU::new(&mem_rc),

            // Keep this below any element which requires a reference to mem so that we can copy it before it is moved
            mem: mem_rc,

            game_loaded: false,
        }
    }

    pub fn load_game(&mut self, file_name: &str) -> io::Result<()> {

        let res = self.mem.borrow_mut().load_game(file_name);

        if res.is_ok() {

            self.game_loaded = true;
        }

        res
    }

    pub fn run(&mut self) {

        if !self.game_loaded {

            panic!("No game loaded!");
        }

        let mut clock_cycles = 0;

        loop {

            if clock_cycles == 0 {

                self.ppu.flip();

                clock_cycles += PPU_FLIP_PERIOD;
            }

            self.cpu.clock();
        }
    }
}
