use std::cell::RefCell;
use std::io;
use std::rc::Rc;

use crate::cpu::CPU;
use crate::memory;
use crate::memory::Memory;

pub struct GBA {
    // Components
    cpu: CPU,
    mem: Rc<RefCell<Memory>>,

    // Misc
    game_loaded: bool,
}

impl GBA {
    pub fn new() -> GBA {

        let mem = RefCell::new(Memory::new());

        let mem_rc = Rc::new(mem);

        GBA {
            cpu: CPU::new(mem_rc.clone()),
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

        let mem_refcell = self.mem.borrow();

        let game_title_slice = mem_refcell.read_slice(memory::GAME_PAK_OFFSET + 0xa0, 12);

        println!("Game: {}", std::str::from_utf8(game_title_slice).unwrap());
    }
}
