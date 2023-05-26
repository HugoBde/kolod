/*** STD LIB IMPORTS ***/
use std::{fs, io};

/*** LOCAL IMPORTS ***/
use crate::cpu::CPU;

pub struct GBA {
    cpu: CPU,
}

impl GBA {
    pub fn new() -> GBA {
        GBA { cpu: CPU::new() }
    }

    pub fn load_game(file_name: &str) -> io::Result<()> {

        let file_data = fs.read(file_name)?;

        Ok(())
    }
}
