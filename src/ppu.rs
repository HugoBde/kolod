#![allow(non_snake_case)]
#![cfg_attr(debug_assertions, allow(dead_code))]

use std::cell::RefCell;
use std::rc::Rc;

use crate::memory::Memory;

pub struct PPU {
    mem: Rc<RefCell<Memory>>,
}

impl PPU {
    pub fn new(mem: &Rc<RefCell<Memory>>) -> PPU {

        PPU {
            mem: mem.clone()
        }
    }

    pub fn flip(&mut self) {}
}
