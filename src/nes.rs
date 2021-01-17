use crate::cpu::c6502::C6502;
use crate::memory::{MemoryMap, Mirrored, Ram, Rom};

pub struct Nes {
    c6502: C6502,
}

impl Nes {
    pub fn new(cart: &str) -> Self {
        use std::fs::File;
        use std::io::prelude::*;

        let mut rom = File::open(cart).expect("Unable to open cartridge");
        let mut bs: [u8; 0xFFFF] = [0; 0xFFFF];

        let _ = rom.read(&mut bs).expect("Unable to read cartridge into memory");
        let mut cpu_mmap = MemoryMap::new();
        //cpu_mmap.register(0x0, 0x1FFF, Box::new(Mirrored::new(0x800, Box::new(Ram::new(0x800)))));
        cpu_mmap.register(0x0, 0xFFFF, Box::new(Rom::new(&bs)));
        Nes {
            c6502: C6502::new(cpu_mmap),
        }
    }

    pub fn tick(&mut self) {
        self.c6502.tick()
    }
}
