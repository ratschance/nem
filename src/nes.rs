use crate::cartridge::{Cartridge, NesCartError};
use crate::cpu::c6502::C6502;
use crate::memory::{MemoryMap, Mirrored, Ram, Rom};

#[derive(Debug)]
pub struct Nes {
    pub c6502: C6502,
}

impl Nes {
    pub fn new(rom_path: &str) -> Result<Self, NesCartError> {
        let cartridge = Cartridge::new(rom_path)?;
        let mut cpu_mmap = MemoryMap::new();
        // TODO: Mappers
        //cpu_mmap.register(0x0, 0xFFFF, Box::new(Rom::new(&bs)));
        cpu_mmap.register(
            0x0,
            0x1FFF,
            Box::new(Mirrored::new(0x800, Box::new(Ram::new(0x800)))),
        );
        Ok(Nes {
            c6502: C6502::new(cpu_mmap),
        })
    }

    pub fn tick(&mut self) {
        self.c6502.tick();
    }

    pub fn reset(&mut self) {
        self.c6502.reset();
    }
}
