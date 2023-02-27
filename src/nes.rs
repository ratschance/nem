use tracing::info;

use crate::cartridge::{Cartridge, NesCartError};
use crate::controller::Controller;
use crate::cpu::c6502::C6502;
use crate::mapper;

#[derive(Debug)]
pub struct Nes {
    pub c6502: C6502,
    controller: Controller,
}

impl Nes {
    pub fn new(rom_path: &str, sdl_context: sdl2::Sdl) -> Result<Self, NesCartError> {
        let cartridge = Cartridge::new(rom_path)?;
        info!(nes_info = ?cartridge.info);

        let cpu_mmap = mapper::map(&cartridge);
        let controller = Controller::new(&sdl_context);
        Ok(Nes {
            c6502: C6502::new(cpu_mmap),
            controller,
        })
    }

    pub fn tick(&mut self) {
        self.controller.handle_user_input();

        ::std::thread::sleep(std::time::Duration::new(0, 50_000));

        self.c6502.tick();
    }

    pub fn reset(&mut self) {
        self.c6502.reset();
    }
}
