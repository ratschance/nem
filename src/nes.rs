use std::cell::RefCell;
use std::rc::Rc;

use tracing::info;

use crate::cartridge::{Cartridge, NesCartError};
use crate::controller::Controller;
use crate::cpu::c6502::C6502;
use crate::mapper;
use crate::n2c02::N2c02;

#[derive(Debug)]
pub struct Nes {
    pub c6502: C6502,
    n2c02: Rc<RefCell<N2c02>>,
    controller: Controller,
}

impl Nes {
    pub fn new(rom_path: &str, sdl_context: sdl2::Sdl) -> Result<Self, NesCartError> {
        let cartridge = Cartridge::new(rom_path)?;
        info!(nes_info = ?cartridge.info);

        let ppu = Rc::new(RefCell::new(N2c02::new()));
        let cpu_mmap = mapper::map(&cartridge, ppu.clone());
        let controller = Controller::new(&sdl_context);
        Ok(Nes {
            c6502: C6502::new(cpu_mmap),
            n2c02: ppu,
            controller,
        })
    }

    pub fn tick(&mut self) {
        self.controller.handle_user_input();

        ::std::thread::sleep(std::time::Duration::new(0, 50_000));

        self.c6502.tick();
        self.n2c02.borrow_mut().tick()
    }

    pub fn reset(&mut self) {
        self.c6502.reset();
    }
}
