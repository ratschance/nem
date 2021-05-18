#[macro_use]
extern crate bitflags;
#[macro_use]
extern crate lazy_static;

mod cpu;
mod memory;
mod nes;

use crate::cpu::c6502::State;
use crate::nes::Nes;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let mut nes = Nes::new(&args[1]);
    nes.reset();
    loop {
        if nes.c6502.get_execution_state() == &State::InfiniteLoop {
            panic!("CPU stuck in infinite loop");
        }
        nes.tick();
    }
}
