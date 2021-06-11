#[macro_use]
extern crate bitflags;
#[macro_use]
extern crate lazy_static;

mod cartridge;
mod cpu;
mod memory;
mod nes;

use crate::cpu::c6502::State;
use crate::nes::Nes;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <rom>", &args[0]);
        std::process::exit(1);
    }

    let maybe_nes: Result<Nes, _> = Nes::new(&args[1]);
    if maybe_nes.is_err() {
        eprintln!("{}", maybe_nes.unwrap_err());
        eprintln!("Usage: {} <rom>", &args[0]);
        std::process::exit(1)
    }

    let mut nes = maybe_nes.unwrap();
    nes.reset();
    loop {
        if nes.c6502.get_execution_state() == &State::InfiniteLoop {
            panic!("CPU stuck in infinite loop");
        }
        nes.tick();
    }
}
