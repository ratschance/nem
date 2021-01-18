#[macro_use]
extern crate bitflags;
#[macro_use]
extern crate lazy_static;

mod cpu;
mod memory;
mod nes;

use nes::Nes;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let mut nes = Nes::new(&args[1]);
    loop {
        nes.tick();
    }
}
