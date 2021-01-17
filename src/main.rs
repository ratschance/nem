mod nes;
mod cpu;
mod memory;

use nes::Nes;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let mut nes = Nes::new(&args[1]);
    nes.tick();
}
