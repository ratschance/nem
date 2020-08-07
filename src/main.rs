mod bus;
mod cpu;
mod memory;

use bus::Bus;

fn main() {
    let mut bus = Bus::init();
    bus.tick();
    let mut c6502 = cpu::c6502::C6502::init();
}
