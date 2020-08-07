use crate::c6502::C6502;

pub struct Bus {
    c6502: Box<C6502>,
    ram: [u8; 64000],
}

impl Bus {
    pub fn init() -> Self {
        Bus {
            c6502: Box::new(C6502::init()),
            ram: [0; 64000],
        }
    }

    pub fn tick(self: &mut Self) {
        self.c6502.tick(self)
    }

    pub fn write(self: &mut Self, addr: u16, val: u8){
        if addr > 0 && addr < 0xFFFF {
            self.ram[addr as usize] = val
        }
    }
}
