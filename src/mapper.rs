use crate::{
    cartridge::Cartridge,
    memory::{MemoryMap, Mirrored, Ram, Rom},
};

pub fn map(cartridge: &Cartridge) -> MemoryMap {
    let mut mmap = MemoryMap::new();
    mmap.register(
        0x0000,
        0x1FFF,
        Box::new(Mirrored::new(0x2000, Box::new(Ram::new(0x800)))),
    );
    //TODO PPU registers 0x2000-0x2007 mirrored to 0x3FFF
    //TODO APU and I/O 0x4000-0x4017
    //SKIP 0x4018-0x401F APU and I/O functionality that is normally disabled
    if cartridge.info.mapper == 0 {
        mmap.register(0x6000, 0x7FFF, Box::new(Ram::new(0x2000)));
        match cartridge.info.prg_rom_size {
            0x4000 => {
                mmap.register(
                    0x8000,
                    0xFFFF,
                    Box::new(Mirrored::new(
                        0x4000,
                        Box::new(Rom::new(&cartridge.prg_rom[..])),
                    )),
                );
            }
            0x8000 => {
                mmap.register(0x8000, 0xFFFF, Box::new(Rom::new(&cartridge.prg_rom[..])));
            }
            _ => {
                panic!("Invalid prg_rom_size: {}", cartridge.info.prg_rom_size);
            }
        }
    }
    mmap
}
