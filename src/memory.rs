pub trait Memory {
    /// Read one byte at the specified address
    ///
    /// # Arguments
    ///
    /// * `addr` - Address to read
    ///
    /// # Returns
    ///
    /// * `val` - Byte read at the requested address
    fn read(&self, addr: u16) -> u8;

    /// Write one byte at the speciifed address
    ///
    /// # Arguments
    ///
    /// * `addr` - Address to write to
    /// * `val` - Value to write
    fn write(&mut self, addr: u16, val: u8);

    fn name(&self) -> &str;
}

pub struct Ram {
    backing_store: Vec<u8>,
}

impl Ram {
    pub fn new(size: usize) -> Self {
        Ram {
            backing_store: vec![0; size],
        }
    }

    pub fn new_with_bs(data: &[u8]) -> Self {
        Ram {
            backing_store: data.to_owned(),
        }
    }
}

impl Memory for Ram {
    fn read(&self, addr: u16) -> u8 {
        //println!("Read@#{:04x}, #{:02x}", addr, self.backing_store[addr as usize]);
        self.backing_store[addr as usize]
    }

    fn write(&mut self, addr: u16, val: u8) {
        //println!("Update@#{:04x}, #{:02x}->#{:02x}", addr, self.backing_store[addr as usize], val);
        self.backing_store[addr as usize] = val;
    }

    fn name(&self) -> &str {
        "RAM"
    }
}

pub struct Rom {
    backing_store: Vec<u8>,
}

impl Rom {
    pub fn new(data: &[u8]) -> Self {
        Rom {
            backing_store: data.to_owned(),
        }
    }
}

impl Memory for Rom {
    fn read(&self, addr: u16) -> u8 {
        self.backing_store[addr as usize]
    }

    fn write(&mut self, addr: u16, val: u8) {
        panic!("Attempted to write [{}] to ROM at addr [{}]", val, addr)
    }

    fn name(&self) -> &str {
        "ROM"
    }
}

pub struct Mirrored {
    virtual_size: usize,
    mem: Box<dyn Memory>,
}

impl Mirrored {
    pub fn new(virtual_size: usize, mem: Box<dyn Memory>) -> Self {
        Mirrored { virtual_size, mem }
    }
}

impl Memory for Mirrored {
    fn read(&self, addr: u16) -> u8 {
        self.mem.read(addr % self.virtual_size as u16)
    }

    fn write(&mut self, addr: u16, val: u8) {
        self.mem.write(addr % self.virtual_size as u16, val)
    }

    fn name(&self) -> &str {
        "Mirrored"
    }
}

#[derive(Debug)]
pub struct MemoryMap {
    items: Vec<(u16, u16, Box<dyn Memory>)>,
}

impl std::fmt::Debug for dyn Memory {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Memory",)
    }
}

impl MemoryMap {
    pub fn new() -> Self {
        MemoryMap { items: Vec::new() }
    }

    pub fn register(&mut self, start: u16, end: u16, mem: Box<dyn Memory>) {
        self.items.push((start, end, mem))
    }

    pub fn read(&self, addr: u16) -> u8 {
        for (start, end, mem) in &self.items {
            if addr >= *start && addr <= *end {
                return mem.read(addr - start);
            }
        }
        panic!("Attempted to read address outside of mmap: {}", addr)
    }

    pub fn read16(&self, addr: u16) -> u16 {
        for (start, end, mem) in &self.items {
            if addr >= *start && addr < *end {
                return u16::from(mem.read(addr - start))
                    | (u16::from(mem.read(addr + 1 - start)) << 8);
            }
        }
        panic!("Attempted to read address outside of mmap: {}", addr)
    }

    pub fn write(&mut self, addr: u16, val: u8) {
        for (start, end, mem) in &mut self.items {
            if addr >= *start && addr <= *end {
                mem.write(addr - *start, val);
                return;
            }
        }
        panic!("Attempted to write to address outside of mmap: {}", addr)
    }
}
