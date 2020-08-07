
pub trait Memory {
    fn read(&self, addr: u8) -> u8;
    fn write(&self, addr: u8, val: u8);
}

struct Ram {
    backing_store: Vec<u8>,
}

impl Ram {
    pub fn new(size: usize) -> Self {
        Ram {
            backing_store: vec![0; size],
        }
    }
}

impl Memory for Ram {
    fn read(&self, addr: u8) -> u8 {
        self.backing_store[addr as usize]
    }

    fn write(&self, addr: u8, val: u8) {
        self.backing_store[addr as usize] = val;
    }
}

struct Rom {
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
    fn read(&self, addr: u8) -> u8 {
        self.backing_store[addr as usize]
    }

    fn write(&self, addr: u8, val: u8) {
        panic!("Attempted to write [{}] to ROM at addr [{}]", val, addr)
    }
}