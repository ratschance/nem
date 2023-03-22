use std::{cell::RefCell, rc::Rc};

use bitflags::bitflags;

use crate::memory::Memory;

bitflags! {
    /// PPU Control Register (PPUCTRL)
    ///
    /// Definition per NESdev Wiki:
    /// ```txt
    /// 7  bit  0
    /// ---- ----
    /// VPHB SINN
    /// |||| ||||
    /// |||| ||++- Base nametable address (0 = $2000; 1 = $2400; 2 = $2800; 3 = $2C00)
    /// |||| |+--- VRAM addr increment per CPU read/write of PPUDATA (0: add 1, going across; 1: add 32, going down)
    /// |||| +---- Sprite pattern table address for 8x8 sprites (0: $0000; 1: $1000; ignored in 8x16 mode)
    /// |||+------ Background pattern table address (0: $0000; 1: $1000)
    /// ||+------- Sprite size (0: 8x8 pixels; 1: 8x16 pixels – see PPU OAM Byte 1)
    /// |+-------- PPU direction select (0: read backdrop from EXT pins; 1: output color on EXT pins)
    /// +--------- Generate an NMI at the start of the vertical blanking interval (0: off; 1: on)
    /// ```
    struct Ctrl: u8 {
        const NAMETABLE_ADDR   = 0b0000_0011;
        const VRAM_ADDR_INC    = 0b0000_0100;
        const SP_PATTERN_TABLE = 0b0000_1000;
        const BG_PATTERN_TABLE = 0b0001_0000;
        const SP_SIZE          = 0b0010_0000;
        const PPU_SEL          = 0b0100_0000;
        const VBLANK_NMI       = 0b1000_0000;
    }

    /// PPU Mask Register (PPUMASK)
    ///
    /// Definition per NESdev Wiki:
    /// ```txt
    /// 7  bit  0
    /// ---- ----
    /// BGRs bMmG
    /// |||| ||||
    /// |||| |||+- Greyscale (0: normal color, 1: produce a greyscale display)
    /// |||| ||+-- 1: Show background in leftmost 8 pixels of screen, 0: Hide
    /// |||| |+--- 1: Show sprites in leftmost 8 pixels of screen, 0: Hide
    /// |||| +---- 1: Show background
    /// |||+------ 1: Show sprites
    /// ||+------- Emphasize red (green on PAL/Dendy)
    /// |+-------- Emphasize green (red on PAL/Dendy)
    /// +--------- Emphasize blue
    /// ```
    struct Mask: u8 {
        const GREYSCALE     = 0b0000_0001;
        const SHOW_BG_LEFT8 = 0b0000_0010;
        const SHOW_SP_LEFT8 = 0b0000_0100;
        const SHOW_BG       = 0b0000_1000;
        const SHOW_SP       = 0b0001_0000;
        const EMPH_RED      = 0b0010_0000;
        const EMPH_GREEN    = 0b0100_0000;
        const EMPH_BLUE     = 0b1000_0000;
    }

    /// PPU Status Register (PPUSTATUS)
    ///
    /// Definition per NESdev Wiki:
    /// ```txt
    /// 7  bit  0
    /// ---- ----
    /// VSO. ....
    /// |||| ||||
    /// |||+-++++- PPU open bus. Returns stale PPU bus contents.
    /// ||+------- Sprite overflow. The intent was for this flag to be set
    /// ||         whenever more than eight sprites appear on a scanline, but a
    /// ||         hardware bug causes the actual behavior to be more complicated
    /// ||         and generate false positives as well as false negatives; see
    /// ||         PPU sprite evaluation. This flag is set during sprite
    /// ||         evaluation and cleared at dot 1 (the second dot) of the
    /// ||         pre-render line.
    /// |+-------- Sprite 0 Hit.  Set when a nonzero pixel of sprite 0 overlaps
    /// |          a nonzero background pixel; cleared at dot 1 of the pre-render
    /// |          line.  Used for raster timing.
    /// +--------- Vertical blank has started (0: not in vblank; 1: in vblank).
    ///            Set at dot 1 of line 241 (the line *after* the post-render
    ///            line); cleared after reading $2002 and at dot 1 of the
    ///            pre-render line.
    /// ```
    struct Status: u8 {
        const SPRITE_OVERFLOW = 0b0010_0000;
        const SPRITE_ZERO_HIT = 0b0100_0000;
        const VBLANK          = 0b1000_0000;
    }
}

/// NTSC 2C02 (PPU)
#[derive(Debug)]
pub struct N2c02 {
    reg_ctrl: Ctrl,
    reg_mask: Mask,
    reg_status: Status,
    reg_oam_addr: u8,
    reg_oam_data: u8,
    reg_scroll: u8,
    reg_addr: u8,
    reg_data: u8,
    reg_oam_dma: u8,
}

impl N2c02 {
    pub fn new() -> Self {
        Self {
            reg_ctrl: Ctrl::empty(),
            reg_mask: Mask::empty(),
            reg_status: Status::empty(),
            reg_oam_addr: 0,
            reg_oam_data: 0,
            reg_scroll: 0,
            reg_addr: 0,
            reg_data: 0,
            reg_oam_dma: 0,
        }
    }

    pub fn tick(&mut self) {
        self.reg_ctrl = Ctrl::all()
    }
}

impl Memory for N2c02 {
    fn read(&self, addr: u16) -> u8 {
        match addr {
            0x0 => self.reg_ctrl.bits(),
            0x1 => self.reg_mask.bits(),
            0x2 => self.reg_status.bits(),
            0x3 => self.reg_oam_addr,
            0x4 => self.reg_oam_data,
            0x5 => self.reg_scroll,
            0x6 => self.reg_addr,
            0x7 => self.reg_data,
            _ => {
                panic!("Invalid PPU Read at addr [#{:02x}]", addr)
            }
        }
    }

    fn write(&mut self, addr: u16, val: u8) {
        match addr {
            0x0 => self.reg_ctrl = Ctrl::from_bits(val).expect("Invalid val for Ctrl"),
            0x1 => self.reg_mask = Mask::from_bits(val).expect("Invalid val for Mask"),
            0x2 => self.reg_status = Status::from_bits(val).expect("Invalid val for Status"),
            0x3 => self.reg_oam_addr = val,
            0x4 => self.reg_oam_data = val,
            0x5 => self.reg_scroll = val,
            0x6 => self.reg_addr = val,
            0x7 => self.reg_data = val,
            _ => {
                panic!("Invalid PPU Write at addr [#{:02x}]", addr)
            }
        }
    }

    fn name(&self) -> &str {
        "2C02"
    }
}

impl Memory for Rc<RefCell<N2c02>> {
    fn read(&self, addr: u16) -> u8 {
        self.borrow().read(addr)
    }

    fn write(&mut self, addr: u16, val: u8) {
        self.borrow_mut().write(addr, val)
    }

    fn name(&self) -> &str {
        "2C02"
    }
}
