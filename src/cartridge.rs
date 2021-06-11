use thiserror::Error;

#[derive(Error, Debug, PartialEq)]
pub enum NesCartError {
    #[error("Cannot read file: {0}")]
    OpenFailure(String),
    #[error("Invalid Magic")]
    InvalidMagic,
    #[error("Not enough header bytes")]
    InvalidHeaderSize,
    #[error("Unsupported ROM type")]
    UnsupportedROM,
    #[error("Unsupported console type")]
    UnsupportedConsole,
    #[error("Not enough bytes for specified ROM sizes")]
    InvalidSize,
}

/// Representation of a cartridge. Supports reading an iNES 1.0 ROM and converting it into `NesInfo` and its contained
/// program and character rom.
#[derive(Debug, PartialEq)]
pub struct Cartridge {
    info: NesInfo,
    prg_rom: Vec<u8>,
    chr_rom: Vec<u8>,
}

impl Cartridge {

    /// Parses an iNES 1.0 ROM file at the specified path and returns a Result with a Cartridge if successful.
    pub fn new(path: &str) -> Result<Self, NesCartError> {
        use std::fs::File;
        use std::io::prelude::*;

        let mut rom = File::open(path).map_err(|e| NesCartError::OpenFailure(e.to_string()))?;
        let mut raw: Vec<u8> = vec![];

        let num_bytes = rom
            .read_to_end(&mut raw)
            .map_err(|e| NesCartError::OpenFailure(e.to_string()))?;

        if num_bytes < 16 {
            return Err(NesCartError::InvalidHeaderSize);
        }

        let nes_info = NesInfo::from_header(&raw[0..16])?;
        let prg_offset = 16 + (nes_info.has_trainer as usize * 512);
        let chr_offset = prg_offset + nes_info.prg_rom_size;
        let chr_end = chr_offset + nes_info.chr_rom_size;
        if num_bytes < chr_end {
            return Err(NesCartError::InvalidSize);
        }

        Ok(Cartridge {
            info: nes_info,
            prg_rom: raw[prg_offset..chr_offset].to_vec(),
            chr_rom: raw[chr_offset..chr_end].to_vec(),
        })
    }
}

/// Mirroring modes supported on the NES
#[derive(Debug, PartialEq)]
enum Mirroring {
    Horizontal,
    Vertical,
    FourScreen,
}

/// Info that can be extracted from an iNES 1.0 ROM. Currently does not support info after flags7 as those are rare
/// in practice.
#[derive(Debug, PartialEq)]
pub struct NesInfo {
    prg_rom_size: usize,
    chr_rom_size: usize,
    mapper: u16,
    mirroring: Mirroring,
    has_trainer: bool,
    has_nvmem: bool,
}

impl NesInfo {

    /// Takes the raw bytes of a iNES 1.0 ROM, parses up to flags7 and returns a Result with a NesInfo on success.
    /// Currently only supports iNES 1.0, and the base NES console.
    pub fn from_header(bytes: &[u8]) -> Result<Self, NesCartError> {
        if &bytes[0..4] != b"NES\x1A" {
            return Err(NesCartError::InvalidMagic);
        }

        let prg_rom_size = bytes[4] as usize * 16 * 1024;
        let chr_rom_size = bytes[5] as usize * 8 * 1024;

        let flags6: u8 = bytes[6];
        let mut mapper = (flags6 as u16 & 0xF0) >> 4;
        let has_trainer = flags6 & 0b0100 > 0;
        let has_nvmem = flags6 & 0b0010 > 0;
        let mirroring = match (flags6 & 0b1000 > 0, flags6 & 0b0001 > 0) {
            (true, _) => Mirroring::FourScreen,
            (false, true) => Mirroring::Vertical,
            (false, false) => Mirroring::Horizontal,
        };

        let flags7: u8 = bytes[7];
        if flags7 & 0b1100 != 0 {
            return Err(NesCartError::UnsupportedROM);
        }
        if flags7 & 0x3 != 0 {
            return Err(NesCartError::UnsupportedConsole);
        }
        mapper |= flags7 as u16 & 0xF0;

        Ok(NesInfo {
            prg_rom_size,
            chr_rom_size,
            mapper,
            mirroring,
            has_trainer,
            has_nvmem,
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn accepts_valid_nes() {
        // flags6: 0b0110_0111 (mapper 6, no four screen, both bools, vertical)
        let raw_nes = b"NES\x1A\x04\x05\x67\x0089ABCDE";
        let maybe_nes_info = NesInfo::from_header(raw_nes);
        assert_eq!(
            maybe_nes_info,
            Ok(NesInfo {
                prg_rom_size: 4 * 16 * 1024,
                chr_rom_size: 5 * 8 * 1024,
                mapper: 6,
                mirroring: Mirroring::Vertical,
                has_trainer: true,
                has_nvmem: true
            })
        )
    }

    #[test]
    fn rejects_nes2() {
        let raw_nes = b"NES\x1A456\x0C89ABCDE";
        let maybe_nes_info = NesInfo::from_header(raw_nes);
        assert_eq!(maybe_nes_info, Err(NesCartError::UnsupportedROM))
    }

    #[test]
    fn rejects_unsupported_console() {
        let raw_nes = b"NES\x1A456\x0389ABCDE";
        let maybe_nes_info = NesInfo::from_header(raw_nes);
        assert_eq!(maybe_nes_info, Err(NesCartError::UnsupportedConsole))
    }

    #[test]
    fn rejects_invalid_magic() {
        let raw_nes = b"NES\x0A456\x0089ABCDE";
        let maybe_nes_info = NesInfo::from_header(raw_nes);
        assert_eq!(maybe_nes_info, Err(NesCartError::InvalidMagic))
    }

    #[test]
    fn rejects_invalid_file() {
        let maybe_cartridge = Cartridge::new("/not/a/valid/filesystem/path.nes");
        assert_eq!(
            maybe_cartridge,
            Err(NesCartError::OpenFailure(
                "No such file or directory (os error 2)".to_owned()
            ))
        )
    }
}
