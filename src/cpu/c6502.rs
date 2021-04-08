use crate::memory::MemoryMap;

lazy_static! {
    static ref INSTRUCTIONS: [Instruction; 256] = C6502::init_instruction_table();
}

bitflags! {
    struct Status: u8 {
        const CARRY    = 0b0000_0001;
        const ZERO     = 0b0000_0010;
        const IDISABLE = 0b0000_0100;
        const DECIMAL  = 0b0000_1000;
        const BLO      = 0b0001_0000;
        const BHI      = 0b0010_0000;
        const OVERFLOW = 0b0100_0000;
        const NEGATIVE = 0b1000_0000;
    }
}

pub struct C6502 {
    acc: u8,
    pc: u16,
    sp: u8,
    status: Status,
    x: u8,
    y: u8,
    mmap: MemoryMap,
}

type AddrFn = fn(&mut C6502) -> bool;
type InstrFn = fn(&mut C6502) -> bool;
struct Instruction {
    name: String,
    instr_fn: InstrFn,
    addr_fn: AddrFn,
    cycles: u8,
}

impl C6502 {
    pub fn new(mmap: MemoryMap) -> Self {
        C6502 {
            acc: 0,
            pc: 0x400,
            sp: 0,
            status: Status::empty(),
            x: 0,
            y: 0,
            mmap,
        }
    }

    pub fn tick(&mut self) {
        let op = self.mmap.read(self.pc) as usize;
        let instr = &INSTRUCTIONS[op];
        println!("Instruction: {}, Cycles: {}", instr.name, instr.cycles);
        let _ = (instr.addr_fn)(self);
        let _ = (instr.instr_fn)(self);
        self.pc += 1;
    }

    // Addressing Modes

    fn imm(&mut self) -> bool {
        unimplemented!()
    }

    fn abs(&mut self) -> bool {
        unimplemented!()
    }

    fn zp0(&mut self) -> bool {
        unimplemented!()
    }

    fn zpx(&mut self) -> bool {
        unimplemented!()
    }

    fn zpy(&mut self) -> bool {
        unimplemented!()
    }

    fn abx(&mut self) -> bool {
        unimplemented!()
    }

    fn aby(&mut self) -> bool {
        unimplemented!()
    }

    fn imp(&mut self) -> bool {
        // Nothing needs to be done
        false
    }

    fn rel(&mut self) -> bool {
        unimplemented!()
    }

    fn ind(&mut self) -> bool {
        unimplemented!()
    }

    fn izx(&mut self) -> bool {
        unimplemented!()
    }

    fn izy(&mut self) -> bool {
        unimplemented!()
    }

    #[rustfmt::skip]
    fn init_instruction_table() -> [Instruction; 256] {
        macro_rules! op {
            ($x:ident, $y:ident, $z:literal) => {
                Instruction {
                    name: stringify!($x).to_string(),
                    instr_fn: C6502::$x,
                    addr_fn: C6502::$y,
                    cycles: $z
                }
            };
        }
        [
            op!(brk, imp, 7), op!(ora, izx, 6), op!(xxx, imp, 2), op!(xxx, imp, 8), op!(xxx, imp, 3), op!(ora, zp0, 3), op!(asl, zp0, 5), op!(xxx, imp, 5), op!(php, imp, 3), op!(ora, imm, 2), op!(asl, imp, 2), op!(xxx, imp, 2), op!(xxx, abs, 4), op!(ora, abs, 4), op!(asl, abs, 6), op!(xxx, imp, 6),
            op!(bpl, rel, 2), op!(ora, izy, 5), op!(xxx, imp, 2), op!(xxx, imp, 8), op!(xxx, imp, 4), op!(ora, zpx, 4), op!(asl, zpx, 6), op!(xxx, imp, 6), op!(clc, imp, 2), op!(ora, aby, 4), op!(xxx, imp, 2), op!(xxx, imp, 7), op!(xxx, imp, 4), op!(ora, abx, 4), op!(asl, abx, 7), op!(xxx, imp, 7),
            op!(jsr, abs, 6), op!(and, izx, 6), op!(xxx, imp, 2), op!(xxx, imp, 8), op!(bit, zp0, 3), op!(and, zp0, 3), op!(rol, zp0, 5), op!(xxx, imp, 5), op!(plp, imp, 4), op!(and, imm, 2), op!(rol, imp, 2), op!(xxx, imp, 2), op!(bit, abs, 4), op!(and, abs, 4), op!(rol, abs, 6), op!(xxx, imp, 6),
            op!(bmi, rel, 2), op!(and, izy, 5), op!(xxx, imp, 2), op!(xxx, imp, 8), op!(xxx, imp, 4), op!(and, zpx, 4), op!(rol, zpx, 6), op!(xxx, imp, 6), op!(sec, imp, 2), op!(and, aby, 4), op!(xxx, imp, 2), op!(xxx, imp, 7), op!(xxx, imp, 4), op!(and, abx, 4), op!(rol, abx, 7), op!(xxx, imp, 7),
            op!(rti, imp, 6), op!(eor, izx, 6), op!(xxx, imp, 2), op!(xxx, imp, 8), op!(xxx, imp, 3), op!(eor, zp0, 3), op!(lsr, zp0, 5), op!(xxx, imp, 5), op!(pha, imp, 3), op!(eor, imm, 2), op!(lsr, imp, 2), op!(xxx, imp, 2), op!(jmp, abs, 3), op!(eor, abs, 4), op!(lsr, abs, 6), op!(xxx, imp, 6),
            op!(bvc, rel, 2), op!(eor, izy, 5), op!(xxx, imp, 2), op!(xxx, imp, 8), op!(xxx, imp, 4), op!(eor, zpx, 4), op!(lsr, zpx, 6), op!(xxx, imp, 6), op!(cli, imp, 2), op!(eor, aby, 4), op!(xxx, imp, 2), op!(xxx, imp, 7), op!(xxx, imp, 4), op!(eor, abx, 4), op!(lsr, abx, 7), op!(xxx, imp, 7),
            op!(rts, rel, 2), op!(adc, izx, 6), op!(xxx, imp, 2), op!(xxx, imp, 8), op!(xxx, imp, 3), op!(adc, zp0, 3), op!(ror, zp0, 5), op!(xxx, imp, 5), op!(pla, imp, 4), op!(adc, imm, 2), op!(ror, imp, 2), op!(xxx, imp, 2), op!(jmp, ind, 5), op!(adc, abs, 4), op!(ror, abs, 6), op!(xxx, imp, 6),
            op!(bvs, rel, 2), op!(adc, izy, 5), op!(xxx, imp, 2), op!(xxx, imp, 8), op!(xxx, imp, 4), op!(adc, zpx, 4), op!(ror, zpx, 6), op!(xxx, imp, 6), op!(sei, imp, 2), op!(adc, aby, 4), op!(xxx, imp, 2), op!(xxx, imp, 7), op!(xxx, imp, 4), op!(adc, abx, 4), op!(ror, abx, 7), op!(xxx, imp, 7),
            op!(xxx, imp, 2), op!(sta, izx, 6), op!(xxx, imp, 2), op!(xxx, imp, 6), op!(sty, zp0, 3), op!(sta, zp0, 3), op!(stx, zp0, 3), op!(xxx, imp, 3), op!(dey, imp, 2), op!(xxx, imp, 2), op!(txa, imp, 2), op!(xxx, imp, 2), op!(sty, abs, 4), op!(sta, abs, 4), op!(stx, abs, 4), op!(xxx, imp, 4),
            op!(bcc, rel, 2), op!(sta, izy, 6), op!(xxx, imp, 2), op!(xxx, imp, 6), op!(sty, zpx, 4), op!(sta, zpx, 4), op!(stx, zpy, 4), op!(xxx, imp, 4), op!(tya, imp, 2), op!(sta, aby, 5), op!(txs, imp, 2), op!(xxx, imp, 5), op!(xxx, imp, 5), op!(sta, abx, 5), op!(xxx, imp, 5), op!(xxx, imp, 5),
            op!(ldy, imm, 2), op!(lda, izx, 6), op!(ldx, imm, 2), op!(xxx, imp, 6), op!(ldy, zp0, 3), op!(lda, zp0, 3), op!(ldx, zp0, 3), op!(xxx, zp0, 3), op!(tay, imp, 2), op!(lda, imm, 2), op!(tax, imp, 2), op!(xxx, imp, 2), op!(ldy, abs, 4), op!(lda, abs, 4), op!(ldx, abs, 4), op!(xxx, imp, 4),
            op!(bcs, rel, 2), op!(lda, izy, 5), op!(xxx, imp, 2), op!(xxx, imp, 6), op!(ldy, zpx, 4), op!(lda, zpx, 4), op!(ldx, zpy, 4), op!(xxx, imp, 4), op!(clv, imp, 2), op!(lda, aby, 4), op!(tsx, imp, 2), op!(xxx, imp, 4), op!(ldy, abx, 4), op!(lda, abx, 4), op!(ldx, aby, 4), op!(xxx, imp, 4),
            op!(cpy, imm, 2), op!(cmp, izx, 6), op!(xxx, imp, 2), op!(xxx, imp, 8), op!(cpy, zp0, 3), op!(cmp, zp0, 3), op!(dec, zp0, 5), op!(xxx, imp, 5), op!(iny, imp, 2), op!(cmp, imm, 2), op!(dex, imp, 2), op!(xxx, imp, 2), op!(cpy, abs, 4), op!(cmp, abs, 4), op!(dec, abs, 6), op!(xxx, imp, 6),
            op!(bne, rel, 2), op!(cmp, izy, 5), op!(xxx, imp, 2), op!(xxx, imp, 8), op!(xxx, imp, 4), op!(cmp, zpx, 4), op!(dec, zpx, 6), op!(xxx, imp, 6), op!(cld, imp, 2), op!(cmp, aby, 4), op!(xxx, imp, 2), op!(xxx, imp, 7), op!(xxx, imp, 4), op!(cmp, abx, 4), op!(dec, abx, 7), op!(xxx, imp, 7),
            op!(cpx, imm, 2), op!(sbc, izx, 6), op!(xxx, imp, 2), op!(xxx, imp, 8), op!(cpx, zp0, 3), op!(sbc, zp0, 3), op!(inc, zp0, 5), op!(xxx, imp, 5), op!(inx, imp, 2), op!(sbc, imm, 2), op!(nop, imp, 2), op!(xxx, imp, 2), op!(cpx, abs, 4), op!(sbc, abs, 4), op!(inc, abs, 6), op!(xxx, imp, 6),
            op!(beq, rel, 2), op!(sbc, izy, 5), op!(xxx, imp, 2), op!(xxx, imp, 8), op!(xxx, imp, 4), op!(sbc, zpx, 4), op!(inc, zpx, 6), op!(xxx, imp, 6), op!(sed, imp, 2), op!(sbc, aby, 4), op!(xxx, imp, 2), op!(xxx, imp, 7), op!(xxx, imp, 4), op!(sbc, abx, 4), op!(inc, abx, 7), op!(xxx, imp, 7),
        ]
    }

    // Instructions
    fn adc(&mut self) -> bool {
        unimplemented!()
    }

    fn and(&mut self) -> bool {
        unimplemented!()
    }

    fn asl(&mut self) -> bool {
        unimplemented!()
    }

    fn bcc(&mut self) -> bool {
        unimplemented!()
    }

    fn bcs(&mut self) -> bool {
        unimplemented!()
    }

    fn beq(&mut self) -> bool {
        unimplemented!()
    }

    fn bit(&mut self) -> bool {
        unimplemented!()
    }

    fn bmi(&mut self) -> bool {
        unimplemented!()
    }

    fn bne(&mut self) -> bool {
        unimplemented!()
    }

    fn bpl(&mut self) -> bool {
        unimplemented!()
    }

    fn brk(&mut self) -> bool {
        unimplemented!()
    }

    fn bvc(&mut self) -> bool {
        unimplemented!()
    }

    fn bvs(&mut self) -> bool {
        unimplemented!()
    }

    fn clc(&mut self) -> bool {
        unimplemented!()
    }

    fn cld(&mut self) -> bool {
        self.status.set(Status::DECIMAL, false);
        false
    }

    fn cli(&mut self) -> bool {
        unimplemented!()
    }

    fn clv(&mut self) -> bool {
        unimplemented!()
    }

    fn cmp(&mut self) -> bool {
        unimplemented!()
    }

    fn cpx(&mut self) -> bool {
        unimplemented!()
    }

    fn cpy(&mut self) -> bool {
        unimplemented!()
    }

    fn dec(&mut self) -> bool {
        unimplemented!()
    }

    fn dex(&mut self) -> bool {
        unimplemented!()
    }

    fn dey(&mut self) -> bool {
        unimplemented!()
    }

    fn eor(&mut self) -> bool {
        unimplemented!()
    }

    fn inc(&mut self) -> bool {
        unimplemented!()
    }

    fn inx(&mut self) -> bool {
        unimplemented!()
    }

    fn iny(&mut self) -> bool {
        unimplemented!()
    }

    fn jmp(&mut self) -> bool {
        unimplemented!()
    }

    fn jsr(&mut self) -> bool {
        unimplemented!()
    }

    fn lda(&mut self) -> bool {
        unimplemented!()
    }

    fn ldx(&mut self) -> bool {
        unimplemented!()
    }

    fn ldy(&mut self) -> bool {
        unimplemented!()
    }

    fn lsr(&mut self) -> bool {
        unimplemented!()
    }

    fn nop(&mut self) -> bool {
        unimplemented!()
    }

    fn ora(&mut self) -> bool {
        unimplemented!()
    }

    fn pha(&mut self) -> bool {
        unimplemented!()
    }

    fn php(&mut self) -> bool {
        unimplemented!()
    }

    fn pla(&mut self) -> bool {
        unimplemented!()
    }

    fn plp(&mut self) -> bool {
        unimplemented!()
    }

    fn rol(&mut self) -> bool {
        unimplemented!()
    }

    fn ror(&mut self) -> bool {
        unimplemented!()
    }

    fn rti(&mut self) -> bool {
        unimplemented!()
    }

    fn rts(&mut self) -> bool {
        unimplemented!()
    }

    fn sbc(&mut self) -> bool {
        unimplemented!()
    }

    fn sec(&mut self) -> bool {
        unimplemented!()
    }

    fn sed(&mut self) -> bool {
        unimplemented!()
    }

    fn sei(&mut self) -> bool {
        unimplemented!()
    }

    fn sta(&mut self) -> bool {
        unimplemented!()
    }

    fn stx(&mut self) -> bool {
        unimplemented!()
    }

    fn sty(&mut self) -> bool {
        unimplemented!()
    }

    fn tax(&mut self) -> bool {
        unimplemented!()
    }

    fn tay(&mut self) -> bool {
        unimplemented!()
    }

    fn tsx(&mut self) -> bool {
        unimplemented!()
    }

    fn txa(&mut self) -> bool {
        unimplemented!()
    }

    fn txs(&mut self) -> bool {
        unimplemented!()
    }

    fn tya(&mut self) -> bool {
        unimplemented!()
    }

    fn xxx(&mut self) -> bool {
        unimplemented!()
    }
}
