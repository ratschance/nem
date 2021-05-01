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

const RESET_VECTOR_ADDRESS: u16 = 0xFFFC;
const STACK_OFFSET: u16 = 0x100;

#[derive(Debug)]
pub struct C6502 {
    acc: u8,
    pc: u16,
    sp: u8,
    status: Status,
    x: u8,
    y: u8,
    mmap: MemoryMap,
    instr: &'static Instruction,
    operand: u16,
    state: State,
}

#[derive(Debug, PartialEq)]
pub enum State {
    Running,
    InfiniteLoop,
}

#[allow(non_camel_case_types)]
#[derive(Debug, PartialEq)]
enum AddrMode {
    abs,
    abx,
    aby,
    imm,
    imp,
    ind,
    izx,
    izy,
    rel,
    zp0,
    zpx,
    zpy,
}

type AddrFn = fn(&mut C6502) -> bool;
type InstrFn = fn(&mut C6502) -> bool;
struct Instruction {
    name: String,
    instr_fn: InstrFn,
    addr_mode: AddrMode,
    addr_fn: AddrFn,
    cycles: u8,
}

impl C6502 {
    pub fn new(mmap: MemoryMap) -> Self {
        C6502 {
            acc: 0,
            pc: 0,
            sp: 0xFF,
            status: Status::empty(),
            x: 0,
            y: 0,
            mmap,
            instr: &INSTRUCTIONS[2],
            operand: 0,
            state: State::Running,
        }
    }

    pub fn reset(&mut self) {
        self.acc = 0;
        self.sp = 0xFF;
        self.status = Status::empty();
        self.x = 0;
        self.y = 0;
        self.instr = &INSTRUCTIONS[2];
        self.operand = 0;
        self.state = State::Running;

        self.pc = u16::from(self.mmap.read(RESET_VECTOR_ADDRESS + 1)) << 8
            | u16::from(self.mmap.read(RESET_VECTOR_ADDRESS));
    }

    pub fn tick(&mut self) {
        self.operand = 0;

        let op = self.mmap.read(self.pc) as usize;
        self.pc += 1;
        self.instr = &INSTRUCTIONS[op];
        let _ = (self.instr.addr_fn)(self);
        let _ = (self.instr.instr_fn)(self);
        println!("{}", self);
    }

    pub fn get_execution_state(&self) -> &State {
        &self.state
    }

    fn set_zn(&mut self, val: u8) {
        self.status.set(Status::ZERO, val == 0);
        self.status.set(Status::NEGATIVE, (val & 0x80) != 0);
    }

    fn push_stack(&mut self, val: u8) {
        self.mmap.write(STACK_OFFSET + u16::from(self.sp), val);
        self.sp -= 1;
    }

    fn pop_stack(&mut self) -> u8 {
        self.sp += 1;
        self.mmap.read(STACK_OFFSET + u16::from(self.sp))
    }

    // Addressing Modes
    fn imm(&mut self) -> bool {
        // Immediate addressing uses the 2nd byte of the instruction as the address
        self.operand = self.mmap.read(self.pc) as u16;
        self.pc += 1;
        false
    }

    fn abs(&mut self) -> bool {
        let lo = self.mmap.read(self.pc) as u16;
        self.pc += 1;
        let hi = self.mmap.read(self.pc) as u16;
        self.pc += 1;

        self.operand = (hi << 8) | lo;
        false
    }

    fn zp0(&mut self) -> bool {
        self.operand = (self.mmap.read(self.pc) as u16) & 0xff;
        self.pc += 1;
        false
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
        // Read and extend sign before casting to u16
        self.operand = self.mmap.read(self.pc) as i8 as i16 as u16;
        self.pc += 1;
        false
    }

    fn ind(&mut self) -> bool {
        let lo = self.mmap.read(self.pc) as u16;
        let hi = self.mmap.read(self.pc + 1) as u16;
        self.pc += 2; // Increment PC for infinite loop detection logic
        let addr = hi << 8 | lo;
        let lo = self.mmap.read(addr) as u16;
        let hi = self.mmap.read(addr + 1) as u16;
        self.operand = hi << 8 | lo;
        false
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
                    addr_mode: AddrMode::$y,
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

    /// # ADC - Add with Carry
    /// Add the contents of a memory location to the acc together with the carry bit. If
    /// overflow occurs, the carry bit is set.
    ///
    /// The following truth table is for calculating overflow based on whether each item
    /// is negative.
    /// 
    /// | acc | op | res | O |
    /// |-----|----|-----|---|
    /// | 0   | 0  | 0   | 0 |
    /// | 0   | 0  | 1   | 1 |
    /// | 0   | 1  | 0   | 0 |
    /// | 0   | 1  | 1   | 0 |
    /// | 1   | 0  | 0   | 0 |
    /// | 1   | 0  | 1   | 0 |
    /// | 1   | 1  | 0   | 1 |
    /// | 1   | 1  | 1   | 0 |
    /// 
    /// `x'y'z + xyz' ->  !(x ^ y) && (y ^ z)`
    fn adc(&mut self) -> bool {
        let carry = self.status.contains(Status::CARRY) as u16;
        let operand = if self.instr.addr_mode == AddrMode::imm {
            self.operand
        } else {
            self.mmap.read(self.operand) as u16
        };
        let val = self.acc as u16 + operand + carry;

        let out_neg = val & 0x80 == 0x80;
        let op_neg = operand & 0x80 == 0x80;
        let acc_neg = self.acc & 0x80 == 0x80;

        // handle overflow
        self.status.set(Status::OVERFLOW, !(acc_neg ^ op_neg) && (op_neg ^ out_neg));

        self.status.set(Status::ZERO, (val as u8) == 0);
        self.status.set(Status::NEGATIVE, out_neg);
        self.status.set(Status::CARRY, val > 0xFF);
        self.acc = val as u8;
        true
    }

    /// AND - Logical AND
    /// Logical AND on the acc with a byte of memory
    fn and(&mut self) -> bool {
        self.acc &= self.mmap.read(self.operand);
        self.set_zn(self.acc);
        false
    }

    /// ASL - Arithmetic Shift Left
    /// Shift acc or memory left one bit. Set carry to bit 7 of old val.
    fn asl(&mut self) -> bool {
        let (old_val, new_val) = if self.instr.addr_mode == AddrMode::imp {
            let old_val = self.acc;
            let new_val = old_val << 1;
            self.acc = new_val;

            (old_val, new_val)
        } else {
            let old_val = self.mmap.read(self.operand);
            let new_val = old_val << 1;
            self.mmap.write(self.operand, new_val);
            (old_val, new_val)
        };
        self.set_zn(new_val);
        self.status.set(Status::CARRY, old_val & 0x80 == 0x80);
        false
    }

    fn branch(&mut self, cond: bool) -> bool {
        if cond {
            if self.operand == 0xfffe {
                self.state = State::InfiniteLoop;
            }
            self.pc = self.pc.wrapping_add(self.operand);
        }
        true //TODO: Figure out page boundary cross for extra cycles
    }

    fn bcc(&mut self) -> bool {
        self.branch(!self.status.contains(Status::CARRY))
    }

    fn bcs(&mut self) -> bool {
        self.branch(self.status.contains(Status::CARRY))
    }

    fn beq(&mut self) -> bool {
        self.branch(self.status.contains(Status::ZERO))
    }

    fn bit(&mut self) -> bool {
        unimplemented!()
    }

    fn bmi(&mut self) -> bool {
        self.branch(self.status.contains(Status::NEGATIVE))
    }

    /// BNE - Branch if Not Equal
    /// If the zero flag is clear, add the relative displacement to the program counter
    fn bne(&mut self) -> bool {
        self.branch(!self.status.contains(Status::ZERO))
    }

    fn bpl(&mut self) -> bool {
        self.branch(!self.status.contains(Status::NEGATIVE))
    }

    fn brk(&mut self) -> bool {
        unimplemented!()
    }

    fn bvc(&mut self) -> bool {
        self.branch(!self.status.contains(Status::OVERFLOW))
    }

    fn bvs(&mut self) -> bool {
        self.branch(self.status.contains(Status::OVERFLOW))
    }

    fn clc(&mut self) -> bool {
        self.status.set(Status::CARRY, false);
        false
    }

    fn cld(&mut self) -> bool {
        self.status.set(Status::DECIMAL, false);
        false
    }

    fn cli(&mut self) -> bool {
        self.status.set(Status::IDISABLE, false);
        false
    }

    fn clv(&mut self) -> bool {
        self.status.set(Status::OVERFLOW, false);
        false
    }

    /// CMP - Compare
    /// Compare contents of accumulator with a memory held value
    ///
    /// C := A >= M, Z:= A == M, N := ((A-M) & 0x80) == 0x80
    fn cmp(&mut self) -> bool {
        let val = if self.instr.addr_mode == AddrMode::imm {
            self.operand as u8
        } else {
            self.mmap.read(self.operand)
        };
        self.status.set(Status::CARRY, self.acc >= val);
        self.status.set(Status::ZERO, self.acc == val);
        self.status
            .set(Status::NEGATIVE, ((self.acc.wrapping_sub(val)) & 0x80) != 0);
        true //TODO: Figure out page boundary cross for extra cycles
    }

    /// CPX - Compare X Register
    /// Compare contents of the X register with a memory held value
    ///
    /// C := X >= M, Z:= X == M, N := ((X-M) & 0x80) == 0x80
    fn cpx(&mut self) -> bool {
        let val = if self.instr.addr_mode == AddrMode::imm {
            self.operand as u8
        } else {
            self.mmap.read(self.operand)
        };
        self.status.set(Status::CARRY, self.x >= val);
        self.status.set(Status::ZERO, self.x == val);
        self.status
            .set(Status::NEGATIVE, ((self.x.wrapping_sub(val)) & 0x80) != 0);
        true //TODO: Figure out page boundary cross for extra cycles
    }

    /// CPY - Compare Y Register
    /// Compare contents of the Y register with a memory held value
    ///
    /// C := Y >= M, Z:= Y == M, N := ((Y-M) & 0x80) == 0x80
    fn cpy(&mut self) -> bool {
        let val = if self.instr.addr_mode == AddrMode::imm {
            self.operand as u8
        } else {
            self.mmap.read(self.operand)
        };
        self.status.set(Status::CARRY, self.y >= val);
        self.status.set(Status::ZERO, self.y == val);
        self.status
            .set(Status::NEGATIVE, ((self.y.wrapping_sub(val)) & 0x80) != 0);
        true //TODO: Figure out page boundary cross for extra cycles
    }

    /// DEC - Decrement Memory
    /// Subtract one from the value held at a specified memory location and set
    /// Z,N as appropriate
    fn dec(&mut self) -> bool {
        let val = self.mmap.read(self.operand).wrapping_sub(1);
        self.mmap.write(self.operand, val);
        self.set_zn(val);
        false
    }

    /// DEX - Decrement X Register
    /// Subtract one from the X register and set Z,N as appropriate
    fn dex(&mut self) -> bool {
        //self.x = (self.x as i8).wrapping_sub(1) as u8;
        self.x = self.x.wrapping_sub(1) as u8;
        self.set_zn(self.x);
        false
    }

    /// DEY - Decrement Y Register
    /// Subtract one from the Y register and set Z,N as appropriate
    fn dey(&mut self) -> bool {
        self.y = self.y.wrapping_sub(1);
        self.set_zn(self.y);
        false
    }

    /// EOR - Exclusive OR
    /// Exclusive OR on the acc with a byte of memory
    fn eor(&mut self) -> bool {
        if self.instr.addr_mode == AddrMode::imm {
            self.acc ^= self.operand as u8;
        } else {
            self.acc ^= self.mmap.read(self.operand);
        }
        self.set_zn(self.acc);
        true //TODO: Figure out page boundary cross for extra cycles
    }

    fn inc(&mut self) -> bool {
        let val = self.mmap.read(self.operand).wrapping_add(1);
        self.mmap.write(self.operand, val);
        self.set_zn(val);
        false
    }

    fn inx(&mut self) -> bool {
        self.x = self.x.wrapping_add(1);
        self.set_zn(self.x);
        false
    }

    fn iny(&mut self) -> bool {
        self.y = self.y.wrapping_add(1);
        self.set_zn(self.y);
        false
    }

    fn jmp(&mut self) -> bool {
        if self.operand == self.pc - 3 {
            self.state = State::InfiniteLoop;
        }
        self.pc = self.operand;
        false
    }

    fn jsr(&mut self) -> bool {
        let pc = self.pc - 1;
        self.push_stack((pc >> 8) as u8);
        self.push_stack(pc as u8);
        self.pc = self.operand;
        false
    }

    /// LDA - Load Accumulator
    /// Loads a byte of memory into the accumulator setting the Zero and Negative flags as appropriate
    fn lda(&mut self) -> bool {
        if self.instr.addr_mode == AddrMode::imm {
            self.acc = self.operand as u8;
        } else {
            self.acc = self.mmap.read(self.operand);
        }
        self.set_zn(self.acc);
        true //TODO: Figure out page boundary cross for extra cycles
    }

    /// LDX - Load X Register
    /// Loads a byte of memory into the X register setting the Zero and Negative flags as appropriate
    fn ldx(&mut self) -> bool {
        if self.instr.addr_mode == AddrMode::imm {
            self.x = self.operand as u8;
        } else {
            self.x = self.mmap.read(self.operand);
        }
        self.set_zn(self.x);
        true //TODO: Figure out page boundary cross for extra cycles
    }

    /// LDY - Load Y Register
    /// Loads a byte of memory into the Y register setting the Zero and Negative flags as appropriate
    fn ldy(&mut self) -> bool {
        if self.instr.addr_mode == AddrMode::imm {
            self.y = self.operand as u8;
        } else {
            self.y = self.mmap.read(self.operand);
        }
        self.set_zn(self.y);
        true //TODO: Figure out page boundary cross for extra cycles
    }

    /// LSR - Logical Shift Right
    /// Shift either acc or memory right one bit. Set carry flag to contents of old bit 0
    fn lsr(&mut self) -> bool {
        let (old_val, new_val) = if self.instr.addr_mode == AddrMode::imp {
            let old_val = self.acc;
            let new_val = old_val >> 1;
            self.acc = new_val;

            (old_val, new_val)
        } else {
            let old_val = self.mmap.read(self.operand);
            let new_val = old_val >> 1;
            self.mmap.write(self.operand, new_val);
            (old_val, new_val)
        };
        self.set_zn(new_val);
        self.status.set(Status::CARRY, old_val & 0x1 == 0x1);
        false
    }

    fn nop(&mut self) -> bool {
        false
    }

    fn ora(&mut self) -> bool {
        unimplemented!()
    }

    fn pha(&mut self) -> bool {
        self.push_stack(self.acc);
        false
    }

    fn php(&mut self) -> bool {
        // PHP sets both bits of the break flag before moving it to the stack
        self.status.set(Status::BLO, true);
        self.status.set(Status::BHI, true);
        self.push_stack(self.status.bits());
        false
    }

    fn pla(&mut self) -> bool {
        self.acc = self.pop_stack();
        self.set_zn(self.acc);
        false
    }

    fn plp(&mut self) -> bool {
        self.status = Status::from_bits(self.pop_stack()).expect("Invalid flags set");
        false
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
        let lo = self.pop_stack() as u16;
        let hi = self.pop_stack() as u16;
        self.pc = ((hi << 8) | lo) + 1;
        false
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
        self.mmap.write(self.operand, self.acc);
        false
    }

    fn stx(&mut self) -> bool {
        unimplemented!()
    }

    fn sty(&mut self) -> bool {
        unimplemented!()
    }

    fn tax(&mut self) -> bool {
        self.x = self.acc;
        self.set_zn(self.acc);
        false
    }

    fn tay(&mut self) -> bool {
        self.y = self.acc;
        self.set_zn(self.acc);
        false
    }

    fn tsx(&mut self) -> bool {
        self.x = self.sp;
        self.set_zn(self.sp);
        false
    }

    fn txa(&mut self) -> bool {
        self.acc = self.x;
        self.set_zn(self.acc);
        false
    }

    fn txs(&mut self) -> bool {
        self.sp = self.x;
        false
    }

    fn tya(&mut self) -> bool {
        self.acc = self.y;
        self.set_zn(self.acc);
        false
    }

    fn xxx(&mut self) -> bool {
        panic!("Invalid Opcode: {}", self.mmap.read(self.pc - 1))
    }
}

impl std::fmt::Debug for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Instruction")
            .field("name", &self.name)
            .field("mode", &self.addr_mode)
            .field("cycles", &self.cycles)
            .finish()
    }
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "instr: {}, mode: {:?}", &self.name, &self.addr_mode)
    }
}

impl std::fmt::Display for C6502 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f,
             "{}, operand: #{:04x}, pc: #{:04x}, acc: #{:02x}, x: #{:02x}, y: #{:02x}, sp: #{:02x}, flags: {:?}", 
             &self.instr,
             &self.operand,
             &self.pc,
             &self.acc,
             &self.x,
             &self.y,
             &self.sp,
             &self.status)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn can_run_opcode_test() {
        use crate::memory::Ram;
        use std::fs::File;
        use std::io::prelude::*;
        use std::path::PathBuf;

        let mut cart = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        cart.push("assets/test/6502_functional_test.bin");
        let mut rom = File::open(cart).expect("Unable to open test rom");
        let mut bs = vec![];
        rom.read_to_end(&mut bs)
            .expect("Unable to read cartridge into memory");

        bs[RESET_VECTOR_ADDRESS as usize] = 0x00;
        bs[(RESET_VECTOR_ADDRESS + 1) as usize] = 0x04;

        let mut cpu_mmap = MemoryMap::new();
        cpu_mmap.register(0x0, 0xFFFF, Box::new(Ram::new_with_bs(&bs)));
        let mut cpu = C6502::new(cpu_mmap);
        cpu.reset();

        loop {
            cpu.tick();

            if cpu.get_execution_state() == &State::InfiniteLoop {
                assert_eq!(
                    cpu.pc, 0x3469,
                    "Functional test suite failed. Current State:\n {:#x?}",
                    cpu
                );
                break;
            }
        }
    }
}
