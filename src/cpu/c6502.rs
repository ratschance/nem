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

const NMI_VECTOR_ADDRESS: u16 = 0xFFFA;
const RESET_VECTOR_ADDRESS: u16 = 0xFFFC;
const IRQ_VECTOR_ADDRESS: u16 = 0xFFFE;
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
    state: State,
    remaining_cycles: u8,
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

type AddrFn = fn(&mut C6502) -> (bool, u16);
type InstrFn = fn(&mut C6502, u16) -> bool;
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
            state: State::Running,
            remaining_cycles: 0,
        }
    }

    pub fn reset(&mut self) {
        self.acc = 0;
        // TODO: sp and status may not be accurate
        self.sp = 0xFF;
        self.status = Status::empty();
        self.x = 0;
        self.y = 0;
        self.state = State::Running;
        self.remaining_cycles = 7;

        self.pc = self.mmap.read16(RESET_VECTOR_ADDRESS);
    }

    pub fn interrupt(&mut self, is_nmi: bool) {
        if is_nmi || !self.status.contains(Status::IDISABLE) {
            self.status.set(Status::BLO, false); // BLO clear for hardware interrupt
            self.status.set(Status::BHI, true);
            self.push_stack((self.pc >> 8) as u8);
            self.push_stack(self.pc as u8);
            self.push_stack(self.status.bits());
            self.status.set(Status::IDISABLE, true);
            self.pc = if is_nmi {
                self.mmap.read16(NMI_VECTOR_ADDRESS)
            } else {
                self.mmap.read16(IRQ_VECTOR_ADDRESS)
            };

            // According to the Visual6502 site, an IRQ takes 7 cycles
            // http://visual6502.org/wiki/index.php?title=6502_Timing_of_Interrupt_Handling
            self.remaining_cycles = 7;
        }
    }

    pub fn tick(&mut self) {
        if self.remaining_cycles == 0 {
            let op = self.next_pc() as usize;
            let instr = &INSTRUCTIONS[op];
            let (extra_addr_cyc, addr) = (instr.addr_fn)(self);
            let extra_op_cyc = (instr.instr_fn)(self, addr);
            self.remaining_cycles += instr.cycles - 1 + (extra_addr_cyc && extra_op_cyc) as u8;
            //println!("{}", self);
        } else {
            self.remaining_cycles -= 1;
        }
    }

    pub fn get_execution_state(&self) -> &State {
        &self.state
    }

    /// Read the next byte at the PC and advance it by one
    fn next_pc(&mut self) -> u8 {
        let byte = self.mmap.read(self.pc);
        self.pc += 1;
        byte
    }

    /// Read the next two bytes at the PC and advance it by two
    fn next_pc16(&mut self) -> u16 {
        let bytes = self.mmap.read16(self.pc);
        self.pc += 2;
        bytes
    }

    fn push_stack(&mut self, val: u8) {
        self.mmap.write(STACK_OFFSET + u16::from(self.sp), val);
        self.sp = self.sp.wrapping_sub(1);
    }

    fn pop_stack(&mut self) -> u8 {
        self.sp = self.sp.wrapping_add(1);
        self.mmap.read(STACK_OFFSET + u16::from(self.sp))
    }

    #[rustfmt::skip]
    fn init_instruction_table() -> [Instruction; 256] {
        macro_rules! op {
            ($x:ident, $y:ident, $z:literal) => {
                Instruction {
                    name: stringify!($x).to_string(),
                    instr_fn: c6502_op_fns::C6502::$x,
                    addr_mode: AddrMode::$y,
                    addr_fn: c6502_addr_fns::C6502::$y,
                    cycles: $z
                }
            };
        }
        let mut instructions = [
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
        ];

        instructions[0x0A] = Instruction { name: "ASL".to_string(), instr_fn: c6502_op_fns::C6502::asl_acc, addr_mode: AddrMode::imp, addr_fn: c6502_addr_fns::C6502::imp, cycles: 2};
        instructions[0x2A] = Instruction { name: "ROL".to_string(), instr_fn: c6502_op_fns::C6502::rol_acc, addr_mode: AddrMode::imp, addr_fn: c6502_addr_fns::C6502::imp, cycles: 2};
        instructions[0x4A] = Instruction { name: "LSR".to_string(), instr_fn: c6502_op_fns::C6502::lsr_acc, addr_mode: AddrMode::imp, addr_fn: c6502_addr_fns::C6502::imp, cycles: 2};
        instructions[0x6A] = Instruction { name: "ROR".to_string(), instr_fn: c6502_op_fns::C6502::ror_acc, addr_mode: AddrMode::imp, addr_fn: c6502_addr_fns::C6502::imp, cycles: 2};
        instructions
    }
}

/// Addressing Modes
mod c6502_addr_fns {
    pub(crate) use super::C6502;

    fn is_same_page(addr1: u16, addr2: u16) -> bool {
        (addr1 & 0xFF00) == (addr2 & 0xFF00)
    }

    impl C6502 {
        /// Absolute
        /// Operand is a 16 bit address
        pub fn abs(&mut self) -> (bool, u16) {
            (false, self.next_pc16())
        }

        /// Absolute,X
        /// Operand is a 16 bit address to be added with x
        pub fn abx(&mut self) -> (bool, u16) {
            let base = self.next_pc16();
            let operand = base + self.x as u16;
            (!is_same_page(operand, base), operand)
        }

        /// Absolute,Y
        /// Operand is a 16 bit address to be added with y
        pub fn aby(&mut self) -> (bool, u16) {
            let base = self.next_pc16();
            let operand = base + self.y as u16;
            (!is_same_page(operand, base), operand)
        }

        /// Immediate
        /// Operand is an 8 bit constant
        pub fn imm(&mut self) -> (bool, u16) {
            let addr = self.pc;
            self.pc += 1;
            (false, addr)
        }

        /// Implicit
        pub fn imp(&mut self) -> (bool, u16) {
            // Nothing needs to be done
            (false, 0)
        }

        /// Relative
        /// Operand is a relative offset
        pub fn rel(&mut self) -> (bool, u16) {
            // Read and extend sign before casting to u16
            let operand = self.next_pc() as i8 as i16 as u16;
            let is_new_page = !is_same_page(self.pc, operand);
            (is_new_page, operand)
        }

        /// Indirect
        /// Operand is a 16 bit address to the low byte of another 16 bit address
        pub fn ind(&mut self) -> (bool, u16) {
            let addr = self.next_pc16();
            (false, self.mmap.read16(addr))
        }

        /// Indexed Indirect
        /// Operand is 8 bit address on zero page to be added with x, pointing to the low byte of a 16 bit address
        pub fn izx(&mut self) -> (bool, u16) {
            let zp_off = self.next_pc().wrapping_add(self.x) as u16;
            (false, self.mmap.read16(zp_off))
        }

        /// Indirect Indexed
        /// Operand is 8 bit address on zero page pointing to the low byte of a 16 bit address to be added with y
        pub fn izy(&mut self) -> (bool, u16) {
            let zp_off = self.next_pc() as u16;
            let base = self.mmap.read16(zp_off);
            let operand = base + self.y as u16;
            (!is_same_page(operand, base), operand)
        }

        /// Zero Page
        /// Operand is 8 bit address for the the first 256 bytes of memory
        pub fn zp0(&mut self) -> (bool, u16) {
            (false, self.next_pc() as u16)
        }

        /// Zero Page,X
        /// Operand is 8 bit address for the the first 256 bytes of memory to be added with x
        pub fn zpx(&mut self) -> (bool, u16) {
            let operand = self.next_pc().wrapping_add(self.x) as u16;
            (false, operand)
        }

        /// Zero Page,Y
        /// Operand is 8 bit address for the the first 256 bytes of memory to be added with y
        pub fn zpy(&mut self) -> (bool, u16) {
            let operand = self.next_pc().wrapping_add(self.y) as u16;
            (false, operand)
        }
    }
}

/// C6502 Opcode Implementations
mod c6502_op_fns {
    pub(crate) use super::C6502;
    use super::{State, Status, IRQ_VECTOR_ADDRESS};

    impl C6502 {
        fn set_zn(&mut self, val: u8) {
            self.status.set(Status::ZERO, val == 0);
            self.status.set(Status::NEGATIVE, (val & 0x80) != 0);
        }

        /// Implementation of the add routine extracted so it can be used by the ADC and SBC
        /// opcodes.
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
        fn add(&mut self, operand: u16) -> bool {
            let carry = self.status.contains(Status::CARRY) as u16;
            let val = self.acc as u16 + operand + carry;

            let out_neg = val & 0x80 == 0x80;
            let op_neg = operand & 0x80 == 0x80;
            let acc_neg = self.acc & 0x80 == 0x80;

            // handle overflow
            self.status
                .set(Status::OVERFLOW, !(acc_neg ^ op_neg) && (op_neg ^ out_neg));

            self.status.set(Status::ZERO, (val as u8) == 0);
            self.status.set(Status::NEGATIVE, out_neg);
            self.status.set(Status::CARRY, val > 0xFF);
            self.acc = val as u8;
            true
        }

        /// If `cond` is true, add the relative displacement in the operand to pc
        fn branch(&mut self, cond: bool, operand: u16) -> bool {
            if cond {
                if operand == 0xfffe {
                    self.state = State::InfiniteLoop;
                }
                self.remaining_cycles += 1; // Add one cycle for taking the branch
                self.pc = self.pc.wrapping_add(operand);
                true // Return true so extra cycles are counted if the page changed
            } else {
                false
            }
        }

        /// Register agnostic implementation of the compare routine
        /// C := X >= M, Z:= X == M, N := ((X-M) & 0x80) == 0x80
        fn compare(&mut self, val: u8, operand: u16) -> bool {
            let operand = self.mmap.read(operand);
            self.status.set(Status::CARRY, val >= operand);
            self.status.set(Status::ZERO, val == operand);
            self.status
                .set(Status::NEGATIVE, val.wrapping_sub(operand) & 0x80 != 0);
            true
        }

        /// ADC - Add with Carry
        /// Add the contents of a memory location to the acc together with the carry bit. If
        /// overflow occurs, the carry bit is set.
        pub fn adc(&mut self, addr: u16) -> bool {
            let operand = self.mmap.read(addr) as u16;
            self.add(operand)
        }

        /// AND - Logical AND
        /// Logical AND on acc with a byte of memory. Set ZN as appropriate
        pub fn and(&mut self, addr: u16) -> bool {
            self.acc &= self.mmap.read(addr);
            self.set_zn(self.acc);
            true
        }

        /// ASL - Arithmetic Shift Left
        /// Shift acc or memory left one bit. Set carry to bit 7 of old val.
        pub fn asl(&mut self, addr: u16) -> bool {
            let old_val = self.mmap.read(addr);
            let new_val = old_val << 1;
            self.mmap.write(addr, new_val);

            self.set_zn(new_val);
            self.status.set(Status::CARRY, old_val & 0x80 == 0x80);
            false
        }

        /// ASL ACC - [CUSTOM] ACC Mode Arithmetic Shift Left
        pub fn asl_acc(&mut self, _addr: u16) -> bool {
            let old_val = self.acc;
            let new_val = old_val << 1;
            self.acc = new_val;

            self.set_zn(new_val);
            self.status.set(Status::CARRY, old_val & 0x80 == 0x80);
            false
        }

        /// BCC - Branch if Carry Clear
        pub fn bcc(&mut self, addr: u16) -> bool {
            self.branch(!self.status.contains(Status::CARRY), addr)
        }

        /// BCS - Branch if Carry Set
        pub fn bcs(&mut self, addr: u16) -> bool {
            self.branch(self.status.contains(Status::CARRY), addr)
        }

        /// BEQ - Branch if Equal
        pub fn beq(&mut self, addr: u16) -> bool {
            self.branch(self.status.contains(Status::ZERO), addr)
        }

        /// BIT - Bit Test
        /// Take AND of acc and memory value to set Z. Bits 7 and 6 of the value from memory are copied into N and V
        pub fn bit(&mut self, addr: u16) -> bool {
            let mem = self.mmap.read(addr);
            self.status.set(Status::ZERO, self.acc & mem == 0x0);
            self.status.set(Status::NEGATIVE, mem & 0x80 == 0x80);
            self.status.set(Status::OVERFLOW, mem & 0x40 == 0x40);
            false
        }

        /// BMI - Branch if Minus
        pub fn bmi(&mut self, addr: u16) -> bool {
            self.branch(self.status.contains(Status::NEGATIVE), addr)
        }

        /// BNE - Branch if Not Equal
        pub fn bne(&mut self, addr: u16) -> bool {
            self.branch(!self.status.contains(Status::ZERO), addr)
        }

        /// BPL - Branch if Positive
        pub fn bpl(&mut self, addr: u16) -> bool {
            self.branch(!self.status.contains(Status::NEGATIVE), addr)
        }

        /// BRK - Force Interrupt
        /// Forces an IRQ. The pc and status are pushed onto the stack, then IRQ vector is loaded into the PC and break flag is set to 1
        pub fn brk(&mut self, _addr: u16) -> bool {
            let mut status = self.status;
            self.pc += 1; // BRK advances the saved pc an extra step
            status.set(Status::BLO, true);
            status.set(Status::BHI, true);
            self.push_stack((self.pc >> 8) as u8);
            self.push_stack(self.pc as u8);
            self.push_stack(status.bits());
            self.status.set(Status::IDISABLE, true);
            self.pc = self.mmap.read16(IRQ_VECTOR_ADDRESS);
            false
        }

        /// BVS - Branch if Overflow Clear
        pub fn bvc(&mut self, addr: u16) -> bool {
            self.branch(!self.status.contains(Status::OVERFLOW), addr)
        }

        /// BVS - Branch if Overflow Set
        pub fn bvs(&mut self, addr: u16) -> bool {
            self.branch(self.status.contains(Status::OVERFLOW), addr)
        }

        /// CLC - Clear Carry Flag
        pub fn clc(&mut self, _addr: u16) -> bool {
            self.status.set(Status::CARRY, false);
            false
        }

        /// CLD - Clear Decimal Mode
        pub fn cld(&mut self, _addr: u16) -> bool {
            self.status.set(Status::DECIMAL, false);
            false
        }

        /// CLI - Clear Interrupt Disable
        pub fn cli(&mut self, _addr: u16) -> bool {
            self.status.set(Status::IDISABLE, false);
            false
        }

        /// CLV - Clear Overflow Flag
        pub fn clv(&mut self, _addr: u16) -> bool {
            self.status.set(Status::OVERFLOW, false);
            false
        }

        /// CMP - Compare
        /// Compare contents of accumulator with a memory held value
        ///
        /// C := A >= M, Z:= A == M, N := ((A-M) & 0x80) == 0x80
        pub fn cmp(&mut self, addr: u16) -> bool {
            self.compare(self.acc, addr)
        }

        /// CPX - Compare X Register
        /// Compare contents of the X register with a memory held value
        ///
        pub fn cpx(&mut self, addr: u16) -> bool {
            self.compare(self.x, addr)
        }

        /// CPY - Compare Y Register
        /// Compare contents of the Y register with a memory held value
        ///
        /// C := Y >= M, Z:= Y == M, N := ((Y-M) & 0x80) == 0x80
        pub fn cpy(&mut self, addr: u16) -> bool {
            self.compare(self.y, addr)
        }

        /// DEC - Decrement Memory
        /// Subtract one from the value at the specified memory location. Set ZN as appropriate
        pub fn dec(&mut self, addr: u16) -> bool {
            let val = self.mmap.read(addr).wrapping_sub(1);
            self.mmap.write(addr, val);
            self.set_zn(val);
            false
        }

        /// DEX - Decrement X Register
        /// Subtract one from x. Set ZN as appropriate
        pub fn dex(&mut self, _addr: u16) -> bool {
            self.x = self.x.wrapping_sub(1) as u8;
            self.set_zn(self.x);
            false
        }

        /// DEY - Decrement Y Register
        /// Subtract one from y. Set ZN as appropriate
        pub fn dey(&mut self, _addr: u16) -> bool {
            self.y = self.y.wrapping_sub(1);
            self.set_zn(self.y);
            false
        }

        /// EOR - Exclusive OR
        /// Exclusive OR on the acc with a byte of memory. Set ZN as appropriate
        pub fn eor(&mut self, addr: u16) -> bool {
            self.acc ^= self.mmap.read(addr);
            self.set_zn(self.acc);
            true
        }

        /// INC - Increment Memory
        /// Add one to the value at the specified memory location. Set ZN as appropriate
        pub fn inc(&mut self, addr: u16) -> bool {
            let val = self.mmap.read(addr).wrapping_add(1);
            self.mmap.write(addr, val);
            self.set_zn(val);
            false
        }

        /// INX - Increment X Register
        /// Add one to x. Set ZN as appropriate
        pub fn inx(&mut self, _addr: u16) -> bool {
            self.x = self.x.wrapping_add(1);
            self.set_zn(self.x);
            false
        }

        /// INY - Increment Y Register
        /// Add one to y. Set ZN as appropriate
        pub fn iny(&mut self, _addr: u16) -> bool {
            self.y = self.y.wrapping_add(1);
            self.set_zn(self.y);
            false
        }

        /// JMP - Jump
        /// Set the pc to the address specified by the operand
        pub fn jmp(&mut self, addr: u16) -> bool {
            if addr == self.pc - 3 {
                self.state = State::InfiniteLoop;
            }
            self.pc = addr;
            false
        }

        /// JSR - Jump to Subroutine
        /// Push the address (minus one) of the return point onto the stack and set pc to the target addr
        pub fn jsr(&mut self, addr: u16) -> bool {
            let pc = self.pc - 1;
            self.push_stack((pc >> 8) as u8);
            self.push_stack(pc as u8);
            self.pc = addr;
            false
        }

        /// LDA - Load Accumulator
        /// Loads a byte of memory into acc. Set ZN as appropriate
        pub fn lda(&mut self, addr: u16) -> bool {
            self.acc = self.mmap.read(addr);
            self.set_zn(self.acc);
            true
        }

        /// LDX - Load X Register
        /// Loads a byte of memory into x. Set ZN as appropriate
        pub fn ldx(&mut self, addr: u16) -> bool {
            self.x = self.mmap.read(addr);
            self.set_zn(self.x);
            true
        }

        /// LDY - Load Y Register
        /// Loads a byte of memory into y. Set ZN as appropriate.
        pub fn ldy(&mut self, addr: u16) -> bool {
            self.y = self.mmap.read(addr);
            self.set_zn(self.y);
            true
        }

        /// LSR - Logical Shift Right
        /// Shift either acc or memory right one bit. Set carry flag to contents of old bit 0
        pub fn lsr(&mut self, addr: u16) -> bool {
            let old_val = self.mmap.read(addr);
            let new_val = old_val >> 1;
            self.mmap.write(addr, new_val);

            self.set_zn(new_val);
            self.status.set(Status::CARRY, old_val & 0x1 == 0x1);
            false
        }

        /// LSR ACC - [CUSTOM] ACC Mode Logical Shift Right
        pub fn lsr_acc(&mut self, _addr: u16) -> bool {
            let old_val = self.acc;
            let new_val = old_val >> 1;
            self.acc = new_val;

            self.set_zn(new_val);
            self.status.set(Status::CARRY, old_val & 0x1 == 0x1);
            false
        }

        /// NOP - No Operation
        pub fn nop(&mut self, _addr: u16) -> bool {
            false
        }

        pub fn ora(&mut self, addr: u16) -> bool {
            self.acc |= self.mmap.read(addr);
            self.set_zn(self.acc);
            true
        }

        /// PHA - Push Accumulator
        /// Push a copy of acc to the stack
        pub fn pha(&mut self, _addr: u16) -> bool {
            self.push_stack(self.acc);
            false
        }

        /// PHP - Push Processor Status
        /// Push a copy of status onto the stack with both Break bits set
        pub fn php(&mut self, _addr: u16) -> bool {
            let mut status = self.status;
            status.set(Status::BLO, true);
            status.set(Status::BHI, true);
            self.push_stack(status.bits());
            false
        }

        /// PLA - Pull Accumulator
        /// Pop stack and load into acc. Set ZN as appropriate.
        pub fn pla(&mut self, _addr: u16) -> bool {
            self.acc = self.pop_stack();
            self.set_zn(self.acc);
            false
        }

        /// PLP - Pull Processor Status
        /// Pop stack and load into status
        pub fn plp(&mut self, _addr: u16) -> bool {
            self.status = Status::from_bits(self.pop_stack()).expect("Invalid flags set");
            false
        }

        /// ROL - Rotate Left
        /// Move each of the bits in either A or M one place to the left. Bit 0 is old C, Old bit 7 assigned to C
        pub fn rol(&mut self, addr: u16) -> bool {
            let old_val = self.mmap.read(addr);
            let new_val = (old_val << 1) | self.status.contains(Status::CARRY) as u8;
            self.mmap.write(addr, new_val);

            self.set_zn(new_val);
            self.status.set(Status::CARRY, old_val & 0x80 == 0x80);
            false
        }

        /// ROL ACC - [CUSTOM] ACC Mode Rotate Left
        pub fn rol_acc(&mut self, _addr: u16) -> bool {
            let old_val = self.acc;
            let new_val = (old_val << 1) | self.status.contains(Status::CARRY) as u8;
            self.acc = new_val;

            self.set_zn(new_val);
            self.status.set(Status::CARRY, old_val & 0x80 == 0x80);
            false
        }

        /// ROR - Rotate Right
        /// Move each of the bits in either A or M one place to the right. Bit 7 is old C, Old bit 0 assigned to C
        pub fn ror(&mut self, addr: u16) -> bool {
            let old_val = self.mmap.read(addr);
            let new_val = (old_val >> 1) | (self.status.contains(Status::CARRY) as u8) << 7;
            self.mmap.write(addr, new_val);

            self.set_zn(new_val);
            self.status.set(Status::CARRY, old_val & 0x1 == 0x1);
            false
        }

        /// ROR ACC - [CUSTOM] ACC Mode Rotate Right
        pub fn ror_acc(&mut self, _addr: u16) -> bool {
            let old_val = self.acc;
            let new_val = (old_val >> 1) | (self.status.contains(Status::CARRY) as u8) << 7;
            self.acc = new_val;

            self.set_zn(new_val);
            self.status.set(Status::CARRY, old_val & 0x1 == 0x1);
            false
        }

        /// RTI - Return from Interrupt
        /// Used after IRQ. Pop stats from stack followed by pc
        pub fn rti(&mut self, _addr: u16) -> bool {
            self.status = Status::from_bits(self.pop_stack()).expect("Invalid flags set");
            let lo = self.pop_stack() as u16;
            let hi = self.pop_stack() as u16;
            self.pc = (hi << 8) | lo;
            false
        }

        /// RTS - Return from Subroutine
        /// Called after JSR. Pull pc (minus one) from stack
        pub fn rts(&mut self, _addr: u16) -> bool {
            let lo = self.pop_stack() as u16;
            let hi = self.pop_stack() as u16;
            self.pc = ((hi << 8) | lo) + 1;
            false
        }

        /// SBC - Subtract with Carry
        /// Subtract the contents of a memory location to the acc together with the !carry bit. If
        /// overflow occurs, the carry bit is cleared.
        pub fn sbc(&mut self, addr: u16) -> bool {
            let operand = self.mmap.read(addr) as u16;
            self.add(operand ^ 0xFF)
        }

        /// SEC - Set Carry Flag
        pub fn sec(&mut self, _addr: u16) -> bool {
            self.status.set(Status::CARRY, true);
            false
        }

        /// SED - Set Decimal Flag
        pub fn sed(&mut self, _addr: u16) -> bool {
            self.status.set(Status::DECIMAL, true);
            false
        }

        /// SEI - Set Interrupt Disable
        pub fn sei(&mut self, _addr: u16) -> bool {
            self.status.set(Status::IDISABLE, true);
            false
        }

        /// STA - Store Accumulator
        /// Store acc into memory
        pub fn sta(&mut self, addr: u16) -> bool {
            self.mmap.write(addr, self.acc);
            false
        }

        /// STX - Store X Register
        /// Store x into memory
        pub fn stx(&mut self, addr: u16) -> bool {
            self.mmap.write(addr, self.x);
            false
        }

        /// STY - Store Y Register
        /// Store y into memory
        pub fn sty(&mut self, addr: u16) -> bool {
            self.mmap.write(addr, self.y);
            false
        }

        /// TAX - Transfer Accumulator to X
        /// Copy acc to x and set ZN as appropriate
        pub fn tax(&mut self, _addr: u16) -> bool {
            self.x = self.acc;
            self.set_zn(self.acc);
            false
        }

        /// TAY - Transfer Accumulator to Y
        /// Copy acc to y and set ZN as appropriate
        pub fn tay(&mut self, _addr: u16) -> bool {
            self.y = self.acc;
            self.set_zn(self.acc);
            false
        }

        /// TSX - Transfer Stack Pointer to X
        /// Copy sp to x and set ZN as appropriate
        pub fn tsx(&mut self, _addr: u16) -> bool {
            self.x = self.sp;
            self.set_zn(self.sp);
            false
        }

        /// TXA - Transfer X to Accumulator
        /// Copy x to acc and set ZN as appropriate
        pub fn txa(&mut self, _addr: u16) -> bool {
            self.acc = self.x;
            self.set_zn(self.acc);
            false
        }

        /// TXS - Transfer X to Stack Pointer
        /// Copy x to sp
        pub fn txs(&mut self, _addr: u16) -> bool {
            self.sp = self.x;
            false
        }

        /// TYA - Transfer Y to Accumulator
        /// Copy y to acc and set ZN as appropriate
        pub fn tya(&mut self, _addr: u16) -> bool {
            self.acc = self.y;
            self.set_zn(self.acc);
            false
        }

        pub fn xxx(&mut self, _addr: u16) -> bool {
            panic!("Invalid Opcode: {}", self.mmap.read(self.pc - 1))
        }
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
        write!(
            f,
            "pc: #{:04x}, acc: #{:02x}, x: #{:02x}, y: #{:02x}, sp: #{:02x}, flags: {:?}",
            &self.pc, &self.acc, &self.x, &self.y, &self.sp, &self.status
        )
    }
}

#[cfg(test)]
impl C6502 {
    fn new_for_test() -> Self {
        use crate::memory::Ram;

        let mut mmap = MemoryMap::new();
        // Add a little scratch space
        mmap.register(0, 0xFF, Box::new(Ram::new(0x100)));
        C6502 {
            acc: 0,
            pc: 0,
            sp: 0xFF,
            status: Status::empty(),
            x: 0,
            y: 0,
            mmap: mmap,
            state: State::Running,
            remaining_cycles: 0,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn subtracts_properly() {
        let mut cpu = C6502::new_for_test();

        // -1 - 1
        cpu.acc = 255;
        cpu.mmap.write(0, 1); // Immediate mode reads operand from pc
        cpu.status.set(Status::CARRY, true);
        cpu.sbc(0);
        assert_eq!(cpu.acc, 254);
        assert_eq!(cpu.status.contains(Status::OVERFLOW), false);
        assert_eq!(cpu.status.contains(Status::CARRY), true);
        assert_eq!(cpu.status.contains(Status::ZERO), false);

        // -1 - -1
        cpu.acc = 255;
        cpu.mmap.write(0, 255);
        cpu.status.set(Status::CARRY, true);
        cpu.sbc(0);
        assert_eq!(cpu.acc, 0);
        assert_eq!(cpu.status.contains(Status::OVERFLOW), false);
        assert_eq!(cpu.status.contains(Status::CARRY), true);
        assert_eq!(cpu.status.contains(Status::ZERO), true);
        assert_eq!(cpu.status.contains(Status::NEGATIVE), false);

        // -127 - 1
        cpu.acc = 128;
        cpu.mmap.write(0, 1);
        cpu.status.set(Status::CARRY, true);
        cpu.sbc(0);
        assert_eq!(cpu.acc, 127);
        assert_eq!(cpu.status.contains(Status::OVERFLOW), true);
        assert_eq!(cpu.status.contains(Status::CARRY), true);
        assert_eq!(cpu.status.contains(Status::ZERO), false);
        assert_eq!(cpu.status.contains(Status::NEGATIVE), false);
    }

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
                // Check that execution gets stuck at the first decimal mode test since I
                // have been unable to compile the functional test suite without decimal mode
                assert_eq!(
                    cpu.pc, 0x3477,
                    "Functional test suite failed. Current State:\n {:#x?}",
                    cpu
                );
                break;
            }
        }
    }
}
